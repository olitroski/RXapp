server <- function(input, output, session){
    # le librarie
    library(openxlsx)
    library(stringr)
    library(dplyr)
    library(shiny)

    options(warn = 2)

    # | PANEL 1: Seleccionar carpeta --------------------------------------------

    # | ---- Cargar savedir ----
    # El savedir se guarda en un archivo en el user folder de windows
    path <- reactiveValues()
    savedir_path <- file.path(Sys.getenv("USERPROFILE"), "savedir")

    # Si no existe lo crea
    if (file.exists(savedir_path) == FALSE){
        writeLines(Sys.getenv("USERPROFILE"), savedir_path)
        path$ruta <- Sys.getenv("USERPROFILE")
        setwd(Sys.getenv("USERPROFILE"))
        # Si existe lo carga
    } else {
        path$ruta <- readLines(savedir_path)
        setwd(readLines(savedir_path))
    }

    # Mostrar Path
    output$pathText <- renderPrint({
        setwd(path$ruta)
        cat(getwd())
    })

    # | ---- Cambiar de savedir ----
    observeEvent(input$pathBoton, {
        # Elije
        ruta <- choose.dir()

        # Por si cancela
        if (is.na(ruta)){
            path$ruta <- readLines(savedir_path)
            setwd(readLines(savedir_path))
            # Si sigue
        } else {
            writeLines(ruta, savedir_path)
            path$ruta <- readLines(savedir_path)
            setwd(readLines(savedir_path))
        }

    })

    # Directorio de trabajo final
    rxdir <- reactive({path$ruta})


    # | ---- Cargar RDS Poll ----
    # Primero ejecutará este
    rx.get <- function(){
        datos <- cargaRDS(rxdir())

        # si el path no tiene rx
        if (nrow(datos) == 1){
            datos
        } else {
            datos <- arrange(datos, rut, refNum, serie, rx)
            datos
        }
    }

    # Chequea el mtime
    rx.check <- function(){
        if (file.exists(file.path(rxdir(), "rxData.RDS")) == TRUE){
            modificado <- file.path(rxdir(), "rxData.RDS")
            modificado <- base::file.info(modificado)
            modificado <- modificado$mtime
            return(modificado)
        } else {
            # cat("No carga el mtime, crea file nuevo\n")
            0   # Ahora que es cero no tira de la otra función
        }
    }

    # El poll
    rxdata <- reactivePoll(2000, session, checkFunc = rx.check, valueFunc = rx.get)


    # | ---- Fitrado de etiqueta -----
    output$tablaRecuento <- renderUI({
        if (nrow(rxdata()) == 1){
            HTML("<p>Nada que mostrar</p>")

        } else {
            lab <- req(rxdata())
            lab <- unique(lab$"etiqueta")
            lab <- c(lab, "Todo")
            radioButtons("filterDir", label = NULL, choices = req(lab), selected = "Todo")
        }
    })

    # | ---- Tabla de recuentos
    output$tableDir <- renderTable({
        if (nrow(rxdata()) == 1){
            rxdata()
        } else {
            # si todo
            tabla <- otable("etiqueta", data = req(rxdata()))
            tabla <- mutate(tabla, pct = round(pct*100, 1))
            tabla <- mutate(tabla, pct = paste0(pct, "%"))
            names(tabla) <- c("Etiqueta", "Conteo", "Porcentaje")
            tabla
        }
    })

    # | ---- Data.frame ----
    output$rxTable <- renderTable({
        if (nrow(rxdata()) == 1){
            rxdata()
        } else if (req(input$filterDir) == "Todo"){
            req(rxdata())
        } else {
            filter(req(rxdata()), etiqueta == input$filterDir)
        }
    }, digits = 0)


    # Mostar N sujetos
    output$nsujeto <- renderText({
        if (nrow(rxdata()) == 1){
            "Nada que mostrar"
        } else{
            length(unique(rxdata()$rut))
        }
    })


    # | ---- Acciones Resetear -----
    resetModal <- function(){
        # El modal
        modalDialog(
            title = "Confirmar Resetear Rut",
            size = "m",
            easyClose = TRUE,

            # El mensaje
            div(p("Va a resetear las etiquetas del RUT: ", input$textrut)),

            footer = tagList(
                modalButton("Cancelar"),
                actionButton("okReset", "Resetear")
            )
        )
    }

    # Mostrar
    observeEvent(input$resetButton,{
        showModal(resetModal())
    })

    # Acciones
    observeEvent(input$okReset, {
        rut <- str_trim(input$textrut)
        datos <- rxdata()

        datos[datos$rut == rut, "etiqueta"] <- "No procesado"
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })

    # <<<<<<<<<<<<< test >>>>>>>>>>>>>>>>>>
    output$test <- renderPrint({
        is.null(input$listaRut.input)
        # paste(input$listaRut.input,
        #       input$listaRef.input,
        #       input$listaSerie.input,
        #       input$listaRX.input,
        #       sep = " - ")
    })
    # <<<<<<<<<<<<<<< >>>>>>>>>>>>>>>>>>>>>


    # | ----
    # | PANEL 2: Seleccion de los RX -------------------------------------------------
    # | -- Lista de RUT -----
    output$listaRut <- renderUI({
        validate(need(rxdata(), "Esperando input!"))
        if (nrow(rxdata()) > 1){
            rut <- filter(rxdata(), etiqueta == "No procesado")
            rut <- unique(rut$rut)
            radioButtons("listaRut.input", label = NULL, choices = req(rut), selected = rut[1])
        # Si no hay folder
        } else {
            HTML("<p>Esperando input!</p>")
        }
    })

    # | -- Lista de referencias -----
    output$listaRef <- renderUI({
        validate(need(input$listaRut.input, "Esperando input!"))
        if (nrow(rxdata()) > 1){
            reflist <- filter(rxdata(), rut == req(input$listaRut.input), etiqueta == "No procesado")
            reflist <- unique(reflist$refNum)
            radioButtons("listaRef.input", label = NULL, choices = req(reflist), selected = reflist[1])
        # Widget
        } else {
            HTML("<p>No hay referencias para cargar</p>")
        }
    })

    # | -- Lista de series -------
    output$listaSerie <- renderUI({
        validate(need(input$listaRef.input, "Esperando input!"))

        if (nrow(rxdata()) > 1){
            serie <- filter(rxdata(), rut == req(input$listaRut.input))
            serie <- filter(serie, refNum == req(input$listaRef.input), etiqueta == "No procesado")
            serie <- serie[["serie"]]
            serie <- unique(serie)

            if (length(serie) == 0){
                HTML("<p>No hay series para cargar</p>")
            } else {
                radioButtons("listaSerie.input", label = NULL, choices = req(serie))
            }

            # Widget
        } else {
            p("No hay sujeto para cargar")
        }
    })

    # | -- Lista de RX ----
    output$listaRX <- renderUI({
        validate(need(input$listaSerie.input, "Esperando input!"))

        # Si no hay folder
        if (nrow(rxdata()) > 1){
            rx <- filter(rxdata(), rut == req(input$listaRut.input))
            rx <- filter(rx, refNum == req(input$listaRef.input))
            rx <- filter(rx, serie == req(input$listaSerie.input))
            rx <- rx[["rx"]]
            rx <- unique(rx)

            # Por si no hubieran RX
            if (length(rx) == 0){
                HTML("<p>No hay rx</p>")
            } else {
                radioButtons("listaRX.input", label = NULL, choices = rx)
            }

            # Widget
        } else {
            p("No hay sujeto para cargar")
        }

    })


    # | -- Imagen RX ----
    output$rxImage <- renderImage({
        validate(need(rxdata(), "Esperando input!"))
        validate(need(input$listaRut.input, "Esperando input!"))
        validate(need(input$listaRef.input, "Esperando input!"))
        validate(need(input$listaSerie.input, "Esperando input!"))
        validate(need(input$listaRX.input, "Esperando input!"))

        # Terminar de filtrar
        img <- dplyr::filter(req(rxdata()),
                             rut == req(input$listaRut.input),
                             refNum == req(input$listaRef.input),
                             serie == req(input$listaSerie.input),
                             rx == req(input$listaRX.input)
        )

        # Cargar la imagen
        img <- img$file
        rxfile <- list(src = file.path(rxdir(), req(img)))
        rxfile

    }, deleteFile = FALSE)


    # | -- Boton Filtrar Rut ----------
    # Modeal de confirmacion
    modal_filterRUT <- function(){
        msg <- paste("Va a descartar el rut", input$listaRut.input, "y todas sus RX")
        modalDialog(
            title = "Confirmar descargar RUT",
            size = "m",
            easyClose = TRUE,
            div(span(msg)),
            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("dropRut", "Chao Rut")
            )
        )
    }

    # Mostar el modal
    observeEvent(input$chaoRUT, {
        showModal(modal_filterRUT())
    })

    # Acciones de aceptar
    observeEvent(input$dropRut, {
        rut <- input$listaRut.input

        datos <- rxdata()
        datos[datos$rut == rut, "etiqueta"] <- "Removido: Rut"
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })



    # | -- Boton filtrar referencia --------
    modal_filterRef <- function(){
        msg <- paste("Va a descartar la Referencia Num:", input$listaRef.input, "y todas sus RX")
        modalDialog(
            title = "Confirmar descargar Referencia",
            size = "m",
            easyClose = TRUE,
            div(span(msg)),
            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("dropRef", "Chao Referencia")
            )
        )
    }

    # Mostar el modal
    observeEvent(input$chaoREF, {
        showModal(modal_filterRef())
    })

    # Acciones de aceptar
    observeEvent(input$dropRef, {
        rut <- input$listaRut.input
        ref <- input$listaRef.input

        datos <- rxdata()
        datos[datos$rut == rut & datos$refNum == ref, "etiqueta"] <- "Removido: Referencia"
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })


    # | -- Boton filtrar serie ----------
    modal_filterSerie <- function(){
        msg <- paste("Va a descartar la Serie:", input$listaSerie.input, "y todas sus RX")
        modalDialog(
            title = "Confirmar descartar serie",
            size = "m",
            easyClose = TRUE,
            div(span(msg)),
            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("dropSerie", "Chao Serie")
            )
        )
    }

    # Mostar el modal serie
    observeEvent(input$chaoSERIE, {
        showModal(modal_filterSerie())
    })

    # Acciones de aceptar serie
    observeEvent(input$dropSerie, {
        # browser()
        rut <- input$listaRut.input
        ref <- input$listaRef.input
        serie <- input$listaSerie.input

        datos <- rxdata()
        datos[datos$rut == rut & datos$refNum == ref & datos$serie == serie, "etiqueta"] <- "Removido: Serie"
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })


    # | -- Boton filtrar RX -------------
    modal_filterRX <- function(){
        msg <- paste("Va a descartar el RX:", input$listaSerie.input)
        modalDialog(
            title = "Confirmar descartar RX",
            size = "m",
            easyClose = TRUE,
            div(span(msg)),
            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("dropRX", "Chao RX")
            )
        )
    }

    # Mostar el modal RX
    observeEvent(input$chaoRX, {
        showModal(modal_filterRX())
    })

    # Acciones de aceptar RX
    observeEvent(input$dropRX, {
        rut <- input$listaRut.input
        ref <- input$listaRef.input
        serie <- input$listaSerie.input
        img <- input$listaRX.input

        datos <- rxdata()
        datos[datos$rut == rut & datos$refNum == ref & datos$serie == serie & datos$rx == img, "etiqueta"] <- "Removido: RX"
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })




    # | -- Boton Confirmar RX -------------------------------------------------------------------
    modal_labelRX <- function(){
        msg <- paste("Va a etiquetar un RX:", input$listaRX.input, "y todas <br> sus imágenes")
        modalDialog(
            title = "Confirmar etiquetado de RX",
            size = "m",
            easyClose = TRUE,

            div(h4("Antecedentes del RX"),
                p("Rut: ", strong(input$listaRut.input),
                  " | Referencia: ", strong(input$listaRef.input),
                  " | Serie: ", strong(input$listaSerie.input),
                  " | Imagen: ", strong(input$listaRX.input)),
            ),
            hr(),
            div(h4("Etiquetas seleccionadas"),
                tags$ul(tags$li(input$tipoRX), tags$li(input$manoRX), tags$li(input$labelRX))
            ),

            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("etiquet", "Etiqueta RX")
            )
        )
    }

    # Mostrar RX
    observeEvent(input$chooseRX, {
        if (is.null(input$tipoRX) & is.null(input$manoRX) & is.null(input$labelRX)){
            showNotification("Debe elegir las etiqueta", closeButton = FALSE, type = "error")
        } else {
            showModal(modal_labelRX())
        }
    })

    # Acciones
    observeEvent(input$etiquet, {
        # Etiqueta la serie primero
        rut <- input$listaRut.input
        ref <- input$listaRef.input
        serie <- input$listaSerie.input
        rx <- input$listaRX.input

        datos <- rxdata()
        datos[datos$rut == rut &
              datos$refNum == ref &
              datos$serie == serie &
              datos$rx == rx, "etiqueta"] <- paste(input$tipoRX, input$manoRX, input$labelRX, sep = " - ")

        # Ahora etiqueta el RX
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })


    # | -- Terminar RUT ---------------------------------------------------------------------------
    # Diseno del modal terminar sujeto
    modal_finRUT <- function(){
        msg <- paste("Finalizar:", input$listaRX.input)
        modalDialog(
            title = "Confirmar terminar sujeto",
            size = "m",
            easyClose = TRUE,

            div(h4("Terminar analisis del sujeto"),
                p("Rut: ", strong(input$listaRut.input),
                  " todas las imágenes no etiquetadas serán descargadas")
            ),

            footer = tagList(
                modalButton("No, me arrepenti"),
                actionButton("terminar", "Finalizar RUT")
            )
        )
    }

    # Mostrar modal terminar sujeto
    observeEvent(input$terminarRX, {
        if (is.null(input$tipoRX) & is.null(input$manoRX) & is.null(input$labelRX)){
            showNotification("Debe elegir las etiqueta", closeButton = FALSE, type = "error")
        } else {
            showModal(modal_finRUT())
        }
    })

    # Acciones para terminar sujeto
    observeEvent(input$terminar, {
        # Antecedentes
        rut <- input$listaRut.input
        ref <- input$listaRef.input
        serie <- input$listaSerie.input
        rx <- input$listaRX.input

        # Hacer los cambios
        datos <- rxdata()
        datos[datos$rut == rut &
              datos$etiqueta == "No procesado", "etiqueta"] <- "RX Descartado"

        # Guardar
        saveRDS(datos, file.path(rxdir(), "rxData.RDS"))
        removeModal()
    })


}


