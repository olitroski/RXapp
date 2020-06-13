server <- function(input, output, session){
    # le librarie
    library(openxlsx)
    library(stringr)
    library(dplyr)
    library(shiny)


    # | PANEL 1: Seleccionar carpeta --------------------------------------------
    # | -- ChooseFolder, Path y Dir ----
    path <- reactiveValues()

    # ---- Checar exista savedir ---- #   y cargar el set
    # El savedir se guarda en un archivo en el user folder de windows
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

    # ---- Boton choose dir ---- #     <<<< path$ruta >>>>
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

    # Mostrar Path
    output$pathText <- renderPrint({
        setwd(path$ruta)
        cat(getwd())
    })

    # En reactivo por si cambia
    rxdir <- reactive({path$ruta})

    # Cargar el directorio al reactivo
    rxdata <- reactive({
        cargaRDS(rxdir())
    })


    # Cargar en la pagina el data frame ----------------------------------------------
    output$rxTable <- renderDataTable({
        rxdata()
    })




    output$test <- renderPrint({
        rxdir()
    })







    wd <- reactiveValues(chooseFolder = "Ingresar directorio")
    # observe({
    #     if (input$folderBtn > 0){
    #         wd$chooseFolder <- choose.dir(caption = "Seleccionar carpeta de sujetos")
    #     }
    # })

    # | -- Mostrar path
    output$folderDir <- renderPrint({
        if (is.na(wd$chooseFolder)){
            cat("Ingresar directorio")
        } else {
            cat(wd$chooseFolder)
        }
    })

    # | -- Mostrar contenido
    output$folderShow <- renderPrint({
        # No hay
        if (wd$chooseFolder == "Ingresar directorio" | is.na(wd$chooseFolder)){
            "Ingresar directorio"

        # es NA
        } else {
            dir(wd$chooseFolder)
        }
    })

    # | -- Reactive con chequeos ----
    okfolder <- reactive({
        if (wd$chooseFolder == "Ingresar directorio"){
            "Ingresar directorio"
        } else {
            # Determinar si hay fotos JPG
            archivos <- dir(wd$chooseFolder)
            archivos <- archivos[dir.exists(file.path(wd$chooseFolder, archivos))]
            archivos <- file.path(wd$chooseFolder, archivos)

            # Revisar todos los folders
            rxs <- lapply(archivos, dir)

            nfile <- lapply(rxs, length)
            nfile <- as.numeric(paste(nfile))

            njpg <- lapply(rxs, function(x) length(grep(".jpg", x)))
            njpg <- as.numeric(paste(njpg))

            # Hay folders sin jpgs
            if (sum(njpg == 0) > 0){
                sinjpg <- archivos[njpg == 0]
                for (f in sinjpg){
                    cat("Sin RX: ", f, "\n")
                }
                jpg <- FALSE
            } else {
                jpg <- TRUE
            }

            # Folders con nfiles difernte njpg
            if (sum(!(nfile == njpg)) > 0){
                difn <- archivos[!(nfile == njpg)]
                for (f in difn){
                    cat("nfile.dif.njpg: ", f, "\n")
                }
                n <- FALSE
            } else {
                n <- TRUE
            }

            # Determinar que hay excel
            xls <- file.exists(file.path(wd$chooseFolder, "rx_status.xlsx"))

            # Fabricar la lista de comprobación
            list(jpg = jpg, xls = xls, n = n)
        }
    })


    # | -- Ventana de confirmacion ------
    # Configuración ventanita
    cargarModal <- function(){
        if (okfolder()$xls == FALSE){
            show <- "El Excel de registros no existe, se va a crear uno nuevo"
        } else {
            show <- "El Excel de registros existe, comenzar a editar"
        }

        modalDialog(
            title = "Cargar directorio seleccionado",
            size = "m",
            easyClose = TRUE,
            # Mensaje
            div(span(show)),
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("cargarOK", "Confirmar")
            )
        )
    }

    # Mostrar ventanita
    observeEvent(input$folderLoad, {
        if (wd$chooseFolder == "Ingresar directorio"){
            showNotification("Ingresar directorio", closeButton = FALSE, type = "warning")

        } else {
            # Si jpg falso
            if (okfolder()$jpg == FALSE){
                showNotification("Directorios sin imagenes, revisar consola ",
                                 closeButton = FALSE, type = "error", duration = 8)

            # Diferencias en archivos
            } else if (okfolder()$n == FALSE) {
                showNotification("Directorios con archivos extra, revisar consola ",
                                 closeButton = TRUE, type = "error", duration = 30)

            # Si hay excel
            } else if (okfolder()$xls == TRUE){
                updateNavbarPage(session, inputId = "TablasApp", selected = "Etiquetar RX")

            # Si no hay excel
            } else {
                showModal(cargarModal())
            }
        }
    })

    # Cargar
    observeEvent(input$cargarOK, {
        if (okfolder()$xls == FALSE){
            # Folders
            archivos <- dir(wd$chooseFolder)
            archivos <- archivos[dir.exists(file.path(wd$chooseFolder, archivos))]
            folderData <- data.frame(folder = archivos, stringsAsFactors = FALSE)
            # folderData <- mutate(folderData, sirveSubj = as.character(NA),
            #                      rxFile = as.character(NA), tagLesion = as.character(NA))
            folderData <- mutate(folderData, sirveSubj = "", rxFile = "", tagLesion = "")

            # Hacer el excel
            excel <- createWorkbook()
            addWorksheet(excel, "Sujetos")
            writeData(excel, "Sujetos", folderData)
            saveWorkbook(excel, file.path(wd$chooseFolder, "rx_status.xlsx"), overwrite = TRUE)

            # Para la siguiente pestaña
            updateNavbarPage(session, inputId = "TablasApp", selected = "Etiquetar RX")
            removeModal()
        } else {
            # Para la siguiente pestaña
            updateNavbarPage(session, inputId = "TablasApp", selected = "Etiquetar RX")
            removeModal()
        }
    })


    # | -----
    # | -- Reactive POLL para el Excel -------------------------------------
    # Funcion leer fecha de modificacion
    xlsx.check <- function(){
        # Hay directorio
        if (dir.exists(wd$chooseFolder)){
            fichero <- file.path(wd$chooseFolder, "rx_status.xlsx")
            if (file.exists(fichero)){
                info <- base::file.info(fichero)
                info <- info$mtime
                return(info)
            } else {
                return("xls0")
            }

        # Se cerro el dialogo
        } else if (is.na(wd$chooseFolder) | wd$chooseFolder == "Ingresar directorio") {
            return("xls0")
        }


        # if (wd$chooseFolder != "Ingresar directorio"){
        #     fichero <- file.path(wd$chooseFolder, "rx_status.xlsx")
        #     if (file.exists(fichero)){
        #         info <- base::file.info(fichero)
        #         info <- info$mtime
        #         return(info)
        #     } else {
        #         return("ola k ase 1")
        #     }
        # } else {
        #     return("ola k ase 2")
        # }
    }

    # Funcion get si es que cambia
    xlsx.get <- function(){
        # Por si no hay nada seleccionado
        if (wd$chooseFolder != "Ingresar directorio"){
            fichero <- file.path(wd$chooseFolder, "rx_status.xlsx")
            if (file.exists(fichero)){
                return(read.xlsx(fichero))
            }
        } else {
            "Ingresar directorio"
        }
    }

    # EL poll
    xlsxRX <- reactivePoll(250, session, checkFunc = xlsx.check, valueFunc = xlsx.get)


    # | ----
    # | PANEL 2: Seleccion de los RX -------------------------------------------------
    # Listado de folders sin edicion
    ch <- reactive({
        # Hay diractorio pero no sirve
        if (is.null(xlsxRX())){
            ch <- length(logical(0))
            ch

        # Directorio y sirve
        } else  {
            # capturar la data
            folderData <- xlsxRX()
            folderData <- mutate(folderData, sub = ifelse(sirveSubj == "", 1, 0),
                                 rxf = ifelse(rxFile == "",1 , 0),
                                 tag = ifelse(tagLesion == "", 1, 0))
            folderData <- mutate(folderData, status = sub + rxf + tag)
            folderData <- filter(folderData, status == 3)
            ch <- folderData$folder
            ch
        }
    })


    # | -- Lista de directorios -----
    output$listaFolders <- renderUI({
        # Si no hay folder
        if (wd$chooseFolder == "Ingresar directorio" | is.na(wd$chooseFolder)){
            textInput("nara1", label = NULL, value = "No hay directorio")

        # Si hubiera un problema de ficheros njpg
        } else if (okfolder()$jpg == FALSE){
            textInput("nara4", label = NULL, value = "Directorios sin imagenes")

        # Por si no quedan archivos
        } else if (length(ch()) == 0){
            textInput("nara3", label = NULL, value = "Todos los RX procesados")

        # No se ha cargado el xlsx
        } else if (length(ch()) == 1) {
            if (ch() == 0){
                textInput("nara1", label = NULL, value = "Cargar directorio")
            } else {
                textInput("nara1", label = NULL, value = "Directorio solo tiene 1 folder")
            }

        # Widget
        } else {
            radioButtons("listaFolders.input", label = NULL, choices = ch())
        }
    })


    # | -- Lista de RX -----
    output$listaRX <- renderUI({
        if (wd$chooseFolder == "Ingresar directorio" | is.na(wd$chooseFolder)){
            textInput("nara2", label = NULL, value = "No hay directorio")

        # Si nos gastamos todo
        } else if (length(ch()) == 0){
            textInput("nara31", label = NULL, value = "Todos los RX procesados")

        } else {
            # Esperar
            validate(need(input$listaFolders.input, "Esperando selección..."))
            # Cargar directorio
            archivos <- dir(file.path(wd$chooseFolder, input$listaFolders.input))
            radioButtons("listRX.input", label = NULL, choices = archivos)
        }
    })


    # | Seccion de edición -----------------------------------------------------
    # | -- Mostar modal ----
    observeEvent(input$rxDesicion, {
        if (wd$chooseFolder == "Ingresar directorio"){
            # Que hay directoirio
            showNotification("No hay directorio", closeButton = FALSE, type = "error")
        } else {
            # Condiciones válidas
            if (input$sirveSubj == "Sirve" & input$sirveRX == "RX definitivo" & input$sirveTag != "No Asignada"){
                showModal(modalSetRX())

            # Condiciones de dropear sujeto
            } else if (input$sirveSubj == "No sirve" & input$sirveRX == "No sirve" & input$sirveTag == "No Asignada"){
                showModal(modalDropRX())

            # Condiciones mal cofiguradas
            } else {
                showNotification("Configuración de valores inválida", closeButton = FALSE, type = "error")
            }
        }
    })


    # | -- Sirve Modal -----
    modalSetRX <- function(){
        modalDialog(
            title = "Confirmar registro de RX",
            size = "m",
            easyClose = TRUE,

            # Mensaje
            div(span(p(strong("Archivo: "), code(input$listRX.input))),
                span(p(strong("Sujeto: "),   code(input$sirveSubj))),
                span(p(strong("Imagen: "),   code(input$sirveRX))),
                span(p(strong("Etiqueta: "), code(input$sirveTag))),
                span(p("Los 3 valores se guardarán en el Excel de edición"))
            ),

            footer = tagList(
                modalButton("Cancelar"),
                actionButton("ModificarRX", "Etiquetar RX")
            )
        )
    }

    # | -- Sirve excel -----
    observeEvent(input$ModificarRX, {
        df <- xlsxRX()
        # Guardar resto
        resto <- filter(df, folder != input$listaFolders.input)
        # El editado
        edit <- data.frame(folder = input$listaFolders.input,
                           sirveSubj = input$sirveSubj,
                           rxFile = input$listRX.input,
                           tagLesion = input$sirveTag, stringsAsFactors = FALSE)
        df <- bind_rows(resto, edit)
        # Lo del excel
        excel <- createWorkbook()
        addWorksheet(excel, "Sujetos")
        writeData(excel, "Sujetos", df)
        saveWorkbook(excel, file.path(wd$chooseFolder, "rx_status.xlsx"), overwrite = TRUE)

        removeModal()
    })


    # | -- Drop Modal ----
    modalDropRX <- function(){
        modalDialog(
            title = "Confirmar excluir sujeto",
            size = "m",
            easyClose = TRUE,

            # Mensaje
            div(span(h3("Va a pasar el sujeto a", strong("NO SIRVE")))),

            footer = tagList(
                modalButton("Cancelar"),
                actionButton("dropRX", "RX no sirve")
            )
        )
    }

    # | -- Drop Excel ----
    observeEvent(input$dropRX, {
        df <- xlsxRX()
        # Guardar resto
        resto <- filter(df, folder != input$listaFolders.input)
        # El editado
        edit <- data.frame(folder = input$listaFolders.input,
                           sirveSubj = input$sirveSubj,
                           rxFile = "",
                           tagLesion = "", stringsAsFactors = FALSE)
        df <- bind_rows(resto, edit)
        # Lo del excel
        excel <- createWorkbook()
        addWorksheet(excel, "Sujetos")
        writeData(excel, "Sujetos", df)
        saveWorkbook(excel, file.path(wd$chooseFolder, "rx_status.xlsx"), overwrite = TRUE)

        removeModal()
    })


    # | Sección imagen ---------------------------------------------------------
    # | -- Mostrar filename ----
    output$rxFile <- renderPrint({
        if (wd$chooseFolder == "Ingresar directorio" | is.na(wd$chooseFolder)){
            cat("Esperando directorio...   :)")

        } else if (length(ch()) == 0){
            cat("Todo procesado")

        } else {
            cat(input$listaFolders.input, "|", input$listRX.input)
        }
    })

    # | -- Mostar RX ----
    output$rxImage <- renderImage({
        validate(need(input$listRX.input, "Esperando input!"))
        rxfile <- file.path(wd$chooseFolder, input$listaFolders.input, input$listRX.input)
        list(src = rxfile)
    }, deleteFile = FALSE)



    # | ----
    # | PANEL 3: Tabla listos -------------------------------------------------
    output$rxlistos <- renderTable({
        xlsxRX()
    }, striped = TRUE, spacing = "m", )

    # output$test <- renderPrint({
    #     # paste(ch(), "----", wd$chooseFolder)
    # })
}

