ui <- navbarPage(
    "Barbi App", id = "TablasApp",

    # | Tab: Seleccionar archivos -----------------------------------------
    tabPanel("Select Folder",
        # CSS
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "estilos.css")),

        # | --- Encabezado y botón ----
        fluidRow(
            column(12,
                h4("Seleccionar el folder de sujetos"),
                p("El directorio debe contener todos los archivos de RX, se usen o no y en formato",
                  code('jpg'), br(),
                  "Este directorio no se debe modificar, no agregar archivos o carpetas.",
                  strong("Dejar siempre original,"), "solo existe el archivo RDS aparte de los RX"),
            )
        ),

        fluidRow(class = "buscar-path",
            # column(7,
                   # Muestra ruta
                   verbatimTextOutput("pathText", placeholder = TRUE),
            # ),
            # column(5,
                   # Carga ruta
                   actionButton("pathBoton", "Buscar...", icon = icon("folder"), width = "110px")
            # )
        ),
        hr(),

        # verbatimTextOutput("test", placeholder = TRUE),

        # | --- Mostrar el data.frame con opciones de filtrado ------
        fluidRow(class = "tabla-filtros",
            div(
            # column(7, offset = 1,
                tableOutput("rxTable")
            # ),
            ),
            div(
            # column(4,
                # Seleccion de filtro
                h4("Filtrar según Status"),
                uiOutput("tablaRecuento"),
                br(),

                # Tabla de recuentos
                h4("Recuentos"),
                tableOutput("tableDir"),
                br(),

                # Conteo de Rut
                h4("Recuento Sujetos"),
                textOutput("nsujeto"),
                br(),

                # Resetear Rut
                h4("Resetear un Rut"),
                textInput("textrut", label = NULL, width = 200, placeholder = "Rut sin espacios"),
                # uiOutput("resetRut"),
                actionButton("resetButton", label ="Reset Rut", icon = icon("warning"))
            # )
            )
        )

    ),



    # | ----
    # | Tab: App  --------------------------------------------------
    tabPanel("Etiquetar RX",
        # Pagina única no más
        fluidRow(
            # | Sidebar ----
            column(1,
                # | -- Sujetos a editar ----
                h4("Rut"),
                p("No analizados"),
                # hr(),
                # div(style = "overflow-y: scroll; height: 450px",
                #     uiOutput("listaRut")
                # )
                uiOutput("listaRut")
            ),

            column(2,
                # | -- Referencia -----
                h4("Referencia"),
                # p("Set de RX de una fecha y hora determinada"),
                uiOutput("referencia"),
                hr(),

                # | -- Serie -----
                h4("Serie"),
                # p("Agrupación de RX"),
                uiOutput("series"),
                hr(),

                # | -- RX -------
                h4("Imagen"),
                uiOutput("rayos"),
                hr(),

                # | -- Botones de filtraje ------
                h4("Filtrar"),
                p("Descartar las imágenes que no sirven"),
                actionButton("chaoRUT", label = "Descartar Rut", width = 180, ),
                br(), br(),
                actionButton("chaoREF", label = "Descartar Referencia", width = 180),
                br(), br(),
                actionButton("chaoSERIE", label = "Descartar Serie", width = 180)
            ),

            # | Main ----
            # Testeo de cosas
            # verbatimTextOutput("test", placeholder = TRUE),

            # | -- La foto ------------------------------------------------------------------------
            column(7,
                imageOutput("rxImage")
            ),

            # | -- Columna de etiquetados ---------------------------------------------------------
            column(2,
                # Encabezado
                fluidRow(
                    h4("Elegir RX del sujeto"),
                    p("Seleccionar RX para estudio marcando tipo imagen y etiqueta correspondiente"),
                    hr(),

                    # Tipo RX
                    div(style = "padding-bottom:10px;",
                        radioButtons("tipoRX", label = "Seleccionar tipo de RX", choices = c("Antero-Posterior", "Lateral"),
                                     inline = TRUE, selected = character(0))
                    ),

                    # Mano
                    div(style = "padding-bottom:10px;",
                        radioButtons("manoRX", label = "Identificar mano RX", choices = c("Izquierda", "Derecha"),
                                     inline = TRUE, selected = character(0))
                    ),

                    # Etiqueta
                    div(style = "",
                        radioButtons("labelRX", label = "Etiqueta fractura", choices = c("Estable", "Inestable"),
                                     inline = TRUE, selected = character(0))
                    ),
                    hr(),

                    # Compilar espuestas
                    div(style = "padding-bottom:30px;",
                        actionButton("chooseRX", label = "Confirmar", icon = icon("check"))
                    ),

                    # Terminar sujeto
                    div(style = "",
                        actionButton("terminarRX", label = "Terminar Sujeto")
                    ),
                    hr(),
                ),
            )
        )
    )
    #
    # ,
    #
    # # | ------
    # # | Tab. Etiquetados --------------
    # tabPanel("RX Etiquetados",
    #     fluidRow(
    #         column(12,
    #             # verbatimTextOutput("test"),
    #             h4("RX ya etiquetados"),
    #             tableOutput("rxlistos")
    #         )
    #     )
    # )
)
