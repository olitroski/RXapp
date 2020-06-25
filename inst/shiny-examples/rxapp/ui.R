ui <- navbarPage(
    "Barbi App", id = "TablasApp",

    # | Tab: Seleccionar archivos -----------------------------------------
    tabPanel("Select Folder",
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

        fluidRow(
            column(7,
                   # Muestra ruta
                   verbatimTextOutput("pathText", placeholder = TRUE),
            ),
            column(5,
                   # Carga ruta
                   actionButton("pathBoton", "Buscar...", icon = icon("folder"), width = "110px")
            )
        ),
        hr(),

        # verbatimTextOutput("test", placeholder = TRUE),

        # | --- Mostrar el data.frame con opciones de filtrado ------
        fluidRow(
            column(7, offset = 1,
                tableOutput("rxTable")
            ),
            column(4,
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
            # verbatimTextOutput("test", placeholder = TRUE),

            column(9,
                fluidRow(
                    # Chao sujeto
                    column(3,
                        h4("Descartar Sujeto"),
                        p("Descartar todas las imágenes del suejto"),
                        # actionButton("chaoRUT", label = "Filtrar Rut", icon = icon("filter"))
                    ),
                    # Chao Referencia
                    column(3,
                        h4("Descartar Referencia"),
                        p("Descartar todas las series de la referencia"),
                        # actionButton("chaoREF", label = "Filtrar Ref.", icon = icon("filter"))
                    ),
                    # Chao Referencia
                    column(3,
                           h4("Descartar Serie"),
                           p("Descartar todas las imágenes de la serie"),
                           # actionButton("chaoSERIE", label = "Filtrar Serie", icon = icon("filter"))
                    ),

                    # Elegir RX
                    column(3,
                        h4("The Choosen One"),
                        p("El RX elegido será incluido en el estudio"),
                        radioButtons("sirveRX", label = NULL, choices = c("Estable", "Inestable", "Elegir"), inline = TRUE, selected = "Elegir"),
                        actionButton("chooseRX", label = "Confirmar", icon = icon("check"))
                    )
                ),
                hr(),


                # | -- La foto -----
                tags$head(tags$style(type="text/css", "#rxImage img {max-width: 70%; width: 100%; height: auto}")),

                fluidRow(
                    column(12,
                        imageOutput("rxImage")
                    )
                )
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
