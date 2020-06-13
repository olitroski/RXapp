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

        verbatimTextOutput("test", placeholder = TRUE),

        # | --- Mostrar el data.frame con opciones de filtrado ------
        fluidRow(
            column(12,
                tableOutput("rxTable")
            )
        )

    ),






    # | ----
    # | Tab: App  --------------------------------------------------
    tabPanel("Etiquetar RX",
        # Pagina única no más
        fluidRow(
            # | Sidebar ----
            column(3,
                # | -- Sujetos a editar ----
                h4("Listado de Directorios"),
                p("Solo se muestran los no analizados"),
                div(style = "overflow-y: scroll; height: 350px",
                    uiOutput("listaFolders")
                ),
                # | -- Files a editar -----
                h4("Listado de Rx"),
                div(style = "overflow-y: scroll; height: 350px",
                    uiOutput("listaRX")
                )

            ),

            # | Main ----
            column(9,
                fluidRow(
                    # Sujeto sirve
                    column(4,
                        h4("Sujeto sirve para el estudio"),
                        radioButtons("sirveSubj", label = NULL, choices = c("Sirve", "No sirve", "Determinar"),
                                     inline = TRUE, selected = "Determinar")
                    ),
                    # Rx sirve
                    column(4,
                        h4("Determinar RX que sirve"),
                        radioButtons("sirveRX", label = NULL, choices = c("RX definitivo", "No sirve"),
                                     inline = TRUE, selected = "No sirve")
                    ),
                    # Etiqueta
                    column(4,
                        h4("Determinar etiqueta"),
                        radioButtons("sirveTag", label = NULL, choices = c("Estable", "Inestable", "No Asignada"),
                                     inline = TRUE, selected = "No Asignada")
                    )
                ),
                hr(),

                # | -- Tomar la desicion ----
                fluidRow(
                    column(8,
                        verbatimTextOutput("rxFile", placeholder = TRUE),
                    ),
                    column(4,
                        actionButton("rxDesicion", label = "Set RX", icon = icon ("warning"), width = "130px")
                    )
                ),

                # | -- La foto -----
                fluidRow(
                    column(12,
                        imageOutput("rxImage")
                    )
                )
            )
        )
    ),


    # | Tab. Etiquetados --------------
    tabPanel("RX Etiquetados",
        fluidRow(
            column(12,
                # verbatimTextOutput("test"),
                h4("RX ya etiquetados"),
                tableOutput("rxlistos")
            )
        )
    )
)
