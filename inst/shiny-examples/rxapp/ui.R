ui <- navbarPage(
    "Barbi App", id = "TablasApp",
    
    # | Tab: Seleccionar archivos -----------------------------------------
    tabPanel("Select Folder",
        fluidRow(
            column(12,
                h4("Seleccionar el folder de sujetos"),
                p("El directorio debe tener: Una carpeta por sujeto, dentro de ella las imagenes de RX en formato", code('jpg'))
                
            )
        ),
        
        # | --- Encabezado y botón ----
        fluidRow(
            # Botón para cargar folder
            column(1,
                actionButton("folderBtn", "Directorio", icon = icon("folder"))
            ),
            # Mostrar directorio
            column(6,
                verbatimTextOutput("folderDir", placeholder = TRUE)
            ),
            column(5,
                actionButton("folderLoad", "Cargar", icon = icon("check"))
            )
        ),
        fluidRow(
            column(12,
                p("Este directorio no se debe modificar, no agregar archivos o carpetas.", 
                  strong("Dejar siempre original,"), "solo se permite el excel de registros"),
            )
        ),
        hr(),
        
        # | --- Mostrar en simple las carpetas del folder ------
        fluidRow(
            column(12,
                verbatimTextOutput("folderShow")
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