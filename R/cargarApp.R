#' @title cargarApp
#' @description Carga la app para etiquetar los rx
#' @export

cargarApp <- function(){
    appDir <- system.file("shiny-examples", "rxapp", package = "RXapp")
    
    # Por si no hubiera nada
    if (appDir == "") {
        stop("Directorio de Apps no existe", call. = FALSE)
    }
    
    # Ejecutar la app
    shiny::runApp(appDir, launch.browser = TRUE)
}