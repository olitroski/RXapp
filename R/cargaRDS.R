#' @title cargar rx RDS
#' @description Carga el folde de RX y crea un archivo RDS con todos los datos, si no existe lo crea.
#' @param rxdir El directorio de donde estan los jpg de rx
#' @return Un data frame con la informacion del RDS y el folder
#' @export
#' @examples
#' # library(stringr); library(dplyr)
#' # rxdir <- "C:/Users/olitr/Desktop/Nueva carpeta"
#' # rxdir <- "D:/OneDrive/BarbiApp/AppShiny/testFolder"
#' # test <- cargaRDS("D:/OneDrive/BarbiApp/AppShiny/testFolder")
#' @import dplyr
#' @import stringr
#'
cargaRDS <- function(rxdir){
    refNum <- rut <- rx <- serie <- serieName <- serieNum <- NULL

    # Si el archivo existe se carga
    if (file.exists(file.path(rxdir, "rxData.RDS")) == TRUE){
        cat("Hay RX, data.frame existe, se carga a la memoria\n")
        rxdata <- readRDS(file.path(rxdir, "rxData.RDS"))

    # Si no existe ejecutar la rutina de crear uno
    } else {
        # Capturar los archivos
        setwd(rxdir)
        archivos <- dir()
        archivos <- archivos[grep(".[Jj][Pp][Gg]", archivos)]

        # Si no hay archivos jpg
        if (length(archivos) == 0){
            cat("El directorio no contienen archivos 'jpg'\n")
            rxdata <- data.frame(Directorio = "El directorio no contiene archivos 'JPG'")

        # Hay jpg
        } else {
            # Ver si si sirven
            temp <- str_split(archivos, "_", simplify = TRUE)
            temp <- ncol(temp)

            # pero no son de RX
            if (temp != 5){
                cat("Directorio con imagenes pero no parecen ser RX\n")
                rxdata <- data.frame(Directorio = "El directorio no contiene archivos RX")

            # si son RX - El separado tiene 5 columnas
            } else if (temp == 5){

                # Si el archivo existe ---> paso esto al inicio, pero dejo el codigo viejo aca no hace daño.
                if (file.exists(file.path(rxdir, "rxData.RDS")) == TRUE){
                    cat("Hay RX, el data.frame ya existe, se carga a la memoria\n")
                    rxdata <- readRDS(file.path(rxdir, "rxData.RDS"))

                # Si no existe
                } else {
                    archivos <- dir()
                    archivos <- archivos[grep(".jpg", archivos)]

                    # Captura
                    cat("Hay RX, data.frame de RX no existe, se crea uno nuevo\n")
                    # Iniciar data.frame
                    rxdata <- data.frame(file = archivos, stringsAsFactors = FALSE)

                    # Separar
                    temp <- str_split(rxdata[["file"]], "_", simplify = TRUE)
                    temp <- as.data.frame(temp, stringsAsFactors = FALSE)
                    names(temp) <- c("rut", "refNum", "serieName", "serieNum", "rx")

                    # Arreglar algunas cosas antes
                    temp <- mutate(temp, serie = paste0(serieName, "-", serieNum)) %>% select(-serieName, -serieNum)
                    temp <- mutate(temp, rx = str_replace(rx, ".jpg", ""))
                    temp <- mutate(temp, serie = str_replace(serie, "^-", ""))
                    temp <- select(temp, rut, refNum, serie, rx)

                    # Agregar variables
                    rxdata <- bind_cols(temp, rxdata)
                    rxdata <- mutate(rxdata, etiqueta = "No procesado")
                    rm(temp)

                    # Guardamos
                    saveRDS(rxdata, "rxData.RDS")
                }

            # El separado tiene mas de 5 columnas
            } else {
                cat("El directorio parece contener \n")
                rxdata <- data.frame(Directorio = "Si hay archivos de RX tienen el nombre supera 5 cols")
            }
        }
    }

    return(rxdata)
}

