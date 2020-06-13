#' @title cargar rx RDS
#' @description Carga el folde de RX y crea un archivo RDS con todos los datos, si no existe lo crea.
#' @param rxdir El directorio de donde estan los jpg de rx
#' @return Un data frame con la informacion del RDS y el folder
#' @export
#' @examples
#' # library(stringr); library(dplyr)
#' # test <- cargaRDS("D:/OneDrive/BarbiApp/AppShiny/testFolder")
#' @import dplyr
#' @import stringr
#'
cargaRDS <- function(rxdir){
    refNum <- rut <- rx <- serie <- serieName <- serieNum <- NULL
    setwd(rxdir)

    # Capturar los archivos
    archivos <- dir()
    archivos <- archivos[grep(".[JjPpGg]", archivos)]

    # No hay archivos jpg
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

            # Si el archivo existe
            if (file.exists(file.path(rxdir, "rxData.RDS")) == TRUE){
                cat("Hay RX, el data.frame ya existe, se carga a la memoria\n")
                rxdata <- readRDS(file.path(rxdir, "rxData.RDS"))

                # Si no existe
            } else {
                archivos <- dir()
                archivos <- archivos[grep(".jpg", archivos)]

                # Captura
                cat("Hay RX, el data.frame de RX no existe, se crea uno nuevo y se carga a la memoria\n")
                # Iniciar data.frame
                rxdata <- data.frame(file = archivos, stringsAsFactors = FALSE)

                # Separar
                temp <- str_split(rxdata[["file"]], "_", simplify = TRUE)
                temp <- as.data.frame(temp)
                names(temp) <- c("rut", "refNum", "serieName", "serieNum", "rx")

                # Arreglar algunas cosas antes
                temp <- mutate(temp, serie = paste0(serieName, "-", serieNum)) %>% select(-serieName, -serieNum)
                temp <- mutate(temp, rx = str_replace(rx, ".jpg", ""))
                temp <- mutate(temp, serie = str_replace(serie, "^-", ""))
                temp <- select(temp, rut, refNum, serie, rx)

                # Agregar variables
                temp <- mutate(temp, refStatus = 1, serieStatus = 1, rxStatus = 1, rdDoble = 0, etiqueta = "")
                rxdata <- bind_cols(temp, rxdata)
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

    return(rxdata)
}

