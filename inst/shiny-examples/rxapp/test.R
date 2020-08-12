# Para testear solamente
rm(list=ls())
library(shiny)
library(dplyr)
library(lubridate)
library(stringr)

# Cargar funcion
setwd("D:/OneDrive/BarbiApp/AppShiny/RXapp/R")
source("cargaRDS.R")
source("otable.R")

# Cargar RDS
datosrx <- cargaRDS("D:/OneDrive/BarbiApp/AppShiny/testFolder")


# Cargar app
setwd("D:/OneDrive/BarbiApp/AppShiny/RXapp/inst/shiny-examples/rxapp")
shiny::runApp(display.mode = "normal")


stop()
# ----- Otros test ------------------------------------------------------
library(openxlsx)
testdir <- "D:/OneDrive/BarbiApp/AppShiny/otrosTest"

# Cargar todos y mirar
rxdir <- "D:/rx_barbarita"
rxfiles <- cargaRDS(rxdir)
setwd(testdir)
write.xlsx(rxfiles, "rxfiles.xlsx")

# Cargar el otro original con las fechas y todo



