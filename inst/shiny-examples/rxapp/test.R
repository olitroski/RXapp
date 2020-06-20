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


stop()
# Cargar app
setwd("D:/OneDrive/BarbiApp/AppShiny/RXapp/inst/shiny-examples/rxapp")
shiny::runApp(display.mode = "normal")




