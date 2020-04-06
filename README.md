# RX App

## Configurar el sistema

La app para procesar y etiquetar los rx para el estudio. Funciona como librería de R, asi que lo primero es tener todo instalado R, RStudio y RTools.

Contiene un comando que solo funciona en Windows, por lo cual generará un error si se usa  en Mac o Linux.

* [Descargar R](https://cran.r-project.org/bin/windows/base/)
* [Descargar Rtools](https://cran.r-project.org/bin/windows/Rtools/)
* [Descargar RStudio Desktop](https://rstudio.com/products/rstudio/download/#download)

## Instalar devtools

Una vez instalado todo hay que instalar la librería `devtools` y la aplicación. Para eso abrimos RStudio y escribimos.

```R
# Devtools
install.packages("devtools")
library(devtools)

# Instalar la aplicacion
devtools::install_github("olitroski/RXapp", update = "never")
```

Esto debiera instalar la app y varias librerías. Ocasionalmente puede arrojar algún error interno de R como que no pueda reinstalar alguna librería como por ejemplo:

```
Error: Failed to install 'RXapp' from GitHub:
  (convertido del aviso) cannot remove prior installation of package ‘Rcpp’
```

En este caso lo que pasa es que no puede actualizar el paquete `Rcpp` y hay que instalarlo manual mediante el comando.

```R
install.packages("Rcpp")
```

Habría que hacer lo mismo si se presentan adicionales errores con otras librerías.

## Ejecutar la App

Para ejecutar es abrir un RStudio y escribir.

```R
# Cargar la app y verificar que carga
library("RXapp")

# Ejecutar App
cargarApp()
```

Con esto debiera abrirse una ventana en el navegador predeterminado.

## Utilizar la App

### 1. Seleccionar un directorio

Debe ser el directorio donde están las carpetas para cada sujeto, con el fin de preservar los datos intactos se requiere que no se toquen esos folders, no va a cargar si:

- Hay archivos adicionales a las imágenes en un folder
- Hay directorios sin imágenes

Cuando se haya seleccionado el directorio hay que cargarlo, si no existiera el archivo **Excel** donde se guardan las etiquetas avisará que creará uno nuevo, si ya existe de previos trabajos se carga automático.

### 2. Etiquetar sujetos

La aplicación luce así en el navegador.









Oliver Rojas, 2020