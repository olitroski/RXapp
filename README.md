# RX App

La app para procesar y etiquetar los rx para el estudio. Funciona como librería de R, asi que lo primero es tener todo instalado R, RStudio y RTools.

Contiene un comando que solo funciona en Windows, por lo cual generará un error si se usa  en Mac o Linux.

* [Descargar R](https://cran.r-project.org/bin/windows/base/)
* [Descargar Rtools](https://cran.r-project.org/bin/windows/Rtools/)
* [Descargar RStudio Desktop](https://rstudio.com/products/rstudio/download/#download)

Una vez instalado todo hay que instalar la librería `devtools` y la aplicación.

```R
# Devtools
install.packages("devtools")
library(devtools)

# Instalar la aplicacion
devtools::install_github("olitroski/RXapp")
```

Esto debiera instalar la app y varias librerías.

### Ejecutar la App

Para ejecutar es abrir un RStudio y escribir.

```R
# Ejecutar App
cargarApp()
```

Con esto debiera abrirse una ventana en el navegador predeterminado.







Oliver Rojas, 2020