# RX App

## Configurar el sistema

La app para procesar y etiquetar los rx para el estudio. Funciona como librería de R, asi que lo primero es tener todo instalado R, RStudio y RTools.

Contiene un comando que solo funciona en Windows, por lo cual generará un error si se usa  en Mac o Linux.

* [Descargar R](https://cran.r-project.org/bin/windows/base/)
* [Descargar Rtools](https://cran.r-project.org/bin/windows/Rtools/)
* [Descargar RStudio Desktop](https://rstudio.com/products/rstudio/download/#download)

## Instalar pacman

Vamos a instalar esta librería con `pacman`, una vez que tengamos todo instalado hay que instalar y cargar la librería. Para eso abrimos RStudio y escribimos.

```R
# Instalar pacman y devtools
install.packages("pacman")
install.packages("devtools")

# Y lo cargamos
library("pacman")
```

Ahora a instalar la librería de RX.

```R
# Instalar desde github
p_install_gh("olitroski/RXapp")
```

## Ejecutar la App

Para ejecutar es abrir un RStudio y escribir.

```R
# Cargar libreria
library("RXapp")

# Ejecutar App
cargarApp()
```

Con esto debiera abrirse una ventana de RStudio.

## Utilizar la App

### 1. Seleccionar un directorio

El directorio de trabajo solo debe tener las imágenes de RX, la app solo carga archivos con estructura estructura de nombre de archivo y para no contaminar la carpeta lo mejor es ni tocarla.

Antes de trabajar en datos reales entrenar con los RX que tiene este archivo ZIP, extraer en alguna carpeta y comenzar a editar.

### 2. Etiquetar sujetos

Es muy simple, se selecciona el Rut y se va descartando de a poco según se vaya viendo.

### 3. Cerrar la aplicación

Simplemente cerrar la ventana





Oliver Rojas, 2020