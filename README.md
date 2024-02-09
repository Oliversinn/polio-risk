# polio-risk
Herramienta para el análisis de riesgo de Polio

# Pasos a seguir

De manera resumida, estos son los pasos por seguir para ejecutar la herramienta.

1. Instalar dependencias.
2. Descargar herramienta.
3. Instalación de paquetes de R.
4. Preparación de shapefiles.
5. Llenar la entrada de datos.
6. Ejecutar herramienta.

# Dependencias

La herramienta de análisis de riesgo de polio fue programada utilizando el lenguaje de programación R. Las dependencias para ejecutar esta herramienta son:

1. Navegador web (Firefox, Google Chrome, Safari, etc.)
2. Excel
3. R
4. RStudio

# Descarga de herramienta

## Navegador Web

Si se desea descargar la herramienta desde el navegador web: 

1. Ingresar al repositorio de github [https://github.com/Oliversinn/polio-risk].
2. Presionar el botón verde que dice "Code" o "Código".
3. Presionar el botón de "Download Zip" o "Descargar Zip".
4. Descomprimir archivo zip.

## Utilizando git

```bash
git clone https://github.com/Oliversinn/polio-risk.git
```

# Instalación de paquetes de R

1. En la carpeta de la herramienta, hacer clic en polio-risk.Rproj. Esto abrirá una ventana de RStudio en la carpeta de la herramienta. 
2. En el panel de archivos hacer clic en el archivo install.R para abrir el archivo.
3. Ejecutar el archivo haciendo clic en el botón "Run".
4. Mientras se estén instalando las dependencias, se nos mostraran algunas preguntas en la terminal, a estas preguntas responder que sí escribiendo "y" y presionando "enter" en la terminal.

# Preparación de Shapefiles

Para que la herramienta logre imprimir los mapas de los distintos países, se necesitan estandarizar los archivos que contienen los polígonos de los mapas, estos archivos los llamaremos shapefiles. Para este procedimiento usted necesita colocar los shapefiles en formato SHP o JSON en la carpeta `./src/Shapefile_prep` de la carpeta del proyecto. Con la ayuda de mapshaper [https://mapshaper.org] llenar la configuración encontrada en el Excel `./src/Shapefile_prep/shapefile_settings.xlsx`.

## Definición de variable de configuración

* Tipo de archivo: Colocar el tipo de archivo de sus shapefiles (JSON/SHP).
* Nombre archivo: Nombre de los shapefiles sin su extensión.
* ADMIN1 ID: Nombre de la variable del shapefile que contiene el código del nivel administrativo más alto.
* ADMIN1 NOMBRE: Nombre de la variable del shapefile que contiene el nombre del nivel administrativo más alto.
* ADMIN2 ID: Nombre de la variable del shapefile que contiene el código del nivel administrativo más bajo.
* ADMIN2 NOMBRE: Nombre de la variable del shapefile que contiene el nombre del nivel administrativo más bajo.
* GEOMETRY: Nombre de la variable del shapefile que contiene los polígonos (usualmente se llama "geometry").

## Mapshaper.org

El sitio de mapshaper.org nos permite cargar los shapefies a un navegador y explorarlos. 

1. Ingresar a https://mapshaper.org.
2. Hacer clic en el botón de "select" file para subir nuestros shapefiles.
3. Seleccionar y subir shapefiles.
4. Seleccionar el botón del cursor que se encuentra en el lado derecho.
5. Hacer clic en algún polígono.
6. Con ayuda del recuadro colocado en la esquina superior izquierda, obtener el nombre de las variables (texto color gris) y colocarlo en el Excel de configuración `./src/Shapefile_prep/shapefile_settings.xlsx`.
7. Guardar cambios.

## Ejecutar script de estandarización

Una vez se tengan los shapefiles en la carpeta `./src/Shapefile_prep/` y el archivo `./src/Shapefile_prep/shapefile_settings.xlsx` con las configuraciones adecuadas, abrir y ejecutar el archivo `./src/Shapefile_prep/geodata_to_shapefiles.R`. 

1. En el panel de archivos, hacer clic en el archivo geodata_to_shapefiles.R
2. En el editor de texto, seleccionar todas las líneas del archivo y ejecutar haciendo clic en el botón "Run".

## Resultado

Como resultado:

1. Los shapefiles estandarizadas se guardarán en `./src/Data/shapefiles/`,
2. Se creará el archivo `./src/Shapefile_prep/geocodigos_nombres.xlsx` que nos servirá para nuestra entrada de datos.

# Llenar la entrada de datos

* Para llenar los datos, es necesario haber estandarizados los shapefiles previamente.
* En la carpeta `./template/` encontraran una carpeta con una platilla de llenado de datos para diferentes idiomas.
* En la primera pestaña del template, se coloca información general de los países.
* En todas las pestañas verán que las primeras 4 variables son las variables de los geo códigos e información sobre los shapefiles. Para llenar estas 4 columnas, utilizar las 4 columnas generadas en el archivo `./src/Shapefile_prep/geocodigos_nombres.xlsx`.
* Llenar el resto de las variables.

# Ejecutar herramienta.

Para ejecutar la herramienta debemos abrir y ejecutar el archivo `./src/run.R`.

# Control de calidad

Para ejecutar el control de calidad debemos abrir y ejecutar el archivo `./src/qa.R`.

