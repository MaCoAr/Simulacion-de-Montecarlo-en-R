# Instalar librerias 
install.packages("dplyr")
install.packages("moments")

# Librerias a usar
library(dplyr)
library(moments)

# Ejectuar archivo de simulación
source("Simulacion-Montercarlo-Sartenes-Especiales.R")

# Ejecutar archivo con el análisis de los datos hallados
source("Analisis-Simulacion-Montecarlo.R")

# Crear documento HTML con los precios promedio x onza 
knitr::knit2html(rmarkdown::render("Resultados-Simulacion-Montecarlo.Rmd"), stylesheet = 'mystyle.css')
# Arbir el archivo HTML creado en el navegador por defecto
htmlFile <- file.path("Resultados-Simulacion-Montecarlo.html")
#rstudioapi::viewer(htmlFile)
