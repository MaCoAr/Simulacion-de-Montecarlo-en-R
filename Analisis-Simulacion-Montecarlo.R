# Referencias
# https://www.youtube.com/watch?v=GeUSWbD2lmU
# https://www.datanalytics.com/libro_r/graficos-basicos.html
# https://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# https://www.asepelt.org/ficheros/File/Anales/2003%20-%20Almeria/asepeltPDF/18.PDF (robustez)
# http://diccionarioempresarial.wolterskluwer.es/Content/Documento.aspx?params=H4sIAAAAAAAEAMtMSbF1jTAAASNjM2MztbLUouLM_DxbIwMDS0NDA1OQQGZapUt-ckhlQaptWmJOcSoAvvhbpzUAAAA=WKE
# https://www.um.es/docencia/pguardio/documentos/Tec_ade.pdf 
# https://www.elsevier.es/es-revista-atencion-primaria-27-articulo-utilizacion-metodos-robustos-estadistica-inferencial-13049898 (cool)

#### Hallar los valores solicitados ####
valor_esperado <- sapply(Utilidad, mean, na.rm = TRUE)
desvi_estandar <- sapply(Utilidad, sd, na.rm = TRUE)
minimo <- sapply(Utilidad, min, na.rm = TRUE)
maximo <- sapply(Utilidad, max, na.rm = TRUE)

percentiles_095 <- quantile(Utilidad$Utilidad_Total_Opcion.1.,c(0.05,0.95))
percentiles_110 <- quantile(Utilidad$Utilidad_Total_Opcion.2.,c(0.05,0.95))
percentiles_115 <- quantile(Utilidad$Utilidad_Total_Opcion.3.,c(0.05,0.95))

#c("Percentil 5","Percentil 95")
percentiles <- matrix(0,3,2)
percentiles[1,1] <- percentiles_095[1]
percentiles[1,2] <- percentiles_095[2]
percentiles[2,1] <- percentiles_110[1]
percentiles[2,2] <- percentiles_110[2]
percentiles[3,1] <- percentiles_115[1]
percentiles[3,2] <- percentiles_115[2]

# Contar las utilidades mayor a cero y calcular la probabilidad
Probabilidad <- c(1:3)
Probabilidad[1] <- as.numeric((Utilidad %>% filter(Utilidad_Total_Opcion.1. > 0) %>% count()) / iteraciones)
Probabilidad[2] <- as.numeric((Utilidad %>% filter(Utilidad_Total_Opcion.2. > 0) %>% count()) / iteraciones)
Probabilidad[3] <- as.numeric((Utilidad %>% filter(Utilidad_Total_Opcion.3. > 0) %>% count()) / iteraciones)

# Armar una tabla con toda la información de cada una de las opciones
tabla_datos_final <- data.frame(cbind(valor_esperado,desvi_estandar,minimo,maximo,Probabilidad,percentiles[,1],percentiles[,2]))

names(tabla_datos_final)[1] <- "Valor_Esperado"
names(tabla_datos_final)[2] <- "Desviacion_Estandar"
names(tabla_datos_final)[3] <- "Minimo"
names(tabla_datos_final)[4] <- "Maximo"
names(tabla_datos_final)[5] <- "Probalidad Mayor a 0"
names(tabla_datos_final)[6] <- "Percentil_05"
names(tabla_datos_final)[7] <- "Percentil_95"

# #Actualizar el contenido de la primera columna
# tabla_datos_final[1,0] <- " 95 Sartenes"
# tabla_datos_final[2,0] <- "110 Sartenes"
# tabla_datos_final[3,0] <- "115 Sartenes"
# 
# # renombrar las columnas de los percentiles
# colnames(tabla_datos_final)[colnames(tabla_datos_final)=="X"] <- "Opcion_Compra"
# colnames(tabla_datos_final)[colnames(tabla_datos_final)=="V6"] <- "Percentil_05"
# colnames(tabla_datos_final)[colnames(tabla_datos_final)=="V7"] <- "Percentil_95"



### Persistir la información en archivos .csv ####

# llevar los datos de utilidades a archivos .csv
write.csv(Utilidad,"Utilidades.csv",row.names = TRUE)
write.csv(tabla_datos_final,"DatosResumen.csv", row.names = TRUE)

# Limpiar toda la memoria
# rm(list = ls())