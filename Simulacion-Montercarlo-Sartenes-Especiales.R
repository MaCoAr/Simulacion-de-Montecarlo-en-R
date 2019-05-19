#### Libreriras a utilizar en la simulacion ####

#### Definición de Contantes ####
# Variables ciertas
cte_Cantidad_Sartenes_Opcion <- c(95,110,115)  # Compra de sartenes especiales
cte_Costo_SE <- 22  # Costo de las sartenes especiales
cte_Costo_SC <- 32  # Costo de las sartenes comunes
cte_Precio_Venta <- 35

# Variables con incertidumbre
cte_dist_normal_promedio <- 100
cte_dist_normal_desv_sta <- 25

cte_dist_log_normal_media <- 2.7
cte_dist_log_normal_desv_ <- 0.008

cte_dist_log_normal_promedio <- 15
cte_dist_log_normal_desv_est <- 2

# variables tipo vector
Cantidad_Vendida_opcion <- c(1:3)
Precio_Venta_Cadena_Dcto_opcion <- c(1:3)

Total_ventas_SE_opcion <- c(1:3)
Total_ventas_SC_opcion <- c(1:3)
Total_ventas_SE_Cadena_Dcto_opcion <- c(1:3)

Total_costos_SE_opcion <- c(1:3)
Total_costos_SC_opcion <- c(1:3)

Acumulado_ventas_sartenes_opcion <- c(1:3)
Acumulado_costos_sartenes_opcion <- c(1:3)

Utilidad_Total_Opcion <- c(1:3)

#### Definición de Varibles ####
#Estructuras de datos
opcion_095 <- data.frame(
    Cantidad_Comprada  = integer(),
    Cantidad_Vendida  = integer(),
    Precio_Venta_Cadena_Dcto = double(),
    Total_Ventas_SE   = double(),
    Total_Ventas_SE_Cadena_Dcto = double(),
    Total_Ventas_SC = double(),
    Acumulado_Venta_Sartenes = double(),
    Total_Costo_SE = integer(),
    Total_Costo_SC = integer(),
    Acumulado_Costo_Sartenes = integer(),
    Utilidad_Total = double()
    )

opcion_110 <- data.frame(
  Cantidad_Comprada  = integer(),
  Cantidad_Vendida  = integer(),
  Precio_Venta_Cadena_Dcto = double(),
  Total_Ventas_SE   = double(),
  Total_Ventas_SE_Cadena_Dcto = double(),
  Total_Ventas_SC = double(),
  Acumulado_Venta_Sartenes = double(),
  Total_Costo_SE = integer(),
  Total_Costo_SC = integer(),
  Acumulado_Costo_Sartenes = integer(),
  Utilidad_Total = double()
)

opcion_115 <- data.frame(
  Cantidad_Comprada  = integer(),
  Cantidad_Vendida  = integer(),
  Precio_Venta_Cadena_Dcto = double(),
  Total_Ventas_SE   = double(),
  Total_Ventas_SE_Cadena_Dcto = double(),
  Total_Ventas_SC = double(),
  Acumulado_Venta_Sartenes = double(),
  Total_Costo_SE = integer(),
  Total_Costo_SC = integer(),
  Acumulado_Costo_Sartenes = integer(),
  Utilidad_Total = double()
)

#### Simulación ####
iteraciones <- 1000
for (i in 1:iteraciones) {

  # Inicializar variables
  Acumulado_ventas_sartenes_opcion[1] <- 0
  Acumulado_ventas_sartenes_opcion[2] <- 0
  Acumulado_ventas_sartenes_opcion[3] <- 0

  Acumulado_costos_sartenes_opcion[1] <- 0
  Acumulado_costos_sartenes_opcion[2] <- 0
  Acumulado_costos_sartenes_opcion[3] <- 0

  Total_ventas_SE_Cadena_Dcto_opcion[1] <- 0
  Total_ventas_SE_Cadena_Dcto_opcion[2] <- 0
  Total_ventas_SE_Cadena_Dcto_opcion[3] <- 0

  Total_ventas_SE_opcion[1] <- 0
  Total_ventas_SE_opcion[2] <- 0
  Total_ventas_SE_opcion[3] <- 0

  Total_ventas_SC_opcion[1] <- 0
  Total_ventas_SC_opcion[2] <- 0
  Total_ventas_SC_opcion[3] <- 0

  Total_costos_SE_opcion[1] <- 0
  Total_costos_SE_opcion[2] <- 0
  Total_costos_SE_opcion[3] <- 0

  Total_costos_SC_opcion[1] <- 0
  Total_costos_SC_opcion[2] <- 0
  Total_costos_SC_opcion[3] <- 0

  Utilidad_Total_Opcion[1] <- 0
  Utilidad_Total_Opcion[2] <- 0
  Utilidad_Total_Opcion[3] <- 0
  
  # hallar la cantidad de unidades a vender con la distribución normal inversa
  Cantidad_Vendida_opcion[1] <- round(qnorm(runif(1,0,1),cte_dist_normal_promedio,cte_dist_normal_desv_sta),0)
  Cantidad_Vendida_opcion[2] <- round(qnorm(runif(1,0,1),cte_dist_normal_promedio,cte_dist_normal_desv_sta),0)
  Cantidad_Vendida_opcion[3] <- round(qnorm(runif(1,0,1),cte_dist_normal_promedio,cte_dist_normal_desv_sta),0)
  
  # Hallar el precio de venta a la cadena de descuenta con la distribución log normal inversa
  Precio_Venta_Cadena_Dcto_opcion[1] <- qlnorm(runif(1,0,1),2.7,0.008)
  Precio_Venta_Cadena_Dcto_opcion[2] <- qlnorm(runif(1,0,1),2.7,0.008)
  Precio_Venta_Cadena_Dcto_opcion[3] <- qlnorm(runif(1,0,1),2.7,0.008)  
  
  # Preguntar si la cantidad vendida es superior a la cantidad comprada 
  # Opcion 1
  if(Cantidad_Vendida_opcion[1] >= cte_Cantidad_Sartenes_Opcion[1]){
    # Ventas
    Total_ventas_SE_opcion[1] <- cte_Cantidad_Sartenes_Opcion[1] * cte_Precio_Venta
    Total_ventas_SC_opcion[1] <- (Cantidad_Vendida_opcion[1] - cte_Cantidad_Sartenes_Opcion[1]) * cte_Precio_Venta
    # Costos
    Total_costos_SE_opcion[1] <- cte_Cantidad_Sartenes_Opcion[1] * cte_Costo_SE
    Total_costos_SC_opcion[1] <- (Cantidad_Vendida_opcion[1] - cte_Cantidad_Sartenes_Opcion[1]) * cte_Costo_SC
  }else{
    # Ventas
    Total_ventas_SE_opcion[1] <- Cantidad_Vendida_opcion[1] * cte_Precio_Venta
    Total_ventas_SE_Cadena_Dcto_opcion[1] <- (cte_Cantidad_Sartenes_Opcion[1] - Cantidad_Vendida_opcion[1]) * Precio_Venta_Cadena_Dcto_opcion[1]
    # Costos
    Total_costos_SE_opcion[1] <- cte_Cantidad_Sartenes_Opcion[1] * cte_Costo_SE
  }

  # Opcion 2
  if(Cantidad_Vendida_opcion[2] >= cte_Cantidad_Sartenes_Opcion[2]){
    # Ventas
    Total_ventas_SE_opcion[2] <- cte_Cantidad_Sartenes_Opcion[2] * cte_Precio_Venta
    Total_ventas_SC_opcion[2] <- (Cantidad_Vendida_opcion[2] - cte_Cantidad_Sartenes_Opcion[2]) * cte_Precio_Venta
    # Costos
    Total_costos_SE_opcion[2] <- cte_Cantidad_Sartenes_Opcion[2] * cte_Costo_SE
    Total_costos_SC_opcion[2] <- (Cantidad_Vendida_opcion[2] - cte_Cantidad_Sartenes_Opcion[2]) * cte_Costo_SC
  }else{
    # Ventas
    Total_ventas_SE_opcion[2] <- Cantidad_Vendida_opcion[2] * cte_Precio_Venta
    Total_ventas_SE_Cadena_Dcto_opcion[2] <- (cte_Cantidad_Sartenes_Opcion[2] - Cantidad_Vendida_opcion[2]) * Precio_Venta_Cadena_Dcto_opcion[2]
    # Costos
    Total_costos_SE_opcion[2] <- cte_Cantidad_Sartenes_Opcion[2] * cte_Costo_SE
  }  
  
  # Opcion 3
  if(Cantidad_Vendida_opcion[3] >= cte_Cantidad_Sartenes_Opcion[3]){
    # Ventas
    Total_ventas_SE_opcion[3] <- cte_Cantidad_Sartenes_Opcion[3] * cte_Precio_Venta
    Total_ventas_SC_opcion[3] <- (Cantidad_Vendida_opcion[3] - cte_Cantidad_Sartenes_Opcion[3]) * cte_Precio_Venta
    # Costos
    Total_costos_SE_opcion[3] <- cte_Cantidad_Sartenes_Opcion[3] * cte_Costo_SE
    Total_costos_SC_opcion[3] <- (Cantidad_Vendida_opcion[3] - cte_Cantidad_Sartenes_Opcion[3]) * cte_Costo_SC
  }else{
    # Ventas
    Total_ventas_SE_opcion[3] <- Cantidad_Vendida_opcion[3] * cte_Precio_Venta
    Total_ventas_SE_Cadena_Dcto_opcion[3] <- (cte_Cantidad_Sartenes_Opcion[3] - Cantidad_Vendida_opcion[3]) * Precio_Venta_Cadena_Dcto_opcion[3]
    # Costos
    Total_costos_SE_opcion[3] <- cte_Cantidad_Sartenes_Opcion[3] * cte_Costo_SE
  }
    
  # Acumular la ventas
  Acumulado_ventas_sartenes_opcion[1] <- Total_ventas_SE_opcion[1] + Total_ventas_SC_opcion[1] + Total_ventas_SE_Cadena_Dcto_opcion[1]
  Acumulado_costos_sartenes_opcion[1] <- Total_costos_SE_opcion[1] + Total_costos_SC_opcion[1] 
  
  Acumulado_ventas_sartenes_opcion[2] <- Total_ventas_SE_opcion[2] + Total_ventas_SC_opcion[2] + Total_ventas_SE_Cadena_Dcto_opcion[2]
  Acumulado_costos_sartenes_opcion[2] <- Total_costos_SE_opcion[2] + Total_costos_SC_opcion[2]   
  
  Acumulado_ventas_sartenes_opcion[3] <- Total_ventas_SE_opcion[3] + Total_ventas_SC_opcion[3] + Total_ventas_SE_Cadena_Dcto_opcion[3]
  Acumulado_costos_sartenes_opcion[3] <- Total_costos_SE_opcion[3] + Total_costos_SC_opcion[3] 
    
  # Calcular utilidad
  Utilidad_Total_Opcion[1] <- Acumulado_ventas_sartenes_opcion[1] - Acumulado_costos_sartenes_opcion[1]
  Utilidad_Total_Opcion[2] <- Acumulado_ventas_sartenes_opcion[2] - Acumulado_costos_sartenes_opcion[2]
  Utilidad_Total_Opcion[3] <- Acumulado_ventas_sartenes_opcion[3] - Acumulado_costos_sartenes_opcion[3]

  # Asginar vectores a data frame temporal
  df_row_095 <- data.frame(cte_Cantidad_Sartenes_Opcion[1],
                       Cantidad_Vendida_opcion[1],
                       Precio_Venta_Cadena_Dcto_opcion[1],
                       Total_ventas_SE_opcion[1],
                       Total_ventas_SE_Cadena_Dcto_opcion[1],
                       Total_ventas_SC_opcion[1],
                       Acumulado_ventas_sartenes_opcion[1],
                       Total_costos_SE_opcion[1],
                       Total_costos_SC_opcion[1],
                       Acumulado_costos_sartenes_opcion[1],
                       Utilidad_Total_Opcion[1])
  
  # Asignar la nueva fila al data frame correspondiente
  opcion_095 <- rbind(opcion_095,df_row_095)
  
  # Asginar vectores a data frame temporal
  df_row_110 <- data.frame(cte_Cantidad_Sartenes_Opcion[2],
                           Cantidad_Vendida_opcion[2],
                           Precio_Venta_Cadena_Dcto_opcion[2],
                           Total_ventas_SE_opcion[2],
                           Total_ventas_SE_Cadena_Dcto_opcion[2],
                           Total_ventas_SC_opcion[2],
                           Acumulado_ventas_sartenes_opcion[2],
                           Total_costos_SE_opcion[2],
                           Total_costos_SC_opcion[2],
                           Acumulado_costos_sartenes_opcion[2],
                           Utilidad_Total_Opcion[2])
  
  # Asignar la nueva fila al data frame correspondiente
  opcion_110 <- rbind(opcion_110,df_row_110)  
  
  # Asginar vectores a data frame temporal
  df_row_115 <- data.frame(cte_Cantidad_Sartenes_Opcion[3],
                           Cantidad_Vendida_opcion[3],
                           Precio_Venta_Cadena_Dcto_opcion[3],
                           Total_ventas_SE_opcion[3],
                           Total_ventas_SE_Cadena_Dcto_opcion[3],
                           Total_ventas_SC_opcion[3],
                           Acumulado_ventas_sartenes_opcion[3],
                           Total_costos_SE_opcion[3],
                           Total_costos_SC_opcion[3],
                           Acumulado_costos_sartenes_opcion[3],
                           Utilidad_Total_Opcion[3])
  
  # Asignar la nueva fila al data frame correspondiente
  opcion_115 <- rbind(opcion_115,df_row_115)  
}

# Vectores con las utilidades de las tres opciones
utilidades_095 <- opcion_095 %>%  select(Utilidad_Total_Opcion.1.)
utilidades_110 <- opcion_110 %>%  select(Utilidad_Total_Opcion.2.)
utilidades_115 <- opcion_115 %>%  select(Utilidad_Total_Opcion.3.)

# Crear data frame con los datos de las tres utilidades
Utilidad <- data.frame(utilidades_095,utilidades_110,utilidades_115)

# Limpiar variables de memoria de la simulación
rm(list = ls(pattern = "cte"))
rm(list = ls(pattern = "Total"))
rm(list = ls(pattern = "df_ro"))
rm(list = ls(pattern = "Acumu"))
rm(list = ls(pattern = "utilidades_"))
rm(Precio_Venta_Cadena_Dcto_opcion)
rm(i)
#rm(iteraciones)
rm(Cantidad_Vendida_opcion)