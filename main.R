# Ejectuar archivo de simulación
source("Simulacion-Montercarlo-Sartenes-Especiales.R")

# Limpiar variables de memoria de la simulación
rm(list = ls(pattern = "cte"))
rm(list = ls(pattern = "Total"))
rm(list = ls(pattern = "df_ro"))
rm(list = ls(pattern = "Acumu"))
#rm(list("i","iteraciones","Cantidad_Vendida_opcion","Precion_Venta_Cadena_Dcto_opcion"))