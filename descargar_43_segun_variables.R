library(eph)

# Definir años y trimestres (sin 2016-T1)
anios <- 2016:2024
trimestres <- 1:4
combinaciones <- expand.grid(year = anios, period = trimestres)
combinaciones <- subset(combinaciones, !(year == 2016 & period == 1))

datos_list <- list()

for (i in 1:nrow(combinaciones)) {
  y <- combinaciones$year[i]
  p <- combinaciones$period[i]
  cat("Descargando: Año", y, "Trimestre", p, "...\n")
  try({
    d <- get_microdata(year = y, period = p, type = "individual",
                       #MODIFICAR VARIABLES A DESCARGAR
                       vars = c("ANO4","REGION","TRIMESTRE","ESTADO",'AGLOMERADO','P21','PONDIIO'))
                    #Datos para regresion:
                      # vars = c("ANO4", "TRIMESTRE", "ESTADO", "PONDERA",
                      #          "AGLOMERADO", "REGION","P21", "PONDIIO", "NIVEL_ED",
                      #          "CH04", "CH15", "PP04G","PP07G_59" ))
                      #Segun el punto a analizar descargamos la variables necesarias
    
    datos_list[[paste0(y, "_T", p)]] <- d
    Sys.sleep(1)  # pausa para no saturar servidor
  }, silent = TRUE)
}

# Unir todos los data.frames descargados
datos <- do.call(rbind, datos_list)


if("REGION" %in% names(datos)) {
  datos_regresion_2019 <- subset(datos, REGION == 43) #subset: filtra por condicion
} else {
  stop("La columna 'REGION' no está presente en los datos.")
}

# Guardar, nombre del objeto:
save(datos_regresion_2019, file = "datos_regresion_2019.RData") #guarda el archivo, 

write.csv(datos_regresion_2019, file = "datos_regresion_2019.csv", row.names = FALSE)
#el nombre del dataset y el nombre del archivo puede ser distinto
#luego se carga con, la ruta donde : 

#load("D:/Cuatri_III/IAD/TP_r/datos_region43.RData")
# el nombre de dataset que lo guardaste





