

# load("D:/Cuatri_III/IAD/TP_r/datosp21.RData")
library(dplyr)

#filtrar P21 > 0, para la regresion no porque imputaremos los -9
# datos_p21 <- subset(datos_p21, P21 > 0) #546085 a 173662 filtas
datos <- datos_multi
# #agregar ponderador anual segun el analisis
# datos$PONDIIO_anual <- datos$PONDIIO / 4 

agregar_deflactados_p21 <- function(dataset) {
  load("D:/Cuatri_III/IAD/TP_r/ipc_anual_dic.RData") #solo cambiar ruta
  resultado <- dataset %>%
    left_join(ipc_anual_dic, by = c("ANO4" = "anio")) %>%
    mutate(
      # P21_deflactado = P21/ipc_norm #* 100 / ipc_anual
      P21_deflactado = case_when(
        P21 == -9 ~ -9, #mantiene los -9 y 0
        P21 < 0 & P21 != 0 ~ -9,
        P21 == 0 ~ 0,
        TRUE ~ P21 / ipc_norm
        )
    )
  return(resultado)
}
datos21_deflactados <- agregar_deflactados_p21(datos)
datos21_deflactados <-subset(datos21_deflactados, P21 > 0)

#con esto podes guardarlo(opcional), sino se queda en memoria hasta que cierra rstudio
save(datos21_deflactados_, file = "datos21_deflactados_.RData")

# write.csv(datos_regresion_defl_2019, file = "datos_regresion_defl_2019.csv", row.names = FALSE)
