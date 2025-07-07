load("D:/Cuatri_III/IAD/TP_r/datos_p21.RData")
load("D:/Cuatri_III/IAD/TP_r/datos_deflactados_ingresos.RData")

#### precio deflactado 
library(dplyr)
library(Hmisc)
library(ggplot2)
# 
# # filtrar datos mayores a 0 
# datos_deflatados_ingresos <- subset(datos_deflactados, P21 > 0) #546085 a 173662
# #ponderador anual
# datos_deflatados_ingresos$PONDIIO_anual <- datos_deflatados_ingresos$PONDIIO / 4
 

##medidas de tendencia central de ingresos,
calcular_media_mediana_anual <-function(dataset, nombre_ingreso, nombre_ponde){
  datos <- dataset |>
  group_by(ANO4) |>
  summarise(
    media = weighted.mean({{nombre_ingreso}}, {{nombre_ponde}}),
    mediana = Hmisc::wtd.quantile({{nombre_ingreso}}, weights = {{nombre_ponde}}, probs = 0.5)
    
  )
  return(datos)
}

p21_tendencia_central <- calcular_media_mediana_anual(datosp21_deflactados,P21_deflactado,
                                                      PONDIIO_anual)
#moda ponderada
modas <- datosp21_deflactados %>%
  group_by(ANO4, P21_deflactado) %>%
  summarise(peso_total = sum(PONDIIO_anual, na.rm = TRUE), .groups = "drop") %>%
  group_by(ANO4) %>%
  slice_max(order_by = peso_total, n = 1, with_ties = FALSE) %>%
  rename(moda = P21_deflactado)

# Paso 3: Unir todo
p21_tendencia_central <- p21_tendencia_central %>%
  left_join(modas, by = "ANO4")
#------------ graficos-----------------//////
load("D:/Cuatri_III/IAD/TP_r/p21_tendencia_central.RData")
ggplot(p21_tendencia_central, aes(x = ANO4)) +
  geom_line(aes(y = media, color = "Media")) +
  geom_point(aes(y = media, color = "Media")) +
  geom_text(aes(y = media, label = round(media, 0), color = "Media"), vjust = -0.8, size = 3) +
  
  scale_x_continuous(breaks = p21_tendencia_central$ANO4) +
  labs(title = "Evolución de media de salarios de la ocupacion principal",
       y = "Salario",
       x = "Año", color ="Color")
#barra
ggplot(p21_tendencia_central, aes(x = as.factor(ANO4), y = media)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(media, 0)), vjust = -0.5, size = 3) +
  labs(
    title = "Evolución de media de salarios de la ocupación principal",
    x = "Año",
    y = "Salario medio"
  ) +
  theme_minimal()

# # TENDENCIACENTRAL POR ALGLOMERADO-------------
# ingresos_anuales_aglom <- datos_deflatados_ingresos |>
#   group_by(ANO4, AGLOMERADO) |>
#   summarise(
#     media = weighted.mean(P21_deflactado, PONDIIO_anual),
#     mediana = Hmisc::wtd.quantile(P21_deflactado, weights = PONDIIO_anual, probs = 0.5)
#     
#   )
# #moda ponderada
# modas_aglom <- datos_deflatados_ingresos %>%
#   group_by(ANO4, AGLOMERADO, P21_deflactado_2016) %>%
#   summarise(peso_total = sum(PONDIIO_anual, na.rm = TRUE), .groups = "drop") %>%
#   group_by(ANO4,AGLOMERADO) %>%
#   slice_max(order_by = peso_total, n = 1, with_ties = FALSE) %>%
#   rename(moda = P21_deflactado_2016)
# 
# # Paso 3: Unir todo
# ingresos_tendencia_aglom <- ingresos_anuales_aglom %>%
#   left_join(modas_aglom, by = "ANO4", "AGLOMERADO")
