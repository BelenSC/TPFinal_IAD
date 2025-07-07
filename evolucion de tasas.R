
load("D:/Cuatri_III/IAD/TP_r/datos_ocupacion.RData")

datos__ocupacion$PONDERA_anual <- datos__ocupacion$PONDERA / 4

#ifelse(condición, valor_si_TRUE, valor_si_FALSE)
datos__ocupacion$OCUPADO <- ifelse(datos__ocupacion$ESTADO == 1, 1, 0)
datos__ocupacion$DESOCUPADO <- ifelse(datos__ocupacion$ESTADO == 2, 1, 0)
#activo: Busca desempleado y empleo y empleados
datos__ocupacion$ACTIVO <- ifelse(datos__ocupacion$ESTADO %in% c(1, 2), 1, 0)
datos__ocupacion$POBLACION <- 1  # Cada fila es una persona

#totales ponderados por año
tasas_aglom <- aggregate(
  cbind(
    OCUPADO = datos__ocupacion$OCUPADO * datos__ocupacion$PONDERA_anual,
    DESOCUPADO = datos__ocupacion$DESOCUPADO * datos__ocupacion$PONDERA_anual,
    ACTIVO = datos__ocupacion$ACTIVO * datos__ocupacion$PONDERA_anual,
    POBLACION = datos__ocupacion$POBLACION * datos__ocupacion$PONDERA_anual
  ),
  by = list(
    ANO4 = datos__ocupacion$ANO4,
    AGLOMERADO = datos__ocupacion$AGLOMERADO # Para hacerlo por aglom.
  ),
  FUN = sum,
  na.rm = TRUE
)


tasas_aglom$tasa_actividad <- round(100 * tasas_aglom$ACTIVO / tasas_aglom$POBLACION, 2)
tasas_aglom$tasa_empleo <- round(100 * tasas_aglom$OCUPADO / tasas_aglom$POBLACION, 2)
tasas_aglom$tasa_desocupacion <- round(100 * tasas_aglom$DESOCUPADO / tasas_aglom$ACTIVO, 2)

print(tasas_aglom[, c("ANO4", "tasa_actividad", "tasa_empleo", "tasa_desocupacion")])
save(tasas_aglom, file = "tasas_evolucion_aglom.RData")
#---------- GRAFICOS

load("D:/Cuatri_III/IAD/TP_r/tasas_evolucion.RData")
library(ggplot2)

ggplot(tasas, aes(x = ANO4)) +
  geom_line(aes(y = tasa_actividad, color = "Actividad")) +
  geom_point(aes(y = tasa_actividad, color = "Actividad")) +
  geom_text(aes(y = tasa_actividad, label = tasa_actividad, color = "Actividad"), vjust = -0.8, size = 3) +
  
  
  geom_line(aes(y = tasa_empleo, color = "Empleo")) +
  geom_point(aes(y = tasa_empleo, color = "Empleo")) +
  geom_text(aes(y = tasa_empleo, label = tasa_empleo, color = "Empleo"), vjust = -0.8, size = 3) +
  
  
  geom_line(aes(y = tasa_desocupacion, color = "Desocupación")) +
  geom_point(aes(y = tasa_desocupacion, color = "Desocupación")) +
  geom_text(aes(y = tasa_desocupacion, label = tasa_desocupacion, color = "Desocupación"), vjust = -0.8, size = 3) +
  

  scale_x_continuous(breaks = tasas$ANO4) +
  labs(title = "Evolución de tasas laborales (Región 43)",
       y = "Porcentaje",
       x = "Año", color ="Tasas")
# write.table(tasas, file ="tasas_evo.csv",sep=";", row.names = F)