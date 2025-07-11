
load("D:/Cuatri_III/IAD/TP_r/datos_ocupacion.RData")

# datos__ocupacion$PONDERA_anual <- datos__ocupacion$PONDERA / 4

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
    AGLOMERADO = datos__ocupacion$AGLOMERADO 
  ),
  FUN = sum,
  na.rm = TRUE
)


tasas_aglom$tasa_actividad <- round(100 * tasas_aglom$ACTIVO / tasas_aglom$POBLACION, 2)
tasas_aglom$tasa_empleo <- round(100 * tasas_aglom$OCUPADO / tasas_aglom$POBLACION, 2)
tasas_aglom$tasa_desocupacion <- round(100 * tasas_aglom$DESOCUPADO / tasas_aglom$ACTIVO, 2)

print(tasas_aglom[, c("ANO4", "tasa_actividad", "tasa_empleo", "tasa_desocupacion")])
save(tasas_aglom, file = "tasas_evolucion_aglom.RData")

#---------- poner nombre a aglom
library(eph)
data("diccionario_aglomerados")
tasas_nombre_aglom <- tasas_aglom %>%
  left_join(diccionario_aglomerados,
            by = c("AGLOMERADO" = "codigo"))

#---------- GRAFICOS

load("D:/Cuatri_III/IAD/TP_r/tasas_evolucion_aglom.RData")
library(tidyr)
library(dplyr)

tasas_largo <- tasas_nombre_aglom %>%
  pivot_longer(
    cols = starts_with("tasa_"),
    names_to = "tipo_tasa",
    values_to = "valor"
  )%>%
  mutate(
    tipo_tasa = recode(
      tipo_tasa,
      "tasa_actividad" = "Actividad",
      "tasa_empleo" = "Empleo",
      "tasa_desocupacion" = "Desocupacion"
    )
  )
library(ggplot2)

ggplot(tasas_largo, aes(x = ANO4, y = valor, color = tipo_tasa)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ aglo) +
  labs(
    title = "Evolución de tasas laborales por aglomerado - Región Pampeana",
    x = "Año",
    y = "Porcentaje",
    color = "Tipo de tasa"
  ) +
  theme_minimal()

