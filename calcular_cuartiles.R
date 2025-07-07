library(dplyr)
library(Hmisc)
library(tidyr)
library(purrr)
library(ggplot2)

cuartiles_aglom <- datosp21_deflactados %>%
  group_by(ANO4, AGLOMERADO) %>%
  nest() %>%
  mutate(
    Q = map(data, ~ {
      Hmisc::wtd.quantile(.x$P21_deflactado,
                          weights = .x$PONDIIO_anual,
                          probs = c(0.25, 0.5, 0.75),
                          na.rm = TRUE)
    })
  ) %>%
  select(-data) %>%
  unnest_wider(Q, names_sep = "_")
#renombrar
cuartiles_aglom <- cuartiles_aglom %>%
  rename(Q1 = `Q_25%`,
         Q2 = `Q_50%`,
         Q3 = `Q_75%`)

#---------- poner nombre a aglom
library(eph)
data("diccionario_aglomerados")
cuartiles_aglom <- cuartiles_aglom %>%
  left_join(diccionario_aglomerados,
            by = c("AGLOMERADO" = "codigo"))
#----guardar
save(cuartiles_aglom, file = "cuartiles_aglom.RData")
library(writexl)
write_xlsx(cuartiles_aglom, "cuartiles_aglom.xlsx")
#----
library(ggplot2)

ggplot(cuartiles_aglom, aes(x = ANO4, group = AGLOMERADO)) +
  geom_line(aes(y = Q1, color = "Q1 (25%)")) +
  geom_line(aes(y = Q2, color = "Q2 (Mediana)")) +
  geom_line(aes(y = Q3, color = "Q3 (75%)")) +
  geom_point(aes(y = Q1, color = "Q1 (25%)")) +
  geom_point(aes(y = Q2, color = "Q2 (Mediana)")) +
  geom_point(aes(y = Q3, color = "Q3 (75%)")) +
  
  facet_wrap(~ aglo) +
  labs(
    title = "Evolución de cuartiles de ingresos por aglomerado -  Region Pampeana",
    x = "Año",
    y = "Ingreso",
    color = "Cuartil"
  ) +
  theme_minimal()
# dispersion, rango intercuartilico:

cuartiles_aglom$rango_intercuartilico  <- cuartiles_aglom$Q3 - cuartiles_aglom$Q1
#////meida de dispersion grafico
ggplot(cuartiles_aglom, aes(x = ANO4)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = "Rango intercuartílico"), alpha = 0.3) +
  geom_line(aes(y = Q2, color = "Mediana"), size = 1) +
  geom_point(aes(y = Q2, color = "Mediana")) +
  facet_wrap(~ aglo) +
  labs(
    title = "Mediana y rango intercuartílico de ingresos anuales por aglomerado - Ocupación principal, Región Pampeana (2016 -2024)",
    x = "Año",
    y = "Ingreso deflactado",
    fill = "",
    color = ""
  ) +
  theme_minimal()