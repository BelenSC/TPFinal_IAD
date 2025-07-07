library(dplyr)
library(Hmisc)
library(tidyr)
library(purrr)
library(ggplot2)

deciles_aglom <- datosp21_deflactados %>%
  group_by(ANO4, AGLOMERADO) %>%
  nest() %>%
  mutate(
    Q = map(data, ~ {
      Hmisc::wtd.quantile(.x$P21_deflactado,
                          weights = .x$PONDIIO_anual,
                          probs =  seq(0.1, 0.9, by = 0.1),
                          na.rm = TRUE)
    })
  ) %>%
  select(-data) %>%
  unnest_wider(Q, names_sep = "_")
#renombrar
deciles_aglom <- deciles_aglom %>%
  rename(D10 = `Q_10%`,
         D20 = `Q_20%`,
         D30 = `Q_30%`,
         D40 = `Q_40%`,
         D50 = `Q_50%`,
         D60 = `Q_60%`,
         D70 = `Q_70%`,
         D80 = `Q_80%`,
         D90 = `Q_90%`)

#---------- poner nombre a aglom
library(eph)
data("diccionario_aglomerados")
deciles_aglom <- deciles_aglom %>%
  left_join(diccionario_aglomerados,
            by = c("AGLOMERADO" = "codigo"))



#----guardar
# save(deciles_aglom, file = "deciles_aglom.RData")
# library(writexl)
# write_xlsx(deciles_aglom, "deciles_aglom.xlsx")
#----
library(tidyr)
library(dplyr)
library(ggplot2)

deciles_long <- deciles_aglom %>%
  filter(AGLOMERADO == 4) %>%
  # filter(aglo %in% c(2, 3, 33, 34, 38)) %>%
  pivot_longer(
    cols = starts_with("D"), 
    names_to = "Decil",
    values_to = "Ingreso"
  )

# Gráfico
ggplot(deciles_long, aes(x = ANO4, y = Ingreso, color = Decil)) +
  geom_line() +
  # geom_point() +
  facet_wrap(~ aglo) +
  labs(
    title = "Evolución de deciles de ingreso, Gran Rosario, Región Pampeana",
    x = "Año",
    y = "Ingreso deflactado"
  ) +
  
  # scale_x_continuous(breaks = unique(deciles_long$ANO4)) +
  theme_minimal()

