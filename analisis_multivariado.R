# install.packages("ggplot2");
# install.packages("ggcorrplot");
use("ggplot2");
use("ggcorrplot");
library('dplyr')

load("D:/Descargas/datosp21_deflactados.RData")
setwd('C:/Users/santi/Desktop/TP AID')

# Filtrar que variables que usemos sean respondidas. Saco los valores de las variables que no responde y que este trabajando
# filter: filtra por condicion
if("ESTADO" %in% names(datosp21_deflactados) & "CH15" %in% names(datosp21_deflactados)) {
                                                            #ESTADO Con trabajo 
  datosp21_con_trabajo <- filter(datosp21_deflactados, ESTADO == 1, CH15 != 9);
                                                            #P21 mas de 0 ingresos, CH15 responde
}else{
  print("La columna estado o  ch15 no existen");
}

#Creo una nueva variable NAC, que divide la variable ch15 en categoria nacional o extranjero
#segun su valor
datosp21_con_trabajo <- datosp21_con_trabajo %>%
  mutate(
    NAC = case_when( #Le doy el valor de la nueva variable (NAC) segun la original (CH15)
      CH15 %in% c(1, 2, 3) ~ "Nacional", #Valores 1, 2, 3 = Nacional
      CH15 %in% c(4, 5) ~ "Extranjero", #Valores 4, 5 = Extranjero
    ),
    #Indico que la variable es categorica, 
    NAC = factor(NAC)
  )%>%
  mutate(
    
    EST = case_when( #Le doy el valor de la nueva variable (EST) segun la original (NIVEL_ED)
      NIVEL_ED %in% c(1, 7) ~ "Sin estudios", #Valores 1, 7 = Sin Estudio
      NIVEL_ED %in% c(2, 3) ~ "Primario", #Valores 2, 3 = Primario
      NIVEL_ED %in% c(4, 5) ~ "Secundario", #Valores 4, 5 = Secundario
      NIVEL_ED %in% c(6) ~ "Universitario", #Valores 6 = Universitario
    ),
    
    #Indico que la variable es categorica, 
    EST = factor(EST)
  )

anios <- 2016:2024

obtenerPromedioPorAno <- function(){
  
  #Creo la estructura de un df nuevo para luego ingresarle las filas
  retorno <- data.frame(
    ANO4 = integer(),
    
    #Nacionalidad
    PROMEDIO_NACIONAL = numeric(),
    PROMEDIO_EXTRANJERO = numeric(),
    
    #Estudios
    
    PROMEDIO_SIN_EST = numeric(),
    PROMEDIO_PRIMARIO = numeric(),
    PROMEDIO_SECUNDARIO = numeric(),
    PROMEDIO_UNIVERSITARIO = numeric()
  )
  
  for(ano in anios){
    
    #Filtro la tabla por cada ano
    df_por_ano <- filter(datosp21_con_trabajo, ANO4 == ano);
    
    #Guardo el promedio del p21 que cumpla con la condicion:
    #Que sea local
    promedio_nacional <- with(df_por_ano, sum(P21_deflactado[NAC == 'Nacional'] * PONDIIO_anual[NAC == 'Nacional']) /
                                sum(PONDIIO_anual[NAC == 'Nacional']));
    
    #Que sea extranjero
    promedio_extranjero <- with(df_por_ano, sum(P21_deflactado[NAC == 'Extranjero'] * PONDIIO_anual[NAC == 'Extranjero']) /
                                  sum(PONDIIO_anual[NAC == 'Extranjero']));
    
    #Que no tenga estudios
    promedio_sin_estudios <- with(df_por_ano, sum(P21_deflactado[EST == 'Sin estudios'] * PONDIIO_anual[EST == 'Sin estudios']) /
                                    sum(PONDIIO_anual[EST == 'Sin estudios']));
    
    #Que tenga el primario completo
    promedio_primario <- with(df_por_ano, sum(P21_deflactado[EST == 'Primario'] * PONDIIO_anual[EST == 'Primario']) /
                                sum(PONDIIO_anual[EST == 'Primario']));
    
    #Que tenga el secundario completo
    promedio_secundario <- with(df_por_ano, sum(P21_deflactado[EST == 'Secundario'] * PONDIIO_anual[EST == 'Secundario']) /
                                  sum(PONDIIO_anual[EST == 'Secundario']));
    
    #Que tenga el universitario completo
    promedio_universitario <- with(df_por_ano, sum(P21_deflactado[EST == 'Universitario'] * PONDIIO_anual[EST == 'Universitario']) /
                                     sum(PONDIIO_anual[EST == 'Universitario']));
    
    
    #rbind ingresa una fila               #Parámetros:
    retorno <- rbind(retorno, data.frame( #Estructura de retorno, e ingresa toda la fila por el nombre de las columnas
      ANO4 = ano,
      PROMEDIO_NACIONAL = promedio_nacional,
      PROMEDIO_EXTRANJERO = promedio_extranjero,
      PROMEDIO_SIN_EST = promedio_sin_estudios,
      PROMEDIO_PRIMARIO = promedio_primario,
      PROMEDIO_SECUNDARIO = promedio_secundario,
      PROMEDIO_UNIVERSITARIO = promedio_universitario
    )) #Ingreso una fila con los valores recorridos recien por ano.
  }
  
  return (retorno)
}


df_bivarial_nac <- obtenerPromedioPorAno();


#Grafico de lineas: evolucion ingreso promedio segun nivel educativo
ggplot(df_bivarial_nac, aes(x = ANO4)) + #Indico el df que voy a utilizar y el eje X que va a ser el mismo para todas las lineas y puntos
  # Indico el eje y
  #Creo las lineas del eje Y de cada caso 
  geom_line(aes(y = PROMEDIO_SIN_EST, color = "Sin estudios")) +
  geom_line(aes(y = PROMEDIO_PRIMARIO, color = "Primario")) +
  geom_line(aes(y = PROMEDIO_SECUNDARIO, color = "Secundario")) +
  geom_line(aes(y = PROMEDIO_UNIVERSITARIO, color = "Universitario")) +

  #Creo los puntos del eje Y en cada caso
  geom_point(aes(y = PROMEDIO_SIN_EST, color = "Sin estudios")) +
  geom_point(aes(y = PROMEDIO_PRIMARIO, color = "Primario")) +
  geom_point(aes(y = PROMEDIO_SECUNDARIO, color = "Secundario")) +
  geom_point(aes(y = PROMEDIO_UNIVERSITARIO, color = "Universitario")) +

  #Indico el valor el valor del ingreso en cada año
  geom_text(aes(y = PROMEDIO_SIN_EST + 400, label= trunc(PROMEDIO_SIN_EST), color = "Sin estudios")) +
  geom_text(aes(y = PROMEDIO_PRIMARIO + 400, label= trunc(PROMEDIO_PRIMARIO), color = "Primario")) +
  geom_text(aes(y = PROMEDIO_SECUNDARIO + 400, label= trunc(PROMEDIO_SECUNDARIO), color = "Secundario")) +
  geom_text(aes(y = PROMEDIO_UNIVERSITARIO + 400, label= trunc(PROMEDIO_UNIVERSITARIO), color = "Universitario")) +
  
  
  labs(
    title = "Ingreso promedio por año segun su nivel educativo",
    y = "Ingreso promedio",
    x = "Año",
    color = "Nivel de estudio"
  )+ #Ingreso texto al grafico para ser mas explicativo
  
  #Le indico colores a cada linea
  scale_color_manual(values = c("Sin estudios" = "red", "Primario" = "orange",
                                "Secundario" = "yellow", "Universitario" = "green")) 



#Codigo con el que vi las correlaciones de p21 con las demas variables interesantes a analizar
# corRegion43 <- cor(region43_soloNumerico[9:15]);
# ggcorrplot(corRegion43, lab=TRUE, insig="blank");


#grafico de bararas
library(ggplot2)
library(dplyr)

# Reorganizamos el dataframe para tener una columna "NACIONALIDAD"
df_barras <- df_bivarial_nac |>
  select(ANO4, PROMEDIO_NACIONAL, PROMEDIO_EXTRANJERO) |>
  tidyr::pivot_longer(
    cols = c(PROMEDIO_NACIONAL, PROMEDIO_EXTRANJERO),
    names_to = "NACIONALIDAD",
    values_to = "INGRESO"
  ) |>
  mutate(NACIONALIDAD = ifelse(NACIONALIDAD == "PROMEDIO_NACIONAL", "Nacional", "Extranjero"))

# Gráfico de barras horizontales
ggplot(df_barras, aes(x = factor(ANO4), y = INGRESO, fill = NACIONALIDAD)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = trunc(INGRESO)), 
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Ingreso promedio por año según nacionalidad",
    x = "Año",
    y = "Ingreso promedio",
    fill = "Nacionalidad"
  ) +
  scale_fill_manual(values = c("Nacional" = "#79a3df", "Extranjero" = "#eeec9c")) +
  theme_minimal()
