remove(list=ls())

# Cargar datos
load("C:/Users/santi/Desktop/TP AID/datosp21_deflactados.RData");
load("C:/Users/santi/Desktop/TP AID/datos_region43_p21_na.RData")

# Filtro por quienes tienen trabajo y responden a su lugar de nacimiento
datosp21_con_trabajo <- filter(datosp21_deflactados, ESTADO == 1, CH15 != 9); 

#INTENTO DE REGRESION MULTIPLE CON VARIABLES CATEGORICAS QUE CONSIDERO
# Y= p21 deflectado
# x= EST + CH15 + AGLOMERADO + CH04 + PP04G + PP07G_59

# modelo de regresión lineal multiple
modelo <- lm(P21_deflactado ~ as.factor(NIVEL_ED) + as.factor(CH15) + AGLOMERADO + as.factor(CH04) +as.factor(PP04G)
             + as.factor(PP07G_59), data = datosp21_con_trabajo, weights = PONDIIO_anual)

# Resumen del modelo
summary(modelo)

plot(modelo)

# Dataset de registros donde P21 es menor o igual a 0
# Le paso validaciones, que estado == 1 y ch15 sea respondida y pp04G <=10*
# limpiar pp04g porque tiene valores que no responden a nada segun el eph registro

datos_region43_p21_na <- filter(datos_region43_p21_na, ESTADO == 1, CH15 != 9, PP04G <= 10)

# Corroboro que las variables independientes no tengan ni una respuesta n/a
colSums(is.na(datos_region43_p21_na))

# Predict te da un vector con los valores imputados teniendo en cuenta el modelo
valores_imputados <- predict(modelo, newdata = datos_region43_p21_na)

#Clono la variable p21 para que no superponga la misma (aunque ninguna este respondida)
datos_region43_p21_na$P21_deflactado_imputado <- datos_region43_p21_na$P21 

# A la nueva columna clonada de p21, le imputo los valores
datos_region43_p21_na$P21_deflactado_imputado <- valores_imputados
