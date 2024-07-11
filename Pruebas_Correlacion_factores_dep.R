###
# Título : Pruebas de correlación para las puntuaciones del idere con los factores de aforntameinto y resiliencia
# Nombre : Ailín Malagón Silva 
# Fecha : 10-07-2024
# Descripción : Este script ejecuta las pruebas de correlación de las puntuaciones del cuestionario IDERE con cada factor de afrontameinto y resiliencia   
# utilizando como input Base_CSI_Res.csv y dando como output Pruebas_Correlacion_factores_dep
###


#Input Dir 
indir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/CopingStrategiesLupus/data"

#Output Dir
outdir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/CopingStrategiesLupus"

#Cargar Dataset 
CSI_Res_base <- read.csv("data/Base_CSI_Res.csv")

# cargar paquetes 
library(tidyverse)

### Limpieza de datos : Crear un df que permita relaizar las correlaciones ####

# Omitir la columna sex 2 porque repite información y omitir las columnas que no son de interés 
CSI_Res_correlation_dep <-  CSI_Res_base %>% 
  select(-sex___2, -nephritis, -age_group, -niv_dep_est, -niv_dep_ras)

#Renombrar la columa sex_1 a Género y la columna edad 
colnames(CSI_Res_correlation_dep)[colnames(CSI_Res_correlation_dep) == "sex___1"] <- "gender"
colnames(CSI_Res_correlation_dep)[colnames(CSI_Res_correlation_dep) == "calculated_age"] <- "age"

#Convetir de la columna "gender" los valores de 0 & 1 a hombre y mujer y luego a factor 
CSI_Res_correlation_dep <- CSI_Res_correlation_dep %>%
  mutate(gender = case_when(
    gender == 1 ~ "Mujer",
    gender == 0 ~ "Hombre"
  ),
  gender = as.factor(gender)
  ) 

# Filtrar los NA de  todas las columnas 
CSI_Res_correlation_dep <- CSI_Res_correlation_dep %>%   filter(if_all(age:sumaescalaafrores,~ !is.na(.)))

#Quitar controles de la columna Lupus / personas que no hayan puesto su edad (NA) / o que su edad sea igual a 0

CSI_Res_correlation_dep <- CSI_Res_correlation_dep %>%
  mutate( lupus = if_else (lupus == 1, lupus, NA_real_) ) %>%
  filter(!is.na(age) & !is.na(lupus) & (age != 0 ) & (age != 1643 ))

# Correlaciones para resiliencia con puntuaciones de estado 
cor(CSI_Res_correlation_dep[, c("factunofcmsumatotal","factdoscompsumatotal", "factresapoyofamsumatotal", "factcuatroapoyosocsumt", "factcincoestructsumtotal", "est_dep")])

# Correlaciones para resiliencia con puntuaciones de rasgo 
cor(CSI_Res_correlation_dep[, c("factunofcmsumatotal","factdoscompsumatotal", "factresapoyofamsumatotal", "factcuatroapoyosocsumt", "factcincoestructsumtotal", "ras_dep")])



 
## ANÁLISIS DE CORRELACIONES  ####

#Factores resiliencia con puntuaciones de estado ####


# Evaluación de un solo factor 
cor.test(CSI_Res_correlation_dep$est_dep, CSI_Res_correlation_dep$factunofcmsumatotal, method = "spearman", exact = FALSE )

# Creación del vector para poder evaluar todos los factores es un bucle

factores_RESI<- c("factunofcmsumatotal", "factdoscompsumatotal", "factresapoyofamsumatotal", 
              "factcuatroapoyosocsumt", "factcincoestructsumtotal")

results_RESI_est <- list()  # Lista para almacenar los resultados



for (factor in factores_RESI) {
  # Realizar la prueba de correlación de Spearman
  cor_test <- cor.test(CSI_Res_correlation_dep$est_dep, CSI_Res_correlation_dep[[factor]], 
                       method = "spearman", exact = FALSE)
  # Almacenar los resultados en la lista
  results_RESI_est[[factor]] <- cor_test
}

# Mostrar los resultados 
for (factor in factores_RESI) {
  cat("Factor:", factor, "\n")
  print(results_RESI_est[[factor]])
  cat("\n")
}



#Factores resiliencia con puntuaciones de rasgo####

results_RESI_ras <- list()  # Lista para almacenar los resultados



for (factor in factores_RESI) {
  # Realizar la prueba de correlación de Spearman
  cor_test <- cor.test(CSI_Res_correlation_dep$ras_dep, CSI_Res_correlation_dep[[factor]], 
                       method = "spearman", exact = FALSE)
  # Almacenar los resultados en la lista
  results_RESI_ras[[factor]] <- cor_test
}

# Mostrar los resultados 
for (factor in factores_RESI) {
  cat("Factor:", factor, "\n")
  print(results_RESI_ras[[factor]])
  cat("\n")
}

#Factores afrontameinto con puntuaciones de estado ####

# Creación del vector para poder evaluar todos los factores es un bucle

factores_CSI<- c("sumaescalaafrorep", "sumaescalaafroauc", "sumaescalaafroeem", "sumaescalaafropsd",
                 "sumaescalaafroaps", "sumaescalaafrorec",  "sumaescalaafroevp", "sumaescalaafrores")

results_CSI_est <- list()  # Lista para almacenar los resultados

for (factor in factores_CSI) {
  # Realizar la prueba de correlación de Spearman
  cor_test <- cor.test(CSI_Res_correlation_dep$est_dep, CSI_Res_correlation_dep[[factor]], 
                       method = "spearman", exact = FALSE)
  # Almacenar los resultados en la lista
  results_CSI_est[[factor]] <- cor_test
}

# Mostrar los resultados 
for (factor in factores_CSI) {
  cat("Factor:", factor, "\n")
  print(results_CSI_est[[factor]])
  cat("\n")
}

#Factores afrontameinto con puntuaciones de rasgo ####

results_CSI_ras <- list()  # Lista para almacenar los resultados

for (factor in factores_CSI) {
  # Realizar la prueba de correlación de Spearman
  cor_test <- cor.test(CSI_Res_correlation_dep$ras_dep, CSI_Res_correlation_dep[[factor]], 
                       method = "spearman", exact = FALSE)
  # Almacenar los resultados en la lista
  results_CSI_ras[[factor]] <- cor_test
}

# Mostrar los resultados 
for (factor in factores_CSI) {
  cat("Factor:", factor, "\n")
  print(results_CSI_ras[[factor]])
  cat("\n")
}

#Guardar mis varibales 
save(CSI_Res_correlation_dep, file = "data/data_correlation_factors_dep.RData")

