###
# Título : Visualización de datos de las variables de resiliencia y afrontamiento en pacientes con Lupus
# Nombre : Ailín Malagón Silva 
# Fecha : 26-06-2024
# Descripción : Este script ejecuta spider graphs de los datos de resilienia y afrontamiento 
# utilizando como input Base_CSI_Res.csv y dando como output CopingStrategiesLupus
###


#Input Dir 
indir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/CopingStrategiesLupus/data"

#Output Dir
outdir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/CopingStrategiesLupus"

#Cargar Dataset 
CSI_Res_base <- read.csv("data/Base_CSI_Res.csv")

# instalación de paquetes
#install.packages("ggplot2")
#installed.packages("tidyverse")
#install.packages("patchwork")
#install.packages("fmsb")

# cargar paquetes 
library(ggplot2)
library(tidyverse)
library(patchwork)
library(fmsb)

### Limpieza de datos ####


# Omitir la columna sex 2 porque repite información y omitir las variables del cuestionario idere
CSI_Res_base <- CSI_Res_base %>% 
  select(-sex___2, -nephritis, -est_dep, -ras_dep, -niv_dep_est, -niv_dep_ras)
 
#Renombrar la columa sex_1 a Género y la columna edad 
colnames(CSI_Res_base)[colnames(CSI_Res_base) == "sex___1"] <- "gender"
colnames(CSI_Res_base)[colnames(CSI_Res_base) == "calculated_age"] <- "age"

#Convetir de la columna "gender" los valores de 0 & 1 a hombre y mujer y luego a factor 
CSI_Res_base <- CSI_Res_base %>%
  mutate(gender = case_when(
    gender == 1 ~ "Mujer",
    gender == 0 ~ "Hombre"
  ),
  gender = as.factor(gender)
  ) 
# Creación de df que engloba unicamente las variables de resiliencia & quitar los NA de estas columnas

RESI_M <- CSI_Res_base %>% 
  select(-sumaescalaafrorep, -sumaescalaafroauc, -sumaescalaafroeem,  -sumaescalaafropsd,
         -sumaescalaafroaps, -sumaescalaafrorec, -sumaescalaafroevp, -sumaescalaafrores) %>%  
  filter(if_all(factunofcmsumatotal:factcincoestructsumtotal,~ !is.na(.)))
# Filtrar el df RESI_M 
#Quitar controles de la columna Lupus / personas que no hayan puesto su edad (NA) / o que su edad sea igual a 0

 RESI_M <- RESI_M %>%
           mutate( lupus = if_else (lupus == 1, lupus, NA_real_) ) %>%
             filter(!is.na(age) & !is.na(lupus) & (age != 0 ) & (age != 1643 ))
 
# Creación de df que engloba unicamente las variables de afrontamiento & quitar los NA de estas columnas 
 CSI <- CSI_Res_base %>% 
   select(-factunofcmsumatotal, -factdoscompsumatotal, -factresapoyofamsumatotal,
          -factcuatroapoyosocsumt, -factcincoestructsumtotal) %>%  
   filter(if_all(sumaescalaafrorep:sumaescalaafrores,~ !is.na(.)))
 
#Filtrar el df CSI 
#Quitar controles de la columna Lupus / personas que no hayan puesto su edad (NA) / o que su edad sea igual a 0
 CSI <- CSI %>%
   mutate(lupus = if_else(lupus == 1, lupus, NA_real_) ) %>%
     filter(!is.na(age) & !is.na(lupus) & (age != 0 ) & (age != 1643 ))

 
 
 ### Spider_graph de RESI_M ####

### Sacar la media de cada factor para simplificar los datos
    #seleccionar únicamente los factores  
RESI_factors <-RESI_M %>% select(factunofcmsumatotal:factcincoestructsumtotal)
 
    #sacar la media de cada factor y redondear las medias al entero más cercano
 media_de_RESI_factors <- RESI_factors %>%
   summarise_all(~ round(mean(.), 0))
 
    #renombrar los elementos dentro del vector 
 names(media_de_RESI_factors) <- c("FCM", "COMP", "APOYOFAM", "APOYOSOC", "ESTRUCT")
 
    #crear  un df con los valores  máximos y mínimos
 max_min_RESI <- data.frame (
   FCM = c(100, 0), COMP = c(100, 0), APOYOFAM = c(100, 0),
   APOYOSOC = c(100, 0), ESTRUCT = c(100 ,0)
                       )
 rownames(max_min_RESI) <- c("Max", "Min")
   #Unir los df anteriores  para que tengan el formato adecuado para una radar chart (max_min_RESI & media_de_RESI_factors)
 df_radar_RESI <- rbind(max_min_RESI, media_de_RESI_factors)
 
 # Plot 
 radarchart(df_radar_RESI,
            axistype = 1, #tipo de ejes radiales
            seg = 5,#núm de segmentos o niveles en los ejes radiales
            pcol=rgb(0.2,0.5,0.5,0.9),# Color de las líneas de área del gráfico
            pfcol=rgb(0.2,0.5,0.5,0.5), # Color de relleno del área 
            plwd = 4, plty = 1, axislabcol = "black", # Grosor y tipo de línea para conectar puntos
            cglcol = "grey", cglty = 1, cglwd = 0.8,  # Configuración de la cuadrícula de fondo
            vlcex = 0.6, # Tamaño de las etiquetas
            caxislabels = seq(0, 100, 20)  # Etiquetas de los ejes radiales
            )
 
 
 
 
 ### Spider_graph de CSI ####
 
 #seleccionar únicamente los factores  
 CSI_factors <- CSI %>% select(sumaescalaafrorep:sumaescalaafrores)
 
 #sacar la media de cada factor y redondear las medias al entero más cercano
 media_de_CSI_factors <- CSI_factors %>%
   summarise_all(~ round(mean(.), 0))
 
 #renombrar los elementos dentro del vector 
 names(media_de_CSI_factors) <- c("AFROREP", "AFROAUC", "AFROEEM", "AFROPSD",
                                  "AFROAPS","AFROREC", "AFROEVP", "AFRORES")
 #crear  un df con los valores  máximos y mínimos
 max_min_CSI <- data.frame (
   AFROREP = c(20, 0),  AFROAUC = c(20, 0), AFROEEM = c(20, 0),
   AFROPSD = c(20, 0), AFROAPS = c(20 ,0),  AFROREC = c(20 ,0),
   AFROEVP = c(20 ,0), AFRORES = c(20 ,0)
   )
 
 rownames(max_min_CSI) <- c("Max", "Min")
 #Unir los df anteriores  para que tengan el formato adecuado para una radar chart (max_min_CSI & media_de_CSI_factors)
 df_radar_CSI <- rbind(max_min_CSI, media_de_CSI_factors) 
 
 
 #plot 
 radarchart(df_radar_CSI,
            axistype = 1, #tipo de ejes radiales
            seg = 4,#núm de segmentos o niveles en los ejes radiales
            pcol=rgb(0.2,0.5,0.5,0.9),# Color de las líneas de área del gráfico
            pfcol=rgb(0.2,0.5,0.5,0.5), # Color de relleno del área 
            plwd = 4, plty = 1, axislabcol = "black", # Grosor y tipo de línea para conectar puntos
            cglcol = "grey", cglty = 1, cglwd = 0.8,  # Configuración de la cuadrícula de fondo
            vlcex = 0.6, # Tamaño de las etiquetas
            caxislabels = seq(0, 20, 5)  # Etiquetas de los ejes radiales
            )
 
 ### Gráfico violin para RESI_M ####
 
 # Pasar los factores de formato ancho a largo 
 RESI_M_longer <- RESI_M %>%
   pivot_longer( cols = c(factunofcmsumatotal:factcincoestructsumtotal),
                        names_to = "Factores",
                        values_to = "Puntuacion"
                                                 ) %>%
                 mutate(Factores = as.factor(Factores))
   

#Gráfico 
 
 ggplot(RESI_M_longer, aes(x = Factores, y = Puntuacion, fill = Factores)) +
   geom_violin() +
   geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Agregar boxplots si deseas
   scale_fill_hue() +  # escala de colores
   theme_minimal() +  
   labs(x = "Factores", y = "Puntuación", title = "Distribuciones de Factores Resiliencia") +
   scale_y_continuous(breaks = seq(0, 80, by = 20),  # Ajustar los valores del eje y
                      limits = c(0, 80)) + # Ajustar los límites del eje y
   coord_flip()
 
 
 ### Gráfico violin para CSI ####
 
 #Pasar los factores de formato ancho a largo 
 CSI_longer <- CSI %>%
   pivot_longer( cols = c(sumaescalaafrorep:sumaescalaafrores),
                 names_to = "Factores",
                 values_to = "Puntuacion"
   ) %>%
   mutate(Factores = as.factor(Factores))
 #Gráfico 
 ggplot(CSI_longer, aes(x = Factores, y = Puntuacion, fill = Factores)) +
   geom_violin() +
   geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Agregar boxplots si deseas
   scale_fill_hue() +  # escala de colores
   theme_minimal() +  
   labs(x = "Factores", y = "Puntuación", title = "Distribuciones de Factores Afrontamiento") +
   scale_y_continuous(breaks = seq(0, 20, by = 2),  # Ajustar los valores del eje y
                      limits = c(0, 20)) +    # Ajustar los límites del eje Y   
   coord_flip()
         
 ### Multiple Radar Chart por grupos de edades #### 
 
 #Convertir a factor la columna age_group    
 RESI_M$age_group <- factor(RESI_M$age_group)
 
 #Agrupar por grupo de edad y calcular sus medias 
 Media_Scores_by_AgeGroups_RESI <-RESI_M %>%
   group_by(age_group) %>%
   summarise(
     Factor1 = round(mean(factunofcmsumatotal), 0),
     Factor2 = round(mean(factdoscompsumatotal), 0),
     Factor3 = round(mean(factresapoyofamsumatotal), 0),
     Factor4 = round(mean(factcuatroapoyosocsumt), 0),
     Factor5 = round(mean(factcincoestructsumtotal), 0)
   )
  #Seleccionar unicamente los resultados
 Results_RESI_AgeGroups <- Media_Scores_by_AgeGroups_RESI %>% select(Factor1:Factor5)
  #Calcular máximos y mínimos 
 max_min_RESI_by_AgeGroups <- data.frame (
   Factor1 = c(100, 0), Factor2 = c(100, 0), Factor3 = c(100, 0),
   Factor4 = c(100, 0), Factor5 = c(100 ,0))
 

 # Juntar los dos df anteriores en uno mismo para crear la radar chart 
 df_Multiple_radar_RESI <- rbind(max_min_RESI_by_AgeGroups, Results_RESI_AgeGroups)
 
 #Define fill colors 
 colors_fill <- c (
   scales::alpha("deepskyblue",0.1),
   scales::alpha("gold",0.1),
   scales::alpha("tomato",0.2),
   scales::alpha("skyblue",0.2),
   scales::alpha("forestgreen",0.2),
   scales::alpha("orchid",0.2))
 
 #Define line colors 
 
 colors_line <- c(
   scales::alpha("deepskyblue",0.9),
   scales::alpha("gold",0.9),
   scales::alpha("tomato",0.9),
   scales::alpha("royalblue",0.9),
   scales::alpha("forestgreen",0.9),
   scales::alpha("orchid",0.9))
 

radarchart(df_Multiple_radar_RESI,
           axistype = 1,
           seg = 5, #Número de segmentos 
           title = "Puntuación de factores de resiliencia por edades",
           pcol = colors_line,
           pfcol = colors_fill, 
           plwd = 4,
           caxislabels = seq(0, 100, 20),
           
)

# Gráficos de violín para los 6 grupos de edades en resiliencia  (RESI_M) ####

   #Convertir a factor la columna age_group del df RESI_M_longer para que sea capaz de clasificar por grupos de edades     
   RESI_M_longer$age_group <- factor(RESI_M_longer$age_group)
  
   #Cambiar los nombres de la columna "Factores " al factor correspondiente del estudio 
  
   RESI_M_longer <- RESI_M_longer %>%
    mutate( Factores = case_when(
      Factores == "factunofcmsumatotal" ~ "Fortaleza y Confianza en sí mismo",
      Factores == "factdoscompsumatotal" ~ "Competencia para relacionarse con los demás",
      Factores == "factresapoyofamsumatotal" ~ "Apoyo Familiar",
      Factores == "factcuatroapoyosocsumt" ~ "Apoyo Social",
      Factores == "factcincoestructsumtotal" ~ "Capacidad de organización",
      TRUE ~ Factores
                 ))

  # Gráfico 
  ggplot(RESI_M_longer, aes(x = Factores, y = Puntuacion, fill = Factores)) +
  geom_violin() +
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Agregar boxplots si deseas
  scale_fill_hue() +  # escala de colores
  theme_minimal() +  
  labs(x = "Factores", y = "Puntuación", title = "Distribuciones de Factores Resiliencia") +
  scale_y_continuous(breaks = seq(0, 80, by = 20),  # Ajustar los valores del eje y
  limits = c(0, 80)) + # Ajustar los límites del eje y
  coord_flip() + #para invertir los ejes 
  facet_wrap(~ age_group) # para separa las gráficas 
  
  
# Gráfico de violin para los 6 grupos de edades para afrontamiento (CSI) ####

  
  #Convertir a factor la columna age_group del df RESI_M_longer para que sea capaz de calsificar por grupos de edades     
  CSI_longer$age_group <- factor(CSI_longer$age_group)
  #Cambiar los nombres de la columna "Factores " al factor correspondiente del estudio 
   CSI_longer <- CSI_longer %>%
    mutate( Factores = case_when(
      Factores == "sumaescalaafrorep" ~ "Resolución de problemas", 
      Factores == "sumaescalaafroauc" ~ "Autrocrítica",
      Factores == "sumaescalaafroeem" ~ "Expresión Emocional", 
      Factores == "sumaescalaafropsd" ~ "Pensamiento de superación", 
      Factores == "sumaescalaafroaps" ~ "Apoyo Social",
      Factores == "sumaescalaafrorec" ~ " Reestructuración cognitiva", 
      Factores == "sumaescalaafroevp" ~ "Evasión de problemas",  
      Factores == "sumaescalaafrores" ~ "Aislamiento Social",
      TRUE ~ Factores
    ))
   
   # Gráfico 
     ggplot(CSI_longer, aes(x = Factores, y = Puntuacion, fill = Factores)) +
     geom_violin() +
     geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +  # Agregar boxplots si deseas
     scale_fill_hue() +  # escala de colores
     theme_minimal() +  
     labs(x = "Factores", y = "Puntuación", title = "Distribuciones de Factores Afrontamiento") +
     scale_y_continuous(breaks = seq(0, 80, by = 20),  # Ajustar los valores del eje y
                        limits = c(0, 80)) + # Ajustar los límites del eje y
     coord_flip() + #para invertir los ejes 
     facet_wrap(~ age_group) # para separa las gráficas 

     
  ### Multiple Radar Chart por género  #### 
  
  #Agrupar por grupo de edad y calcular sus medias 
  Media_Scores_by_Gender_RESI <- RESI_M %>%
    group_by(gender) %>%
    summarise(
      Factor1 = round(mean(factunofcmsumatotal), 0),
      Factor2 = round(mean(factdoscompsumatotal), 0),
      Factor3 = round(mean(factresapoyofamsumatotal), 0),
      Factor4 = round(mean(factcuatroapoyosocsumt), 0),
      Factor5 = round(mean(factcincoestructsumtotal), 0)
    )
  
  #Seleccionar unicamente los resultados
  Results_RESI_Gender <- Media_Scores_by_Gender_RESI %>% select(Factor1:Factor5) 
  
  #Calcular máximos y mínimos 
  max_min_RESI_by_Gender <- data.frame (
    Factor1 = c(100, 0), Factor2 = c(100, 0), Factor3 = c(100, 0),
    Factor4 = c(100, 0), Factor5 = c(100 ,0))
 
  # Juntar los dos df anteriores en uno mismo para crear la radar chart 
  df_Multiple_radar_RESI_Bygender <- rbind(max_min_RESI_by_Gender, Results_RESI_Gender)
  
  # Plot radar chart 
  radarchart(df_Multiple_radar_RESI_Bygender,
             axistype = 1,
             seg = 5, #Número de segmentos 
             title = "Puntuación de factores de resiliencia por género",
             pcol = colors_line,
             pfcol = colors_fill, 
             plwd = 4,
             caxislabels = seq(0, 100, 20),
              )
 
#guardar  todas mis variables
 
# Variables iniciales para los primeros 4 gráficos (Gráficos de violin CSI & Resi/Spider Graph CSI & Resi)  
save(CSI_Res_base, CSI, RESI_M, RESI_factors, media_de_RESI_factors,df_radar_RESI, df_radar_CSI, RESI_M_longer, CSI_longer, file = "data/variables_CIS_Res.RData")
# Variables para Multiple Radar Chart por grupos de edades 
save(Media_Scores_by_AgeGroups_RESI, Results_RESI_AgeGroups, max_min_RESI_by_AgeGroups, df_Multiple_radar_RESI, file = "data/variables_RESI_Multiple_Radar_Chart.RData")
# Variables para Multiple Radar Chart por género 
save(Media_Scores_by_Gender_RESI, Results_RESI_Gender, max_min_RESI_by_Gender, df_Multiple_radar_RESI_Bygender, file = "data/variables_RESI_Multiple_Radar_Chart_Gender.RData")
# CSV de la data inical filtrada 
write.csv(CSI_Res_base, file = "data/CSI_Res_base_filtered.csv", row.names = FALSE)


 
 
 
 
 
 

     
 
 
