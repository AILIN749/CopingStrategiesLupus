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
install.packages("fmsb")

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
         
 
 
 
#guardar mis variables
 
save(CSI_Res_base, CSI, RESI_M, RESI_factors, media_de_RESI_factors,df_radar_RESI, df_radar_CSI, RESI_M_longer, CSI_longer, file = "data/variables_CIS_Res.RData")
write.csv(CSI_Res_base, file = "data/CSI_Res_base_filtered.csv", row.names = FALSE)

 
 
 
 
 
 

     
 
 
