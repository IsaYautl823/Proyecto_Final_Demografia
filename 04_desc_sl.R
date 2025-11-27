#**************************************************************************************#
#**************************************************************************************#
#
#                        Trabajo Final del curso de Demografía
#                                       2026-1
#                             Facultad de Ciencias UNAM
#                      Tablas de mortalidad México 2010, 2019 y 2021
#                            Descomposición de las tablas de vida
#
#         Creado por:               Isa Yautl Cortes lima
#                                   Natalia Cruz Noriega
#                                      
#         Fecha de creación:        14/11/2025
#         Actualizado por:          Isa Yautl Cortes Lima
#         Fecha de actualización:   25/11/2025
#         Contacto:                 isacorlim@ciencias.unam.mx
#
#**************************************************************************************#
#**************************************************************************************#

# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("scripts/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)
library(knitr)

## Carga de tablas de datos ----

lt <- fread("data/lt_sl.csv")


# Preparar datos para la descomposición
descomposicion_datos <- list()

for(s in c('m', 'f')) {
  
  # Obtener datos para cada año
  lt_2010 <- lt[sex == s & year == 2010]
  lt_2019 <- lt[sex == s & year == 2019]
  lt_2021 <- lt[sex == s & year == 2021]
  
  
    
    # Comparación 2010-2019
  
    desc_2010_2019 <- desc(
      lx1 = lt_2010$lx,
      Lx1 = lt_2010$Lx,
      lx2 = lt_2019$lx,
      Lx2 = lt_2019$Lx,
      age = lt_2010$age
     
    )
    
    # Comparación 2019-2021
    
    desc_2019_2021 <- desc(
      lx1 = lt_2019$lx,
      Lx1 = lt_2019$Lx,
      lx2 = lt_2021$lx,
      Lx2 = lt_2021$Lx,
      age = lt_2021$age
      )
      
    
    # Guardar resultados
    descomposicion_datos[[paste0(s, "_2010_2019")]] <- desc_2010_2019
    descomposicion_datos[[paste0(s, "_2019_2021")]] <- desc_2019_2021
  
}


# Preparar datos para la gráfica de descomposición

descomp_hombres_2010_2019 <- descomposicion_datos[["m_2010_2019"]]
descomp_mujeres_2010_2019 <- descomposicion_datos[["f_2010_2019"]]
descomp_hombres_2019_2021 <- descomposicion_datos[["m_2019_2021"]]
descomp_mujeres_2019_2021 <- descomposicion_datos[["f_2019_2021"]]

descomp_hombres_2010_2019[,sex:="m"]
descomp_mujeres_2010_2019[,sex:="f"] 
descomp_hombres_2019_2021[,sex:="m"] 
descomp_mujeres_2019_2021[,sex:="f"]

descomp_hombres_2010_2019[,per:="2010-2019"]
descomp_mujeres_2010_2019[,per:="2010-2019"] 
descomp_hombres_2019_2021[,per:="2019-2021"] 
descomp_mujeres_2019_2021[,per:="2019-2021"]

datos_descomp <- rbind(descomp_hombres_2010_2019,
                       descomp_mujeres_2010_2019,
                       descomp_hombres_2019_2021, 
                       descomp_mujeres_2019_2021)



ggplot(datos_descomp, aes(age , dif,fill = dif > 0)) + 
  geom_col(alpha = 0.8, width = 0.7) + 
  facet_grid(sex ~per,scales = "free_x", space = "free_x") + 
  scale_fill_manual(
    values = c("TRUE" = "#2E8B57", "FALSE" = "#CD5C5C"),
    labels = c("TRUE" = "Ganancia", "FALSE" = "Pérdida"),
    name = "Efecto"
  ) +
  labs(
    title = "Descomposición de Cambios en Esperanza de Vida\n en San Luis Potosí 2010-2019 y 2019-2021",
    x = "Grupo de Edad",
    y = "Contribución (años)",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal()

