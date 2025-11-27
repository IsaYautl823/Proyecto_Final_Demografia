#******************************#
#******************************#
#
#                          Trabajo Final del Curso de Demografía 
#                                        2026_1
#                         Facultad de Ciencias UNAM
#
#                 Tablas de mortalidad San Luis Potosí 2010,2020
#                                 Años Persona Vividos
#
#
#         Creado por:               Isa Yautl Cortes Lima. Natalia Cruz Noriega
#         Fecha de creación:        04/11/2025
#         Actualizado por:          Andrés Peña M.
#         Fecha de actualización:   06/11/2025
#         Contacto:                 isacorlim@ciencias.unam.mx
#
#******************************#
#******************************#

# Preambulo ----

## Limpieza de gráficas ----


graphics.off()

## Limpieza de memoria ----

rm(list = ls())


## Carga de paquetes y Funciones ----

source("scripts/functions.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de tablas de datos ----
censos_pro <- fread("data/censos_pro_sl.csv")

# Cálculo de años persona vividos (población a mitad de año)
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex, N)]
apv2010[,year := 2010]



## Grafica de la piramide poblacional 2010

a <-apv2010[age<5]
a[,N:= sum(N),sex]
datospiramides <- rbind(a,apv2010[age>= 5]) 
datospiramides <- datospiramides[age != 1,]
 

ggplot(datospiramides, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#FB64B6"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2010",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo:",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Calculo de APV 2020 ----

# Cálculo de años persona vividos (población a mitad de año)
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)

apv2020 <- censos_pro[year==2020, .(age, sex, N)]
apv2020[, year := 2019]


## Grafica de la piramide poblacional 2019

a <-apv2020[age<5]
a[,N:= sum(N),sex]
datospiramides <- rbind(a,apv2020[age>= 5]) 
datospiramides <- datospiramides[age != 1,]

ggplot(datospiramides, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#FB64B6"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2019",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
    
  )

# Calculo de APV 2021----

# Cálculo de años persona vividos (población a mitad de año)
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)

apv2021 <- censos_pro[year==2020, .(age, sex, N)]
apv2021[, year := 2021]


## Grafica de la piramide poblacional 2021
a <-apv2021[age<5]
a[,N:= sum(N),sex]
datospiramides <- rbind(a,apv2021[age>= 5]) 
datospiramides <- datospiramides[age != 1,]

ggplot(datospiramides, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#FB64B6"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2021",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
    
  )

apv <- rbind(apv2010,apv2020,apv2021)
# Guardar tabla de censos ----

write.csv(apv,"data/apv_sl.csv")