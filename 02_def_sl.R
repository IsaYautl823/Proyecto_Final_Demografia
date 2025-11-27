#******************************#
#******************************#
#
#                          Trabajo Final del Curso de Demografía 
#                                        2026_1
#                         Facultad de Ciencias UNAM
#
#                 Tablas de mortalidad San Luis Potosí 2010,2020
#                                 Defunciones
#
#
#         Creado por:               Isa Yautl Cortes Lima.
#                                   Natalia Cruz Noriega
#         Fecha de creación:        11/11/2025
#         Actualizado por:          Isa Yautl Cortes.
#         Fecha de actualización:   25/11/2025
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
def_pro <- fread("data/def_pro_sl.csv") %>% 
  .[year %in% c(2009, 2010, 2011, 2018, 2019,2020,2021,2022)]


## calculo del promedio para el año de referencia
def_pro[ , year_new := ifelse( year %in% 2009:2011,2010,
                               ifelse( year %in% 2018:2019,2019,
                                     ifelse( year %in% 2020:2021,
                                       2021,
                                        year )))]

# datos preparados de defunciones
def <- 
  def_pro[ , 
           .( deaths = mean( deaths ) ),
           .( year = year_new, sex, age ) ] 

# Gráficas----

def_pro <- fread("data/def_pro_sl.csv")  
def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]

ggplot(def_gr, aes(x = year, y = deaths, color = sex, group = sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Evolución de Defunciones por Sexo (1990-2024)",
       x = "Año",
       y = "Número de Defunciones",
       color = "Sexo") +
  scale_color_manual(values = c("male" = "blue", "female" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

## Gráfica  Defunciones por grupos de edad (promedio 2010-2021) ----

def_edad <- def_pro[year %in% 2010:2021, .(deaths = mean(deaths)), .(age, sex)]

ggplot(def_edad, aes(x = age, y = deaths, fill = sex)) +
  geom_col(position = "dodge") +
  labs(title = "Defunciones Promedio por Edad y Sexo (2010-2021)",
       x = "Edad",
       y = "Defunciones Promedio",
       fill = "Sexo") +
  scale_fill_manual(values = c("male" = "blue", "female" = "red")) +
  theme_minimal()


# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro.csv", row.names = F)

# Guardar tabla de DEF ----
write.csv(def, "data/def_sl.csv", row.names = F)
