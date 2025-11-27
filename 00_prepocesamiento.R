#******************************#
#******************************#
#
#                          Trabajo Final del Curso de Demografía 
#                                        2026_1
#                         Facultad de Ciencias UNAM
#
#                 Tablas de mortalidad San Luis Potosí 2010,2020
#                                 Preprocesamiento
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

## Carga de tablas de datos ----

c2010 <- read_xlsx("data/poblacion_san_luis_2010.xlsx", sheet = 1, 
                   range = "A6:D30")
c2020 <- read_xlsx("data/poblacion_san_luis_2020.xlsx", sheet = 1, 
                   range = "A6:D30")

# Preprocesamiento censo 2010 ----



names(c2010) <- c("age", "tot", "male", "female")
setDT(c2010)

c2010 <- c2010[-1 , ]
c2010 <- c2010[-1 , ] 

c2010[ , age := gsub("De ", "", age)]
c2010[ , age := substr(age, 1, 2)]
c2010[age=="No", age:=NA]
c2010[ , age:=as.numeric(age)]

c2010[ , tot := as.numeric(gsub(",", "", tot))]
c2010[ , male := as.numeric(gsub(",", "", male))]
c2010[ , female := as.numeric(gsub(",", "", female))]

c2010 <- c2010[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2010 <- melt.data.table(c2010, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2010[ , year:=2010]

# Preprocesamiento censo 2020 ----



names(c2020) <- c("age", "tot", "male", "female")
setDT(c2020)

c2020 <- c2020[-1 , ] 
c2020 <- c2020[-1 , ] 

c2020[ , age := gsub("De ", "", age)]
c2020[ , age := substr(age, 1, 2)]
c2020[age=="No", age:=NA]
c2020[ , age:=as.numeric(age)]

c2020[ , tot := as.numeric(gsub(",", "", tot))]
c2020[ , male := as.numeric(gsub(",", "", male))]
c2020[ , female := as.numeric(gsub(",", "", female))]

c2020 <- c2020[ , age := ifelse(age %in% 1:4, 1, age)] %>% 
  .[ , .(tot = sum(tot), 
         male = sum(male), 
         female = sum(female)), .(age)]

c2020 <- melt.data.table(c2020, 
                         id.vars = "age",
                         measure.vars = c("male", "female"),
                         variable.name = "sex",
                         value.name = "pop")

c2020[ , year:=2020]


#Unir los dos censos

censos <- rbind(c2010, c2020)
## Guardar tabla de censos ----

write.csv(censos,"data/censos_pro_sl.csv", row.names = F)


"---------------------------------------------------------"

# Preprocesamiento de defunciones 1990-2024----
def <- read_xlsx("data/defunciones_san_luis.xlsx", sheet = 1, 
                 range = "A6:G3861")

names(def) <- c("age","reg","year", 
                "tot", "male", "female", "ns")
setDT(def)

# Filtro
def <- def[age!="Total" & year!="Total" & year>=1990]

def[ , .N, .(age)]

def[ , age := gsub("Menores de ", "", age)]
def[ , age := substr(age, 1, 2)]
def[age=="1 ", age:=0]
def[age=="1-", age:=1]
def[age=="5-", age:=5]
def[age=="No", age:=NA] # prorrateo
def[ , age:=as.numeric(age)]


def[ , tot := as.numeric(gsub(",", "", tot))]
def[ , male := as.numeric(gsub(",", "", male))]
def[ , female := as.numeric(gsub(",", "", female))]
def[ , ns := as.numeric(gsub(",", "", ns))]

# Tabla de defunciones - comprobación
def_comp <- def[ , .(tot=sum(tot, na.rm = T),
                     male=sum(male, na.rm = T), 
                     female=sum(female, na.rm = T),
                     ns=sum(ns, na.rm = T)), 
                 .(year)]

# Imputación
def[year=="No especificado", year:=reg] 
def[ , year:=as.numeric(year)] 
def_comp[ , sum(tot)]


# Tabla final de defunciones 
def_pro <- def[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]

# Prorrateo de los valores perdidos (missing)
def_pro[ , tot:=male+female]
def_pro[ , p_male:=male/tot,]
def_pro[ , p_female:=male/tot,]
def_pro[ , male_adj:=male+p_male*ns,]
def_pro[,female_adj:=female+p_female*ns,]
def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]
sum(def_pro$male)+sum(def_pro$female)

def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")
sum(def_pro$deaths)
def_pro[ , sum(deaths), .(year, sex)]

#

def_pro <- def_pro[ !is.na(age) ] %>% 
  .[ , p_deaths := deaths / sum(deaths), .(year, sex)] %>% 
  merge( def_pro[ is.na(age), 
                  .(sex, year, na_deaths=deaths)], 
         by = c("sex", "year")) %>% 
  .[ , deaths_adj := deaths + na_deaths * p_deaths] %>% 
  .[ , .(year, sex, age, deaths = deaths_adj) ]

def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]

# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro_sl.csv", row.names = F)

# Preprocesamiento de defunciones por homicidio 1990-2024----

def_h <- read_xlsx("data/def_homicidios_san_luis.xlsx", sheet = 1, 
                 range = "A6:G1706")

names(def_h) <- c("age","year", "reg",
                "tot", "male", "female", "ns")
setDT(def_h)

# Filtro
def_h<- def_h[age!="Total" & year!="Total" & year>=1990]

def_h[ , .N, .(age)]

def_h[ , age := gsub("Menores de ", "", age)]
def_h[ , age := substr(age, 1, 2)]
def_h[age=="1 ", age:=0]
def_h[age=="1-", age:=1]
def_h[age=="5-", age:=5]
def_h[age=="No", age:=NA] # prorrateo
def_h[ , age:=as.numeric(age)]


def_h[ , tot := as.numeric(gsub(",", "", tot))]
def_h[ , male := as.numeric(gsub(",", "", male))]
def_h[ , female := as.numeric(gsub(",", "", female))]
def_h[ , ns := as.numeric(gsub(",", "", ns))]

# Tabla de defunciones - comprobación
def_comp <- def_h[ , .(tot=sum(tot, na.rm = T),
                     male=sum(male, na.rm = T), 
                     female=sum(female, na.rm = T),
                     ns=sum(ns, na.rm = T)), 
                 .(year)]

# Imputación
def_h[year=="No especificado", year:=reg] 
def_h[ , year:=as.numeric(year)] 
def_comp[ , sum(tot)]


# Tabla final de defunciones 
def_pro <- def_h[ , .(male=sum(male, na.rm = T), 
                    female=sum(female, na.rm = T),
                    ns=sum(ns, na.rm = T)), 
                .(year, age)]

# Prorrateo de los valores perdidos (missing)
def_pro[ , tot:=male+female]
def_pro[ , p_male:=male/tot,]
def_pro[ , p_female:=male/tot,]
def_pro[ , male_adj:=male+p_male*ns,]
def_pro[,female_adj:=female+p_female*ns,]
def_pro <- def_pro[ , .(year, age, male=male_adj, female=female_adj)]
sum(def_pro$male)+sum(def_pro$female)

def_pro <- melt.data.table(def_pro, 
                           id.vars = c("year", "age"),
                           measure.vars = c("male", "female"),
                           variable.name = "sex",
                           value.name = "deaths")
sum(def_pro$deaths)
def_pro[ , sum(deaths), .(year, sex)]

#

def_pro <- def_pro[ !is.na(age) ] %>% 
  .[ , p_deaths := deaths / sum(deaths), .(year, sex)] %>% 
  merge( def_pro[ is.na(age), 
                  .(sex, year, na_deaths=deaths)], 
         by = c("sex", "year")) %>% 
  .[ , deaths_adj := deaths + na_deaths * p_deaths] %>% 
  .[ , .(year, sex, age, deaths = deaths_adj) ]

def_gr <- def_pro[ , .(deaths=sum(deaths)), .(year, sex)]

# Guardar tabla de DEF prorrateadas----
write.csv(def_pro, "data/def_pro_homicidios_sl.csv", row.names = F)

