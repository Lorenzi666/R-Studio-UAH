# Practico 3

#Latinobarometro

library(pacman)

pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

setwd("~/GitHub/R-Studio-UAH/Bases de datos/F00011660-Latinobarometro_2020_Esp_Stata_v1_0")

Latinobarometro <- read_dta("Latinobarometro_2020_Esp_Stata_v1_0.dta")
View(Latinobarometro)

#cargamos la base de datos desde internet
load(url("https://github.com/Kevin-carrasco/metod1-MCS/raw/main/files/data/external_data/latinobarometro2020.RData"))

dim(Latinobarometro) # dimension de la base

###Seleccion de variables a usar 

find_var(data = Latinobarometro,"Confianza")

proc_data <- Latinobarometro %>% select(p13st_e, # Confianza en el Gobierno
                                            p13st_d, # Confianza en el congreso
                                            p13st_f, # Confianza en el Poder Judicial
                                            p13st_g, # Confianza en los partidos políticos
                                            reeduc_1,# nivel educacional
                                            sexo,# sexo
                                            edad,# edad
                                            idenpa) # pais 

# Comprobar
names(proc_data)

#Atributo label de las variables
sjlabelled:: get_label(proc_data)

#Filtrar base de datos
proc_data <- proc_data %>% dplyr::filter(idenpa==152)


###Procesamiento de variables

#Confianza en el gobierno 

#Revisar descriptivos
frq(proc_data$p13st_e)

#Recodificacion 
proc_data$p13st_e <- recode(proc_data$p13st_e, "c(-2,-1)=NA")
proc_data$p13st_d <- recode(proc_data$p13st_d, "c(-2,-1)=NA")
proc_data$p13st_f <- recode(proc_data$p13st_f, "c(-2,-1)=NA")
proc_data$p13st_g <- recode(proc_data$p13st_g, "c(-2,-1)=NA")

proc_data <- proc_data %>% set_na(., na = c(-2, -1))

proc_data$p13st_e <- recode(proc_data$p13st_e, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_d <- recode(proc_data$p13st_d, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_f <- recode(proc_data$p13st_f, "1=3; 2=2; 3=1; 4=0")
proc_data$p13st_g <- recode(proc_data$p13st_g, "1=3; 2=2; 3=1; 4=0")

#Etiquetado

#Dar nombre a las variables con funcion rename
proc_data <- proc_data %>% rename("conf_gob"=p13st_e, # Confianza en el gobierno
                                  "conf_cong"=p13st_d, # Confianza en el congreso
                                  "conf_jud"=p13st_f, # Confianza en el Poder Judicial
                                  "conf_partpol"=p13st_g) # Confianza en los partidos políticos 

#Cambiar las etiquetas de las variables
proc_data$conf_gob <- set_label(x = proc_data$conf_gob,label = "Confianza: Gobierno")
get_label(proc_data$conf_gob)

proc_data$conf_cong  <- set_label(x = proc_data$conf_cong, label = "Confianza: Congreso")
get_label(proc_data$conf_cong)

proc_data$conf_jud  <- set_label(x = proc_data$conf_jud, label = "Confianza: Poder judicial")
get_label(proc_data$conf_jud)

proc_data$conf_partpol  <- set_label(x = proc_data$conf_partpol, label = "Confianza: Partidos politicos")
get_label(proc_data$conf_partpol)

#Otros ajustes
proc_data$conf_inst <- (proc_data$conf_gob+proc_data$conf_cong+proc_data$conf_jud+proc_data$conf_partpol)
summary(proc_data$conf_inst)

get_label(proc_data$conf_inst)

proc_data$conf_inst  <- set_label(x = proc_data$conf_inst, label = "Confianza en instituciones")

#Revision Final
frq(proc_data$conf_gob)
frq(proc_data$conf_cong)
frq(proc_data$conf_inst)
proc_data$conf_gob <- set_labels(proc_data$conf_gob,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_cong <- set_labels(proc_data$conf_cong,
                                  labels=c( "Ninguna"=0,
                                            "Poca"=1,
                                            "Algo"=2,
                                            "Mucha"=3))

proc_data$conf_jud <- set_labels(proc_data$conf_jud,
                                 labels=c( "Ninguna"=0,
                                           "Poca"=1,
                                           "Algo"=2,
                                           "Mucha"=3))

proc_data$conf_partpol <- set_labels(proc_data$conf_partpol,
                                     labels=c( "Ninguna"=0,
                                               "Poca"=1,
                                               "Algo"=2,
                                               "Mucha"=3))
frq(proc_data$conf_gob)
frq(proc_data$conf_cong)


#Educación

frq(proc_data$reeduc_1)

#Redodificacion 
# recodificacion usando funcion 'recode' de la libreria car
proc_data$reeduc_1 <- car::recode(proc_data$reeduc_1, "c(1,2,3)=1; c(4,5)=2; c(6,7)=3")

#Comprobar
frq(proc_data$reeduc_1) 

#Etiquetado
proc_data$reeduc_1 <- set_labels(proc_data$reeduc_1,
                                 labels=c( "Educacion basica"=1,
                                           "Educacion media"=2,
                                           "Educacion superior"=3))

#Renombramos la variable con un nombre mas sustantivo 
proc_data <- rename(proc_data,"educacion"=reeduc_1)

#Cambiar la etiqueta de la variable
get_label(proc_data$educacion)
proc_data$educacion <- set_label(x = proc_data$educacion,label = "Educación")


#Sexo 

#Descriptivo
frq(proc_data$sexo)

#Recodificacion 
proc_data$sexo <- car::recode(proc_data$sexo, "1=0;2=1")

#Etiquetado
proc_data$sexo <- set_labels(proc_data$sexo,
                             labels=c( "Hombre"=0,
                                       "Mujer"=1))
#Cambiar etiqueta de la variable
get_label(proc_data$sexo)
proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
frq(proc_data$sexo)


#Edad
frq(proc_data$edad)

#Recodificacion no es necesaria en este caso

#Cambio la etiqueta de la variable.

get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad, label= "Edad")


### Generación de base de datos procesada para el análisis

#Reformatear el objeto proc_data como base de datos
proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

stargazer(proc_data, type="text", summary.stat = c("mean", "n", "sd", "p25"))

#Guardar base de datos procesada
save(proc_data,file = "C:/Users/Alumno/Documents/GitHub/R-Studio-UAH/Bases de datos/F00011660-Latinobarometro_2020_Esp_Stata_v1_0/Latinobarometro_proc.RData")
save(proc_data,file = "C:/Users/Alumno/Documents/GitHub/R-Studio-UAH/Bases de datos/F00011660-Latinobarometro_2020_Esp_Stata_v1_0/Latinobarometro_proc.RData")


#Descriptivos basicos de las variables

#Media por grupos
proc_data %>% dplyr::group_by(sexo) %>% summarise(mean(conf_inst, na.rm=TRUE))
proc_data %>% dplyr::group_by(educacion) %>% summarise(mean(conf_inst, na.rm=TRUE))

#Representacion 
install.packages("sjPlot")
library(sjPlot)

sjt.xtab(proc_data$educacion, proc_data$conf_inst, encoding = "UTF-8")
