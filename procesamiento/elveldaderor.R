# ******************************************************************************
#
#             POSIBLE TITULO Y WEA JAJAJAJAJAJAJAJAJAAJAJAJ
#             Violeta Paz Rivera Sepulveda
#             Benjamin Felipe Andres Sepulveda Rodriguez
#
#
#
# ******************************************************************************


# Carga Librerías --------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,  
               car,
               stargazer,
               sjlabelled,
               sjPlot,      
               confintr,    
               gginference,  
               rempsyc,     
               broom,       
               sjmisc,      
               dplyr,
               knitr)             

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo


# Carga datos ------------------------------------------------------------------

load("~/GitHub/Trabajo_1/input/ELSOC_Long.RData")


# Limpieza de datos ------------------------------------------------------------


## Filtrar y seleccionar -------------------------------------------------------
data <- elsoc_long_2016_2022 %>% 
  filter(ola==1) %>%
  select(sexo=m0_sexo,edad=m0_edad,nedu=m01,
         m14,c01,c15,d01_01,d02_02,d02_03)

## Remover NA's ----------------------------------------------------------------
data <- data %>% 
  set_na(., na = c(-888, -999)) %>% 
  na.omit()

#Reetiquetado de variables

data <- data %>% rename("nivel_educacional"=nedu,  #Nivel educacional.
                        "ing_liqui_mens"=m14,      #Ingresos liquidos mensuales.
                        "satis_demo"=c01,          #Satisfacción con el funcionamiento de la democracia.
                        "tend_poli"=c15,           #Tendencia política.
                        "autoubi_socie"=d01_01,    #Autoubicacion en la sociedad chilena.
                        "just_acc_educ"=d02_02,    #Justicia en el acceso a educación según ingresos.
                        "just_acc_salud"=d02_03)   #Justicia en el acceso a salud según ingresos.

data$nivel_educacional <- set_label(x = data$nivel_educacional,label = "Nivel educacional")
data$ing_liqui_mens <- set_label(x = data$ing_liqui_mens,label = "Ingresos liquidos mensuales")
data$satis_demo <- set_label(x = data$satis_demo,label = "Satisfacción con el funcionamiento de la democracia")
data$tend_poli <- set_label(x = data$tend_poli,label = "Tendencia política")
data$autoubi_socie <- set_label(x = data$autoubi_socie,label = "Autoubicacion en la sociedad chilena")
data$just_acc_educ <- set_label(x = data$just_acc_educ,label = "Justicia en el acceso a educación según ingresos")
data$just_acc_salud <- set_label(x = data$just_acc_salud,label = "Justicia en el acceso a salud según ingresos")

# Recodificación en 10 grupos más detallados
data$ing_liqui_mens <- case_when(
  data$ing_liqui_mens == 1              ~ 1,
  data$ing_liqui_mens == 2              ~ 2,
  data$ing_liqui_mens %in% 3:4          ~ 3,
  data$ing_liqui_mens %in% 5:6          ~ 4,
  data$ing_liqui_mens %in% 7:8          ~ 5,
  data$ing_liqui_mens %in% 9:10         ~ 6,
  data$ing_liqui_mens == 11             ~ 7,
  data$ing_liqui_mens == 12             ~ 8,
  data$ing_liqui_mens %in% 13:14        ~ 9,
  data$ing_liqui_mens %in% 15:16        ~ 10,
  TRUE ~ NA_real_
)


stargazer(data,type= "text")






#LLLASFDKJASFASF

frq(data$satis_demo)
 