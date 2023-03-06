## Removemos notacion cientifica
options(scipen = 999)

## Cargamos librerias 
library(pacman)
p_load(sjmisc, sjlabelled, tidyverse, haven, foreign, survey, dplyr, ggplot2)

## Cargar datos
# Cargamos datos demograficos
demografico <- read_csv("01_datos/TSDEM.csv")

# Convertir a numerico
demografico$P3_4 <- as.numeric(demografico$P3_4)

# Descriptivos
summary(demografico$P3_4) #Solo para ver la conversion a numerico, falta filtrar

# Ingresos si no responde o no sabe en la encuesta lo toma como 99999
ingresos_dem <- demografico %>%
  filter(P3_4 %in% c(100:98000))

# Cada cuanto gana
ingresos_dem <- demografico %>%
  filter(P3_4A %in% c(1:4))

# Convertir a mensual
ingresos_dem$convertir <- NA
ingresos_dem$convertir[ingresos_dem$P3_4A == "1"] <- 4
ingresos_dem$convertir[ingresos_dem$P3_4A == "2"] <- 2
ingresos_dem$convertir[ingresos_dem$P3_4A == "3"] <- 1

# Ingresos mensuales
ingresos_dem$ing_mes <- NA
ingresos_dem$ing_mes <- ingresos_dem$P3_4 * ingresos_dem$convertir

# Descriptivos
summary(ingresos_dem$ing_mes)

## Crear NSE
ingresos_dem$nse <- NA
ingresos_dem$nse[ingresos_dem$ing_mes <= 2000] <- "E"
ingresos_dem$nse[ingresos_dem$ing_mes >= 2001 & ingresos_dem$ing_mes <= 4790] <- "D"
ingresos_dem$nse[ingresos_dem$ing_mes >= 4791 & ingresos_dem$ing_mes <= 10500] <- "D+"
ingresos_dem$nse[ingresos_dem$ing_mes >= 10501 & ingresos_dem$ing_mes <= 22900] <- "C-"
ingresos_dem$nse[ingresos_dem$ing_mes >= 22901 & ingresos_dem$ing_mes <= 34900] <- "C"
ingresos_dem$nse[ingresos_dem$ing_mes >= 34901 & ingresos_dem$ing_mes <= 58000] <- "C+"
ingresos_dem$nse[ingresos_dem$ing_mes >= 58001] <- "A/B"

## Frecuencias de NSE
ingresos_dem %>% frq(nse, weights= FACTOR)

# Cargamos datos de viviendas
viviendas <- read_csv("01_datos/TVIVIENDA.csv")

# Union de bases de datos
data_caracteristicas <- left_join(ingresos_dem, viviendas)

# Crear variable por regiones
data_caracteristicas %>% frq(data_caracteristicas$ENT)
# Checar archivo
# https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/702825197285.pdf

data_caracteristicas$region[data_caracteristicas$ENT == "05" | data_caracteristicas$ENT == "19" |
                        data_caracteristicas$ENT == "28"] <- "Noreste"

data_caracteristicas$region[data_caracteristicas$ENT == "02" | data_caracteristicas$ENT == "03" | data_caracteristicas$ENT == "26" | data_caracteristicas$ENT == "25" | data_caracteristicas$ENT == "08"] <- "Noroeste"

data_caracteristicas$region[data_caracteristicas$ENT == "01" | data_caracteristicas$ENT == "10" | data_caracteristicas$ENT == "11" | data_caracteristicas$ENT == "24" | data_caracteristicas$ENT == "32"] <- "Altiplano"

data_caracteristicas$region[data_caracteristicas$ENT == "06" | data_caracteristicas$ENT == "14" | data_caracteristicas$ENT == "16" | data_caracteristicas$ENT == "18"] <- "Occidente"

data_caracteristicas$region[data_caracteristicas$ENT == "09" | data_caracteristicas$ENT == "13" | data_caracteristicas$ENT == "15" | data_caracteristicas$ENT == "17" | data_caracteristicas$ENT == "21" | data_caracteristicas$ENT == "22" | data_caracteristicas$ENT == "29"] <- "Centro"

data_caracteristicas$region[data_caracteristicas$ENT == "07" | data_caracteristicas$ENT == "12" | data_caracteristicas$ENT == "20"] <- "Sur"

data_caracteristicas$region[data_caracteristicas$ENT == "04" | data_caracteristicas$ENT == "23" | data_caracteristicas$ENT == "27" | data_caracteristicas$ENT == "30" | data_caracteristicas$ENT == "31"] <- "Golfo"

table(data_caracteristicas$region)

## Asequibilidad
# Cuando se supera el 30% del ingreso, no es asequible
table(data_caracteristicas$P5_8)
data_caracteristicas$P5_8 <- as.numeric(data_caracteristicas$P5_8)
table(data_caracteristicas$P5_8)

# Filtrar
data_caracteristicas <- data_caracteristicas %>%
  filter(P5_8 %in% c(100:98000))

summary(data_caracteristicas$ing_mes) # Para ver cuanto ganan al mes
summary(data_caracteristicas$P5_8) # Para ver cuanto pagarian por su casa al mes

# Por localidad
tapply(data_caracteristicas$ing_mes, data_caracteristicas$TLOC, summary)
tapply(data_caracteristicas$P5_8, data_caracteristicas$TLOC, summary)

# Por region
tapply(data_caracteristicas$ing_mes, data_caracteristicas$region, summary)
tapply(data_caracteristicas$P5_8, data_caracteristicas$region, summary)

# Por entidad
tapply(data_caracteristicas$ing_mes, data_caracteristicas$ENT, summary)
tapply(data_caracteristicas$P5_8, data_caracteristicas$ENT, summary)

# Grupos por localidad, gasto en renta vivienda
aggregate(x= data_caracteristicas$P5_8,     
          by = list(data_caracteristicas$TLOC),      
          FUN = mean)

# Crear variable de asequibilidad
data_caracteristicas$asequibilidad <- NA
data_caracteristicas$asequibilidad <- data_caracteristicas$P5_8/data_caracteristicas$ing_mes
summary(data_caracteristicas$asequibilidad)

data_caracteristicas$asequible[data_caracteristicas$asequibilidad > 0.300000] <- "0"
data_caracteristicas$asequible[data_caracteristicas$asequibilidad <= 0.300000] <- "1"
table(data_caracteristicas$asequible)

# Asequibilidad por región y localidad
# Region 
asequibilidad_region = data_caracteristicas %>% group_by(region)  %>%
  summarise(asequibles = frq(asequible),
            .groups = 'drop')

asequibilidad_region

# Localidad
asequibilidad_localidad = data_caracteristicas %>% group_by(TLOC)  %>%
  summarise(asequibles = frq(asequible),
            .groups = 'drop')

asequibilidad_localidad

# Entidad
asequibilidad_entidad = data_caracteristicas %>% group_by(ENT)  %>%
  summarise(asequibles = frq(asequible),
            .groups = 'drop')

asequibilidad_entidad

### Checar variables si son númericas
glimpse(data_caracteristicas)

# Convertir a númericas las variables de disponibilidad de los servicios

data_caracteristicas$P4_12 <- as.numeric(data_caracteristicas$P4_12)
data_caracteristicas$P4_13 <- as.numeric(data_caracteristicas$P4_13)
data_caracteristicas$P4_14 <- as.numeric(data_caracteristicas$P4_14)
data_caracteristicas$P4_15 <- as.numeric(data_caracteristicas$P4_15)
data_caracteristicas$P4_16 <- as.numeric(data_caracteristicas$P4_16)

# Crear nuevo data frame con solo las variables de disponibilidad de los sercivios
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR, Region
# Llamar al data frame dispo_servicios 

disp_serv <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$VIV_SEL, data_caracteristicas$ENT,
                        data_caracteristicas$TLOC, data_caracteristicas$EST_DIS, data_caracteristicas$UPM_DIS,
                        data_caracteristicas$FACTOR, data_caracteristicas$P4_12, data_caracteristicas$P4_13,
                        data_caracteristicas$P4_14, data_caracteristicas$P4_15, data_caracteristicas$P4_16,
                        data_caracteristicas$P4_17, data_caracteristicas$region)

names(disp_serv)

disp_serv <- rename(disp_serv, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                    TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                    FACTOR = data_caracteristicas.FACTOR, P4_12 = data_caracteristicas.P4_12, P4_13 = data_caracteristicas.P4_13,
                    P4_14 = data_caracteristicas.P4_14, P4_15 = data_caracteristicas.P4_15, P4_16 = data_caracteristicas.P4_16,
                    P4_17 = data_caracteristicas.P4_17, region = data_caracteristicas.region)
                    
# Puntajes disponibilidad de servicios
# Servicio sanitario
disp_serv$puntos_sanitario<-0
disp_serv$puntos_sanitario[is.na(disp_serv$P4_12)]<-NA
disp_serv$puntos_sanitario[disp_serv$P4_12=="1"]<-6
disp_serv$puntos_sanitario[disp_serv$P4_12=="2"]<-3
disp_serv$puntos_sanitario[disp_serv$P4_12=="3"]<-0
table(disp_serv$puntos_sanitario)

# Agua llaves o manguera
disp_serv$puntos_aguallave<-0
disp_serv$puntos_aguallave[is.na(disp_serv$P4_13)]<-NA
disp_serv$puntos_aguallave[disp_serv$P4_13=="1"]<-6
disp_serv$puntos_aguallave[disp_serv$P4_13=="2"]<-3
disp_serv$puntos_aguallave[disp_serv$P4_13=="3"]<-0
table(disp_serv$puntos_aguallave)

# Agua entubada
disp_serv$puntos_aguaentubada<-0
disp_serv$puntos_aguaentubada[is.na(disp_serv$P4_14)]<-NA
disp_serv$puntos_aguaentubada[disp_serv$P4_14=="1"]<-9
disp_serv$puntos_aguaentubada[disp_serv$P4_14=="2"]<-3
disp_serv$puntos_aguaentubada[disp_serv$P4_14=="3"]<-3
disp_serv$puntos_aguaentubada[disp_serv$P4_14=="4"]<-3
disp_serv$puntos_aguaentubada[disp_serv$P4_14=="5"]<-3
table(disp_serv$puntos_aguaentubada)

# Drenaje
disp_serv$puntos_drenaje<-0
disp_serv$puntos_drenaje[is.na(disp_serv$P4_15)]<-NA
disp_serv$puntos_drenaje[disp_serv$P4_15=="1"]<-9
disp_serv$puntos_drenaje[disp_serv$P4_15=="2"]<-6
disp_serv$puntos_drenaje[disp_serv$P4_15=="3"]<-3
disp_serv$puntos_drenaje[disp_serv$P4_15=="4"]<-1
disp_serv$puntos_drenaje[disp_serv$P4_15=="5"]<-0
table(disp_serv$puntos_drenaje)

# Luz
disp_serv$puntos_luz<-0
disp_serv$puntos_luz[is.na(disp_serv$P4_16)]<-NA
disp_serv$puntos_luz[disp_serv$P4_16=="1"]<-9
disp_serv$puntos_luz[disp_serv$P4_16=="2"]<-0
table(disp_serv$puntos_luz)

# Combustible_cocina
disp_serv$puntos_combcocina<-0
disp_serv$puntos_combcocina[is.na(disp_serv$P4_17)]<-NA
disp_serv$puntos_combcocina[disp_serv$P4_17=="1"]<-6
disp_serv$puntos_combcocina[disp_serv$P4_17=="2"]<-3
disp_serv$puntos_combcocina[disp_serv$P4_17=="3"]<-6
disp_serv$puntos_combcocina[disp_serv$P4_17=="4"]<-3
disp_serv$puntos_combcocina[disp_serv$P4_17=="5"]<-1
table(disp_serv$puntos_combcocina)

#Suma Puntajes
disp_serv$puntos_disp_serv<-
  disp_serv$puntos_sanitario+disp_serv$puntos_aguallave+disp_serv$puntos_aguaentubada+disp_serv$puntos_drenaje+
  disp_serv$puntos_luz+disp_serv$puntos_combcocina

table(disp_serv$puntos_disp_serv)

# Puntos por localidad y entidad
tapply(disp_serv$puntos_disp_serv, disp_serv$TLOC, summary)
tapply(disp_serv$puntos_disp_serv, disp_serv$ENT, summary)
tapply(disp_serv$puntos_disp_serv, disp_serv$region, summary)


# Crear data frame con las variables de habitabilidad 
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR
# Llamar al data frame habitabilidad  

habitabilidad <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$ENT,data_caracteristicas$TLOC, 
                            data_caracteristicas$VIV_SEL, data_caracteristicas$EST_DIS, data_caracteristicas$FACTOR,
                            data_caracteristicas$P4_4, data_caracteristicas$P4_5,  data_caracteristicas$P4_6,
                            data_caracteristicas$P4_7_1, data_caracteristicas$P4_7_2,  data_caracteristicas$P4_7_3, 
                            data_caracteristicas$P4_8_1, data_caracteristicas$P4_8_2,  data_caracteristicas$P4_8_3, 
                            data_caracteristicas$P4_8_4, data_caracteristicas$P4_9, data_caracteristicas$P4_11, data_caracteristicas$P4_22_1, 
                            data_caracteristicas$P4_22_2, data_caracteristicas$P4_22_3,  data_caracteristicas$P4_22_4, 
                            data_caracteristicas$P4_22_5, data_caracteristicas$P4_22_6,  data_caracteristicas$P4_22_7, 
                            data_caracteristicas$P4_22_8, data_caracteristicas$P4_22_9,  data_caracteristicas$P4_23_1, 
                            data_caracteristicas$P4_23_2, data_caracteristicas$P4_23_4,  data_caracteristicas$P4_23_5, 
                            data_caracteristicas$P4_23_6, data_caracteristicas$P4_25_1,  data_caracteristicas$P4_25_2, 
                            data_caracteristicas$P4_25_3, data_caracteristicas$P4_25_4,  data_caracteristicas$P4_25_5,
                            data_caracteristicas$P4_25_6, data_caracteristicas$P4_25_7, data_caracteristicas$P6_9_2,
                            data_caracteristicas$P6_9_3, data_caracteristicas$P6_9_4, data_caracteristicas$P6_9_5,
                            data_caracteristicas$P6_9_6, data_caracteristicas$P6_9_7, data_caracteristicas$UPM_DIS,
                            data_caracteristicas$region)
                            
                            
names(habitabilidad)

habitabilidad <- rename(habitabilidad, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                        TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                        FACTOR = data_caracteristicas.FACTOR, P4_4 = data_caracteristicas.P4_4, P4_5 = data_caracteristicas.P4_5,  P4_6 = data_caracteristicas.P4_6,
                        P4_7_1 = data_caracteristicas.P4_7_1, P4_7_2 = data_caracteristicas.P4_7_2,  P4_7_3 = data_caracteristicas.P4_7_3, 
                        P4_8_1= data_caracteristicas.P4_8_1, P4_8_2 = data_caracteristicas.P4_8_2,  P4_8_3 = data_caracteristicas.P4_8_3, 
                        P4_8_4 = data_caracteristicas.P4_8_4, P4_9 = data_caracteristicas.P4_9, P4_11 = data_caracteristicas.P4_11, 
                        P4_22_1 = data_caracteristicas.P4_22_1, P4_22_2 = data_caracteristicas.P4_22_2, P4_22_3 = data_caracteristicas.P4_22_3,  
                        P4_22_4 = data_caracteristicas.P4_22_4, P4_22_5 = data_caracteristicas.P4_22_5, P4_22_6 = data_caracteristicas.P4_22_6,  
                        P4_22_7 = data_caracteristicas.P4_22_7, P4_22_8 = data_caracteristicas.P4_22_8, P4_22_9 = data_caracteristicas.P4_22_9,  
                        P4_23_1 = data_caracteristicas.P4_23_1, P4_23_2 = data_caracteristicas.P4_23_2, P4_23_4 = data_caracteristicas.P4_23_4,  
                        P4_23_5 = data_caracteristicas.P4_23_5, P4_23_6 = data_caracteristicas.P4_23_6, P4_25_1 = data_caracteristicas.P4_25_1,  
                        P4_25_2 = data_caracteristicas.P4_25_2, P4_25_3 = data_caracteristicas.P4_25_3, P4_25_4 = data_caracteristicas.P4_25_4,  
                        P4_25_5 = data_caracteristicas.P4_25_5, P4_25_6 = data_caracteristicas.P4_25_6, P4_25_7 = data_caracteristicas.P4_25_7, 
                        P6_9_2 = data_caracteristicas.P6_9_2, P6_9_3 = data_caracteristicas.P6_9_3, P6_9_4 = data_caracteristicas.P6_9_4, 
                        P6_9_5 = data_caracteristicas.P6_9_5, P6_9_6 = data_caracteristicas.P6_9_6, P6_9_7 = data_caracteristicas.P6_9_7, 
                        region = data_caracteristicas.region)

# Puntajes habitabilidad
# Material de paredes
habitabilidad$puntos_material_paredes<-0
habitabilidad$puntos_material_paredes[is.na(habitabilidad$P4_4)]<-NA
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="1"]<-1
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="2"]<-1
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="3"]<-2
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="4"]<-2
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="5"]<-2
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="6"]<-2
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="7"]<-3
habitabilidad$puntos_material_paredes[habitabilidad$P4_4=="8"]<-6
table(habitabilidad$puntos_material_paredes)

# Material del techo
habitabilidad$puntos_material_techo<-0
habitabilidad$puntos_material_techo[is.na(habitabilidad$P4_5)]<-NA
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="1"]<-1
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="2"]<-1
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="3"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="4"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="5"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="6"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="7"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="8"]<-2
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="9"]<-3
habitabilidad$puntos_material_techo[habitabilidad$P4_5=="10"]<-6
table(habitabilidad$puntos_material_techo)

# Material de piso
habitabilidad$puntos_material_piso<-0
habitabilidad$puntos_material_piso[is.na(habitabilidad$P4_6)]<-NA
habitabilidad$puntos_material_piso[habitabilidad$P4_6=="1"]<-1
habitabilidad$puntos_material_piso[habitabilidad$P4_6=="2"]<-3
habitabilidad$puntos_material_piso[habitabilidad$P4_6=="3"]<-6
table(habitabilidad$puntos_material_piso)

# Aislamiento térmico techo
habitabilidad$puntos_aislamiento_termico_techo<-0
habitabilidad$puntos_aislamiento_termico_techo[is.na(habitabilidad$P4_7_1)]<-NA
habitabilidad$puntos_aislamiento_termico_techo[habitabilidad$P4_7_1=="1"]<-3
habitabilidad$puntos_aislamiento_termico_techo[habitabilidad$P4_7_1=="2"]<-0
habitabilidad$puntos_aislamiento_termico_techo[habitabilidad$P4_7_1=="3"]<-0
table(habitabilidad$puntos_aislamiento_termico_techo)

# Aislamiento térmico paredes
habitabilidad$puntos_aislamiento_termico_paredes<-0
habitabilidad$puntos_aislamiento_termico_paredes[is.na(habitabilidad$P4_7_2)]<-NA
habitabilidad$puntos_aislamiento_termico_paredes[habitabilidad$P4_7_2=="1"]<-3
habitabilidad$puntos_aislamiento_termico_paredes[habitabilidad$P4_7_2=="2"]<-0
habitabilidad$puntos_aislamiento_termico_paredes[habitabilidad$P4_7_2=="3"]<-0
table(habitabilidad$puntos_aislamiento_termico_paredes)

# Aislamiento térmico ventanas
habitabilidad$puntos_aislamiento_termico_ventanas<-0
habitabilidad$puntos_aislamiento_termico_ventanas[is.na(habitabilidad$P4_7_3)]<-NA
habitabilidad$puntos_aislamiento_termico_ventanas[habitabilidad$P4_7_3=="1"]<-3
habitabilidad$puntos_aislamiento_termico_ventanas[habitabilidad$P4_7_3=="2"]<-0
habitabilidad$puntos_aislamiento_termico_ventanas[habitabilidad$P4_7_3=="3"]<-0
table(habitabilidad$puntos_aislamiento_termico_ventanas)

# Aislamiento ruido techo
habitabilidad$puntos_aislamiento_ruido_techo<-0
habitabilidad$puntos_aislamiento_ruido_techo[is.na(habitabilidad$P4_8_1)]<-NA
habitabilidad$puntos_aislamiento_ruido_techo[habitabilidad$P4_8_1=="1"]<-3
habitabilidad$puntos_aislamiento_ruido_techo[habitabilidad$P4_8_1=="2"]<-0
habitabilidad$puntos_aislamiento_ruido_techo[habitabilidad$P4_8_1=="3"]<-0
table(habitabilidad$puntos_aislamiento_ruido_techo)

# Aislamiento ruido paredes
habitabilidad$puntos_aislamiento_ruido_paredes<-0
habitabilidad$puntos_aislamiento_ruido_paredes[is.na(habitabilidad$P4_8_2)]<-NA
habitabilidad$puntos_aislamiento_ruido_paredes[habitabilidad$P4_8_2=="1"]<-3
habitabilidad$puntos_aislamiento_ruido_paredes[habitabilidad$P4_8_2=="2"]<-0
habitabilidad$puntos_aislamiento_ruido_paredes[habitabilidad$P4_8_2=="3"]<-0
table(habitabilidad$puntos_aislamiento_ruido_paredes)

# Aislamiento ruido ventanas
habitabilidad$puntos_aislamiento_ruido_ventanas<-0
habitabilidad$puntos_aislamiento_ruido_ventanas[is.na(habitabilidad$P4_8_3)]<-NA
habitabilidad$puntos_aislamiento_ruido_ventanas[habitabilidad$P4_8_3=="1"]<-3
habitabilidad$puntos_aislamiento_ruido_ventanas[habitabilidad$P4_8_3=="2"]<-0
habitabilidad$puntos_aislamiento_ruido_ventanas[habitabilidad$P4_8_3=="3"]<-0
table(habitabilidad$puntos_aislamiento_ruido_ventanas)

# Aislamiento ruido puertas
habitabilidad$puntos_aislamiento_ruido_puertas<-0
habitabilidad$puntos_aislamiento_ruido_puertas[is.na(habitabilidad$P4_8_4)]<-NA
habitabilidad$puntos_aislamiento_ruido_puertas[habitabilidad$P4_8_4=="1"]<-3
habitabilidad$puntos_aislamiento_ruido_puertas[habitabilidad$P4_8_4=="2"]<-0
habitabilidad$puntos_aislamiento_ruido_puertas[habitabilidad$P4_8_4=="3"]<-0
table(habitabilidad$puntos_aislamiento_ruido_puertas)

# Cuarto para cocina
habitabilidad$puntos_cuerto_cocina<-0
habitabilidad$puntos_cuerto_cocina[is.na(habitabilidad$P4_9)]<-NA
habitabilidad$puntos_cuerto_cocina[habitabilidad$P4_9=="1"]<-3
habitabilidad$puntos_cuerto_cocina[habitabilidad$P4_9=="2"]<-0
table(habitabilidad$puntos_cuerto_cocina)

# Cuenta con desague (retrete, excusado, sanitario, etc)
habitabilidad$puntos_desague<-0
habitabilidad$puntos_desague[is.na(habitabilidad$P4_11)]<-NA
habitabilidad$puntos_desague[habitabilidad$P4_11=="1"]<-3
habitabilidad$puntos_desague[habitabilidad$P4_11=="2"]<-0
table(habitabilidad$puntos_desague)

# ¿Cuenta con lavadero?
habitabilidad$puntos_lavadero<-0
habitabilidad$puntos_lavadero[is.na(habitabilidad$P4_22_1)]<-NA
habitabilidad$puntos_lavadero[habitabilidad$P4_22_1=="1"]<-3
habitabilidad$puntos_lavadero[habitabilidad$P4_22_1=="2"]<-0
table(habitabilidad$puntos_lavadero)

# ¿Cuenta con fregadero?
habitabilidad$puntos_fregadero<-0
habitabilidad$puntos_fregadero[is.na(habitabilidad$P4_22_2)]<-NA
habitabilidad$puntos_fregadero[habitabilidad$P4_22_2=="1"]<-3
habitabilidad$puntos_fregadero[habitabilidad$P4_22_2=="2"]<-0
table(habitabilidad$puntos_fregadero)

# ¿Cuenta con tinaco?
habitabilidad$puntos_tinaco<-0
habitabilidad$puntos_tinaco[is.na(habitabilidad$P4_22_3)]<-NA
habitabilidad$puntos_tinaco[habitabilidad$P4_22_3=="1"]<-3
habitabilidad$puntos_tinaco[habitabilidad$P4_22_3=="2"]<-0
table(habitabilidad$puntos_tinaco)

# ¿Cuenta con aljibe?
habitabilidad$puntos_aljibe<-0
habitabilidad$puntos_aljibe[is.na(habitabilidad$P4_22_4)]<-NA
habitabilidad$puntos_aljibe[habitabilidad$P4_22_4=="1"]<-3
habitabilidad$puntos_aljibe[habitabilidad$P4_22_4=="2"]<-0
table(habitabilidad$puntos_aljibe)

# ¿Cuenta con boiler?
habitabilidad$puntos_boiler<-0
habitabilidad$puntos_boiler[is.na(habitabilidad$P4_22_5)]<-NA
habitabilidad$puntos_boiler[habitabilidad$P4_22_5=="1"]<-3
habitabilidad$puntos_boiler[habitabilidad$P4_22_5=="2"]<-0
table(habitabilidad$puntos_boiler)

# ¿Cuenta con calentador solar?
habitabilidad$puntos_calentador_solar<-0
habitabilidad$puntos_calentador_solar[is.na(habitabilidad$P4_22_6)]<-NA
habitabilidad$puntos_calentador_solar[habitabilidad$P4_22_6=="1"]<-3
habitabilidad$puntos_calentador_solar[habitabilidad$P4_22_6=="2"]<-0
table(habitabilidad$puntos_calentador_solar)

# ¿Cuenta con gas estacionario?
habitabilidad$puntos_gas_estacionario<-0
habitabilidad$puntos_gas_estacionario[is.na(habitabilidad$P4_22_7)]<-NA
habitabilidad$puntos_gas_estacionario[habitabilidad$P4_22_7=="1"]<-3
habitabilidad$puntos_gas_estacionario[habitabilidad$P4_22_7=="2"]<-0
table(habitabilidad$puntos_gas_estacionario)

# ¿Cuenta con aire acondicionado?
habitabilidad$puntos_aire_acondicionado<-0
habitabilidad$puntos_aire_acondicionado[is.na(habitabilidad$P4_22_8)]<-NA
habitabilidad$puntos_aire_acondicionado[habitabilidad$P4_22_8=="1"]<-3
habitabilidad$puntos_aire_acondicionado[habitabilidad$P4_22_8=="2"]<-0
table(habitabilidad$puntos_aire_acondicionado)

# ¿Cuenta con calefacción?
habitabilidad$puntos_calefaccion<-0
habitabilidad$puntos_calefaccion[is.na(habitabilidad$P4_22_9)]<-NA
habitabilidad$puntos_calefaccion[habitabilidad$P4_22_9=="1"]<-3
habitabilidad$puntos_calefaccion[habitabilidad$P4_22_9=="2"]<-0
table(habitabilidad$puntos_calefaccion)

# ¿Cuenta con sala comedor?
habitabilidad$puntos_comedor<-0
habitabilidad$puntos_comedor[is.na(habitabilidad$P4_23_1)]<-NA
habitabilidad$puntos_comedor[habitabilidad$P4_23_1=="1"]<-3
habitabilidad$puntos_comedor[habitabilidad$P4_23_1=="2"]<-0
table(habitabilidad$puntos_comedor)

# ¿Cuenta con jardín?
habitabilidad$puntos_jardin<-0
habitabilidad$puntos_jardin[is.na(habitabilidad$P4_23_2)]<-NA
habitabilidad$puntos_jardin[habitabilidad$P4_23_2=="1"]<-3
habitabilidad$puntos_jardin[habitabilidad$P4_23_2=="2"]<-0
table(habitabilidad$puntos_jardin)

# ¿Cuenta con patio?
habitabilidad$puntos_patio<-0
habitabilidad$puntos_patio[is.na(habitabilidad$P4_23_3)]<-NA
habitabilidad$puntos_patio[habitabilidad$P4_23_3=="1"]<-3
habitabilidad$puntos_patio[habitabilidad$P4_23_3=="2"]<-0
table(habitabilidad$puntos_patio)

# ¿Cuenta con cuarto de lavado?
habitabilidad$puntos_cuarto_lavado<-0
habitabilidad$puntos_cuarto_lavado[is.na(habitabilidad$P4_23_4)]<-NA
habitabilidad$puntos_cuarto_lavado[habitabilidad$P4_23_4=="1"]<-3
habitabilidad$puntos_cuarto_lavado[habitabilidad$P4_23_4=="2"]<-0
table(habitabilidad$puntos_cuarto_lavado)

# ¿Cuenta con cuarto de televisión?
habitabilidad$puntos_cuarto_television<-0
habitabilidad$puntos_cuarto_television[is.na(habitabilidad$P4_23_5)]<-NA
habitabilidad$puntos_cuarto_television[habitabilidad$P4_23_5=="1"]<-3
habitabilidad$puntos_cuarto_television[habitabilidad$P4_23_5=="2"]<-0
table(habitabilidad$puntos_cuarto_television)

# ¿Cuenta con cochera?
habitabilidad$puntos_cochera<-0
habitabilidad$puntos_cochera[is.na(habitabilidad$P4_23_6)]<-NA
habitabilidad$puntos_cochera[habitabilidad$P4_23_6=="1"]<-3
habitabilidad$puntos_cochera[habitabilidad$P4_23_6=="2"]<-0
table(habitabilidad$puntos_cochera)

# ¿Cuenta con grietas en techos o muros?
habitabilidad$puntos_grietas<-0
habitabilidad$puntos_grietas[is.na(habitabilidad$P4_25_1)]<-NA
habitabilidad$puntos_grietas[habitabilidad$P4_25_1=="1"]<-0
habitabilidad$puntos_grietas[habitabilidad$P4_25_1=="2"]<-3
habitabilidad$puntos_grietas[habitabilidad$P4_25_1=="3"]<-0
table(habitabilidad$puntos_grietas)

# ¿Problemas con los marcos de las puertas o ventanas?
habitabilidad$puntos_problemas_marcos_pyv<-0
habitabilidad$puntos_problemas_marcos_pyv[is.na(habitabilidad$P4_25_2)]<-NA
habitabilidad$puntos_problemas_marcos_pyv[habitabilidad$P4_25_2=="1"]<-0
habitabilidad$puntos_problemas_marcos_pyv[habitabilidad$P4_25_2=="2"]<-3
habitabilidad$puntos_problemas_marcos_pyv[habitabilidad$P4_25_2=="3"]<-0
table(habitabilidad$puntos_problemas_marcos_pyv)

# ¿Problemas con hundimiento del piso?
habitabilidad$puntos_hundimiento_piso<-0
habitabilidad$puntos_hundimiento_piso[is.na(habitabilidad$P4_25_3)]<-NA
habitabilidad$puntos_hundimiento_piso[habitabilidad$P4_25_3=="1"]<-0
habitabilidad$puntos_hundimiento_piso[habitabilidad$P4_25_3=="2"]<-3
habitabilidad$puntos_hundimiento_piso[habitabilidad$P4_25_3=="3"]<-0
table(habitabilidad$puntos_hundimiento_piso)

# ¿Problemas con la humedad?
habitabilidad$puntos_humedad<-0
habitabilidad$puntos_humedad[is.na(habitabilidad$P4_25_4)]<-NA
habitabilidad$puntos_humedad[habitabilidad$P4_25_4=="1"]<-0
habitabilidad$puntos_humedad[habitabilidad$P4_25_4=="2"]<-3
habitabilidad$puntos_humedad[habitabilidad$P4_25_4=="3"]<-0
table(habitabilidad$puntos_humedad)

# ¿Problemas con fracturas en columnas, vigas o trabes?
habitabilidad$puntos_fracturas_columnas<-0
habitabilidad$puntos_fracturas_columnas[is.na(habitabilidad$P4_25_5)]<-NA
habitabilidad$puntos_fracturas_columnas[habitabilidad$P4_25_5=="1"]<-0
habitabilidad$puntos_fracturas_columnas[habitabilidad$P4_25_5=="2"]<-3
habitabilidad$puntos_fracturas_columnas[habitabilidad$P4_25_5=="3"]<-0
table(habitabilidad$puntos_fracturas_columnas)

# ¿Problemas con sistema eléctrico?
habitabilidad$puntos_problemas_electricidad<-0
habitabilidad$puntos_problemas_electricidad[is.na(habitabilidad$P4_25_6)]<-NA
habitabilidad$puntos_problemas_electricidad[habitabilidad$P4_25_6=="1"]<-0
habitabilidad$puntos_problemas_electricidad[habitabilidad$P4_25_6=="2"]<-3
habitabilidad$puntos_problemas_electricidad[habitabilidad$P4_25_6=="3"]<-0
table(habitabilidad$puntos_problemas_electricidad)

# ¿Problemas con la tubería?
habitabilidad$puntos_problemas_tuberia<-0
habitabilidad$puntos_problemas_tuberia[is.na(habitabilidad$P4_25_7)]<-NA
habitabilidad$puntos_problemas_tuberia[habitabilidad$P4_25_7=="1"]<-0
habitabilidad$puntos_problemas_tuberia[habitabilidad$P4_25_7=="2"]<-3
habitabilidad$puntos_problemas_tuberia[habitabilidad$P4_25_7=="3"]<-0
table(habitabilidad$puntos_problemas_tuberia)

# Exceso de ruido en la colonia:
habitabilidad$puntos_ruido_colonia<-0
habitabilidad$puntos_ruido_colonia[is.na(habitabilidad$P6_9_2)]<-NA
habitabilidad$puntos_ruido_colonia[habitabilidad$P6_9_2=="1"]<-0
habitabilidad$puntos_ruido_colonia[habitabilidad$P6_9_2=="2"]<-2
habitabilidad$puntos_ruido_colonia[habitabilidad$P6_9_2=="3"]<-4
habitabilidad$puntos_ruido_colonia[habitabilidad$P6_9_2=="4"]<-6
habitabilidad$puntos_ruido_colonia[habitabilidad$P6_9_2=="5"]<-0
table(habitabilidad$puntos_ruido_colonia)

# Problemas de basura en la colonia:
habitabilidad$puntos_basura_colonia<-0
habitabilidad$puntos_basura_colonia[is.na(habitabilidad$P6_9_3)]<-NA
habitabilidad$puntos_basura_colonia[habitabilidad$P6_9_3=="1"]<-0
habitabilidad$puntos_basura_colonia[habitabilidad$P6_9_3=="2"]<-2
habitabilidad$puntos_basura_colonia[habitabilidad$P6_9_3=="3"]<-4
habitabilidad$puntos_basura_colonia[habitabilidad$P6_9_3=="4"]<-6
habitabilidad$puntos_basura_colonia[habitabilidad$P6_9_3=="5"]<-0
table(habitabilidad$puntos_basura_colonia)

# Problemas de contaminación del ambiente por fábricas/construcciones en la colonia:
habitabilidad$puntos_contaminación_fabricas_colonia<-0
habitabilidad$puntos_contaminación_fabricas_colonia[is.na(habitabilidad$P6_9_4)]<-NA
habitabilidad$puntos_contaminación_fabricas_colonia[habitabilidad$P6_9_4=="1"]<-0
habitabilidad$puntos_contaminación_fabricas_colonia[habitabilidad$P6_9_4=="2"]<-2
habitabilidad$puntos_contaminación_fabricas_colonia[habitabilidad$P6_9_4=="3"]<-4
habitabilidad$puntos_contaminación_fabricas_colonia[habitabilidad$P6_9_4=="4"]<-6
habitabilidad$puntos_contaminación_fabricas_colonia[habitabilidad$P6_9_4=="5"]<-0
table(habitabilidad$puntos_contaminación_fabricas_colonia)

# Deterioro de espacios publicos en la colonia:
habitabilidad$puntos_deterioro_ep_colonia<-0
habitabilidad$puntos_deterioro_ep_colonia[is.na(habitabilidad$P6_9_5)]<-NA
habitabilidad$puntos_deterioro_ep_colonia[habitabilidad$P6_9_5=="1"]<-0
habitabilidad$puntos_deterioro_ep_colonia[habitabilidad$P6_9_5=="2"]<-2
habitabilidad$puntos_deterioro_ep_colonia[habitabilidad$P6_9_5=="3"]<-4
habitabilidad$puntos_deterioro_ep_colonia[habitabilidad$P6_9_5=="4"]<-6
habitabilidad$puntos_deterioro_ep_colonia[habitabilidad$P6_9_5=="5"]<-0
table(habitabilidad$puntos_deterioro_ep_colonia)

# Vandalismo en la colonia:
habitabilidad$puntos_vandalismo_colonia<-0
habitabilidad$puntos_vandalismo_colonia[is.na(habitabilidad$P6_9_6)]<-NA
habitabilidad$puntos_vandalismo_colonia[habitabilidad$P6_9_6=="1"]<-0
habitabilidad$puntos_vandalismo_colonia[habitabilidad$P6_9_6=="2"]<-2
habitabilidad$puntos_vandalismo_colonia[habitabilidad$P6_9_6=="3"]<-4
habitabilidad$puntos_vandalismo_colonia[habitabilidad$P6_9_6=="4"]<-6
habitabilidad$puntos_vandalismo_colonia[habitabilidad$P6_9_6=="5"]<-0
table(habitabilidad$puntos_vandalismo_colonia)

# Robos en la colonia:
habitabilidad$puntos_robos_colonia<-0
habitabilidad$puntos_robos_colonia[is.na(habitabilidad$P6_9_7)]<-NA
habitabilidad$puntos_robos_colonia[habitabilidad$P6_9_7=="1"]<-0
habitabilidad$puntos_robos_colonia[habitabilidad$P6_9_7=="2"]<-2
habitabilidad$puntos_robos_colonia[habitabilidad$P6_9_7=="3"]<-4
habitabilidad$puntos_robos_colonia[habitabilidad$P6_9_7=="4"]<-6
habitabilidad$puntos_robos_colonia[habitabilidad$P6_9_7=="5"]<-0
table(habitabilidad$puntos_robos_colonia)


#Suma Puntajes (EJEMPLO - SUMAR SOLAMENTE LAS PREGUNTAS)
habitabilidad$puntos_habitabilidad<-
  habitabilidad$puntos_material_paredes+habitabilidad$puntos_material_techo+habitabilidad$puntos_material_piso+
  habitabilidad$puntos_aislamiento_termico_techo+habitabilidad$puntos_aislamiento_termico_paredes+
  habitabilidad$puntos_aislamiento_termico_ventanas+habitabilidad$puntos_aislamiento_ruido_techo+
  habitabilidad$puntos_aislamiento_ruido_paredes+habitabilidad$puntos_aislamiento_ruido_ventanas+
  habitabilidad$puntos_aislamiento_ruido_puertas+habitabilidad$puntos_cuerto_cocina+habitabilidad$puntos_desague+
  habitabilidad$puntos_lavadero+habitabilidad$puntos_fregadero+habitabilidad$puntos_tinaco+habitabilidad$puntos_aljibe+
  habitabilidad$puntos_boiler+habitabilidad$puntos_calentador_solar+habitabilidad$puntos_gas_estacionario+
  habitabilidad$puntos_aire_acondicionado+habitabilidad$puntos_calefaccion+habitabilidad$puntos_comedor+
  habitabilidad$puntos_jardin+habitabilidad$puntos_patio+habitabilidad$puntos_cuarto_lavado+
  habitabilidad$puntos_cuarto_television+habitabilidad$puntos_cochera+habitabilidad$puntos_grietas+
  habitabilidad$puntos_problemas_marcos_pyv+habitabilidad$puntos_hundimiento_piso+habitabilidad$puntos_humedad+
  habitabilidad$puntos_fracturas_columnas+habitabilidad$puntos_problemas_electricidad+habitabilidad$puntos_problemas_tuberia+
  habitabilidad$puntos_ruido_colonia+habitabilidad$puntos_basura_colonia+habitabilidad$puntos_contaminación_fabricas_colonia+
  habitabilidad$puntos_deterioro_ep_colonia+habitabilidad$puntos_vandalismo_colonia+habitabilidad$puntos_robos_colonia

table(habitabilidad$puntos_habitabilidad)

# Puntos por localidad y entidad (EJEMPLO)
tapply(habitabilidad$puntos_habitabilidad, habitabilidad$TLOC, summary)
tapply(habitabilidad$puntos_habitabilidad, habitabilidad$ENT, summary)
tapply(habitabilidad$puntos_habitabilidad, habitabilidad$region, summary)


# Crear nuevo data frame con solo las variables de accesiblidad:
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR
# Llamar al data frame accesibilidad  

accesibilidad <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$ENT,data_caracteristicas$TLOC, 
                            data_caracteristicas$VIV_SEL, data_caracteristicas$EST_DIS, data_caracteristicas$FACTOR, 
                            data_caracteristicas$UPM_DIS, data_caracteristicas$P6_7_1, data_caracteristicas$P6_7_2,
                            data_caracteristicas$P6_7_3, data_caracteristicas$P6_7_4, data_caracteristicas$region)

names(accesibilidad)


accesibilidad <- rename(accesibilidad, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                        TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                        FACTOR = data_caracteristicas.FACTOR, P6_7_1 = data_caracteristicas.P6_7_1, P6_7_2 = data_caracteristicas.P6_7_2,
                        P6_7_3 = data_caracteristicas.P6_7_3, P6_7_4 = data_caracteristicas.P6_7_4, region = data_caracteristicas.region)


# Puntajes accesibilidad
# Necesidad de adaptación para persona c/ discapacidad (poner rampas)
accesibilidad$puntos_discapacidad_rampas<-0
accesibilidad$puntos_discapacidad_rampas[is.na(accesibilidad$P6_7_1)]<-NA
accesibilidad$puntos_discapacidad_rampas[accesibilidad$P6_7_1=="1"]<-0
accesibilidad$puntos_discapacidad_rampas[accesibilidad$P6_7_1=="2"]<-3
table(accesibilidad$puntos_discapacidad_rampas)

# Necesidad de adaptación para persona c/ discapacidad (ampliar puertas)
accesibilidad$puntos_discapacidad_puertas<-0
accesibilidad$puntos_discapacidad_puertas[is.na(accesibilidad$P6_7_2)]<-NA
accesibilidad$puntos_discapacidad_puertas[accesibilidad$P6_7_2=="1"]<-0
accesibilidad$puntos_discapacidad_puertas[accesibilidad$P6_7_2=="2"]<-3
table(accesibilidad$puntos_discapacidad_puertas)

# Necesidad de adaptación para persona c/ discapacidad (adecuar baños)
accesibilidad$puntos_discapacidad_baños<-0
accesibilidad$puntos_discapacidad_baños[is.na(accesibilidad$P6_7_3)]<-NA
accesibilidad$puntos_discapacidad_baños[accesibilidad$P6_7_3=="1"]<-0
accesibilidad$puntos_discapacidad_baños[accesibilidad$P6_7_3=="2"]<-3
table(accesibilidad$puntos_discapacidad_baños)

# Necesidad de adaptación para persona c/ discapacidad (poner pasamanos)
accesibilidad$puntos_discapacidad_pasamanos<-0
accesibilidad$puntos_discapacidad_pasamanos[is.na(accesibilidad$P6_7_4)]<-NA
accesibilidad$puntos_discapacidad_pasamanos[accesibilidad$P6_7_4=="1"]<-0
accesibilidad$puntos_discapacidad_pasamanos[accesibilidad$P6_7_4=="2"]<-3
table(accesibilidad$puntos_discapacidad_pasamanos)


#Suma Puntajes
accesibilidad$puntos_accesibilidad<-
  accesibilidad$puntos_discapacidad_rampas+accesibilidad$puntos_discapacidad_puertas+
  accesibilidad$puntos_discapacidad_baños+accesibilidad$puntos_discapacidad_pasamanos

table(accesibilidad$puntos_accesibilidad)

# Puntos por localidad y entidad (EJEMPLO)
tapply(accesibilidad$puntos_accesibilidad, accesibilidad$TLOC, summary)
tapply(accesibilidad$puntos_accesibilidad, accesibilidad$ENT, summary)
tapply(accesibilidad$puntos_accesibilidad, accesibilidad$region, summary)


# Crear data frame con las variables de ubicacion 
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR
# Llamar al data frame ubicacion 


ubicacion <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$ENT,data_caracteristicas$TLOC, 
                            data_caracteristicas$VIV_SEL, data_caracteristicas$EST_DIS, data_caracteristicas$FACTOR, 
                            data_caracteristicas$UPM_DIS, data_caracteristicas$P6_5_1, data_caracteristicas$P6_5_2,
                            data_caracteristicas$P6_5_3, data_caracteristicas$P6_5_4, data_caracteristicas$P6_5_5, data_caracteristicas$region)

names(ubicacion)

ubicacion <- rename(ubicacion, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                    TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                    FACTOR = data_caracteristicas.FACTOR, P6_5_1 = data_caracteristicas.P6_5_1, P6_5_2 = data_caracteristicas.P6_5_2,
                    P6_5_3 = data_caracteristicas.P6_5_3, P6_5_4 = data_caracteristicas.P6_5_4, P6_5_5 = data_caracteristicas.P6_5_5,
                    region = data_caracteristicas.region)


## Puntos vivienda-trabajo
# Satisfechos están  con la “distancia-tiempo” entre esta vivienda y el trabajo
ubicacion$puntos_vivtrabajo<-0
ubicacion$puntos_vivtrabajo[is.na(ubicacion$P6_5_1)]<-NA
ubicacion$puntos_vivtrabajo[ubicacion$P6_5_1=="1"]<-6
ubicacion$puntos_vivtrabajo[ubicacion$P6_5_1=="2"]<-4
ubicacion$puntos_vivtrabajo[ubicacion$P6_5_1=="3"]<-2
ubicacion$puntos_vivtrabajo[ubicacion$P6_5_1=="4"]<-0
table(ubicacion$puntos_vivtrabajo)

## Puntos vivienda-escuela
# Satisfechos están  con la “distancia-tiempo” entre esta vivienda y centros escolares
ubicacion$puntos_vivescuela<-0
ubicacion$puntos_vivescuela[is.na(ubicacion$P6_5_2)]<-NA
ubicacion$puntos_vivescuela[ubicacion$P6_5_2=="1"]<-6
ubicacion$puntos_vivescuela[ubicacion$P6_5_2=="2"]<-4
ubicacion$puntos_vivescuela[ubicacion$P6_5_2=="3"]<-2
ubicacion$puntos_vivescuela[ubicacion$P6_5_2=="4"]<-0
table(ubicacion$puntos_vivescuela)

## Puntos vivienda-hospital
# Satisfechos están  con la “distancia-tiempo” entre esta vivienda y centros de salud
ubicacion$puntos_vivhospital<-0
ubicacion$puntos_vivhospital[is.na(ubicacion$P6_5_3)]<-NA
ubicacion$puntos_vivhospital[ubicacion$P6_5_3=="1"]<-6
ubicacion$puntos_vivhospital[ubicacion$P6_5_3=="2"]<-4
ubicacion$puntos_vivhospital[ubicacion$P6_5_3=="3"]<-2
ubicacion$puntos_vivhospital[ubicacion$P6_5_3=="4"]<-0
table(ubicacion$puntos_vivhospital)

## Puntos vivienda-mercados
# Satisfechos están  con la “distancia-tiempo” entre esta vivienda y mercados
ubicacion$puntos_vivmercado<-0
ubicacion$puntos_vivmercado[is.na(ubicacion$P6_5_4)]<-NA
ubicacion$puntos_vivmercado[ubicacion$P6_5_4=="1"]<-6
ubicacion$puntos_vivmercado[ubicacion$P6_5_4=="2"]<-4
ubicacion$puntos_vivmercado[ubicacion$P6_5_4=="3"]<-2
ubicacion$puntos_vivmercado[ubicacion$P6_5_4=="4"]<-0
table(ubicacion$puntos_vivmercado)

## Puntos vivienda-parques
# Satisfechos están  con la “distancia-tiempo” entre esta vivienda y parques
ubicacion$puntos_vivparques<-0
ubicacion$puntos_vivparques[is.na(ubicacion$P6_5_5)]<-NA
ubicacion$puntos_vivparques[ubicacion$P6_5_5=="1"]<-6
ubicacion$puntos_vivparques[ubicacion$P6_5_5=="2"]<-4
ubicacion$puntos_vivparques[ubicacion$P6_5_5=="3"]<-2
ubicacion$puntos_vivparques[ubicacion$P6_5_5=="4"]<-0
table(ubicacion$puntos_vivparques)

#Suma Puntajes
ubicacion$puntos_ubicacion<-
  ubicacion$puntos_vivescuela+ubicacion$puntos_vivtrabajo+
  ubicacion$puntos_vivhospital+ubicacion$puntos_vivmercado+ubicacion$puntos_vivparques
table(ubicacion$puntos_ubicacion)

# Puntos por localidad y entidad (EJEMPLO)
tapply(ubicacion$puntos_ubicacion, ubicacion$TLOC, summary)
tapply(ubicacion$puntos_ubicacion, ubicacion$ENT, summary)
tapply(ubicacion$puntos_ubicacion, ubicacion$region, summary)

# Crear data frame con las variables de adecuacion cultural 
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR
# Llamar al data frame cultural 

cultural <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$ENT,data_caracteristicas$TLOC, 
                        data_caracteristicas$VIV_SEL, data_caracteristicas$EST_DIS, data_caracteristicas$FACTOR, 
                        data_caracteristicas$UPM_DIS, data_caracteristicas$P6_6, data_caracteristicas$region)

names(cultural)

cultural <- rename(cultural, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                    TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                    FACTOR = data_caracteristicas.FACTOR, P6_6 = data_caracteristicas.P6_6, region = data_caracteristicas.region)

## Puntos costumbres
# Consideran que la vivienda se identifica con sus gustos, costumbres y tradiciones
cultural$puntos_cultura<-0
cultural$puntos_cultura[is.na(cultural$P6_6)]<-NA
cultural$puntos_cultura[cultural$P6_6=="1"]<-6
cultural$puntos_cultura[cultural$P6_6=="2"]<-4
cultural$puntos_cultura[cultural$P6_6=="3"]<-2
cultural$puntos_cultura[cultural$P6_6=="4"]<-0
table(cultural$puntos_cultura)

# Puntos por localidad y entidad (EJEMPLO)
tapply(cultural$puntos_cultura, cultural$TLOC, summary)
tapply(cultural$puntos_cultura, cultural$ENT, summary)
tapply(cultural$puntos_cultura, cultural$region, summary)

# Crear data frame con las variables de adecuacion seguridad de la tenencia 
# Integrar también en ese data frame las variables de FoLIO, VIV_SEL, ENT, TLOC, EST_DIS, UPM_DIS, FACTOR
# Llamar al data frame seguridad 

seguridad <- data.frame(data_caracteristicas$FOLIO, data_caracteristicas$ENT,data_caracteristicas$TLOC, 
                       data_caracteristicas$VIV_SEL, data_caracteristicas$EST_DIS, data_caracteristicas$FACTOR, 
                       data_caracteristicas$UPM_DIS, data_caracteristicas$P5_1, data_caracteristicas$P5_7,
                       data_caracteristicas$P6_13, data_caracteristicas$region)

names(seguridad)

seguridad <- rename(seguridad, FOLIO = data_caracteristicas.FOLIO, VIV_SEL = data_caracteristicas.VIV_SEL, ENT = data_caracteristicas.ENT,
                   TLOC = data_caracteristicas.TLOC, EST_DIS = data_caracteristicas.EST_DIS, UPM_DIS = data_caracteristicas.UPM_DIS,
                   FACTOR = data_caracteristicas.FACTOR, P5_1 = data_caracteristicas.P5_1, P5_7 = data_caracteristicas.P5_7,
                   P6_13 = data_caracteristicas.P6_13, region = data_caracteristicas.region)

# Puntos tipo-tenencia
## ¿Esta vivienda...
seguridad$puntos_tenencia<-0
seguridad$puntos_tenencia[is.na(seguridad$P5_1)]<-NA
seguridad$puntos_tenencia[seguridad$P5_1=="1"]<-3
seguridad$puntos_tenencia[seguridad$P5_1=="2"]<-3
seguridad$puntos_tenencia[seguridad$P5_1=="3"]<-3
seguridad$puntos_tenencia[seguridad$P5_1=="4"]<-6
seguridad$puntos_tenencia[seguridad$P5_1=="5"]<-9
seguridad$puntos_tenencia[seguridad$P5_1=="6"]<-0
table(seguridad$puntos_tenencia)

# Puntos escrituras
## Esta vivienda cuenta con escritura o título de propiedad... 
seguridad$puntos_escrituras<-0
seguridad$puntos_escrituras[is.na(seguridad$P5_7)]<-NA
seguridad$puntos_escrituras[seguridad$P5_7=="1"]<-6
seguridad$puntos_escrituras[seguridad$P5_7=="2"]<-3
seguridad$puntos_escrituras[seguridad$P5_7=="3"]<-0
seguridad$puntos_escrituras[seguridad$P5_7=="4"]<-0
table(seguridad$puntos_escrituras)

# Puntos desalojo
## En esta vivienda, ¿qué tanto riesgo tienen de sufrir algún desalojo?
seguridad$puntos_desalojo<-0
seguridad$puntos_desalojo[is.na(seguridad$P6_13)]<-NA
seguridad$puntos_desalojo[seguridad$P6_13=="1"]<-0
seguridad$puntos_desalojo[seguridad$P6_13=="2"]<-1
seguridad$puntos_desalojo[seguridad$P6_13=="3"]<-3
seguridad$puntos_desalojo[seguridad$P6_13=="4"]<-6
table(seguridad$puntos_desalojo)

#Suma Puntajes
seguridad$puntos_seguridad_tenencia<-
  seguridad$puntos_tenencia+seguridad$puntos_escrituras+seguridad$puntos_desalojo
table(seguridad$puntos_seguridad_tenencia)

# Puntos por localidad y entidad (EJEMPLO)
tapply(seguridad$puntos_seguridad_tenencia, seguridad$TLOC, summary)
tapply(seguridad$puntos_seguridad_tenencia, seguridad$ENT, summary)
tapply(seguridad$puntos_seguridad_tenencia, seguridad$region, summary)

## Filtrar casas y departamentos
# Casas
table(data_caracteristicas$P4_3)
viviendas_casas <- data_caracteristicas %>%
  filter(P4_3 %in% c(1:3))
# Departamentos
table(data_caracteristicas$P4_3)
viviendas_departamentos <- data_caracteristicas %>%
  filter(P4_3 %in% c(4))