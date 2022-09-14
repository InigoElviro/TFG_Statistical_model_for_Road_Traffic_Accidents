#LECTURA E INICIALIZACION DE VARIABLES#
#------------------------------------#

setwd("D:/Users/Íñigo/Desktop/TFG")

#------------------------------------#

#LECTURA

# delta_21=read_xlsx("2021_TRAFICO.xlsx", skip=2, col_names = FALSE)
# colnames(delta_21)=colnames(read_xlsx("2021_TRAFICO.xlsx", col_names = TRUE))
# dim(delta_21)
# Poligonos=read.csv("Poligonos.csv")

delta=read_xlsx("TRAFICO_2009_2022.xlsx", skip=2, col_names = FALSE)
colnames(delta)=colnames(read_xlsx("TRAFICO_2009_2022.xlsx", col_names = TRUE))
dim(delta)

#------------------------------------#

#GRAVEDAD Y ACCIDENTADOS

delta$muertos=is.element(delta$GRADO,c("4"))
sum(delta$GRADO==4,na.rm = TRUE)
delta$muertos=as.numeric(delta$muertos)
delta$gravedad=is.element(delta$GRADO,c("2","3","4"))
delta$gravedad=as.numeric(delta$gravedad)

delta$accidentes=delta$MULTIPLES
delta$accidentes=is.element(delta$accidentes,c("1"))
delta$accidentes=as.numeric(delta$accidentes)
delta$accidentes=delta$accidentes+1
delta$y=delta$accidentes

#VARIABLES TEMPORALES

delta$wday=wday(delta$FECHAACCIDENTE)
delta$day_of_week=weekdays(delta$FECHAACCIDENTE)
delta$week=week(delta$FECHAACCIDENTE)
delta$month=month(delta$FECHAACCIDENTE)
delta$year=year(delta$FECHAACCIDENTE)
delta$day_year=yday(delta$FECHAACCIDENTE)
delta$hour=delta$HORA
delta$dia_hora=as.POSIXct(paste(delta$FECHAACCIDENTE, delta$hour), format="%Y-%m-%d %H")

#UBICACION POLIGONOS

delta$plaza=is.element(delta$COPOSTALE ,c(50197)) #Codigo de la empresa contratante
delta$plaza.ocurrencia=is.element(delta$CODPOSTALCT ,c(50197)) #Codigo de la empresa donde ha ocurrido el accidente
table(delta$plaza, delta$plaza.ocurrencia)


#delta$cuarte=is.element(delta$COPOSTALE ,c(50410, 50411, 50420, 50430,50019)) #Codigo de la empresa contratante
#delta$cuarte.ocurrencia=is.element(delta$CODPOSTALCT ,c(50410, 50411, 50420, 50430,50019)) #Codigo de la empresa donde ha ocurrido el accidente
delta$cuarte=is.element(delta$COPOSTALE ,c(50410)) #Codigo de la empresa contratante
delta$cuarte.ocurrencia=is.element(delta$CODPOSTALCT ,c(50410)) #Codigo de la empresa donde ha ocurrido el accidente
table(delta$cuarte, delta$cuarte.ocurrencia)


#delta$villanueva=is.element(delta$COPOSTALE ,c(50830, 50820, 50020)) #Codigo de la empresa contratante
#delta$villanueva.ocurrencia=is.element(delta$CODPOSTALCT ,c(50830, 50820, 50020)) #Codigo de la empresa donde ha ocurrido el accidente
delta$villanueva=is.element(delta$COPOSTALE ,c(50830)) #Codigo de la empresa contratante
delta$villanueva.ocurrencia=is.element(delta$CODPOSTALCT ,c(50830)) #Codigo de la empresa donde ha ocurrido el accidente
table(delta$villanueva, delta$villanueva.ocurrencia)

delta$figueruelas=is.element(delta$COPOSTALE ,c(50639)) #Codigo de la empresa contratante
delta$figueruelas.ocurrencia=is.element(delta$CODPOSTALCT ,c(50639)) #Codigo de la empresa donde ha ocurrido el accidente
table(delta$figueruelas, delta$figueruelas.ocurrencia)

#VARIABLES CATEGORICAS

aux.variable.selected  <- as.matrix( delta[  , 60]  )
aux.name.variable <-  'lluvia'
aux.words.selected <- c('lluvia', 'nieve', 'hielo', 'niebla', 'precipitacion')
aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)

names(delta)
head(delta[, c(60,104)])
table(delta$lluvia)
delta$lluvia = as.integer(delta$lluvia == "TRUE")

aux.marcador=is.element(delta$lluvia,c(1))
aux.marcador=delta$dia_hora[aux.marcador]
delta$lluvia=ifelse(is.element(delta$dia_hora,aux.marcador),1,0)

aux.name.variable <-  'itinere'
delta$itinere=delta$LUGAR==3
table(delta$itinere)
delta$itinere = as.integer(delta$itinere == "TRUE")

aux.variable.selected  <- as.matrix( delta[  , 60]  )
aux.name.variable <-  'alcance'
aux.words.selected <- c('alcance', 'trasero', 'choque','colisi?n')
aux.words.selected.wide <- paste(aux.words.selected, collapse = '|')
delta[[as.name(aux.name.variable)]] <-  grepl(aux.words.selected.wide , aux.variable.selected, perl=TRUE, ignore.case = T)

head(delta[, c(60,105)])
table(delta$alcance)
delta$alcance = as.integer(delta$alcance == "TRUE")

#------------------------------------#

aux.marcador=is.element(delta$itinere,c(1))&(!is.na(delta$itinere))
y1=delta$y[aux.marcador]
hour1=delta$hour[aux.marcador]
wday1=delta$wday[aux.marcador]

aux_collapse1=tapply(y1, list(hour1, wday1), sum, na.rm=TRUE)
aux_collapse1=as.data.frame(aux_collapse1)
aux_collapse1[is.na(aux_collapse1)] = 0

y=delta$y
hour=delta$hour
wday=delta$wday
month=delta$month
day_year=delta$day_year

aux_collapse=tapply(y, list(hour, wday), sum, na.rm=TRUE)
aux_collapse=as.data.frame(aux_collapse)
aux_collapse[is.na(aux_collapse)] = 0


#------------------------------------#


plaza=delta$plaza
#plaza=delta$plaza.ocurrencia
plaza=as.factor(plaza)
levels(plaza)=c(NA,"PLAZA")
delta$PLAZA=plaza

figueruelas=delta$figueruelas
#figueruelas=delta$figueruelas.ocurrencia
figueruelas=as.factor(figueruelas)
levels(figueruelas)=c(NA,"FIGUERUELAS")
delta$FIGUERUELAS=figueruelas

cuarte=delta$cuarte
#cuarte=delta$cuarte.ocurrencia
cuarte=as.factor(cuarte)
levels(cuarte)=c(NA,"CUARTE")
delta$CUARTE=cuarte

villanueva=delta$villanueva
#villanueva=delta$villanueva.ocurrencia
villanueva=as.factor(villanueva)
levels(villanueva)=c(NA,"VILLANUEVA")
delta$VILLANUEVA=villanueva

prueba=delta %>% mutate (mycol4 = coalesce(PLAZA,FIGUERUELAS,CUARTE,VILLANUEVA)) 
#prueba=cbind(prueba, mycol4 = na.omit(unlist(data[-1])))
prueba=subset(prueba, (!is.na(prueba$mycol4)))
delta_P=prueba
colnames(delta_P)[which(names(delta_P) == "mycol4")] ="ubicacion"
delta_poligonos=delta_P

tamaño=delta_poligonos$PLANTILLAE
tamaño=cut(tamaño, breaks=c(0,50,249,Inf))
levels(tamaño)
levels(tamaño)=c("Pequeña Eª","Mediana Eª","Gran Eª")
delta_poligonos$tamaño=tamaño

#------------------------------------#

#Para mostrar repeticiones

aux.acc=delta_poligonos
aux.df=table(aux.acc$IPF_MD5)
aux.df2=aux.df[aux.df>1]

aux.df3=aux.acc[is.element(aux.acc$IPF_MD5,names(aux.df2)),]
aux.df4=aux.df3[order(aux.df3$IPF_MD5,aux.df3$FECHAACCIDENTE),]
head(aux.df4[1:10,c(5,7,53,83)])#REPETICIONES
head(aux.df4[1:10,c(5,7,24,51)])#REPETICIONES
sum(duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE))

#delta_poligonos=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])



#------------------------------------#

#PASOS PREVIOS A LOS MODELOS 2021#

# delta_21=subset(delta, year==2021)
# y=delta_21$y
# hour=delta_21$hour
# wday=delta_21$wday
# month=delta_21$month
# day_year=delta_21$day_year
# 
# aux.yday.hour = round((delta_21$day_year + (delta_21$hour-1)/24),6)
# respuesta_2021=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
# respuesta_2021=as.data.frame(respuesta_2021)
# respuesta_2021[is.na(respuesta_2021)] = 0
# names(respuesta_2021)=c('y')
# 
# respuesta_2021$day_year=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta_2021$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta_2021$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta_2021$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta_2021$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta_2021$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
# 
# respuesta_2021$lluvia=(tapply(delta_21$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
# respuesta_2021$itinere=tapply(delta_21$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
# respuesta_2021$alcance=(tapply(delta_21$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
# respuesta_2021$fin_de_semana=is.element(respuesta_2021$wday,c(6,7))
# 
# dim(respuesta_2021)
# respuesta_2021=rownames_to_column(respuesta_2021, "aux.yday.hour")
# respuesta_2021$aux.yday.hour=as.numeric(respuesta_2021$aux.yday.hour)
# 
# aux.vday_year=1:365
# aux.vday_hour=1:24
# aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
# aux.v$aux.yday.hour=round((aux.v$Var1+aux.v$Var2), 6)
# 
# prueba=merge(respuesta_2021, aux.v, all = TRUE)
# prueba[is.na(prueba)] = 0
# prueba$Var1=NULL
# prueba$Var2=NULL
# respuesta_2021=prueba
# 
# respuesta_2021$c1d=cos(2*pi* respuesta_2021$arm_dia/365)
# respuesta_2021$s1d=sin(2*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$c2d=cos(4*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$s2d=sin(4*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$c3d=cos(6*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$s3d=sin(6*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$c4d=cos(8*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$s4d=sin(8*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$c5d=cos(10*pi*respuesta_2021$arm_dia/365)
# respuesta_2021$s5d=sin(10*pi*respuesta_2021$arm_dia/365)
# 
# respuesta_2021$c1h=cos(2*pi*respuesta_2021$arm_hora/24)
# respuesta_2021$s1h=sin(2*pi*respuesta_2021$arm_hora/24)
# respuesta_2021$c2h=cos(4*pi*respuesta_2021$arm_hora/24)
# respuesta_2021$s2h=sin(4*pi*respuesta_2021$arm_hora/24)
# respuesta_2021$c3h=cos(6*pi*respuesta_2021$arm_hora/24)
# respuesta_2021$s3h=sin(6*pi*respuesta_2021$arm_hora/24)










