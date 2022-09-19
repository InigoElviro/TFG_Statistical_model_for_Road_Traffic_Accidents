#LECTURA E INICIALIZACION DE VARIABLES#
#------------------------------------#

setwd("") #DIRECTORIO DE TRABAJO

#------------------------------------#

#LECTURA DE BASE DE DATOS

delta=read_xlsx("base_datos.xlsx", skip=2, col_names = FALSE)
colnames(delta)=colnames(read_xlsx("base_datos.xlsx", col_names = TRUE))
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

