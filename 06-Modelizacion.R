#MODELIZACION#
#------------------------------------#


#dev.off()
#------------------------------------#

#PASOS PREVIOS A LOS MODELOS 2009-2021#

años=c(2009:2021)
delta_09_21=subset(delta, year %in% años)
delta_09_21=(delta_09_21[!duplicated(delta_09_21$IPF_MD5,delta_09_21$FECHAACCIDENTE),])
y=delta_09_21$y
hour=delta_09_21$hour
wday=delta_09_21$wday
month=delta_09_21$month
day_year=delta_09_21$day_year

# delta_09_21$dias_años=as.numeric(difftime(as.Date(delta_09_21$FECHAACCIDENTE), as.Date("2008-12-31"), unit="days"))
# dias_años=delta_09_21$dias_años


aux.yday.hour=(delta_09_21$year*100000+delta_09_21$day_year*100+delta_09_21$hour)
#aux.yday.hour = round((delta_09_21$dias_años + (delta_09_21$hour-1)/24),6)
respuesta=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
respuesta=as.data.frame(respuesta)
respuesta[is.na(respuesta)] = 0
names(respuesta)=c('y')

#respuesta$y se depuran los datos para evitar que accidentes con multiples personas nvolucradoas como el autobus del 30-nov-2010, den lugar a un número de accidentes sobrerepresentado
respuesta$y[respuesta$y==75]=1
#respuesta$y[is.element(respuesta$year,c(2010))&is.element(respuesta$day_year,c(330))&is.element(respuesta$hour,c(6))]=1
respuesta$y[respuesta$y==19]=1
#respuesta$y[respuesta$y>=15]=1
# respuesta$dias_años=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta$arm_dia=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
# respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)

respuesta$lluvia=(tapply(delta_09_21$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
respuesta$itinere=tapply(delta_09_21$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
respuesta$alcance=(tapply(delta_09_21$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
#respuesta$fin_de_semana=is.element(respuesta$wday,c(6,7))
respuesta$leve=(tapply(delta_09_21$GRADO==1, list(aux.yday.hour), sum, na.rm=TRUE)>0)
grave1=(tapply(delta_09_21$GRADO==2, list(aux.yday.hour), sum, na.rm=TRUE)>0)
grave2=(tapply(delta_09_21$GRADO==3, list(aux.yday.hour), sum, na.rm=TRUE)>0)
respuesta$grave=grave1+grave2
respuesta$muertos=(tapply(delta_09_21$GRADO==4, list(aux.yday.hour), sum, na.rm=TRUE)>0)


dim(respuesta)
respuesta=rownames_to_column(respuesta, "aux.yday.hour")
respuesta$aux.yday.hour=as.numeric(respuesta$aux.yday.hour)

aux.date=seq(ymd('2009-01-01'),ymd('2021-12-31'), by = 'days')
aux.date=seq(as.POSIXct("2009-01-01 0","%Y-%m-%d %H", tz="UTC"),to=as.POSIXct("2021-12-31 23", "%Y-%m-%d %H", tz="UTC"), by="hour")
head(aux.date)
aux.date=cbind(aux.date,year(aux.date),yday(aux.date),hour(aux.date))
aux.date=cbind(aux.date,aux.date[,2]*100000+aux.date[,3]*100+aux.date[,4])
aux.v=aux.date[,2:5]
colnames(aux.v)=c("year","day_year","hour","aux.yday.hour")
#aux.yday.hour=round((delta$year*100000+delta$day_year*100+delta$hour), 6)

#aux.vday_year=1:(365*length(años)+sum(leap_year(años)))
# aux.vday_year=aux.date[,4]
# aux.vday_hour=1:24
# aux.v = expand.grid(aux.vday_year , (aux.vday_hour-1)/24)
# aux.v$aux.yday.hour=round((aux.v$Var1+aux.v$Var2), 6)

prueba=merge(respuesta, aux.v, all = TRUE)
prueba[is.na(prueba)] = 0
# prueba$Var1=NULL
# prueba$Var2=NULL
respuesta=prueba

aux.yday.hour=respuesta$aux.yday.hour
day_year=respuesta$day_year
hour=respuesta$hour
respuesta$arm_dia=tapply(day_year, list(aux.yday.hour), mean, na.rm=TRUE)
respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)

# respuesta$day_years=respuesta$day_year+365*(respuesta$year-2009)+sum(leap_year(respuesta$year-2009))
# day_years=respuesta$day_years
# respuesta$arm_dia=tapply(day_years, list(aux.yday.hour), mean, na.rm=TRUE)

#dias=365*length(años)+sum(leap_year(años))
respuesta$c1d=cos(2*pi*respuesta$arm_dia/366)
respuesta$s1d=sin(2*pi*respuesta$arm_dia/366)
respuesta$c2d=cos(4*pi*respuesta$arm_dia/366)
respuesta$s2d=sin(4*pi*respuesta$arm_dia/366)
respuesta$c3d=cos(6*pi*respuesta$arm_dia/366)
respuesta$s3d=sin(6*pi*respuesta$arm_dia/366)
respuesta$c4d=cos(8*pi*respuesta$arm_dia/366)
respuesta$s4d=sin(8*pi*respuesta$arm_dia/366)
respuesta$c5d=cos(10*pi*respuesta$arm_dia/366)
respuesta$s5d=sin(10*pi*respuesta$arm_dia/366)


respuesta$c1h=cos(2*pi*respuesta$arm_hora/24)
respuesta$s1h=sin(2*pi*respuesta$arm_hora/24)
respuesta$c2h=cos(4*pi*respuesta$arm_hora/24)
respuesta$s2h=sin(4*pi*respuesta$arm_hora/24)
respuesta$c3h=cos(6*pi*respuesta$arm_hora/24)
respuesta$s3h=sin(6*pi*respuesta$arm_hora/24)

#------------------------------------#

respuesta$dia_año=respuesta$day_year+respuesta$year*1000
i=(as.Date("15-03-2020", "%d-%m-%Y"))
f=(as.Date("21-06-2020", "%d-%m-%Y"))
f_est_alar=(as.Date("9-05-2021", "%d-%m-%Y"))
inicio=yday(i)+year(i)*1000
final=yday(f)+year(f)*1000
final_estado_alarma=yday(f_est_alar)+year(f_est_alar)*1000

respuesta$conf=as.numeric(is.element(respuesta$dia_año,inicio:final))
respuesta$covid=as.numeric(is.element(respuesta$dia_año,final:final_estado_alarma))

#------------------------------------#
subset_respuesta=subset(respuesta,respuesta$dia_año>0)

# prob_acc=tapply(subset_respuesta$y, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
# summary(prob_acc)

prob_leve=tapply(subset_respuesta$leve, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
prob_grave=tapply(subset_respuesta$grave, list(subset_respuesta$dia_año), sum, na.rm=TRUE)
prob_muerte=tapply(subset_respuesta$muertos, list(subset_respuesta$dia_año), sum, na.rm=TRUE)

p_leve=summary(prob_leve)[4]
p_grave=summary(prob_grave)[4]
p_muerte=summary(prob_muerte)[4]

cat(p_leve,p_grave,p_muerte,sep="&",fill=TRUE)




basura=table(respuesta$alcance,respuesta$grave)
basura[2,]/apply(basura,2,sum)

#------------------------------------#

#M1 BASE-Armonicos 

M1= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M2 = M1 + LLUVIA

M2= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia, data=respuesta,family = "poisson",control = glm.control(maxit=100))

#M3 = M1 + ITINERE

M3= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+itinere, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M4 = M1 + ALCANCE

M4= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M5 = M1 + LLUVIA + ITINERE

M5= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M6 = M1 + LLUVIA + ALCANCE

M6= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M7 = M1 + ITINERE + ALCANCE

M7= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+itinere+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M8 = M1 + LLUVIA + ITINERE + ALCANCE

M8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M9 = TODAS LAS VARIABLES 

M9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+lluvia+itinere+alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#------------------------------------#

#Interacciones

#IM1 = Armonicos * UBICACION 

IM1= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d),data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM2 = BASE * LLUVIA 

IM2= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM3 = BASE * ITINERE

IM3= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*itinere,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM4 = BASE * ALCANCE

IM4= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM5 = LLUVIA * ITINERE

IM5= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM6 = LLUVIA * ALCANCE

IM6= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM7 = ITINERE* ALCANCE

IM7= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*itinere*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM8 = LLUVIA  * ITINERE* ALCANCE

IM8= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#IM9 = TODAS LAS VARIABLES INTERACCIONAN

#IM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=respuesta, family = "poisson",control = glm.control(maxit=100))

#?Warning message: glm.fit: algorithm did not converge?

#------------------------------------#

my_list_M=list(M1,M2,M3,M4,M5,M6,M7,M8,M9)
my_list_IM=list(IM1,IM2,IM3,IM4,IM5,IM6,IM7,IM8)
obten_modelos(my_list_M)
obten_modelos(my_list_IM)

#IM8_step=step(IM8)
#summary(IM8_step)
#plot_model(M2)
func.summary(IM8)
plot_model(IM8,axis.lim = c(0,10*exp(+09)))


#------------------------------------#

ter_im5=terminos(IM5)
ter_im7=terminos(IM7)
ter_im8=terminos(IM8)
#length(ter_im8)

terfr_im5=terminos_frame(IM5)
terfr_im7=terminos_frame(IM7)
terfr_im8=terminos_frame(IM8)
terms=cbind(terfr_im5,terfr_im7,terfr_im8)

terms_unicos=merge(ter_im5,ter_im7)
terms_unicos=merge(terms_unicos,ter_im8)

terms_todos=merge(ter_im5,ter_im7, all = TRUE)
terms_todos=merge(terms_todos,ter_im8, all = TRUE)

a=terms_unicos$terminos
cat(a,sep="+",fill=TRUE)
a=terms_todos$terminos
cat(a,sep="+",fill=TRUE)

#M_unicos = LAS VARIABLES REPETIDAS DE LOS 3 MODELOS CON MENOR AIC

M_unicos=glm(y~(c1d+c1d*itinere+c2d+c3d+c3d*itinere+c4d+c4d*itinere+itinere+s2d*itinere+s3d+
                  s3d*itinere+s4d*itinere),data=respuesta, family = "poisson",control = glm.control(maxit=100))

#M_todos = TODAS LAS VARIABLES DE LOS 3 MODELOS CON MENOR AIC

M_todos=glm(y~(alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
                 c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
                 c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
                 itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
                 s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
                 s4d*itinere*alcance),data=respuesta, family = "poisson",control = glm.control(maxit=100))

lista_final=list(M_unicos,M_todos)
obten_modelos(lista_final)

summary(M_unicos)
summary(M_todos)
par(mfrow = c(1,1))
# plot_model(M_unicos,axis.lim = c(0,10*exp(+0100)))
# plot_model(M_unicos)
# 
# par(mfrow = c(2,2))
# plot(M_unicos)

a=varImp(M_unicos, scale = FALSE)
a=varImp(M_todos, scale = FALSE)
#plot(a)

plot_importancia(a)
#dev.print(pdf, 'M_unicos.pdf' ,  height=10, width=10 )
#dev.print(pdf, 'M_todos.pdf' ,  height=10, width=10 )

#Para obtener tabla coeficientes

obten_coeff(M_todos)
#summary(M_todos)$coeff
#obten_confint(M_todos)

plot_model(M_todos,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
#dev.print(pdf, 'plot_model_M_todos.pdf' ,  height=10, width=10 )

#------------------------------------#

#EFECTO CONFINAMIENTO Y COVID

M_conf=glm(y~(conf+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
                 c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
                 c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
                 itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
                 s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
                 s4d*itinere*alcance),data=respuesta, family = "poisson",control = glm.control(maxit=100))

M_covid=glm(y~(covid+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
                c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
                c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
                itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
                s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
                s4d*itinere*alcance),data=respuesta, family = "poisson",control = glm.control(maxit=100))

lista_COVID=list(M_conf,M_covid)
obten_modelos(lista_COVID)

#------------------------------------#

# M_global=glm(y~((c1h+s1h+c2h+s2h+c3h+s3h)+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
#                  c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
#                  c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
#                  itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
#                  s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
#                  s4d*itinere*alcance+lluvia*(c1h+s1h+c2h+s2h+c3h+s3h)+itinere*(c1h+s1h+c2h+s2h+c3h+s3h)),data=respuesta, family = "poisson",control = glm.control(maxit=100))

# M_g=glm(y~((c1h+s1h+c2h+s2h+c3h+s3h)+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
#                    c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
#                    c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
#                    itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
#                    s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
#                    s4d*itinere*alcance),data=respuesta, family = "poisson",control = glm.control(maxit=100))

M_g1=glm(y~((c1h+s1h+c2h+s2h+c3h+s3h)+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
                  c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
                  c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
                  itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
                  s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
                  s4d*itinere*alcance+itinere*(c1h+s1h+c2h+s2h+c3h+s3h)),data=respuesta, family = "poisson",control = glm.control(maxit=100))

 # M_g2=glm(y~((c1h+s1h+c2h+s2h+c3h+s3h)+alcance+c1d+c1d*alcance+c1d*itinere+c1d*itinere*alcance+c2d+c2d*itinere+
 #                   c2d*itinere*alcance+c3d+c3d*alcance+c3d*itinere+c3d*itinere*alcance+c4d+
 #                   c4d*alcance+c4d*itinere+c4d*itinere*alcance+c4d*lluvia*itinere+itinere+
 #                   itinere*alcance+lluvia+lluvia*itinere+s1d+s1d*itinere+s2d*itinere+
 #                   s2d*itinere*alcance+s3d+s3d*itinere+s3d*itinere*alcance+s4d+s4d*itinere+
 #                   s4d*itinere*alcance+lluvia*(c1h+s1h+c2h+s2h+c3h+s3h)),data=respuesta, family = "poisson",control = glm.control(maxit=100))

 lista_comparativa=list(M_global,M_g,M_g1,M_g2)
 obten_modelos(lista_comparativa) 
 
#------------------------------------#

#MODELO FINAL

M_Final=M_g1

obten_coeff(M_Final)
#summary(M_Final)$coeff
#obten_confint(M_Final)

a=varImp(M_Final, scale = FALSE)
a1=as.data.frame(a[0:((dim(a)[1])/2),])
rownames(a1)=rownames(a)[0:((dim(a)[1])/2)]
colnames(a1)=colnames(a)

a2=as.data.frame(a[((dim(a)[1]/2)+1):(dim(a)[1]+1),])
rownames(a2)=rownames(a)[((dim(a)[1]/2)+1):(dim(a)[1]+1)]
colnames(a2)=colnames(a)


plot_importancia(a)
#dev.print(pdf, 'plot_importancia_M_Final.pdf' ,  height=10, width=10 )
plot_importancia(a1)
#dev.print(pdf, 'plot_importancia_M_Final_1.pdf' ,  height=10, width=10 )
plot_importancia(a2)
#dev.print(pdf, 'plot_importancia_M_Final_2.pdf' ,  height=10, width=10 )


plot_model(M_Final,sort.est=TRUE)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
#dev.print(pdf, 'plot_model_M_Final.pdf' ,  height=10, width=10 )
#names=names((M_Final$coefficients)[0:((length(M_Final$coefficients))/2)])


#------------------------------------#

#REPRESENTACIÓN DEL CICLO HORARIO

aux.x=respuesta$hour
aux.y=summary(M_g1)$coef[2,1]*respuesta$c1h+
  summary(M_g1)$coef[3,1]*respuesta$s1h+
  summary(M_g1)$coef[4,1]*respuesta$c2h+
  summary(M_g1)$coef[5,1]*respuesta$s2h+
  summary(M_g1)$coef[6,1]*respuesta$c3h+
  summary(M_g1)$coef[7,1]*respuesta$s3h
aux.y=tapply(aux.y, aux.x, mean)
plot(0:23, exp(aux.y), type="n",main="",xlab = "hour",ylab = "effect",cex.axis=1.5)
lines(0:23, exp(aux.y))

aux.y=summary(M_g1)$coef[37,1]*respuesta$c1h+
  summary(M_g1)$coef[38,1]*respuesta$s1h+
  summary(M_g1)$coef[39,1]*respuesta$c2h+
  summary(M_g1)$coef[40,1]*respuesta$s2h+
  summary(M_g1)$coef[41,1]*respuesta$c3h+
  summary(M_g1)$coef[42,1]*respuesta$s3h+
  summary(M_g1)$coef[2,1]*respuesta$c1h+
  summary(M_g1)$coef[3,1]*respuesta$s1h+
  summary(M_g1)$coef[4,1]*respuesta$c2h+
  summary(M_g1)$coef[5,1]*respuesta$s2h+
  summary(M_g1)$coef[6,1]*respuesta$c3h+
  summary(M_g1)$coef[7,1]*respuesta$s3h
aux.y=tapply(aux.y, aux.x, mean)
lines(0:23, exp(aux.y),col="red")
abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
abline(h=c(1), lty=3, lwd=0.1,col="blue")

#------------------------------------#

#INCLUIR INTERACCIÓN  ITINERE
#aux.y=0+0*respuesta$year+summary(M_g1)$coef[1]

aux.x=respuesta$day_year
aux.y=0+0*respuesta$year
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.mision=aux.y.plot
plot(0:366, exp(aux.y.plot), type="n",main="",xlab = "día",ylab = "effect",cex.lab=1.5,cex.axis=2)
my_label=cut(0:366, c(0,32,60,91,121,152,182,213,244,274,305,335,366),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ), include.lowest=T)
a=c(0,32,60,91,121,152,182,213,244,274,305,335)
b=c(32,60,91,121,152,182,213,244,274,305,335,366)
meses_plot=(a+b)/2
axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
lines(0:366, exp(aux.y.plot), lwd=3)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
abline(h=c(1), lty=3, lwd=2,col="blue")

#aux.y=aux.y+summary(M_g1)$coef["itinere"]

for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))&grepl(c("itinere"),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))&grepl(c("itinere"),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
lines(0:366, exp(aux.y.plot),col="red", lwd=3)
legend("top", legend=c("Itinere - 0", "Itinere - 1"),text.font=2,horiz=TRUE, lty=1,lwd=2, col=c("black","red" ),  cex=1)
#dev.print(pdf, 'interacc_itinere.pdf' ,  height=10, width=10 )
aux.y.plot.itinere=aux.y.plot
summary(aux.y.plot.itinere/aux.y.plot.mision)
hist(aux.y.plot.itinere/aux.y.plot.mision)

#------------------------------------#

#INCLUIR INTERACCIÓN  

aux.x=respuesta$day_year
efect_0=summary(M_g1)$coef[1]
aux.y=0+0*respuesta$year+efect_0
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
}

aux.y.plot_0=tapply(aux.y, aux.x, mean)
aux.y.plot.mision=aux.y.plot_0

efect_1=(summary(M_g1)$coef[is.element(rownames(summary(M_g1)$coef),"itinere")])[1]
aux.y=aux.y+efect_1
efect_2=mean(aux.y)
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))&grepl(c("itinere"),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_g1)$coef))&grepl(c("itinere"),rownames(summary(M_g1)$coef))
  aux.y=aux.y+(summary(M_g1)$coef[aux.position.armonics,1])[1]*respuesta[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.itinere=aux.y.plot

max=1.6*max(abs(exp(efect_0)),abs(exp(efect_2)))
min=1/1.6*min(abs(exp(efect_0)),abs(exp(efect_2)))
aux.ylab1=TeX("\\hat{\\mu}")

#PLOT EFECTO 0
linea=5

plot(0:366, exp(aux.y.plot_0),ylim=c(min,max), type="n",main="",xlab = "día",ylab = "",cex.lab=1.5,cex.axis=2)
title(ylab=aux.ylab, line=2, cex.lab=1.75)
my_label=cut(0:366, c(0,32,60,91,121,152,182,213,244,274,305,335,366),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ), include.lowest=T)
a=c(0,32,60,91,121,152,182,213,244,274,305,335)
b=c(32,60,91,121,152,182,213,244,274,305,335,366)
meses_plot=(a+b)/2
axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
lines(0:366, exp(aux.y.plot_0), lwd=linea)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
abline(h=c(exp(efect_0)), lty=3, lwd=linea,col="black")

#PLOT EFECTO 1

lines(0:366, exp(aux.y.plot),col="red", lwd=linea)
abline(h=c(exp(efect_2)), lty=3, lwd=linea,col="red")
axis(4, at=c(exp(efect_0),exp(efect_2)),labels = c( round(exp(efect_0),2) , round(exp(efect_2),2)))

max_0=max(exp(aux.y.plot.mision))
min_0=min(exp(aux.y.plot.mision))
abline(h=c(max_0,min_0), lty=4, lwd=1,col="black")

max_1=max(exp(aux.y.plot.itinere))
min_1=min(exp(aux.y.plot.itinere))
abline(h=c(max_1,min_1), lty=4, lwd=1,col="black")

legend("topleft",horiz = TRUE, legend=c("ITINERE - 0", "ITINERE - 1"),text.font=2, lty=1,lwd=linea, col=c("black","red" ),  cex=1.1)
#dev.print(pdf, 'interacc_itinere2.pdf' ,  height=10, width=10 )

itinere_mision=exp(aux.y.plot.itinere)/exp(aux.y.plot.mision)
sum_itinere_mision=summary(itinere_mision)

xmin=floor(sum_itinere_mision[1])
xmax=ceiling(sum_itinere_mision[6])
hist(exp(aux.y.plot.itinere)/exp(aux.y.plot.mision),main="",xlim = c(xmin,xmax),cex.axis=2,cex.lab=1.5, xlab="Itinere / Misión")
#dev.print(pdf, 'itinere_vs_mision.pdf' ,  height=10, width=10 )

#------------------------------------#


# aux.position.armonics=grepl(c("c1d"),rownames(summary(M_g1)$coef))
# (summary(M_g1)$coef[aux.position.armonics,1])[1]
# 
# aux.y=summary(M_g1)$coef[2,1]*respuesta$c1d+
#   summary(M_g1)$coef[3,1]*respuesta$s1d+
#   summary(M_g1)$coef[4,1]*respuesta$c2d+
#   summary(M_g1)$coef[5,1]*respuesta$s2d+
#   summary(M_g1)$coef[6,1]*respuesta$c3d+
#   summary(M_g1)$coef[7,1]*respuesta$s3d+
#   summary(M_g1)$coef[8,1]*respuesta$c4d+
#   summary(M_g1)$coef[9,1]*respuesta$s4d


#------------------------------------#

#LLUVIA

aux.x=respuesta$day_year
aux.y=exp((1.954+0.0169*respuesta$c4d))
aux.y2=exp((1.954+0.327*respuesta$c4d))
y.liminf=(6/7)*min(aux.y,aux.y2)
y.limsup=(7/6)*max(aux.y,aux.y2)
aux.ylim=c(y.liminf,y.limsup)
plot(aux.x, aux.y, ylim=aux.ylim, main="lluvia",cex.axis=1.5)
points(aux.x, aux.y2, ylim=aux.ylim,cex.axis=1.5,col='red')
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)

aux.y=exp((1.954+0.0169*respuesta$c4d))
plot(aux.x, aux.y, main="lluvia",cex.axis=1.5)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)


#ITINERE

aux.x=respuesta$day_year
aux.y=exp((0.998-0.359*respuesta$c1d-0.213*respuesta$c2d+0.152*respuesta$c3d
          +0.4*respuesta$c4d+0.03*respuesta$s1d-0.093*respuesta$s2d
          +0.108*respuesta$s3d+0.128*respuesta$s4d-0.576*respuesta$alcance-0.418*respuesta$lluvia
          +0.393*respuesta$c1d*respuesta$alcance+0.279*respuesta$c2d*respuesta$alcance
          -0.132*respuesta$c3d*respuesta$alcance-0.425*respuesta$c4d*respuesta$alcance
          +0.076*respuesta$s2d*respuesta$alcance-0.092*respuesta$s3d*respuesta$alcance
          -0.143*respuesta$c1d*respuesta$alcance-0.22*respuesta$c4d*respuesta$lluvia
          ))

aux.y=exp((0.998-0.359*respuesta$c1d-0.213*respuesta$c2d+0.152*respuesta$c3d
                             +0.4*respuesta$c4d+0.03*respuesta$s1d-0.093*respuesta$s2d
                             +0.108*respuesta$s3d+0.128*respuesta$s4d-0.576*respuesta$alcance-0.418*respuesta$lluvia
                             ))

plot(aux.x, aux.y, main="itinere",cex.axis=1.5)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)

#------------------------------------#

# terminos[1]=paste0(terminos[1],terminos[2],terminos[3],terminos[4], sep="")
# form = as.formula(paste(y, "~", terminos))
# M= glm(form,data=respuesta, family = "poisson")
# 
# M=glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+itinere+c1d*itinere+s1d*itinere+c2d*itinere+s2d*itinere+c3d*itinere+s3d*itinere+c4d*itinere+s4d*itinere+itinere*alcance+c1d*itinere*alcance+s1d*itinere*alcance+c2d*itinere*alcance+s2d*itinere*alcance+c3d*itinere*alcance+s3d*itinere*alcance+c4d*itinere*alcance+s4d*itinere*alcance),data=respuesta, family = "poisson")
# summary(M)
# 
# 
# 
# step(M1)
# 
# summary(IM7)$aic
# step(IM7)$aic


# 
# terminos=function(modelo){
#   aux.y=as.data.frame(summary(modelo)$coefficients)
#   lista=(aux.y %>% filter(aux.y[,4]<0.001) %>% rownames())
#   lista=gsub(":","*",lista[2:length(lista)])
#   cat(lista,sep="+",fill=TRUE)
#   
#   terminos=capture.output(cat(lista,sep="+",fill=TRUE))
#   for (i in 1 :length(terminos)){
#     terminos[1]=paste0(terminos[1],terminos[i], sep="")
#   }
#   terminos=terminos[1]
# }

#M8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere+alcance, data=respuesta, family = "poisson",control = glm.control(maxit=100))

# graf=ggplot2::ggplot(a, aes(x=reorder(rownames(a),Overall), y=Overall)) +
#   geom_point( color="blue", size=4, alpha=0.6)+
#   geom_segment( aes(x=rownames(a), xend=rownames(a), y=0, yend=Overall), 
#                 color='skyblue') +
#   xlab('Variable')+
#   ylab('Overall Importance')+
#   theme_light() +
#   coord_flip()
#graf2=graf +theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))



basura=predict(M_Final, type="response")
basura1=tapply(basura, list(respuesta$dia_año), sum, na.rm=TRUE)
basura2=tapply(basura, list(respuesta$dia_año,respuesta$itinere>0), sum, na.rm=TRUE)
basura3=apply(basura2, 2, mean, na.rm=TRUE)

summary(basura1-apply(basura2,1,sum))
