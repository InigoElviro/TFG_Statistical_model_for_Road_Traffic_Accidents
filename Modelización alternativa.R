#MODELIZACION ALTERNATIVA#
#------------------------------------#

aux.respuesta=delta_09_21
y=aux.respuesta$y
hour=aux.respuesta$hour
wday=aux.respuesta$wday
month=aux.respuesta$month
day_year=aux.respuesta$day_year
aux.respuesta$day_years=aux.respuesta$day_year+365*(aux.respuesta$year-2009)+sum(leap_year(aux.respuesta$year-2009))

dias=365*length(años)+sum(leap_year(años))
aux.respuesta$c1d=cos(2*pi* aux.respuesta$day_years/dias)
aux.respuesta$s1d=sin(2*pi*aux.respuesta$day_years/dias)
aux.respuesta$c2d=cos(4*pi*aux.respuesta$day_years/dias)
aux.respuesta$s2d=sin(4*pi*aux.respuesta$day_years/dias)
aux.respuesta$c3d=cos(6*pi*aux.respuesta$day_years/dias)
aux.respuesta$s3d=sin(6*pi*aux.respuesta$day_years/dias)
aux.respuesta$c4d=cos(8*pi*aux.respuesta$day_years/dias)
aux.respuesta$s4d=sin(8*pi*aux.respuesta$day_years/dias)
aux.respuesta$c5d=cos(10*pi*aux.respuesta$day_years/dias)
aux.respuesta$s5d=sin(10*pi*aux.respuesta$day_years/dias)
# 
aux.respuesta$c1h=cos(2*pi*aux.respuesta$hour/24)
aux.respuesta$s1h=sin(2*pi*aux.respuesta$hour/24)
aux.respuesta$c2h=cos(4*pi*aux.respuesta$hour/24)
aux.respuesta$s2h=sin(4*pi*aux.respuesta$hour/24)
aux.respuesta$c3h=cos(6*pi*aux.respuesta$hour/24)
aux.respuesta$s3h=sin(6*pi*aux.respuesta$hour/24)


#------------------------------------#

#M1 BASE-Armonicos 

aM1= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux.respuesta, family = "poisson")

#M2 = M1 + LLUVIA

aM2= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia, data=aux.respuesta,family = "poisson")

#M3 = M1 + ITINERE

aM3= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+itinere, data=aux.respuesta, family = "poisson")

#M4 = M1 + ALCANCE

aM4= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+alcance, data=aux.respuesta, family = "poisson")

#M5 = M1 + LLUVIA + ITINERE

aM5= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere, data=aux.respuesta, family = "poisson")

#M6 = M1 + LLUVIA + ALCANCE

aM6= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+alcance, data=aux.respuesta, family = "poisson")

#M7 = M1 + ITINERE + ALCANCE

aM7= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+itinere+alcance, data=aux.respuesta, family = "poisson")

#M8 = M1 + LLUVIA + ITINERE + ALCANCE

aM8= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+lluvia+itinere+alcance, data=aux.respuesta, family = "poisson")

#M9 = TODAS LAS VARIABLES 

aM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)+lluvia+itinere+alcance,data=aux.respuesta, family = "poisson")

#------------------------------------#

#Interacciones

#IM1 = Armonicos * UBICACION 

aIM1= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d),data=aux.respuesta, family = "poisson")

#IM2 = BASE * LLUVIA 

aIM2= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia,data=aux.respuesta, family = "poisson")

#IM3 = BASE * ITINERE

aIM3= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*itinere,data=aux.respuesta, family = "poisson")

#IM4 = BASE * ALCANCE

aIM4= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*alcance,data=aux.respuesta, family = "poisson")

#IM5 = LLUVIA * ITINERE

aIM5= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere,data=aux.respuesta, family = "poisson")

#IM6 = LLUVIA * ALCANCE

aIM6= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*alcance,data=aux.respuesta, family = "poisson")

#IM7 = ITINERE* ALCANCE

aIM7= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*itinere*alcance,data=aux.respuesta, family = "poisson")

#IM8 = LLUVIA  * ITINERE* ALCANCE

aIM8= glm(y~(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=aux.respuesta, family = "poisson")

#IM9 = TODAS LAS VARIABLES INTERACCIONAN

#IM9= glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)*(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d)*lluvia*itinere*alcance,data=aux.respuesta, family = "poisson")

#?Warning message: glm.fit: algorithm did not converge?

#------------------------------------#

a_my_list_M=list(aM1,aM2,aM3,aM4,aM5,aM6,aM7,aM8,aM9)
a_my_list_IM=list(aIM1,aIM2,aIM3,aIM4,aIM5,aIM6,aIM7,aIM8)
obten_modelos(a_my_list_M)
obten_modelos(a_my_list_IM)







