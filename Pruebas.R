#PRUEBAS#
#------------------------------------#

accidentes_años=function(){
  
  accidentados=delta$accidentes
  dia=delta$day_year
  año=delta$year
  accidentados_por_dia_año=tapply(accidentados, list(dia, año),sum , na.rm=T)
  accidentados_por_dia_año= as.data.frame(accidentados_por_dia_año)
  inicio=which( colnames(accidentados_por_dia_año)=="2009" )
  fin=which( colnames(accidentados_por_dia_año)=="2021" )
  accidentados_por_dia_año=accidentados_por_dia_año[,c(inicio:fin)]
  par(mfrow=c(2,4))
  aux.x=rownames(accidentados_por_dia_año)
  for(i.year in 1:length(colnames(accidentados_por_dia_año))){
    aux.y=accidentados_por_dia_año[,i.year]
    plot(aux.x, aux.y, main=i.year)
    abline(v=which.min(aux.y), col=4)
    abline(v=which.max(aux.y), col=2)
  }
}



efecto_dia_año=function(){
  
  par(mfrow=c(2,4))
  for(i.year in 2009:2021){
    
  aux.marcador=is.element(delta$year,i.year)
  subset=subset(delta,aux.marcador==TRUE)
  subset$c1d=cos(2*pi*subset$day_year/365)
  subset$s1d=sin(2*pi*subset$day_year/365)
  subset$c2d=cos(4*pi*subset$day_year/365)
  subset$s2d=sin(4*pi*subset$day_year/365)
  subset$c3d=cos(6*pi*subset$day_year/365)
  subset$s3d=sin(6*pi*subset$day_year/365)
  subset$c4d=cos(8*pi*subset$day_year/365)
  subset$s4d=sin(8*pi*subset$day_year/365)
  subset$y=subset$accidentes
  aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=subset,family = "poisson")
  summary(aux_glm)
  aux.x=subset$day_year
  aux.y=summary(aux_glm)$coef[2,1]*subset$c1d+
    summary(aux_glm)$coef[3,1]*subset$s1d+
    summary(aux_glm)$coef[4,1]*subset$c2d+
    summary(aux_glm)$coef[5,1]*subset$s2d+
    summary(aux_glm)$coef[6,1]*subset$c3d+
    summary(aux_glm)$coef[7,1]*subset$s3d+
    summary(aux_glm)$coef[8,1]*subset$c4d+
    summary(aux_glm)$coef[9,1]*subset$s4d
  plot(aux.x, exp(aux.y), main=i.year)
  abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
   } 
}
#efecto_dia_año()

efecto_dia_año_periodo=function(periodo){
  
    par(mfrow=c(1,1))
    aux.marcador=is.element(delta$year,periodo)
    subset=subset(delta,aux.marcador==TRUE)
    subset$c1d=cos(2*pi*subset$day_year/365)
    subset$s1d=sin(2*pi*subset$day_year/365)
    subset$c2d=cos(4*pi*subset$day_year/365)
    subset$s2d=sin(4*pi*subset$day_year/365)
    subset$c3d=cos(6*pi*subset$day_year/365)
    subset$s3d=sin(6*pi*subset$day_year/365)
    subset$c4d=cos(8*pi*subset$day_year/365)
    subset$s4d=sin(8*pi*subset$day_year/365)
    subset$y=subset$accidentes
    aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=subset,family = "poisson")
    summary(aux_glm)
    aux.x=subset$day_year
    aux.y=summary(aux_glm)$coef[2,1]*subset$c1d+
      summary(aux_glm)$coef[3,1]*subset$s1d+
      summary(aux_glm)$coef[4,1]*subset$c2d+
      summary(aux_glm)$coef[5,1]*subset$s2d+
      summary(aux_glm)$coef[6,1]*subset$c3d+
      summary(aux_glm)$coef[7,1]*subset$s3d+
      summary(aux_glm)$coef[8,1]*subset$c4d+
      summary(aux_glm)$coef[9,1]*subset$s4d
    plot(aux.x, exp(aux.y), cex.axis=2)
    abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
}
efecto_dia_año_periodo(2009:2016)
efecto_dia_año_periodo(2017:2019)
efecto_dia_año_periodo(2020:2021)

dev.print(pdf, 'arm_dia_20.pdf' ,  height=10, width=10 )


accidentes_años_semana=function(){
  
  accidentados=delta$accidentes
  mes=delta$month
  año=delta$year
  accidentados_por_dia_año=tapply(accidentados, list(mes, año),sum , na.rm=T)
  accidentados_por_dia_año= as.data.frame(accidentados_por_dia_año)
  inicio=which( colnames(accidentados_por_dia_año)=="2009" )
  fin=which( colnames(accidentados_por_dia_año)=="2021" )
  accidentados_por_dia_año=accidentados_por_dia_año[,c(inicio:fin)]
  par(mfrow=c(2,4))
  aux.x=rownames(accidentados_por_dia_año)
  for(i.year in 1:length(colnames(accidentados_por_dia_año))){
    aux.y=accidentados_por_dia_año[,i.year]
    plot(aux.x, aux.y, main=i.year)
    abline(v=which.min(aux.y), col=4)
    abline(v=which.max(aux.y), col=2)
    lines(lowess(aux.x, aux.y, f=0.01),lty=3, lwd=1.2, col = "blue")
  }
}



#------------------------------------#

#CREAR FUNCION BASE DE DATOS UNICO FREC HORARIA ANUAL


poligono=function(data_frame,poligono){
  poligono=toupper(poligono)
  aux.poligono=subset(data_frame, ubicacion==poligono)
  return(aux.poligono)
}

poligono(delta_poligonos, "plaza")



#------------------------------------#


