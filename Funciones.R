#FUNCIONES#
#------------------------------------#

func.discrete.descrip=function(aux.df, aux.time, aux.class, FUN="mean"){
  
  aux.class=aux.df[[aux.name_classif]]
  aux.table=table(aux.time,aux.class)
  aux.summary=apply(aux.table,2,FUN,na.rm=T) 
  #barplot(aux.table)
  
  ##Significa que se ha comentado para evitar cargar varias veces el mismo gr?fico
  
  ##aux.table2=table(aux.class,aux.time)
  ##ylim.sup=max(aux.table2)*1.2
  ##aux.ylim=c(0,ylim.sup)
  #barplot(aux.table2, cex.names = 0.75) 
  #barplot(aux.table2,legend.text=c("Rural","Unallocated","Urban"), cex.names = 0.75) 
  ##barplot(aux.table2,beside=TRUE,ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=0.75), cex.names = 0.75) 
  #barplot(aux.table2, cex.names = 0.75)
  #legend("topleft", legend=c("Rural","Unallocated","Urban"),cex = 0.5,pch = 15,col=factor(aux.name_classif))
  #legend("topleft", legend=names(table(aux.class)),cex = 0.5,pch = 15)
  print(aux.summary)
  return(aux.summary)
}#End_function_1

func.discrete.descrip.quantile=function(aux.df, aux.time, aux.class, FUN="quantile"){
  
  
  aux.class=aux.df[[aux.name_classif]]
  aux.table=table(aux.time,aux.class)
  aux.summary=apply(aux.table,2,FUN,probs=0.9,na.rm=T) 
  #barplot(aux.table)
  aux.boxplot=as.data.frame.matrix(aux.table) 
  boxplot(aux.boxplot,cex.axis=2)
  dev.print(pdf, '4.0.pdf' ,  height=10, width=10 )
  
  aux.table2=table(aux.class,aux.time)
  ylim.sup=max(aux.table2)*1.2
  aux.ylim=c(0,ylim.sup)
  #barplot(aux.table2,legend.text=c("Rural","Unallocated","Urban"), cex.names = 0.75) 
  barplot(aux.table2,beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.25), cex.names = 2,cex.axis=2) 
  print(aux.summary)
  return(aux.summary)
}#End_function_2

func.summary=function(Modelo){
  
  aux.y=summary(Modelo)$coefficients
  aux.y=as.data.frame(aux.y)
  print(aux.y %>% arrange(desc(abs(aux.y[,3]))) %>% filter(aux.y[,4]<0.001))
  
  cat("\nAIC: ", summary(Modelo)$aic)
  cat("\nNull deviance:     ", summary(Modelo)$null.deviance, " on ", summary(Modelo)$df.null," degreees of freedom")
  cat("\nResidual deviance: ", summary(M5)$deviance, " on ", summary(Modelo)$df.residual," degreees of freedom")
  
}#End_function_3


efecto_hora=function(df){
  
  df$c1h=cos(2*pi*df$hour/24)
  df$s1h=sin(2*pi*df$hour/24)
  df$c2h=cos(4*pi*df$hour/24)
  df$s2h=sin(4*pi*df$hour/24)
  df$c3h=cos(6*pi*df$hour/24)
  df$s3h=sin(6*pi*df$hour/24)
  df$c4h=cos(8*pi*df$hour/24)
  df$s4h=sin(8*pi*df$hour/24)
  df$y=df$accidentes
  aux_glm= glm(y~c1h+s1h+c2h+s2h+c3h+s3h+c4h+s4h+day_of_week, data=df, family = "poisson")
  summary(aux_glm)
  aux.x=df$hour
  aux.y=summary(aux_glm)$coef[2,1]*df$c1h+
    summary(aux_glm)$coef[3,1]*df$s1h+
    summary(aux_glm)$coef[4,1]*df$c2h+
    summary(aux_glm)$coef[5,1]*df$s2h+
    summary(aux_glm)$coef[6,1]*df$c3h+
    summary(aux_glm)$coef[7,1]*df$s3h+
    summary(aux_glm)$coef[8,1]*df$c4h+
    summary(aux_glm)$coef[9,1]*df$s4h
  ciclico_hora=plot(aux.x, exp(aux.y), main="efecto hora",cex.axis=2.5)
  abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
}#End_function_4

accidentes_dia_semana=function(df){
  
  accidentados=df$accidentes
  hora=df$hour
  dia_semana=df$day_of_week
  accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
  accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
  accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
  accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
  colnames(accidentados_por_hora_dia)=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo")
  par(mfrow=c(2,4))
  aux.x=rownames(accidentados_por_hora_dia)
  for(i.day in 1:7){
    aux.y=accidentados_por_hora_dia[,i.day]
    plot(aux.x, aux.y, main=colnames(accidentados_por_hora_dia[i.day]),cex.axis=2.5,cex.main=2.5)
    abline(v=which.min(aux.y)-1, col=4)
    abline(v=which.max(aux.y)-1, col=2)
  }
}#End_function_5

accidentes_todos_los_dias_semana_color=function(df){
  
  accidentados=df$accidentes
  hora=df$hour
  dia_semana=df$day_of_week
  accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
  accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
  accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
  accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
  ylim.sup=max(accidentados_por_hora_dia[1:7], na.rm=T)*1.05
  par(mfrow=c(1,1))
  aux.x=rownames(accidentados_por_hora_dia)
  for(i.day in 1:7){
    aux.y=accidentados_por_hora_dia[,i.day]
    if(i.day==1) plot(aux.x, aux.y, ylim = c(0, ylim.sup),lwd=1.5,cex.axis=2,ylab = "Nº accidentes",xlab = "",cex.lab=1.5)
    lines(aux.x, aux.y, lty=i.day,col=i.day,lwd=1.5)
    #if(i.day==6|i.day==7)lines(aux.x, aux.y, lty=i.day, col=c("green"))
    abline(v=which.min(aux.y)-1, col=4, lty=i.day)
    abline(v=which.max(aux.y)-1, col=2, lty=i.day)
    cat(i.day, which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
    legend("topright", legend=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"), lty=1:7,col=1:7,lwd=3, cex=1)
  
  }
  
}#End_function_6

accidentes_todos_los_dias_semana=function(df){
  
  accidentados=df$accidentes
  hora=df$hour
  dia_semana=df$day_of_week
  accidentados_por_hora_dia=tapply(accidentados, list(hora, dia_semana),sum , na.rm=T)
  accidentados_por_hora_dia= as.data.frame(accidentados_por_hora_dia)
  accidentados_por_hora_dia=accidentados_por_hora_dia[,c(3,4,5,2,7,6,1)]
  accidentados_por_hora_dia$Total=rowSums(accidentados_por_hora_dia, na.rm=T)
  ylim.sup=max(accidentados_por_hora_dia[1:7], na.rm=T)*1.05
  par(mfrow=c(1,1))
  aux.x=rownames(accidentados_por_hora_dia)
  for(i.day in 1:7){
    aux.y=accidentados_por_hora_dia[,i.day]
    if(i.day==1) plot(aux.x, aux.y, ylim = c(0, ylim.sup),cex.axis=2,ylab = "Nº accidentes",xlab = "",cex.lab=1.5)
    lines(aux.x, aux.y, lty=i.day)
    if(i.day==6|i.day==7)lines(aux.x, aux.y, lty=i.day, col=c("green"))
    abline(v=which.min(aux.y)-1, col=4, lty=i.day)
    abline(v=which.max(aux.y)-1, col=2, lty=i.day)
    cat(i.day, which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
    
  }
  legend("topright", legend=c("Entre semana","Fin de semana"), col=c("black","green"), lwd=3,  cex=1.1)

}#End_function_6_extra

efecto_dia_año=function(df){
  
  df$c1d=cos(2*pi*df$day_year/365)
  df$s1d=sin(2*pi*df$day_year/365)
  df$c2d=cos(4*pi*df$day_year/365)
  df$s2d=sin(4*pi*df$day_year/365)
  df$c3d=cos(6*pi*df$day_year/365)
  df$s3d=sin(6*pi*df$day_year/365)
  df$c4d=cos(8*pi*df$day_year/365)
  df$s4d=sin(8*pi*df$day_year/365)
  df$y=df$accidentes
  aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d+day_of_week, data=df,family = "poisson")
  summary(aux_glm)
  aux.x=df$day_year
  aux.y=summary(aux_glm)$coef[2,1]*df$c1d+
    summary(aux_glm)$coef[3,1]*df$s1d+
    summary(aux_glm)$coef[4,1]*df$c2d+
    summary(aux_glm)$coef[5,1]*df$s2d+
    summary(aux_glm)$coef[6,1]*df$c3d+
    summary(aux_glm)$coef[7,1]*df$s3d+
    summary(aux_glm)$coef[8,1]*df$c4d+
    summary(aux_glm)$coef[9,1]*df$s4d
  plot(aux.x, exp(aux.y), main="",cex.axis=2.5)
  abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
  a=c(0,32,60,91,121,152,182,213,244,274,305,335)
  b=c(32,60,91,121,152,182,213,244,274,305,335,366)
  meses_plot=(a+b)/2
  axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
  
}#End_function_7


accidentes_año=function(df){
  
  par(mfrow=c(1,1))
  accidentados=df$accidentes
  dia=df$day_year
  accidentados_por_dia=tapply(accidentados, list(dia),sum)
  accidentados_por_dia= as.data.frame(accidentados_por_dia)
  aux.x=rownames(accidentados_por_dia)
  aux.y=accidentados_por_dia[,1]
  ylim.sup=max(accidentados_por_dia, na.rm=T)*1.05
  plot(aux.x, aux.y, ylim = c(0, ylim.sup),cex.axis=2.5)
  abline(v=which.min(aux.y)-1, col=4)
  abline(v=which.max(aux.y)-1, col=2)
  abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
  cat(which.min(aux.y)-1,which.max(aux.y)-1, sep="&", fill=T )
  abline(h=c(quantile(aux.y,probs=0.99),quantile(aux.y,probs=0.01)), lty=3, lwd=0.1, col=3)
  cat(quantile(aux.y,probs=0.01),quantile(aux.y,probs=0.99),mean(aux.y),sd(aux.y), sep="&", fill=T )
  abline(h=c(mean(aux.y)), lty=3, lwd=0.1, col=5)
  a=c(0,32,60,91,121,152,182,213,244,274,305,335)
  b=c(32,60,91,121,152,182,213,244,274,305,335,366)
  meses_plot=(a+b)/2
  axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
  
}#End_function_8


efecto_dia_semana=function(df,dias,año){
  
  aux.marcador=is.element(df$wday,dias)
  aux_day_week=yday(df$FECHAACCIDENTE[aux.marcador])
  aux_day_week=as.data.frame(aux_day_week)
  names(aux_day_week)="y_day"
  aux_day_week$y=df$accidentes[aux.marcador]
  aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
  aux_collapse=tapply(aux_day_week$y_day, aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse=as.data.frame(aux_collapse)
  names(aux_collapse)="y_day"
  aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$y_day/366, sum)
  aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$y_day/366, sum)
  aux_collapse=aux_collapse[aux_collapse$year==año,]
  aux_collapse$c1d=cos(2*pi*aux_collapse$y_day/365)
  aux_collapse$s1d=sin(2*pi*aux_collapse$y_day/365)
  aux_collapse$c2d=cos(4*pi*aux_collapse$y_day/365)
  aux_collapse$s2d=sin(4*pi*aux_collapse$y_day/365)
  aux_collapse$c3d=cos(6*pi*aux_collapse$y_day/365)
  aux_collapse$s3d=sin(6*pi*aux_collapse$y_day/365)
  aux_collapse$c4d=cos(8*pi*aux_collapse$y_day/365)
  aux_collapse$s4d=sin(8*pi*aux_collapse$y_day/365)
  aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse,family = "poisson")
  aux.x=aux_collapse$y_day
  aux.y=predict(aux_glm,type = "response")
  plot(aux.x, aux.y, main="",cex.axis=2.5)
  abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
  abline(h=mean(aux_collapse$y), col="red")
  lines(lowess(aux.x, aux.y, f=0.01),lty=3, lwd=1.2, col = "blue")
  a=c(0,32,60,91,121,152,182,213,244,274,305,335)
  b=c(32,60,91,121,152,182,213,244,274,305,335,366)
  meses_plot=(a+b)/2
  axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
  
}#End_function_9


efecto_dia_hora=function(df,hora,año){
  
  aux.marcador=is.element(df$hour,c(hora))
  aux_day_week=yday(df$FECHAACCIDENTE[aux.marcador])
  aux_day_week=as.data.frame(aux_day_week)
  names(aux_day_week)="y_day"
  aux_day_week$y=df$accidentes[aux.marcador]
  aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
  aux_collapse=tapply(aux_day_week$y_day, aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse=as.data.frame(aux_collapse)
  names(aux_collapse)="y_day"
  aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$y_day/366, mean)
  aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$y_day/366, sum)
  aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$y_day/366, sum)
  aux_collapse=aux_collapse[aux_collapse$year==año,]
  aux_collapse$c1d=cos(2*pi*aux_collapse$y_day/365)
  aux_collapse$s1d=sin(2*pi*aux_collapse$y_day/365)
  aux_collapse$c2d=cos(4*pi*aux_collapse$y_day/365)
  aux_collapse$s2d=sin(4*pi*aux_collapse$y_day/365)
  aux_collapse$c3d=cos(6*pi*aux_collapse$y_day/365)
  aux_collapse$s3d=sin(6*pi*aux_collapse$y_day/365)
  aux_collapse$c4d=cos(8*pi*aux_collapse$y_day/365)
  aux_collapse$s4d=sin(8*pi*aux_collapse$y_day/365)
  
  aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse, 
               family = "poisson")
  aux.x=aux_collapse$y_day
  aux.y=predict(aux_glm,type = "response")
  plot(aux.x, aux.y, main="",cex.axis=2.5)
  abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
  abline(h=mean(aux_collapse$y), col="red")
  lines(lowess(aux.x, aux.y, f=0.01),lty=3, lwd=1.2, col = "blue")  
  a=c(0,32,60,91,121,152,182,213,244,274,305,335)
  b=c(32,60,91,121,152,182,213,244,274,305,335,366)
  meses_plot=(a+b)/2
  axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
  
}#End_function_10

efecto_dia_semana_hora=function(df,hora.min,hora.max,año){
  ancho_graf=length(hora.min:hora.max)
  if(ancho_graf<=4){par(mfrow=c(1,ancho_graf))} else{par(mfrow=c(2,4))} 
  
  for(i.hour in hora.min:hora.max){
    aux.marcador=is.element(df$hour,c(i.hour))&(!is.na(df$hour))
    aux_day_week=wday(df$FECHAACCIDENTE[aux.marcador])
    aux_day_week=as.data.frame(aux_day_week)
    names(aux_day_week)="w_day"
    aux_day_week$y=df$accidentes[aux.marcador]
    aux_day_week$year=year(df$FECHAACCIDENTE[aux.marcador])
    aux_collapse=tapply(aux_day_week$w_day, aux_day_week$year+aux_day_week$w_day/7, mean)
    aux_collapse=as.data.frame(aux_collapse)
    names(aux_collapse)="w_day"
    aux_collapse$aux_day_week=tapply(wday(df$FECHAACCIDENTE[aux.marcador]), aux_day_week$year+aux_day_week$w_day/7, mean)
    aux_collapse$year=tapply(aux_day_week$year, aux_day_week$year+aux_day_week$w_day/7, mean)
    aux_collapse$y=tapply(aux_day_week$y, aux_day_week$year+aux_day_week$w_day/7, sum)
    aux_collapse$y.accidente=tapply(aux_day_week$y>0, aux_day_week$year+aux_day_week$w_day/7, sum)
    aux_collapse=aux_collapse[aux_collapse$year==año,]
    aux_collapse$c1d=cos(2*pi*aux_collapse$w_day/7)
    aux_collapse$s1d=sin(2*pi*aux_collapse$w_day/7)
    aux_collapse$c2d=cos(4*pi*aux_collapse$w_day/7)
    aux_collapse$s2d=sin(4*pi*aux_collapse$w_day/7)
    aux_collapse$c3d=cos(6*pi*aux_collapse$w_day/7)
    aux_collapse$s3d=sin(6*pi*aux_collapse$w_day/7)
    aux_collapse$c4d=cos(8*pi*aux_collapse$w_day/7)
    aux_collapse$s4d=sin(8*pi*aux_collapse$w_day/7)
    aux_glm= glm(y~c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d, data=aux_collapse, family = "poisson")
    aux.x=aux_collapse$w_day
    aux.y=predict(aux_glm,type = "response")
    plot(aux.x, aux.y,cex.axis=2.5)
  }
  mtext("Efecto dia de la semana - hora", side = 3, line = - 2, outer = TRUE)
  par(mfrow=c(1,1))  
}#End_function_11

obten_tabla=function(aux.tabla){
  aux.tabla=as.matrix(aux.tabla)
  fin=dim(aux.tabla)[1]
  aux.tabla=cbind(aux.tabla)
  cat("",colnames(aux.tabla),sep="&",fill=TRUE)
  for (i in 1:fin) {
    cat(rownames(aux.tabla)[i],aux.tabla[i,],sep="&",fill=TRUE)
  }
  cat(sep="\n")
}#End_function_12

obten_modelos=function(my_list){
  #cat(paste("Modelo"),paste(""),paste(""),paste("AIC"),sep="&",fill=TRUE)
  cat(paste("Modelo"),paste("Deviance"),paste("AIC"),sep="&",fill=TRUE)
  for(i in 1:length(my_list)) { 
    Modelo=my_list[[i]]
    #cat(paste("M", i, sep = ""),summary(Modelo)$df.null,summary(Modelo)$df.residual,round(summary(Modelo)$aic),sep="&",fill=TRUE)
    cat(paste("M", i, sep = ""),round(summary(Modelo)$deviance),round(summary(Modelo)$aic),sep="&",fill=TRUE)
  }
}#End_function_13

obten_coeff=function(modelo){
  cat(paste("Coeficiente"),paste("Estimate"),paste("Std. Error"),paste("z value"),paste("Pr(>|z|)"),paste(""),sep="&",fill=TRUE)
  aux.coeff=summary(modelo)$coeff
  fin=dim(aux.coeff)[1]
  aux.coeff=cbind(aux.coeff,exp(aux.coeff[,1]))
  for (i in 1:fin) {
    cat(rownames(aux.coeff)[i], round(aux.coeff[i,],digits=3),sep="&",fill=TRUE)
  }
}#End_function_14

obten_confint=function(modelo){
  cat(paste("Coeficiente"),paste("2.5 %"),paste("97.5 %"),sep="&",fill=TRUE)
  aux.conf=confint(modelo)
  fin=dim(aux.conf)[1]
  aux.conf=cbind(aux.conf,exp(aux.conf[,1]))
  for (i in 1:fin) {
    cat(rownames(aux.conf)[i], format(round(aux.conf[i,],digits=3), scientific=FALSE),sep="&",fill=TRUE)
  }
}#End_function_15

frame_poligono=function(data_frame, poligono, años){
  
  #PASOS PREVIOS A LOS MODELOS 2009-2021#
  poligono=as.character(poligono)
  poligono=toupper(poligono)
  delta_09_21=subset(data_frame, ubicacion==poligono)
  delta_09_21=subset(delta_09_21, year %in% años)
  
  
  y=delta_09_21$y
  hour=delta_09_21$hour
  # wday=delta_09_21$wday
  # month=delta_09_21$month
  day_year=delta_09_21$day_year
  
  # delta_09_21$dias_años=as.numeric(difftime(as.Date(delta_09_21$FECHAACCIDENTE), as.Date("2008-12-31"), unit="days"))
  # dias_años=delta_09_21$dias_años
  
  
  aux.yday.hour=(delta_09_21$year*100000+delta_09_21$day_year*100+delta_09_21$hour)
  #aux.yday.hour = round((delta_09_21$dias_años + (delta_09_21$hour-1)/24),6)
  respuesta=tapply(y, list(aux.yday.hour ), sum, na.rm=TRUE)
  respuesta=as.data.frame(respuesta)
  respuesta[is.na(respuesta)] = 0
  names(respuesta)=c('y')
  
  # respuesta$dias_años=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$hour=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$wday=tapply(wday, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$month=tapply(month, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$arm_dia=tapply(dias_años, list(aux.yday.hour), mean, na.rm=TRUE)
  # respuesta$arm_hora=tapply(hour, list(aux.yday.hour), mean, na.rm=TRUE)
  
  respuesta$lluvia=(tapply(delta_09_21$lluvia, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$itinere=tapply(delta_09_21$itinere, list(aux.yday.hour), sum, na.rm=TRUE)
  respuesta$alcance=(tapply(delta_09_21$alcance, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$ubicacion=c(poligono)
  respuesta$leve=(tapply(delta_09_21$GRADO==1, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  grave1=(tapply(delta_09_21$GRADO==2, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  grave2=(tapply(delta_09_21$GRADO==3, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  respuesta$grave=grave1+grave2
  respuesta$muertos=(tapply(delta_09_21$GRADO==4, list(aux.yday.hour), sum, na.rm=TRUE)>0)
  
  #respuesta$fin_de_semana=is.element(respuesta$wday,c(6,7))
  
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
  
  aux.poligono=respuesta
  return(aux.poligono)
}#End_function_16

terminos=function(modelo){
  aux.y=as.data.frame(summary(modelo)$coefficients)
  lista=(aux.y %>% filter(aux.y[,4]<0.001) %>% rownames())
  terminos=gsub(":","*",lista[2:length(lista)])
  terminos=as.data.frame(terminos)
  return(terminos)
}#End_function_17

terminos_frame=function(modelo){
  aux.y=as.data.frame(summary(modelo)$coefficients)
  lista=(aux.y %>% filter(aux.y[,4]<0.001) %>% rownames())
  terminos=gsub(":","*",lista[2:length(lista)])
  terminos=(terminos)
  return(terminos)
}#End_function_18

pie2=function (x, labels = names(x), edges = 200, radius = 1.6, clockwise = FALSE, 
               init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
               col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", 
        "lightcyan", "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      lines(c(1, 1.35) * P$x, c(1, 1.35) * P$y)
      text(1.5 * P$x, 1.5 * P$y, labels[i], xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}#End_function_19

plot_importancia=function(a){
  
  ggplot(a, aes(x=reorder(rownames(a),Overall), y=Overall)) +
    geom_point( color="blue", size=4, alpha=0.6)+
    geom_segment( aes(x=rownames(a), xend=rownames(a), y=0, yend=Overall), 
                  color='skyblue') +
    xlab('Variable')+
    ylab('Overall Importance')+
    theme_light() +
    coord_flip()+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
  
}#End_function_20


#------------------------------------#