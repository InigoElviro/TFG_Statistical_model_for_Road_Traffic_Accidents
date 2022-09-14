#EXPLORATORIO#
#------------------------------------#


par(mfrow=c(1,1))

aux.df=delta
aux.time=delta$month

aux.name_classif="lluvia"
aux.mean=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
aux.sd=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="sd")
aux.quantile=func.discrete.descrip.quantile(aux.df, aux.time, aux.name_classif,FUN="quantile")
#dev.print(pdf, '4.01.pdf' ,  height=10, width=10 )
#plot(aux.mean,aux.sd^2, type = "b", lty = 1)
#lines(1,1)

aux.name_classif="itinere"
aux.mean=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
aux.sd=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="sd")
aux.quantile=func.discrete.descrip.quantile(aux.df, aux.time, aux.name_classif,FUN="quantile")
#dev.print(pdf, '4.02.pdf' ,  height=10, width=10 )

aux.name_classif="alcance"
aux.mean=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="mean")
aux.sd=func.discrete.descrip(aux.df, aux.time, aux.name_classif, FUN="sd")
aux.quantile=func.discrete.descrip.quantile(aux.df, aux.time, aux.name_classif,FUN="quantile")
#dev.print(pdf, '4.03.pdf' ,  height=10, width=10 )


aux.time2=delta$day_year
aux.name_classif="lluvia"
aux.name_classif="itinere"
aux.name_classif="alcance"

aux.df$new.factor=interaction(aux.df$month, aux.df$itinere,is.element(aux.df$wday,c(1,7)))
aux.name_classif="new.factor"

mean_and_sd2(aux.df, aux.time2, aux.name_classif)
dev.print(pdf, '4.0.pdf' ,  height=10, width=10 )
mean_VS_sd2(aux.df, aux.time, aux.name_classif)
dev.print(pdf, '4.0.pdf' ,  height=10, width=10 )

mean_and_sd2=function(aux.df, aux.time2, aux.name_classif){
  aux.mean.plot=func.discrete.descrip(aux.df, aux.time2, aux.name_classif, FUN="mean")
  aux.sd.plot=func.discrete.descrip(aux.df, aux.time2, aux.name_classif, FUN="sd")
  aux.ylim=c(0,max(aux.sd.plot^2, aux.mean.plot))
  aux.x=1:length(table(aux.df[[as.name(aux.name_classif)]]))
  plot(aux.x, aux.mean.plot, col=3, ylim = aux.ylim,cex.axis=2,lwd = 2)
  points(aux.x, aux.mean.plot, cex=1, pch=16)
  lines(aux.x, aux.sd.plot^2, col=2,lwd = 2)
  abline(h=aux.mean.plot[1],lwd = 2)

}

mean_VS_sd2=function(aux.df, aux.time2, aux.name_classif){
  aux.mean.plot=func.discrete.descrip(aux.df, aux.time2, aux.name_classif, FUN="mean")
  aux.sd.plot=func.discrete.descrip(aux.df, aux.time2, aux.name_classif, FUN="sd")
  aux.xlim=c(0,max(aux.mean.plot))
  aux.ylim=c(0,max(aux.sd.plot^2))
  plot(aux.mean.plot, aux.sd.plot^2,ylim = aux.ylim, xlim = aux.xlim,cex.axis=2,lwd = 2)
  abline(c(0,1), lty=2,lwd = 2)
  abline(lm(aux.sd.plot^2 ~ aux.mean.plot),lwd = 2)

}

#------------------------------------#


plot_mean_VS_sd2=function(aux.df, aux.time){
  mn_discrete=tapply(aux.df, list(aux.time), mean, na.rm=TRUE)
  sd_discrete=tapply(aux.df, list(aux.time), sd, na.rm=TRUE)
  aux.xlim=c(0,max(mn_discrete))
  aux.ylim=c(0,max(sd_discrete^2))
  plot(mn_discrete, sd_discrete^2,ylim = aux.ylim, xlim = aux.xlim,cex.axis=2,lwd = 2)
  abline(c(0,1), lty=2,lwd = 2)
  abline(lm(sd_discrete^2 ~ mn_discrete),lwd = 2)
}

aux.df=delta$lluvia
# aux.df=delta$itinere
# aux.df=delta$alcance

aux.time=delta$month
# aux.time=delta$day_year

plot_mean_VS_sd2(aux.df,aux.time)

años=c(2009:2021)
aux.df=subset(delta, year %in% años)
aux.time=aux.df$year*1000+aux.df$day_year
aux.number.acc=tapply(aux.df$year>0, aux.time, sum)
aux.group=tapply(1000*aux.df$year+round(aux.df$day_year/15,digits=0), aux.time, mean)
#aux.group=tapply(1000*aux.df$year+round(aux.df$day_year/7,digits=0), aux.time, mean)
aux.y=tapply(aux.number.acc,aux.group,sd)^2
aux.x=tapply(aux.number.acc,aux.group,mean)
aux.ylim=c(0,30)
plot(aux.x,aux.y, ylim = aux.ylim,xlab = "", ylab = "",  cex.axis=2.5,cex.lab=3)
title(xlab = "E(y)", ylab = "Var(y)",line=3.25, cex.lab=2.75)
abline(c(0,1))
#abline(lm(aux.y ~ aux.x),lwd = 2)
lines(lowess(aux.y ~ aux.x),lwd = 2,col="red")
par(mfrow=c(1,1))


#dev.print(pdf, 'mean_vs_sd_15.pdf' ,  height=10, width=10 )
#dev.print(pdf, 'mean_vs_sd_07.pdf' ,  height=10, width=10 )

#par(mar=c(5.1,5.25,4.1,2.1))
par(mar=c(5.1,4.1,4.1,2.1))

# plot2_mean_VS_sd2=function(aux.df,aux.classif, aux.time){
#   mn_discrete=tapply(aux.df, list(aux.classif,aux.time), mean, na.rm=TRUE)
#   sd_discrete=tapply(aux.df, list(aux.classif,aux.time), sd, na.rm=TRUE)
#   aux.xlim=c(0,max(mn_discrete))
#   aux.ylim=c(0,max(sd_discrete^2))
#   plot(mn_discrete, sd_discrete^2,ylim = aux.ylim, xlim = aux.xlim,cex.axis=2,lwd = 2)
#   abline(c(0,1), lty=2,lwd = 2)
#   abline(lm(sd_discrete^2 ~ mn_discrete),lwd = 2)
# }
# 
# aux.df=delta
# 
# aux.classif=delta$lluvia
# aux.classif=delta$itinere
# aux.classif=delta$alcance
# 
# aux.time=delta$month
# aux.time=delta$day_year
# 
# plot2_mean_VS_sd2(aux.df,aux.classif,aux.time)

#------------------------------------#

#ANALISIS TEMPORAL

#------------------------------------#


#TASA PROMEDIO POR HORA

efecto_hora(delta)
#dev.print(pdf, '4.04.pdf' ,  height=10, width=10 )

#------------------------------------#

#Para explorar hora con max y min incidencia

#1 Grafica por cada dia de la semana


accidentes_dia_semana(delta)
#dev.print(pdf, '4.05.pdf' ,  height=10, width=10 )
par(mfrow=c(1,1))

#1 Grafica para todos los dias de la semana

accidentes_todos_los_dias_semana_color(delta)
#dev.print(pdf, '4.06.pdf' ,  height=10, width=10 )

accidentes_todos_los_dias_semana(delta)
#dev.print(pdf, '4.06extra.pdf' ,  height=10, width=10 )



#------------------------------------#


#Analisis ciclicidad de los dias del a?o

efecto_dia_año(delta)
#dev.print(pdf, '4.07.pdf' ,  height=10, width=10 )

accidentes_año(delta)
#dev.print(pdf, '4.08.pdf' ,  height=10, width=10 )

efecto_dia_semana(delta,c(1,7),2021)
#dev.print(pdf, '4.09.pdf' ,  height=10, width=10 )

efecto_dia_hora(delta,8,2021)
#dev.print(pdf, '4.10.pdf' ,  height=10, width=10 )

efecto_dia_semana_hora(delta,8,14,2021)
#dev.print(pdf, '4.11.pdf' ,  height=10, width=10 )



#------------------------------------#
