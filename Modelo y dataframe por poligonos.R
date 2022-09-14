#MODELO Y DATAFRAME POR POLIGONOS#
#------------------------------------#


#poligono="plaza"
años=c(2009:2021)

aux.plaza=frame_poligono(delta_poligonos,"plaza",años)
aux.figueruelas=frame_poligono(delta_poligonos,"figueruelas",años)
aux.cuarte=frame_poligono(delta_poligonos,"cuarte",años)
aux.villanueva=frame_poligono(delta_poligonos,"villanueva",años)

aux.plaza$ubicacion="plaza"
aux.figueruelas$ubicacion="figueruelas"
aux.cuarte$ubicacion="cuarte"
aux.villanueva$ubicacion="villanueva"
aux.frame.poligonos=rbind(aux.plaza,aux.figueruelas,aux.cuarte,aux.villanueva)

aux.y=aux.frame.poligonos$ubicacion
aux.y=as.factor(aux.y)
levels(aux.y)=c(3,2,1,4)
#table(aux.frame.poligonos$ubicacion, aux.y)
aux.frame.poligonos$ubicacion=aux.y

respuesta_poligono=aux.frame.poligonos
respuesta_poligono$ubic_plaza=0+1*(aux.frame.poligonos$ubicacion==1)
respuesta_poligono$ubic_figueruelas=0+1*(aux.frame.poligonos$ubicacion==2)
respuesta_poligono$ubic_cuarte=0+1*(aux.frame.poligonos$ubicacion==3)
respuesta_poligono$ubic_villanueva=0+1*(aux.frame.poligonos$ubicacion==4)


#------------------------------------#
respuesta_poligono$dia_año=respuesta_poligono$day_year+respuesta_poligono$year*1000

subset_respuesta_poligono=subset(respuesta_poligono,respuesta_poligono$dia_año>0)

# prob_acc_poligono=tapply(subset_respuesta_poligono$y, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
# summary(prob_acc_poligono)

prob_leve_poligono=tapply(subset_respuesta_poligono$leve, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
prob_grave_poligono=tapply(subset_respuesta_poligono$grave, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)
prob_muerte_poligono=tapply(subset_respuesta_poligono$muertos, list(subset_respuesta_poligono$dia_año), sum, na.rm=TRUE)

p_leve_P=summary(prob_leve_poligono)[4]
p_grave_P=summary(prob_grave_poligono)[4]
p_muerte_P=summary(prob_muerte_poligono)[4]

cat(p_leve_P,p_grave_P,p_muerte_P,sep="&",fill=TRUE)

#------------------------------------#


#M_R0=step(glm(y~(c1h+s1h+c2h+s2h+c3h+s3h)+(c1d+s1d+c2d+s2d+c3d+s3d+c4d+s4d),data=respuesta_poligono, family = "poisson"))

#Modelos reducidos a apartir de M_R0

M_ar1=(glm(y~(c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + c1h*(c1d) ,data=respuesta_poligono, family = "poisson"))
M_ar2=(glm(y~(c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + c1h*(c1d) ,data=respuesta_poligono, family = "poisson"))
M_ar3=(glm(y~(c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d) ,data=respuesta_poligono, family = "poisson"))
M_ar4=(glm(y~(c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d) ,data=respuesta_poligono, family = "poisson"))
#M_R1=step(M_ar4)

#------------------------------------#

#Modelos reducidos con var categoricas. Modelo reducido armonicos + var categorica

M_R1=(M_ar4)
M_R_2=(glm( y ~ ubic_figueruelas+ubic_cuarte+ubic_villanueva+((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d)), family = "poisson", data = respuesta_poligono))
#M_R_ubic2=(glm( y ~ ubicacion+((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d)), family = "poisson", data = respuesta_poligono))
M_R_3=(glm( y ~ ubic_figueruelas+ubic_cuarte+ubic_villanueva+((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d))+ubic_figueruelas*((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d)), family = "poisson", data = respuesta_poligono))
M_R_4=(glm( y ~ ubic_figueruelas+ubic_cuarte+ubic_villanueva+((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d))+ubic_cuarte*((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d)), family = "poisson", data = respuesta_poligono))
M_R_5=(glm( y ~ ubic_figueruelas+ubic_cuarte+ubic_villanueva+((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d))+ubic_villanueva*((c1h + c2h + s2h + c3h + s3h + c1d + s3d + c4d + s4d) + (c1h + c2h + s2h)*(c1d)), family = "poisson", data = respuesta_poligono))

my_list_M_R=list(M_R1,M_R_2,M_R_3,M_R_4,M_R_5)
obten_modelos(my_list_M_R)

#------------------------------------#

ter_M_R_3=terminos(M_R_3)
ter_M_R_4=terminos(M_R_4)
ter_M_R_5=terminos(M_R_5)

terms_M_R_unicos=merge(ter_M_R_3,ter_M_R_4)
terms_M_R_unicos=merge(terms_M_R_unicos,ter_M_R_5)
#solo c3h y s3h, no es relevante

terms_M_R_todos=merge(ter_M_R_3,ter_M_R_4, all = TRUE)
terms_M_R_todos=merge(terms_M_R_todos,ter_M_R_5, all = TRUE)

a=terms_M_R_unicos$terminos
cat(a,sep="+",fill=TRUE)
a=terms_M_R_todos$terminos
cat(a,sep="+",fill=TRUE)

#M_R_unicos = LAS VARIABLES REPETIDAS DE LOS 3 MODELOS CON MENOR AIC

M_R_unicos=glm(y~(c1h+c2h+c3h+s3h+ubic_cuarte+ubic_figueruelas+ubic_villanueva),data=respuesta_poligono, family = "poisson",control = glm.control(maxit=100))

#M_R_todos = TODAS LAS VARIABLES DE LOS 3 MODELOS CON MENOR AIC

M_R_todos=glm(y~(c1d+c1h+c2h+c3h+s3d+s3h+ubic_cuarte+ubic_cuarte*s3h+ubic_figueruelas+ubic_figueruelas*c2h*c1d+ubic_figueruelas*c3h+ubic_figueruelas*c4d+ubic_figueruelas*s3d+ubic_figueruelas*s3h+ubic_villanueva),data=respuesta_poligono, family = "poisson",control = glm.control(maxit=100))

lista_final_R=list(M_R_unicos,M_R_todos)
obten_modelos(lista_final_R)

a=varImp(M_R_unicos, scale = FALSE)
a=varImp(M_R_todos, scale = FALSE)
#plot(a)

ggplot(a, aes(x=reorder(rownames(a),Overall), y=Overall)) +
  geom_point( color="blue", size=4, alpha=0.6)+
  geom_segment( aes(x=rownames(a), xend=rownames(a), y=0, yend=Overall), 
                color='skyblue') +
  xlab('Variable')+
  ylab('Overall Importance')+
  theme_light() +
  coord_flip()+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))

#dev.print(pdf, 'M_R_unicos.pdf' ,  height=10, width=10 )
#dev.print(pdf, 'M_R_todos.pdf' ,  height=10, width=10 )

obten_coeff(M_R_todos)
#confint(M_R_todos)
#obten_confint(M_R_todos)

plot_model(M_R_todos)+theme (axis.text.x = element_text(size=rel(2)),axis.text.y = element_text(size=rel(2)), axis.title = element_text(face="bold", size=rel(1.5)))
#dev.print(pdf, 'plot_model_M_R_todos.pdf' ,  height=10, width=10 )


#------------------------------------#

#             NIVEL DIARIO

#------------------------------------#

#INCLUIR INTERACCIÓN POLÍGONO

aux.x=respuesta_poligono$day_year
efect_0=summary(M_R_todos)$coef[1]
aux.y=0+0*respuesta_poligono$year+efect_0
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
  aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  #aux.y_c=sum(aux.y_c,na.rm=TRUE)
  aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
  aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  #aux.y_s=sum(aux.y_s,na.rm=TRUE)
  aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}
aux.y=aux.y
aux.y.plot_0=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas0=aux.y.plot_0

#CUARTE

efect_c=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_cuarte")])[1]
efect_c=aux.y+efect_c
efect_cm=mean(efect_c)
efect_c=tapply(efect_c, aux.x, mean)


#VILLANUEVA

efect_v=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_villanueva")])[1]
efect_v=aux.y+efect_v
efect_vm=mean(efect_v)
efect_v=tapply(efect_v, aux.x, mean)

#FIGUERUELAS

efect_1=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_figueruelas")])[1]
aux.y=aux.y+efect_1
efect_2=mean(aux.y)

for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas1=aux.y.plot

max=1.6*max(abs(exp(efect_0)),abs(exp(efect_2)))
min=1/1.6*min(abs(exp(efect_0)),abs(exp(efect_2)))
aux.ylab=TeX("\\hat{\\mu}")

# max=1.6*max(abs(exp(efect_0)/tr_PLAZA),abs(exp(efect_c)/tr_CUARTE),abs(exp(efect_v)/tr_VILLANUEVA),abs(exp(efect_2)/tr_FIGUERUELAS))
# min=1/1.6*min(abs(exp(efect_0)/tr_PLAZA),abs(exp(efect_c)/tr_CUARTE),abs(exp(efect_v)/tr_VILLANUEVA),abs(exp(efect_2)/tr_FIGUERUELAS))

#PLOT EFECTO 0
linea=5

plot(0:366, exp(aux.y.plot_0),ylim=c(min,max), type="n",main="",xlab = "día",ylab = "",cex.lab=1.5,cex.axis=1.4)
title(ylab=aux.ylab, line=2, cex.lab=1.75)
my_label=cut(0:366, c(0,32,60,91,121,152,182,213,244,274,305,335,366),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ), include.lowest=T)
a=c(0,32,60,91,121,152,182,213,244,274,305,335)
b=c(32,60,91,121,152,182,213,244,274,305,335,366)
meses_plot=(a+b)/2
axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
lines(0:366, exp(aux.y.plot_0), lwd=linea)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
abline(h=c(exp(efect_0)), lty=3, lwd=linea,col="black")

lines(0:366, exp(efect_c),col="blue", lwd=linea)
abline(h=c(exp(efect_cm)), lty=3, lwd=linea,col="blue")
lines(0:366, exp(efect_v),col="darkgreen", lwd=linea)
abline(h=c(exp(efect_vm)), lty=3, lwd=linea,col="darkgreen")


#PLOT EFECTO 1
#plot(0:366, exp(aux.y.plot), type="n",main="",xlab = "día",ylab = "effect",cex.lab=1.5,cex.axis=2)
lines(0:366, exp(aux.y.plot),col="red", lwd=linea)
abline(h=c(exp(efect_2)), lty=3, lwd=linea,col="red")
legend("top", ncol=2 ,legend=c("PLAZA", "FIGUERUELAS","CUARTE","VILLANUEVA"),text.font=2, lty=1,lwd=linea, col=c("black","red","blue","darkgreen" ),  cex=1.1)
axis(4, cex.axis=1, at=c(exp(efect_0),exp(efect_2),exp(efect_cm),exp(efect_vm)),labels = c( round(exp(efect_0),3) , round(exp(efect_2),3), round(exp(efect_cm),3), round(exp(efect_vm),3)))

max_0=max(exp(aux.y.plot.figueruelas0))
min_0=min(exp(aux.y.plot.figueruelas0))
abline(h=c(max_0,min_0), lty=4, lwd=1,col="black")

max_1=max(exp(aux.y.plot.figueruelas1))
min_1=min(exp(aux.y.plot.figueruelas1))
abline(h=c(max_1,min_1), lty=4, lwd=1,col="black")

max_c=max(exp(efect_c))
min_c=min(exp(efect_c))
abline(h=c(max_c,min_c), lty=4, lwd=1,col="black")

max_v=max(exp(efect_v))
min_v=min(exp(efect_v))
abline(h=c(max_v,min_v), lty=4, lwd=1,col="black")

#dev.print(pdf, 'poligonos_diario.pdf' ,  height=10, width=10 )

figueruelas_0_1=exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0)
sum_figueruelas_0_1=summary(figueruelas_0_1)

xmin=floor(sum_figueruelas_0_1[1])
xmax=ceiling(sum_figueruelas_0_1[6])
hist(exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0),xlim = c(xmin,xmax))

#------------------------------------#

          #CON TRABAJADORES

#------------------------------------#

#INCLUIR INTERACCIÓN  POLÍGONO CON TRABAJADORES

tr_PLAZA=tabla_2017_2019[4,1]/1000
tr_FIGUERUELAS=tabla_2017_2019[4,2]/1000
tr_CUARTE=tabla_2017_2019[4,3]/1000
tr_VILLANUEVA=tabla_2017_2019[4,4]/1000

aux.x=respuesta_poligono$day_year
efect_0=summary(M_R_todos)$coef[1]
aux.y=0+0*respuesta_poligono$year+efect_0
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot_0=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas0=aux.y.plot_0

#CUARTE

efect_c=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_cuarte")])[1]
efect_c=aux.y+efect_c
efect_cm=mean(efect_c)
efect_c=tapply(efect_c, aux.x, mean)


#VILLANUEVA

efect_v=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_villanueva")])[1]
efect_v=aux.y+efect_v
efect_vm=mean(efect_v)
efect_v=tapply(efect_v, aux.x, mean)

#FIGUERUELAS

efect_1=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_figueruelas")])[1]
aux.y=aux.y+efect_1
efect_2=mean(aux.y)

for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"d",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas1=aux.y.plot

# max=1.6*max(abs(exp(efect_0)),abs(exp(efect_2)))
# min=1/1.6*min(abs(exp(efect_0)),abs(exp(efect_2)))

max=1.6*max(abs(exp(efect_0)/tr_PLAZA),abs(exp(efect_cm)/tr_CUARTE),abs(exp(efect_vm)/tr_VILLANUEVA),abs(exp(efect_2)/tr_FIGUERUELAS))
min=1/1.6*min(abs(exp(efect_0)/tr_PLAZA),abs(exp(efect_cm)/tr_CUARTE),abs(exp(efect_vm)/tr_VILLANUEVA),abs(exp(efect_2)/tr_FIGUERUELAS))

aux.ylab1=TeX("\\hat{\\lambda}")

#PLOT EFECTO 0
linea=5

plot(0:366, exp(aux.y.plot_0/tr_PLAZA),ylim=c(min,max), type="n",main="",xlab = "día",ylab = "",cex.lab=1.5,cex.axis=1.4)
title(ylab=aux.ylab1, line=2, cex.lab=1.75)
my_label=cut(0:366, c(0,32,60,91,121,152,182,213,244,274,305,335,366),labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ), include.lowest=T)
a=c(0,32,60,91,121,152,182,213,244,274,305,335)
b=c(32,60,91,121,152,182,213,244,274,305,335,366)
meses_plot=(a+b)/2
axis(3,at=meses_plot, cex.axis=1.5,labels=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic" ))
lines(0:366, exp(aux.y.plot_0)/tr_PLAZA, lwd=linea)
abline(v=c(32,60,91,121,152,182,213,244,274,305,335), lty=3, lwd=0.1)
abline(h=c(exp(efect_0)/tr_PLAZA), lty=3, lwd=linea,col="black")

lines(0:366, exp(efect_c)/tr_CUARTE,col="blue", lwd=linea)
abline(h=c(exp(efect_cm)/tr_CUARTE), lty=3, lwd=linea,col="blue")
lines(0:366, exp(efect_v)/tr_VILLANUEVA,col="darkgreen", lwd=linea)
abline(h=c(exp(efect_vm)/tr_VILLANUEVA), lty=3, lwd=linea,col="darkgreen")


#PLOT EFECTO 1
#plot(0:366, exp(aux.y.plot), type="n",main="",xlab = "día",ylab = "effect",cex.lab=1.5,cex.axis=2)
lines(0:366, exp(aux.y.plot)/tr_FIGUERUELAS,col="red", lwd=linea)
abline(h=c(exp(efect_2)/tr_FIGUERUELAS), lty=3, lwd=2,col="red")
legend("top", ncol=2 ,legend=c("PLAZA", "FIGUERUELAS","CUARTE","VILLANUEVA"),text.font=2, lty=1,lwd=linea, col=c("black","red","blue","darkgreen" ),  cex=1.1)
axis(4, cex.axis=1, at=c(exp(efect_0)/tr_PLAZA,exp(efect_2)/tr_FIGUERUELAS,exp(efect_cm)/tr_CUARTE,exp(efect_vm)/tr_VILLANUEVA),labels = c( round(exp(efect_0)/tr_PLAZA,4) , round(exp(efect_2)/tr_FIGUERUELAS,4), round(exp(efect_cm)/tr_CUARTE,4), round(exp(efect_vm)/tr_VILLANUEVA,4)))

max_0=max(exp(aux.y.plot.figueruelas0)/tr_PLAZA)
min_0=min(exp(aux.y.plot.figueruelas0)/tr_PLAZA)
abline(h=c(max_0,min_0), lty=4, lwd=1,col="black")

max_1=max(exp(aux.y.plot.figueruelas1)/tr_FIGUERUELAS)
min_1=min(exp(aux.y.plot.figueruelas1)/tr_FIGUERUELAS)
abline(h=c(max_1,min_1), lty=4, lwd=1,col="black")

max_c=max(exp(efect_c)/tr_CUARTE)
min_c=min(exp(efect_c)/tr_CUARTE)
abline(h=c(max_c,min_c), lty=4, lwd=1,col="black")

max_v=max(exp(efect_v)/tr_VILLANUEVA)
min_v=min(exp(efect_v)/tr_VILLANUEVA)
abline(h=c(max_v,min_v), lty=4, lwd=1,col="black")

#dev.print(pdf, 'poligonos_diario_trabaj.pdf' ,  height=10, width=10 )

figueruelas_0_1=exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0)
sum_figueruelas_0_1=summary(figueruelas_0_1)

xmin=floor(sum_figueruelas_0_1[1])
xmax=ceiling(sum_figueruelas_0_1[6])
hist(exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0),xlim = c(xmin,xmax))

#------------------------------------#

#             NIVEL HORARIO

#------------------------------------#

#INCLUIR INTERACCIÓN POLÍGONO

aux.x=respuesta_poligono$hour
efect_0=summary(M_R_todos)$coef[1]
aux.y=0+0*respuesta_poligono$year+efect_0
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}
aux.y=aux.y
aux.y.plot_0=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas0=aux.y.plot_0

#CUARTE

efect_c=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_cuarte")])[1]
efect_c=aux.y+efect_c
efect_cm=mean(efect_c)
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_cuarte"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    efect_c=efect_c+aux.y_c}
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_cuarte"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    efect_c=efect_c+aux.y_s}
}
efect_c=tapply(efect_c, aux.x, mean)


#VILLANUEVA

efect_v=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_villanueva")])[1]
efect_v=aux.y+efect_v
efect_vm=mean(efect_v)
efect_v=tapply(efect_v, aux.x, mean)

#FIGUERUELAS

efect_1=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_figueruelas")])[1]
aux.y=aux.y+efect_1
efect_2=mean(aux.y)

for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas1=aux.y.plot

max=1.6*max(abs(exp(aux.y.plot_0)),abs(exp(efect_c)),abs(exp(efect_v)),abs(exp(aux.y.plot)))
min=1/1.6*min(abs(exp(aux.y.plot_0)),abs(exp(efect_c)),abs(exp(efect_v)),abs(exp(aux.y.plot)))
aux.ylab=TeX("\\hat{\\mu}")

#PLOT EFECTO 0
linea=5

plot(0:23, exp(aux.y.plot_0),ylim=c(min,max), type="n",main="",xlab = "día",ylab = "",cex.lab=1.5,cex.axis=1.4)
title(ylab=aux.ylab, line=2, cex.lab=1.75)
lines(0:23, exp(aux.y.plot_0), lwd=linea)
abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
axis(3,at=0:23, cex.axis=1.1,labels=c(0:23))
abline(h=c(exp(efect_0)), lty=3, lwd=linea,col="black")

lines(0:23, exp(efect_c),col="blue", lwd=linea)
abline(h=c(exp(efect_cm)), lty=3, lwd=linea,col="blue")
lines(0:23, exp(efect_v),col="darkgreen", lwd=linea)
abline(h=c(exp(efect_vm)), lty=3, lwd=linea,col="darkgreen")


#PLOT EFECTO 1
#plot(0:23, exp(aux.y.plot), type="n",main="",xlab = "día",ylab = "effect",cex.lab=1.5,cex.axis=2)
lines(0:23, exp(aux.y.plot),col="red", lwd=linea)
abline(h=c(exp(efect_2)), lty=3, lwd=linea,col="red")
legend("top", ncol=2 ,legend=c("PLAZA", "FIGUERUELAS","CUARTE","VILLANUEVA"),text.font=2, lty=1,lwd=linea, col=c("black","red","blue","darkgreen" ),  cex=1.1)
axis(4, cex.axis=1, at=c(exp(efect_0),exp(efect_2),exp(efect_cm),exp(efect_vm)),labels = c( round(exp(efect_0),3) , round(exp(efect_2),3), round(exp(efect_cm),3), round(exp(efect_vm),3)))

max_0=max(exp(aux.y.plot.figueruelas0))
min_0=min(exp(aux.y.plot.figueruelas0))
abline(h=c(max_0,min_0), lty=4, lwd=1,col="black")

max_1=max(exp(aux.y.plot.figueruelas1))
min_1=min(exp(aux.y.plot.figueruelas1))
abline(h=c(max_1,min_1), lty=4, lwd=1,col="black")

max_c=max(exp(efect_c))
min_c=min(exp(efect_c))
abline(h=c(max_c,min_c), lty=4, lwd=1,col="black")

max_v=max(exp(efect_v))
min_v=min(exp(efect_v))
abline(h=c(max_v,min_v), lty=4, lwd=1,col="black")

#dev.print(pdf, 'poligonos_horario.pdf' ,  height=10, width=10 )

cuarte_0_1=exp(efect_c)/exp(aux.y.plot.figueruelas0)
sum_cuarte_0_1=summary(cuarte_0_1)

xmin=floor(sum_cuarte_0_1[1])
xmax=ceiling(sum_cuarte_0_1[6])
hist(exp(efect_c)/exp(aux.y.plot.figueruelas0),xlim = c(xmin,xmax))



#------------------------------------#

#CON TRABAJADORES

#------------------------------------#

#INCLUIR INTERACCIÓN  POLÍGONO CON TRABAJADORES

tr_PLAZA=tabla_2017_2019[4,1]/1000
tr_FIGUERUELAS=tabla_2017_2019[4,2]/1000
tr_CUARTE=tabla_2017_2019[4,3]/1000
tr_VILLANUEVA=tabla_2017_2019[4,4]/1000

aux.x=respuesta_poligono$hour
efect_0=summary(M_R_todos)$coef[1]
aux.y=0+0*respuesta_poligono$year+efect_0
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot_0=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas0=aux.y.plot_0

#CUARTE

efect_c=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_cuarte")])[1]
efect_c=aux.y+efect_c
efect_cm=mean(efect_c)
for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_cuarte"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    efect_c=efect_c+aux.y_c}
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_cuarte"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    efect_c=efect_c+aux.y_s}
}
efect_c=tapply(efect_c, aux.x, mean)


#VILLANUEVA

efect_v=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_villanueva")])[1]
efect_v=aux.y+efect_v
efect_vm=mean(efect_v)
efect_v=tapply(efect_v, aux.x, mean)

#FIGUERUELAS

efect_1=(summary(M_R_todos)$coef[is.element(rownames(summary(M_R_todos)$coef),"ubic_figueruelas")])[1]
aux.y=aux.y+efect_1
efect_2=mean(aux.y)

for (i.coeff in 1:4){
  aux.variable=paste("c",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_c=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_c=sum(aux.y_c,na.rm=TRUE)
    aux.y=aux.y+aux.y_c}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
  aux.variable=paste("s",i.coeff,"h",sep = "",collapse = "")
  aux.position.armonics=grepl(c(aux.variable),rownames(summary(M_R_todos)$coef))&grepl(c("ubic_figueruelas"),rownames(summary(M_R_todos)$coef))
  if(sum(aux.position.armonics)>0){
    aux.y_s=(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
    #aux.y_s=sum(aux.y_s,na.rm=TRUE)
    aux.y=aux.y+aux.y_s}
  #aux.y=aux.y+(summary(M_R_todos)$coef[aux.position.armonics,1])[1]*respuesta_poligono[[aux.variable]]
}

aux.y.plot=tapply(aux.y, aux.x, mean)
aux.y.plot.figueruelas1=aux.y.plot

max=1.6*max(abs(exp(aux.y.plot_0)/tr_PLAZA),abs(exp(efect_c)/tr_CUARTE),abs(exp(efect_v)/tr_VILLANUEVA),abs(exp(aux.y.plot)/tr_FIGUERUELAS))
min=1/1.6*min(abs(exp(aux.y.plot_0)/tr_PLAZA),abs(exp(efect_c)/tr_CUARTE),abs(exp(efect_v)/tr_VILLANUEVA),abs(exp(aux.y.plot)/tr_FIGUERUELAS))
aux.ylab1=TeX("\\hat{\\lambda}")

#PLOT EFECTO 0
linea=5

plot(0:23, exp(aux.y.plot_0/tr_PLAZA),ylim=c(min,max), type="n",main="",xlab = "día",ylab = "",cex.lab=1.5,cex.axis=1.4)
title(ylab=aux.ylab1, line=2, cex.lab=1.75)
lines(0:23, exp(aux.y.plot_0)/tr_PLAZA, lwd=linea)
abline(v=c(3,6,9,12,15,18,21), lty=3, lwd=0.1)
axis(3,at=0:23, cex.axis=1.1,labels=c(0:23))
abline(h=c(exp(efect_0)/tr_PLAZA), lty=3, lwd=linea,col="black")

lines(0:23, exp(efect_c)/tr_CUARTE,col="blue", lwd=linea)
abline(h=c(exp(efect_cm)/tr_CUARTE), lty=3, lwd=linea,col="blue")
lines(0:23, exp(efect_v)/tr_VILLANUEVA,col="darkgreen", lwd=linea)
abline(h=c(exp(efect_vm)/tr_VILLANUEVA), lty=3, lwd=linea,col="darkgreen")


#PLOT EFECTO 1
#plot(0:23, exp(aux.y.plot), type="n",main="",xlab = "día",ylab = "effect",cex.lab=1.5,cex.axis=2)
lines(0:23, exp(aux.y.plot)/tr_FIGUERUELAS,col="red", lwd=linea)
abline(h=c(exp(efect_2)/tr_FIGUERUELAS), lty=3, lwd=2,col="red")
legend("top", ncol=2 ,legend=c("PLAZA", "FIGUERUELAS","CUARTE","VILLANUEVA"),text.font=2, lty=1,lwd=linea, col=c("black","red","blue","darkgreen" ),  cex=1.1)
axis(4, cex.axis=1, at=c(exp(efect_0)/tr_PLAZA,exp(efect_2)/tr_FIGUERUELAS,exp(efect_cm)/tr_CUARTE,exp(efect_vm)/tr_VILLANUEVA),labels = c( round(exp(efect_0)/tr_PLAZA,3) , round(exp(efect_2)/tr_FIGUERUELAS,3), round(exp(efect_cm)/tr_CUARTE,3), round(exp(efect_vm)/tr_VILLANUEVA,3)))

max_0=max(exp(aux.y.plot.figueruelas0)/tr_PLAZA)
min_0=min(exp(aux.y.plot.figueruelas0)/tr_PLAZA)
abline(h=c(max_0,min_0), lty=4, lwd=1,col="black")

max_1=max(exp(aux.y.plot.figueruelas1)/tr_FIGUERUELAS)
min_1=min(exp(aux.y.plot.figueruelas1)/tr_FIGUERUELAS)
abline(h=c(max_1,min_1), lty=4, lwd=1,col="black")

max_c=max(exp(efect_c)/tr_CUARTE)
min_c=min(exp(efect_c)/tr_CUARTE)
abline(h=c(max_c,min_c), lty=4, lwd=1,col="black")

max_v=max(exp(efect_v)/tr_VILLANUEVA)
min_v=min(exp(efect_v)/tr_VILLANUEVA)
abline(h=c(max_v,min_v), lty=4, lwd=1,col="black")

#dev.print(pdf, 'poligonos_horario_trabaj.pdf' ,  height=10, width=10 )

figueruelas_0_1=exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0)
sum_figueruelas_0_1=summary(figueruelas_0_1)

xmin=floor(sum_figueruelas_0_1[1])
xmax=ceiling(sum_figueruelas_0_1[6])
hist(exp(aux.y.plot.figueruelas1)/exp(aux.y.plot.figueruelas0),xlim = c(xmin,xmax))

