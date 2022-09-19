#INFORME ISSLA#
#------------------------------------#

#1º Selecciono solo los poligonos y años 2017-2019
accidentes_en_poligonos=subset(delta_poligonos, !is.na(delta_poligonos$ubicacion))
accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos, year==2017|year==2018|year==2019)

#2º Obtengo la media de trabajadores por empresa o el dato más reciente

aux.acc=accidentes_en_poligonos_17_19 %>% group_by(CIF_EMPRESA)  %>% mutate(nº_trabajadores=mean(PLANTILLAE))
aux.acc$nº_trabajadores=as.integer(aux.acc$nº_trabajadores)
aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_reciente=PLANTILLAE[which.max(FECHAACCIDENTE)])
aux.acc$plantilla_reciente=as.integer(aux.acc$plantilla_reciente)
aux.acc=aux.acc %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT=PLANTILLACT[which.max(FECHAACCIDENTE)])
aux.acc$plantilla_ACT=as.integer(aux.acc$plantilla_ACT)

# aux.acc=aux.acc  %>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT2=PLANTILLACT[which.max(1:n())] )
# aux.acc$plantilla_ACT2=as.integer(aux.acc$plantilla_ACT2)

aux.acc=aux.acc%>% group_by(CIF_EMPRESA) %>% mutate(plantilla_ACT_reciente=mean(PLANTILLACT))
aux.acc$plantilla_ACT_reciente=as.integer(aux.acc$plantilla_ACT_reciente)

#3º Elimino accidentes repetidos y autonomos

accidentes_en_poligonos_17_19=(aux.acc[!duplicated(aux.acc$IPF_MD5,aux.acc$FECHAACCIDENTE),])
accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos_17_19, SITUACION==1|SITUACION==2)

#4º Selecciono solo "in itinere"
accidentes_en_poligonos_17_19_itinere=subset(accidentes_en_poligonos_17_19, itinere==TRUE)


#------------------------------------#
#par(mfrow=c(1,2))
par(mfrow=c(1,1))
0.75
tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$year)
ylim.sup=max(tabla_accidentes)*1.2
aux.ylim=c(0,ylim.sup)
titulo="TOTAL ALT POR AÑOS"
bp=barplot(tabla_accidentes, beside=TRUE,  yaxt = "n", ylim=aux.ylim, main=titulo, cex.main=2.5,cex.names = 2, col=rgb(0.18,0.36,0.58,1.0) )
abline(h=seq(1, ylim.sup, by=20), col="gray")
bp=barplot(tabla_accidentes, beside=TRUE,add=TRUE,  yaxt = "n", ylim=aux.ylim, main=titulo, cex.main=2.5,cex.names = 2, col=rgb(0.18,0.36,0.58,1.0) )
axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=1.5, col = NA)
text(bp, as.matrix(tabla_accidentes), round(as.matrix(tabla_accidentes), 1),cex=2,pos=3)
#dev.print(pdf, 'Informe_3.01_TOTAL_ALT_POR_AÑOS.pdf' ,  height=10, width=10 )

tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$ubicacion)
etiquetas = paste0(names(tabla_accidentes), "\n", as.numeric(tabla_accidentes), "\n",round(100 * tabla_accidentes/sum(tabla_accidentes), 2), "%")
pie2(tabla_accidentes,labels = etiquetas, main="TOTAL POR UBICACION", cex.main=2.5, cex=2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
#dev.print(pdf, 'Informe_3.02_TOTAL_POR_UBICACION.pdf' ,  height=10, width=10 )

itinere_poligonos=table(accidentes_en_poligonos_17_19_itinere$year,accidentes_en_poligonos_17_19_itinere$ubicacion)
ylim.sup=max(itinere_poligonos)*1.2
aux.ylim=c(0,ylim.sup)
titulo="TOTAL ALT POR ZONA Y AÑO"
itinere_poligonos=itinere_poligonos[,c(1,3,2,4)]
bp=barplot(itinere_poligonos, beside=TRUE,yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.75, col = c("steelblue","brown2", "olivedrab3"))
axis(2, at = seq(0, ylim.sup, by=10), las = 1, cex.axis=0.75, col = NA)
abline(h=seq(1, ylim.sup, by=10), col="gray")
bp=barplot(itinere_poligonos, beside=TRUE,add=TRUE,yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.75, col = c("steelblue","brown2", "olivedrab3"))
text(bp, as.matrix(itinere_poligonos), round(as.matrix(itinere_poligonos), 1),font=2,cex=1.5,pos=3)
itinere_poligonos=itinere_poligonos[,c(1,3,2,4)]
#dev.print(pdf, 'Informe_3.03_TOTAL_POR_UBICACION.pdf' ,  height=10, width=10 )

#------------------------------------#

#Elimino las empresas repetidas

subset_2017_2019=subset(accidentes_en_poligonos_17_19)[!duplicated(accidentes_en_poligonos_17_19$CIF_EMPRESA),]
ubicacion=subset_2017_2019$ubicacion
tamañosubset_2017_2019=subset_2017_2019$tamaño
tabla_2017_2019=tapply(subset_2017_2019$plantilla_ACT, list(tamañosubset_2017_2019, ubicacion), sum)
sum(subset_2017_2019$plantilla_ACT[subset_2017_2019$LUGAR==3|subset_2017_2019$LUGAR==2])
#tabla_2017_2019=tapply(subset_2017_2019$plantilla_reciente, list(tamañosubset_2017_2019, ubicacion), sum)
tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
tabla_2017_2019$total=rowSums(tabla_2017_2019)
aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
tabla_2017_2019=tabla_2017_2019 %>% bind_rows(summarise_all(., ~aux.function(.x)))
tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
row.names(tabla_2017_2019)[4]="Total"
tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
tabla_2017_2019
obten_tabla(tabla_2017_2019)

trabajadores2017_2019=as.numeric(tabla_2017_2019[4,1:4])
etiquetas = paste0(names(tabla_2017_2019[4,1:4]), "\n", trabajadores2017_2019, "\n",round(100 * trabajadores2017_2019[1:4]/sum(trabajadores2017_2019), 2), "%")
pie2(trabajadores2017_2019,labels = etiquetas ,main="Nº DE TRABAJADORES TOTALES POR ZONA", cex.main=2.5, cex=2, col = c("steelblue", "olivedrab3","brown2", "purple2"))
#dev.print(pdf, 'Informe_3.04_Nº_DE_TRABAJADORES_TOTALES_POR_ZONA.pdf' ,  height=10, width=10 )

empresas_trabajadores=t(tabla_2017_2019)[5,1:3]
etiquetas = paste0(names(empresas_trabajadores), "\n", empresas_trabajadores, "\n",round(100 * empresas_trabajadores/sum(empresas_trabajadores), 2), "%")
pie2(empresas_trabajadores,labels = etiquetas,main="Nº DE TRABAJADORES TOTALES POR TAMAÑO DE EMPRESA", cex.main=2.5,cex=2, col = c("steelblue","brown2", "olivedrab3"))
#dev.print(pdf, 'Informe_3.05_Nº_DE_TRABAJADORES_TOTALES_POR_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )

tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
ylim.sup=max(tabla_2017_2019[1:3,1:4])*1.2
aux.ylim=c(0,ylim.sup)
titulo="Nº DE TRABAJADORES POR UBICACION \n Y TAMAÑO DE EMPRESA"
bp=barplot((as.matrix(tabla_2017_2019[1:3,1:4])),yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 1.75, col = c("steelblue","brown2", "olivedrab3"))
text(bp, as.matrix(tabla_2017_2019[1:3,1:4]), round(as.matrix(tabla_2017_2019[1:3,1:4]), 1),font=2,cex=1.5,pos=3)
#dev.print(pdf, 'Informe_3.06_Nº_DE_TRABAJADORES_POR_UBICACION_Y_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )


#------------------------------------#
subset=accidentes_en_poligonos_17_19_itinere
table(subset$year)

tabla_17_19_itinere=table(subset$tamaño,subset$ubicacion)
tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
tabla_17_19_itinere$total=rowSums(tabla_17_19_itinere)
#tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))
aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
tabla_17_19_itinere=tabla_17_19_itinere %>% bind_rows(summarise_all(., ~aux.function(.x)))
tabla_17_19_itinere=as.data.frame.matrix(tabla_17_19_itinere)
row.names(tabla_17_19_itinere)[4]="Total"
tabla_17_19_itinere=tabla_17_19_itinere[c(3,2,1,4),]
tabla_17_19_itinere
obten_tabla(tabla_17_19_itinere)

accidentes2017_2019=as.numeric(tabla_17_19_itinere[4,1:4])
etiquetas = paste0(names(tabla_17_19_itinere[4,1:4]), "\n", accidentes2017_2019, "\n",round(100 * accidentes2017_2019[1:4]/sum(accidentes2017_2019), 2), "%")
pie2(accidentes2017_2019,labels = etiquetas ,main="Nº TOTAL DE ALT POR UBICACIÓN", cex.main=2.5, cex=2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
#dev.print(pdf, 'Informe_3.07_Nº_TOTAL_DE_ALT_POR_UBICACIÓN.pdf' ,  height=10, width=10 )

empresas_17_19=t(tabla_17_19_itinere)[5,1:3]
etiquetas = paste0(names(empresas_17_19), "\n", empresas_17_19, "\n",round(100 * empresas_17_19/sum(empresas_17_19), 2), "%")
pie2(empresas_17_19,labels = etiquetas,main="Nº TOTAL DE ALT POR TAMAÑO DE EMPRESA", cex.main=2.5,cex=2, col = c("steelblue","brown2", "olivedrab3"))
#dev.print(pdf, 'Informe_3.08_Nº_TOTAL_DE_ALT_POR_TAMAÑO_DE_EMPRESA.pdf' ,  height=10, width=10 )
#0.75
ylim.sup=max(tabla_17_19_itinere[1:3,1:4])*1.2
aux.ylim=c(0,ylim.sup)
#titulo="Nº TOTAL DE ACCIDENTES LABORALES DE TRÁFICO 'IN ITINERE' \n EN PEQUEÑAS, MEDIANAS Y GRANDES EMPRESAS Y ZONA"
titulo=""
bp=barplot(t(as.matrix(tabla_17_19_itinere[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1, cex.names = 1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"))
grid(nx =NA , ny = NULL, lty = 1, col = "gray")
bp=barplot(t(as.matrix(tabla_17_19_itinere[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1, cex.names = 1.5, col = c("steelblue","brown2", "olivedrab3", "purple2"))
text(bp, t(as.matrix(tabla_17_19_itinere[1:3,1:4])), round(t(as.matrix(tabla_17_19_itinere[1:3,1:4])), 1),font=2,cex=1.5,pos=3)
#dev.print(pdf, 'Informe_3.09_Nº_TOTAL_DE_ACCIDENTES_LABORALES_DE_TRÁFICO_IN_ITINERE_EN_EMPRESAS_Y_ZONA.pdf' ,  height=10, width=10 )

ylim.sup=max(tabla_17_19_itinere[1:3,1:4])*1.2
aux.ylim=c(0,ylim.sup)
titulo="Nº DE ALT POR TAMAÑO DE EMPRESA Y UBICACION"
bp=barplot((as.matrix(tabla_17_19_itinere[1:3,1:4])), beside=TRUE, yaxt = "n", ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.5, inset = c(- 0.05, 0)), main=titulo, cex.main=1.5, cex.names = 2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
text(bp, as.matrix(tabla_17_19_itinere[1:3,1:4]), round(as.matrix(tabla_17_19_itinere[1:3,1:4]), 1),font=2,cex=1.5,pos=3)
#dev.print(pdf, 'Informe_3.10_Nº_DE_ALT_POR_TAMAÑO_DE_EMPRESA_Y_UBICACION.pdf' ,  height=10, width=10 )


#------------------------------------#
promedio_trabajadores=tabla_2017_2019[c(3,2,1,4),]
promedio=as.numeric(round((promedio_trabajadores[4,1:5]/promedio_trabajadores[4,5]),2))
promedio_trabajadores[5,]=label_percent()(promedio)
promedio_trabajadores
obten_tabla(promedio_trabajadores)

aux.df=(promedio_trabajadores[1:3,1:5])
aux.df= as.data.frame(sapply(aux.df, as.numeric))
porcentaje_trabajadores=as.data.frame(round(prop.table(as.matrix(aux.df),2),2))
porcentaje_trabajadores[4,]=colSums(porcentaje_trabajadores)
porcentaje_trabajadores=sapply(porcentaje_trabajadores, function(x) percent(x, accuracy=1))
porcentaje_trabajadores
obten_tabla(porcentaje_trabajadores)

tabla_17_19_itinere
obten_tabla(tabla_17_19_itinere)

aux.df=(tabla_17_19_itinere[1:3,1:4])
aux.df= as.data.frame(sapply(aux.df, as.numeric))
porcentaje_acc_itinere=as.data.frame(round(prop.table(as.matrix(aux.df)),2))
porcentaje_acc_itinere$total=rowSums(porcentaje_acc_itinere)
porcentaje_acc_itinere[4,]=colSums(porcentaje_acc_itinere)
porcentaje_acc_itinere=sapply(porcentaje_acc_itinere, function(x) percent(x, accuracy=1))
porcentaje_acc_itinere
obten_tabla(porcentaje_acc_itinere)

#------------------------------------#

empr_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$tamaño))))
row.names(empr_trab_accs)=names((table(subset_2017_2019$tamaño)))
colnames(empr_trab_accs)=c("Nº_de_empresas")
empr_trab_accs["Total" ,] = colSums(empr_trab_accs)

porcentaje=round((empr_trab_accs[1:4,1]/empr_trab_accs[4,1]), 2)
empr_trab_accs$trabajadores_nº_eªs=percent(porcentaje)

tabla_aux=tabla_2017_2019[c(3,2,1,4),]
empr_trab_accs$Nº_de_trabajadores=tabla_aux[1:4,5]
porcentaje=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
empr_trab_accs$trabajadores=percent(porcentaje)

tabla_aux=tabla_17_19_itinere[c(3,2,1,4),]
empr_trab_accs$Nº_de_accidentes=tabla_aux[1:4,5]
porcentaje=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
empr_trab_accs$accidentes=percent(porcentaje)

A=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)
B=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)
C=A-B
empr_trab_accs$accidentes_trabajadores=percent(C)

empr_trab_accs[c(3,2,1,4),]
obten_tabla(empr_trab_accs[c(3,2,1,4),])

Trabajadores=round((empr_trab_accs[1:4,3]/empr_trab_accs[4,3]), 2)*100
Accidentes=round((empr_trab_accs[1:4,5]/empr_trab_accs[4,5]), 2)*100
aux.dat=data.frame(Trabajadores, Accidentes)
row.names(aux.dat)=row.names(empr_trab_accs)
aux.dat=as.matrix(aux.dat)
aux.dat=aux.dat[c(3,2,1,4),]

ylim.sup=max(aux.dat[1:3,1:2])*1.2
aux.ylim=c(0,ylim.sup)
bp=barplot(t(aux.dat[1:3,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1, inset = c(- 0.05, 0)), cex.names = 1, col = c("steelblue","brown2") )
#dev.print(pdf, 'Informe_3.11_ESTUDIO_DE_LA_ACCIDENTABILIDAD_POR_TAMAÑO.pdf' ,  height=10, width=10 )

#------------------------------------#

ubic_trab_accs=as.data.frame(as.numeric(t(table(subset_2017_2019$ubicacion))))
row.names(ubic_trab_accs)=names((table(subset_2017_2019$ubicacion)))
colnames(ubic_trab_accs)=c("Nº_de_empresas")
ubic_trab_accs["Total" ,] = colSums(ubic_trab_accs)

porcentaje=round((ubic_trab_accs[1:5,1]/ubic_trab_accs[5,1]), 2)
ubic_trab_accs$trabajadores_nº_eªs=percent(porcentaje)

ubic_trab_accs$Nº_de_trabajadores=as.numeric((t(tabla_2017_2019))[1:5,4])
porcentaje=round((ubic_trab_accs[1:5,3]/ubic_trab_accs[5,3]), 2)
ubic_trab_accs$trabajadores=percent(porcentaje)

ubic_trab_accs$Nº_de_accidentes=as.numeric((t(tabla_17_19_itinere))[1:5,4])
porcentaje=round((ubic_trab_accs[1:5,5]/ubic_trab_accs[5,5]), 2)
ubic_trab_accs$accidentes=percent(porcentaje)

A=round((ubic_trab_accs[1:5,3]/ubic_trab_accs[5,3]), 2)
B=round((ubic_trab_accs[1:5,5]/ubic_trab_accs[5,5]), 2)
C=A-B
ubic_trab_accs$accidentes_trabajadores=percent(C)

ubic_trab_accs
obten_tabla(ubic_trab_accs)

Trabajadores=round((ubic_trab_accs[1:5,3]/ubic_trab_accs[5,3]), 2)*100
Accidentes=round((ubic_trab_accs[1:5,5]/ubic_trab_accs[5,5]), 2)*100
aux.dat=data.frame(Trabajadores, Accidentes)
row.names(aux.dat)=row.names(ubic_trab_accs)
aux.dat=as.matrix(aux.dat)

ylim.sup=max(aux.dat[1:4,1:2])*1.2
aux.ylim=c(0,ylim.sup)
bp=barplot(t(aux.dat[1:4,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1, inset = c(- 0.05, 0)),cex.names = 1, col = c("steelblue","brown2") )
#dev.print(pdf, 'Informe_3.12_ESTUDIO_DE_ACCIDENTABILIDAD_POR_UBICACIÓN.pdf' ,  height=10, width=10 )

#------------------------------------#

tamaño=subset$tamaño
ubicacion=subset$ubicacion
table(tamaño, ubicacion)

aux.dat=accidentes_en_poligonos_17_19_itinere
aux.dat=(aux.dat[!duplicated(aux.dat$IPF_MD5,aux.dat$FECHAACCIDENTE),])
aux.dat=aux.dat[,c(19,87,112,113,114,115,116,117,106)]

#aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(accidentes))
aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(itinere))
aux.dat=subset(aux.dat)[!duplicated(aux.dat$CIF_EMPRESA),]
aux.dat$promedio=aux.dat$acc_empr/aux.dat$nº_trabajadores*1000/3

ubicacion=aux.dat$ubicacion
tamaño=aux.dat$tamaño
promedio=aux.dat$promedio
subtabla=tapply(promedio, list(tamaño, ubicacion), sum)
tabla=subtabla/table(tamaño, ubicacion)
tabla=as.data.frame.matrix(tabla)
tabla$total= rowSums(subtabla) / table(tamaño)

library(plyr)
total= (colSums(subtabla) / table(ubicacion))
total=data.frame(rbind(total))
tabla <- rbind.fill(tabla, total)
tabla[4,5]=sum(subtabla)/ sum(table(ubicacion))
detach("package:plyr", unload = TRUE)

rownames(tabla)=rownames(tabla_2017_2019)
aux.tabla.plot1=tabla

tabla=tabla[c(3,2,1,4),]
tabla=as.data.frame.matrix(round(tabla ,2 ))
tabla[] <- lapply(tabla, paste0, " \211")
tabla

tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
df=( (tabla_17_19_itinere[1:4,1:5]/3) / (tabla_2017_2019[1:4,1:5]) )*1000
df=as.data.frame(round(df ,2 ))
aux.tabla.plot2=df
df[] <- lapply(df, paste0, " \211")
df

plot.tabla=aux.tabla.plot1[c(3,2,1,4),]
obten_tabla(plot.tabla)
ylim.sup=max(plot.tabla)*1.2
aux.ylim=c(0,ylim.sup)

# bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "bottom",ncol=4, cex=1, inset = c( 0.0, -0.25)), main=titulo, cex.main=1.25, cex.names = 1, col = c("steelblue","brown2", "olivedrab3", "purple2"))
# abline(h=seq(1, ylim.sup, by=10), col="gray")
# bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "bottom",ncol=4, cex=1, inset = c( 0.0, -0.25)), main=titulo, cex.main=1.25, cex.names = 1, col = c("steelblue","brown2", "olivedrab3", "purple2"))
# axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=1, col = NA)
# text(bp, t(as.matrix(plot.tabla[1:3,1:4])), round(t(as.matrix(plot.tabla[1:3,1:4])), 2),cex=1,pos=3)
#dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD.pdf' ,  height=10, width=10 )


bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1 ), main=titulo, cex.main=1.25, cex.names = 1, col = c("steelblue","brown2", "olivedrab3", "purple2"))
abline(h=seq(1, ylim.sup, by=10), col="gray")
bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1), main=titulo, cex.main=1.25, cex.names = 1, col = c("steelblue","brown2", "olivedrab3", "purple2"))
axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=1, col = NA)
text(bp, t(as.matrix(plot.tabla[1:3,1:4])), paste0(round(t(as.matrix(plot.tabla[1:3,1:4])), 2)," \211"),font=2,cex=1.25,pos=3)
#dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD.pdf' ,  height=10, width=10 )



#------------------------------------#

                              
