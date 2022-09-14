#INFORME ISSLA AMPLIACIÓN#
#------------------------------------#


años=c(2009:2016)
#años=c(2017:2019)
años=c(2020:2021)

informe(delta_poligonos,años)

informe=function(df, años){
  
  #Conclusiones años 
  
  #1º Selecciono solo los poligonos y años 
  accidentes_en_poligonos=subset(delta_poligonos, !is.na(delta_poligonos$ubicacion))
  aux.marcador=is.element(accidentes_en_poligonos$year,años)
  accidentes_en_poligonos_17_19=subset(accidentes_en_poligonos,aux.marcador==TRUE)
  
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
  
  tabla_accidentes=table(accidentes_en_poligonos_17_19_itinere$year)
  
  #------------------------------------#
  
  #Elimino las empresas repetidas
  
  subset_2017_2019=subset(accidentes_en_poligonos_17_19)[!duplicated(accidentes_en_poligonos_17_19$CIF_EMPRESA),]
  ubicacion=subset_2017_2019$ubicacion
  tamañosubset_2017_2019=subset_2017_2019$tamaño
  tabla_2017_2019=tapply(subset_2017_2019$plantilla_ACT, list(tamañosubset_2017_2019, ubicacion), sum, na.rm = TRUE)
  tabla_2017_2019[is.na(tabla_2017_2019)]=0
  sum(subset_2017_2019$plantilla_ACT[subset_2017_2019$LUGAR==3|subset_2017_2019$LUGAR==2])
  #tabla_2017_2019=tapply(subset_2017_2019$plantilla_reciente, list(tamañosubset_2017_2019, ubicacion), sum)
  tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
  tabla_2017_2019$total=rowSums(tabla_2017_2019)
  aux.function=function(x){if(is.numeric(x)) y=sum(x) else y="Total";return(y)}
  tabla_2017_2019=tabla_2017_2019 %>% bind_rows(summarise_all(., ~aux.function(.x)))
  tabla_2017_2019=as.data.frame.matrix(tabla_2017_2019)
  row.names(tabla_2017_2019)[4]="Total"
  tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
  #tabla_2017_2019
  
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
  #tabla_17_19_itinere
  
  #------------------------------------#
  promedio_trabajadores=tabla_2017_2019[c(3,2,1,4),]
  promedio=as.numeric(round((promedio_trabajadores[4,1:5]/promedio_trabajadores[4,5]),2))
  promedio_trabajadores[5,]=label_percent()(promedio)
  #promedio_trabajadores
  #obten_tabla(promedio_trabajadores)
  
  aux.df=(promedio_trabajadores[1:3,1:5])
  aux.df= as.data.frame(sapply(aux.df, as.numeric))
  porcentaje_trabajadores=as.data.frame(round(prop.table(as.matrix(aux.df),2),2))
  porcentaje_trabajadores[4,]=colSums(porcentaje_trabajadores)
  porcentaje_trabajadores=sapply(porcentaje_trabajadores, function(x) percent(x, accuracy=1))
  # porcentaje_trabajadores
  # obten_tabla(porcentaje_trabajadores)
  
  # tabla_17_19_itinere
  # obten_tabla(tabla_17_19_itinere)
  
  aux.df=(tabla_17_19_itinere[1:3,1:4])
  aux.df= as.data.frame(sapply(aux.df, as.numeric))
  porcentaje_acc_itinere=as.data.frame(round(prop.table(as.matrix(aux.df)),2))
  porcentaje_acc_itinere$total=rowSums(porcentaje_acc_itinere)
  porcentaje_acc_itinere[4,]=colSums(porcentaje_acc_itinere)
  porcentaje_acc_itinere=sapply(porcentaje_acc_itinere, function(x) percent(x, accuracy=1))
  # porcentaje_acc_itinere
  # obten_tabla(porcentaje_acc_itinere)
  
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
  
  ylim.sup=max(aux.dat[1:3,1:2],na.rm = TRUE)*1.2
  aux.ylim=c(0,ylim.sup)
  bp=barplot(t(aux.dat[1:3,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=2.5, inset = c(- 0.05, 0)), cex.names = 2.5,cex.axis=2.5, col = c("steelblue","brown2") )
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
  
  # ubic_trab_accs
  # obten_tabla(ubic_trab_accs)
  
  Trabajadores=round((ubic_trab_accs[1:5,3]/ubic_trab_accs[5,3]), 2)*100
  Accidentes=round((ubic_trab_accs[1:5,5]/ubic_trab_accs[5,5]), 2)*100
  aux.dat=data.frame(Trabajadores, Accidentes)
  row.names(aux.dat)=row.names(ubic_trab_accs)
  aux.dat=as.matrix(aux.dat)
  
  ylim.sup=max(aux.dat[1:4,1:2],na.rm = TRUE)*1.2
  aux.ylim=c(0,ylim.sup)
  bp=barplot(t(aux.dat[1:4,1:2]), beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=2.5, inset = c(- 0.05, 0)),cex.names = 2,cex.axis=2.5, col = c("steelblue","brown2") )
  #dev.print(pdf, 'Informe_3.12_ESTUDIO_DE_ACCIDENTABILIDAD_POR_UBICACIÓN.pdf' ,  height=10, width=10 )
  
  #------------------------------------#
  
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
  
  rownames(tabla)=rownames(tabla_2017_2019[c(3,2,1,4),])
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
  ylim.sup=max(plot.tabla,na.rm = TRUE)*1.2
  aux.ylim=c(0,ylim.sup)
  bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
  abline(h=seq(1, ylim.sup, by=10), col="gray")
  bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topleft", cex=1.5 ), cex.names = 2, col = c("steelblue","brown2", "olivedrab3", "purple2"))
  axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=2, col = NA)
  text(bp, t(as.matrix(plot.tabla[1:3,1:4])), round(t(as.matrix(plot.tabla[1:3,1:4])), 2),font=2,cex=1.5,pos=3)
  #dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD.pdf' ,  height=10, width=10 )
  plot.tabla=round(plot.tabla, 2)
  #------------------------------------#
  
  obten_tabla(promedio_trabajadores)
  obten_tabla(porcentaje_trabajadores)
  obten_tabla(tabla_17_19_itinere)
  obten_tabla(porcentaje_acc_itinere)
  obten_tabla(empr_trab_accs)
  obten_tabla(ubic_trab_accs)
  obten_tabla(plot.tabla)
  
  #------------------------------------#
  
  
  return(list(promedio_trabajadores,porcentaje_trabajadores,tabla_17_19_itinere,porcentaje_acc_itinere, empr_trab_accs,ubic_trab_accs,plot.tabla))
}



#------------------------------------#
#------------------------------------#
#------------------------------------#

años=c(2009:2016)
años=c(2020:2021)
#años=c(2017:2019)





# 
#   aux.dat=accidentes_en_poligonos_17_19_itinere
#   aux.dat=(aux.dat[!duplicated(aux.dat$IPF_MD5,aux.dat$FECHAACCIDENTE),])
#   aux.dat=aux.dat[,c(19,87,112,113,114,115,116,117,106)]
# 
#   #aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(accidentes))
#   aux.dat=aux.dat %>% group_by(CIF_EMPRESA) %>% mutate(acc_empr=sum(itinere))
#   aux.dat=subset(aux.dat)[!duplicated(aux.dat$CIF_EMPRESA),]
#   aux.dat$promedio=aux.dat$acc_empr/aux.dat$nº_trabajadores*1000/3
# 
#   ubicacion=aux.dat$ubicacion
#   tamaño=aux.dat$tamaño
#   promedio=aux.dat$promedio
#   subtabla=tapply(promedio, list(tamaño, ubicacion), sum)
#   tabla=subtabla/table(tamaño, ubicacion)
#   tabla=as.data.frame.matrix(tabla)
#   tabla$total= rowSums(subtabla) / table(tamaño)
# 
#   library(plyr)
#   total= (colSums(subtabla) / table(ubicacion))
#   total=data.frame(rbind(total))
#   tabla <- rbind.fill(tabla, total)
#   tabla[4,5]=sum(subtabla)/ sum(table(ubicacion))
#   detach("package:plyr", unload = TRUE)
# 
#   rownames(tabla)=rownames(tabla_2017_2019)
#   aux.tabla.plot1=tabla
# 
#   tabla=tabla[c(3,2,1,4),]
#   tabla=as.data.frame.matrix(round(tabla ,2 ))
#   tabla[] <- lapply(tabla, paste0, " \211")
#   tabla
# 
#   tabla_2017_2019=tabla_2017_2019[c(3,2,1,4),]
#   df=( (tabla_17_19_itinere[1:4,1:5]/3) / (tabla_2017_2019[1:4,1:5]) )*1000
#   df=as.data.frame(round(df ,2 ))
#   aux.tabla.plot2=df
#   df[] <- lapply(df, paste0, " \211")
#   df
# 
#   plot.tabla=aux.tabla.plot1[c(3,2,1,4),]
#   ylim.sup=max(plot.tabla)*1.2
#   aux.ylim=c(0,ylim.sup)
#   bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])), yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "bottom",ncol=4, cex=0.625, inset = c( 0.0, -0.25)), main=titulo, cex.main=0.8, cex.names = 0.75, col = c("steelblue","brown2", "olivedrab3", "purple2"))
#   abline(h=seq(1, ylim.sup, by=10), col="gray")
#   bp=barplot(t(as.matrix(plot.tabla[1:3,1:4])),add=TRUE, yaxt = "n", beside=TRUE, ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "bottom",ncol=4, cex=0.625, inset = c( 0.0, -0.25)), main=titulo, cex.main=0.8, cex.names = 0.75, col = c("steelblue","brown2", "olivedrab3", "purple2"))
#   axis(2, at = seq(0, ylim.sup, by=20), las = 1, cex.axis=0.75, col = NA)
#   text(bp, t(as.matrix(plot.tabla[1:3,1:4])), round(t(as.matrix(plot.tabla[1:3,1:4])), 2),cex=0.625,pos=3)
#   dev.print(pdf, 'Informe_3.13_ESTUDIO_POR_RATIO_DE_SINIESTRALIDAD' ,  height=10, width=10 )









































