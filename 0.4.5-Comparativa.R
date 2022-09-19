#Comparativa#
#------------------------------------#

aux.09=informe(delta_poligonos,c(2009:2016))
aux.17=informe(delta_poligonos,c(2017:2019))
aux.20=informe(delta_poligonos,c(2020:2021))



graficos_años=function(a){
v1=aux.09[[3]][a,1:4]
v2=aux.17[[3]][a,1:4]
v3=aux.20[[3]][a,1:4]
accidentabilidad=rbind(v1,v2,v3)
row.names(accidentabilidad)=c("2009-2016","2017-2019","2020-2021")
accidentabilidad=as.matrix(accidentabilidad)
ylim.sup=max(accidentabilidad,na.rm = TRUE)*1.2
aux.ylim=c(0,ylim.sup)
bp=barplot(accidentabilidad, beside=TRUE,ylim=aux.ylim,legend = TRUE, args.legend = list(bty = "n", x = "topright", cex=1.75 ),cex.names = 1.5,cex.axis = 2.5,col = c("steelblue","brown2", "olivedrab3"))
text(bp, (as.matrix(accidentabilidad[1:3,1:4])), round((as.matrix(accidentabilidad[1:3,1:4])), 2),cex=2.5,pos=3)
return(accidentabilidad)
}

graficos_años(2)
#dev.print(pdf, 'acc_medianas.pdf' ,  height=10, width=10 )


v1=aux.09[[4]]
v2=aux.17[[4]]
v3=aux.20[[4]]
obten_tabla(t(matrix(paste(v1,v2,v3, sep="/"),nrow = 5, byrow = TRUE)))

