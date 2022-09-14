#Comparativa#
#------------------------------------#

comp_plaza=c(104,63,24)
comp_cuarte=c(69,34,27)
m_comp = (rbind(comp_plaza, comp_cuarte))
rownames(m_comp) <- c("Plaza", "Cuarte")
colnames(m_comp) <- c("09-16", "17-19", "20-21")
ylim.sup=max(m_comp)*1.2
aux.ylim=c(0,ylim.sup)
bp=barplot(m_comp, beside=TRUE,legend = TRUE, ylim=aux.ylim, args.legend = list(bty = "n", x = "topright"), cex=1, cex.main=1.25, cex.names = 1, col = c("steelblue","brown2"))
text(bp, (as.matrix(m_comp)), round((as.matrix(m_comp)), 2),cex=1,pos=3,cex.axis=1)


comp_plaza=c(17,16,12)
comp_cuarte=c(11,9,13)
m_comp = (rbind(comp_plaza, comp_cuarte))
rownames(m_comp) <- c("Plaza", "Cuarte")
colnames(m_comp) <- c("09-16", "17-19", "20-21")
ylim.sup=max(m_comp)*1.2
aux.ylim=c(0,ylim.sup)
bp=barplot(m_comp, beside=TRUE,legend = TRUE, ylim=aux.ylim, args.legend = list(bty = "n", x = "topright", cex=2), cex=1,cex.axis=2, cex.main=2.5, cex.names = 2, col = c("steelblue","brown2"))
text(bp, (as.matrix(m_comp)), labels=(paste0(as.matrix(m_comp), '%')),cex=2,pos=3)
#dev.print(pdf, 'comparativa_01.pdf' ,  height=10, width=10 )



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



