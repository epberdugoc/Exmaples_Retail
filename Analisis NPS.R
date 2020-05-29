#######################################################

library(Rcmdr)

library(readxl)

CONSOLIDADO_NPS_ATLTCO_y_BOLIV <- read_excel("D:/OneDrive - Muebles Jamar/2019/NPS/CONSOLIDADO NPS ATLTCO y BOLIV.xlsx", 
            col_types = c("text", "date", "numeric","text", "text", "text", "text", "text","numeric"))

Calificacion_Agrupada=rep("Neutro",length(CONSOLIDADO_NPS_ATLTCO_y_BOLIV$Calificacion))

Calificacion_Agrupada=ifelse(CONSOLIDADO_NPS_ATLTCO_y_BOLIV$Calificacion<=6,"Detractor",Calificacion_Agrupada)
Calificacion_Agrupada=ifelse(CONSOLIDADO_NPS_ATLTCO_y_BOLIV$Calificacion>8,"Promotor",Calificacion_Agrupada)

CONSOLIDADO_NPS_ATLTCO_y_BOLIV$CalificacionAgrupada=as.factor(Calificacion_Agrupada)


## conversion de las variables tipo caracter a FACTOR:

mydata=CONSOLIDADO_NPS_ATLTCO_y_BOLIV

mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)

CONSOLIDADO_NPS_ATLTCO_y_BOLIV=mydata

rm(list=c("mydata","Calificacion_Agrupada"))

################################################################################

# #Calificacion_Agrupada=rep("Neutro",nrow(ATLANTICO_ENCUESTAS_GRUPO_12_18[,14]))
# length(Calificacion_Agrupada)
# 
# attach(CONSOLIDADO_NPS_ATLTCO_y_BOLIV)
# Calificacion_Agrupada=ifelse(Calificacion<=6,"Detractor",Calificacion_Agrupada)
# Calificacion_Agrupada=ifelse(Calificacion>8,"Promotor",Calificacion_Agrupada)
# detach(CONSOLIDADO_NPS_ATLTCO_y_BOLIV)


NPS=function(Datos){
  (Tabla=100*table(Datos$CalificacionAgrupada)/sum(table(Datos$CalificacionAgrupada)))
  
  NPS=round(Tabla["Promotor"]-Tabla["Detractor"],1)
  names(NPS)="NPS"     #  paste(Datos$)
  return(NPS)
}

#NPS(CONSOLIDADO_NPS_ATLTCO_y_BOLIV)

################ RESUMEN CATEGORÍA - DEPARTAMENTO:

Resumen_NPS=by(CONSOLIDADO_NPS_ATLTCO_y_BOLIV,CONSOLIDADO_NPS_ATLTCO_y_BOLIV[,c("Producto","Ubicación","Grupo")], NPS, simplify = F)

#class(Resumen_NPS)

attr(Resumen_NPS, "dimnames")

#unlist(Resumen_NPS)
#tapply(CONSOLIDADO_NPS_ATLTCO_y_BOLIV,CONSOLIDADO_NPS_ATLTCO_y_BOLIV[,c("Producto","Ubicación","Grupo")], NPS, simplify = F)

Resumen_NPS["CLOSET",,];Resumen_NPS["COMEDOR",,];Resumen_NPS["DORMITORIO",,];Resumen_NPS["SALA",,]



Info_total_NPS="CONSOLIDADO_NPS_ATLTCO_y_BOLIV"

Producto=levels(get(Info_total_NPS)$Producto)[1]
A=as.data.frame( apply(Resumen_NPS[Producto,,],2,as.numeric))
names(A) <- make.names(names(A))
assign(Producto,cbind("producto"=Producto,data.frame("Depto"=rownames(Resumen_NPS[Producto,,])),stack(A[, c("X12.18","X24.36","X37.60","X60.80")]))) #

NPS_DEPT_PRODUCTO=get(Producto)

#levels(NPS_DEPT_PRODUCTO$producto)=levels(get(Info_total_NPS)$Producto)

for (i in 2:length(levels(get(Info_total_NPS)$Producto))) {
  
  Producto=levels(get(Info_total_NPS)$Producto)[i]
  A=as.data.frame( apply(Resumen_NPS[Producto,,],2,as.numeric))
  names(A) <- make.names(names(A))
  assign(Producto,cbind("producto"=Producto,data.frame("Depto"=rownames(Resumen_NPS[Producto,,])),stack(A[, c("X12.18","X24.36","X37.60","X60.80")]))) #
  
  NPS_DEPT_PRODUCTO=rbind(NPS_DEPT_PRODUCTO,get(Producto))
}

names(NPS_DEPT_PRODUCTO)=c("producto","Departamento","NPS","Tiempo_compra")
levels(NPS_DEPT_PRODUCTO$Tiempo_compra)=c("12 a 18","24 a 36","37 a 60", "61 a 80")
#NPS_DEPT_PRODUCTO=get(levels(get(Info_total_NPS)$Producto)[1])

## Gráfico:

require("ggplot2")

windows()

datos_NPS=NPS_DEPT_PRODUCTO

.df <- data.frame(x = datos_NPS$Tiempo_compra, z = datos_NPS$Departamento, s = datos_NPS$producto, Freq=datos_NPS$NPS)

.plot <- ggplot(data = .df, aes(x = x, y = Freq, fill = z)) + 
  geom_bar(width = 0.9, position = position_dodge(), stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0), limits=c(0,100)) + 
  facet_wrap( ~ s) + 
  xlab("Tiempo de uso (meses)") + 
  ylab("NPS (%)") + 
  geom_text(aes(label=round(Freq,1)), vjust=-1,hjust = 1,size = 4,position = position_dodge(0.9)) +
  scale_radius(range = c(3,6)) +
  labs(fill = "DEPARTAMENTO") + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines"), legend.position = "right")+
  ggtitle("Distribución del NPS por categrorías y departamentos")
print(.plot)


###### RESUMEN CATEGORÍA:

Resumen_NPS_CAT=by(CONSOLIDADO_NPS_ATLTCO_y_BOLIV,CONSOLIDADO_NPS_ATLTCO_y_BOLIV[,c("Producto","Grupo")], NPS, simplify = F)

attributes(Resumen_NPS_CAT)

Resumen_NPS_CAT["CLOSET",,drop=F];Resumen_NPS_CAT["COMEDOR",,drop=F];Resumen_NPS_CAT["DORMITORIO",,drop=F];Resumen_NPS_CAT["SALA",,drop=F]

Info_total_NPS="CONSOLIDADO_NPS_ATLTCO_y_BOLIV"

Producto=levels(get(Info_total_NPS)$Producto)[1]
A=as.data.frame(t(apply(t(Resumen_NPS_CAT[Producto,,drop=F]),1,as.numeric)))#
names(A) <- make.names(names(A))
assign(Producto,cbind("producto"=Producto,data.frame(stack(A[, c("X12.18","X24.36","X37.60","X60.80")])) ))#

NPS_PRODUCTO=get(Producto)

for (i in 2:length(levels(get(Info_total_NPS)$Producto))) {
  
  Producto=levels(get(Info_total_NPS)$Producto)[i]
  A=as.data.frame(t(apply(t(Resumen_NPS_CAT[Producto,,drop=F]),1,as.numeric)))#
  names(A) <- make.names(names(A))
  assign(Producto,cbind("producto"=Producto,data.frame(stack(A[, c("X12.18","X24.36","X37.60","X60.80")])) ))#
  
  NPS_PRODUCTO=rbind(NPS_PRODUCTO,get(Producto))
}

names(NPS_PRODUCTO)=c("producto","NPS","Tiempo_compra")
levels(NPS_PRODUCTO$Tiempo_compra)=c("12 a 18","24 a 36","37 a 60", "61 a 80")

### GRAFICO:

datos_NPS=NPS_PRODUCTO

.df <- data.frame(x = datos_NPS$Tiempo_compra, s = datos_NPS$producto, Freq=datos_NPS$NPS)
windows()
.plot <- ggplot(data = .df, aes(x = x, y = Freq, fill = s)) + 
  geom_bar(width = 0.9, position = position_dodge(), stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0), limits=c(0,100)) + 
  # facet_wrap( ~ s) + 
  xlab("Tiempo de uso (meses)") + 
  ylab("NPS (%)") + 
  geom_text(aes(label=round(Freq,1)), vjust=-1,hjust = 1,size = 4,position = position_dodge(0.9)) +
  scale_radius(range = c(3,6)) +
  labs(fill = "Producto") + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines"), legend.position = "right")+
  ggtitle("Distribución del NPS por categrorías")
print(.plot)

########### CONSOLIDADO SAC:


library(xlsx)
write.xlsx2(x=NPS_total_3019_Q2_V3,file="D:\\OneDrive - Muebles Jamar\\2019\\Medicion indicadores mercadeo\\Matrices encuestas NPS SAC\\Consolidado Total Matrices NPS.xlsx",sheetName="V4",append=TRUE,row.names=T)


library(writexl)
write_xlsx(NPS_total_3019_Q2_V3, path ="D:\\OneDrive - Muebles Jamar\\2019\\Medicion indicadores mercadeo\\Matrices encuestas NPS SAC\\Consolidado Total Matrices NPS V4.xlsx" )



