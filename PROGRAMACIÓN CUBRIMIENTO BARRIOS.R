
#### PROGRAMACIÓN CUBRIMIENTO BARRIOS ####### 




require(Rcmdr)

RepartosCampañas <-  readXL("D:/OneDrive - Muebles Jamar/GeoEstimaciones/Archivos para R.xlsx", 
                            rownames=FALSE, header=TRUE, na="", sheet="repartos_campaña", 
                            stringsAsFactors=TRUE)

library(sqldf)
## Para COSTA NORTE:

Ciudades=c("BARRANQUILLA","SOLEDAD","CARTAGENA DE INDIAS","SANTA MARTA","CIENAGA","VALLEDUPAR",
                "MONTERIA","SINCELEJO","COROZAL","RIOHACHA","BUCARAMANGA","FLORIDABLANCA","ENVIGADO",
                "SABANETA","ITAGUI","MEDELLIN","SOACHA","BELLO")




for (k in 1:(length(Ciudades))) {

#CIUDAD="BARRANQUILLA"  
  
CIUDAD=Ciudades[k]

(Repartos_ciudad=sqldf(paste("SELECT Ciudad, REP_1,	REP_2 ,REP_3,REP_4,	REP_5,	REP_6,	REP_7,	REP_8,	REP_9,	REP_10
      FROM RepartosCampañas WHERE Ciudad=","'",CIUDAD,"'", sep=""),stringsAsFactors = TRUE))


(catalogos_por_reparto=apply(Repartos_ciudad[,-(1)],2,sum))

Meses=seq(1,12,1)
(catalogos_por_mes=c(sum(catalogos_por_reparto[1]), sum(catalogos_por_reparto[2:4]), sum(catalogos_por_reparto[5:10]),rep(c(sum(catalogos_por_reparto[1:3]), sum(catalogos_por_reparto[4:7]),
                        sum(catalogos_por_reparto[8:10])),1),c(rep(c(sum(catalogos_por_reparto[1:5]),
                                                                   sum(catalogos_por_reparto[6:10])),3))))

##########################################################################################################

(Data_meses=data.frame("Mes"=Meses, "N_catalogs"=catalogos_por_mes, "Disponibilidad"=c(0,catalogos_por_mes[-1]) ))


#j=min(which(Data_meses$Disponibilidad>0))

Barrios <- readXL("D:/OneDrive - Muebles Jamar/GeoEstimaciones/Archivos para R.xlsx", rownames=FALSE, 
                  header=TRUE, na="", sheet=CIUDAD, stringsAsFactors=TRUE)

for(i in 1:(dim(Barrios)[1])){  #
  j=min(which(Data_meses$Disponibilidad>0))
  delta= Data_meses$Disponibilidad[j]-Barrios$N_catalogosEstimado[i]
  if(delta>=0){
    Data_meses$Disponibilidad[j]=delta
    Barrios$MesEstimadoCubrimiento[i]=month.abb[j]
  }else{
    Data_meses$Disponibilidad[j]=0
    j=j+1
    Data_meses$Disponibilidad[j]=Data_meses$Disponibilidad[j]+delta
    Barrios$MesEstimadoCubrimiento[i]=month.abb[j]
      }
}

Barrios$MesEstimadoCubrimiento=as.factor(Barrios$MesEstimadoCubrimiento)
Barrios$N_catalogos_real=rep(0,dim(Barrios)[1])
Barrios$Porc_avance=rep(0,dim(Barrios)[1])
Barrios$Estatus=as.factor(rep(c("En curso","Pendiente"),c(table(Barrios$MesEstimadoCubrimiento)[2],dim(Barrios)[1]-table(Barrios$MesEstimadoCubrimiento)[2])))
Barrios$MesRealCubrimiento=as.factor(rep(".....",dim(Barrios)[1]))

require(xlsx)
write.xlsx2(x=Barrios,file="D:\\OneDrive - Muebles Jamar\\GeoEstimaciones\\Seguimiento Distribucion y resultados finales Sin Bogota.xlsx"
           ,sheetName=CIUDAD,append=TRUE ,row.names=FALSE)

}



