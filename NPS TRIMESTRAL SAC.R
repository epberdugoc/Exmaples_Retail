
####################################################################
############# NPS TRIMESTRAL SAC ###################################
####################################################################

### BASE CONSOLIDADA:

library(readxl)
Consolidado_Total_Matrices_NPS <- read_excel("D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado Total Matrices NPS hasta 2019-Q3_RECAT.xlsx", 
                                                           sheet = "version nov 2019", col_types = c("text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text", "text", 
                                                                                          "text", "text", "text", "text"))


View(Consolidado_Total_Matrices_NPS)

library(Rcmdr)
#names(Consolidado_Total_Matrices_NPS) <- make.names(names(Consolidado_Total_Matrices_NPS))

# Consolidado_Total_Matrices_NPS <- within(Consolidado_Total_Matrices_NPS, {
#   ID_cliente <- as.factor(ID_cliente)
# })

## Convirtiendo campos tipo caracter a factor:

# mydata=Consolidado_Total_Matrices_NPS
# mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
# Consolidado_Total_Matrices_NPS=mydata

#rm(list=c("mydata"))

library(Hmisc)

describe(Consolidado_Total_Matrices_NPS)


### ADICIONANDO CAMPOS FALTANTES:

attach(Consolidado_Total_Matrices_NPS)
Query=ifelse(is.na(ID_cliente)==FALSE,
paste0("SELECT
  CLIENTES.CLIENTE_CODIGO AS  CLIENTE_CODIGO,ZONA_GEOGRAFICA.ESTRATO AS ESTR,
       trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360) as AGE,CLIENTES.CLIENTES_SEXO AS SEX,
       CLIENTES.CLIENTE_OCUP AS JOB
       FROM
       ZONA_GEOGRAFICA,
       CLIENTES
       WHERE
       ( CLIENTES.CLIENTE_ZONA=ZONA_GEOGRAFICA.ZONA_CODIGO  )
       AND  (
       CLIENTES.CLIENTE_CODIGO  =  '",ID_cliente,"'
       )"),"")
detach(Consolidado_Total_Matrices_NPS)

#CamposAdicionales=data.frame(rep,)


Consulta=function(query){
  #require(RODBC) 
  if(query!=""){
    CAMPOS_ADD=sqlQuery(DWJAMAR_Connection,query)
  }else{
    CAMPOS_ADD=query
  }
  return(CAMPOS_ADD)
}


## 
DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
library(RODBC)
system.time({result=lapply(Query,Consulta)})
odbcClose(DWJAMAR_Connection)


CamposAdd=data.frame("CLIENTE_CODIGO"=rep(NA,nrow(Consolidado_Total_Matrices_NPS)),
                     "ESTR"=rep(NA,nrow(Consolidado_Total_Matrices_NPS)),
                     "AGE"=rep(NA,nrow(Consolidado_Total_Matrices_NPS)),
                     "SEX"=as.character(rep(NA,nrow(Consolidado_Total_Matrices_NPS))),
                     "JOB"=as.character(rep(NA,nrow(Consolidado_Total_Matrices_NPS)))
                     )

CamposAdd$SEX=as.character(CamposAdd$SEX)
CamposAdd$JOB=as.character(CamposAdd$JOB)



for (i in 1:nrow(Consolidado_Total_Matrices_NPS)) {
  
  
  if(nrow(result[[i]])>0){
    B=result[[i]]
    B$SEX=as.character(B$SEX)
    B$JOB=as.character(B$JOB)
    
    CamposAdd[i,] = B[1,1:5] 
  }
}

CamposAdd$CLIENTE_CODIGO=as.character(CamposAdd$CLIENTE_CODIGO)
CamposAdd$CLIENTE_CODIGO[1]="07842"

library(sqldf)

ConsolidadoTotal_NPS=Consolidado_Total_Matrices_NPS


ConsolidadoTotal_NPS$NSE=ifelse( ((is.na(ConsolidadoTotal_NPS$NSE)==T)|(ConsolidadoTotal_NPS$NSE=="No responde"))&(is.na(CamposAdd$ESTR)==F),paste("NSE",as.character(CamposAdd$ESTR),sep=" "),ConsolidadoTotal_NPS$NSE)
ConsolidadoTotal_NPS$NSE=ifelse( ConsolidadoTotal_NPS$NSE=="No responde",NA,ConsolidadoTotal_NPS$NSE)

#((is.na(ConsolidadoTotal_NPS$Ocupacion)==T)|(ConsolidadoTotal_NPS$Ocupacion=="No responde"))&

ConsolidadoTotal_NPS$Ocupacion=ifelse( (is.na(CamposAdd$JOB)==F),CamposAdd$JOB,ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse( ConsolidadoTotal_NPS$Ocupacion=="No responde",NA,ConsolidadoTotal_NPS$Ocupacion)

#attach(ConsolidadoTotal_NPS)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Desempleado","DESEMPLEADO",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Empleado/a","EMPLEADO",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Hogar","HOGAR",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Pensionado","PENSIONADO",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Estudia/trabaja","EMPLEADO",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Estudiante","DESEMPLEADO",ConsolidadoTotal_NPS$Ocupacion)
ConsolidadoTotal_NPS$Ocupacion=ifelse(ConsolidadoTotal_NPS$Ocupacion=="Independiente","INDEPENDIENTE NO FORMAL",ConsolidadoTotal_NPS$Ocupacion)
#detach(ConsolidadoTotal_NPS)


require(car)
CamposAdd$AGE_RANGE=recode(CamposAdd$AGE,
                            "18:25='De 18 a 25 años'; 26:35='De 26 a 35 años';36:45='De 36 a 45 años';46:55='De 46 a 55 años';56:hi='Más de 55 años';else=NA")


ConsolidadoTotal_NPS$Edad=ifelse( ((is.na(ConsolidadoTotal_NPS$Edad)==T)|(ConsolidadoTotal_NPS$Edad=="No responde"))&(is.na(CamposAdd$AGE_RANGE)==F),CamposAdd$AGE_RANGE,ConsolidadoTotal_NPS$Edad)
ConsolidadoTotal_NPS$Edad=ifelse( ConsolidadoTotal_NPS$Edad=="No responde",NA,ConsolidadoTotal_NPS$Edad)


ConsolidadoTotal_NPS$Sexo=ifelse( (is.na(ConsolidadoTotal_NPS$Sexo)==T)&(is.na(CamposAdd$SEX)==F),CamposAdd$SEX,ConsolidadoTotal_NPS$Sexo)
ConsolidadoTotal_NPS$Sexo=ifelse( ConsolidadoTotal_NPS$Sexo=="---",NA,ConsolidadoTotal_NPS$Sexo)

ConsolidadoTotal_NPS$Sexo=ifelse(ConsolidadoTotal_NPS$Sexo=="Femenino","FEMENINO",ConsolidadoTotal_NPS$Sexo)
ConsolidadoTotal_NPS$Sexo=ifelse(ConsolidadoTotal_NPS$Sexo=="Masculino","MASCULINO",ConsolidadoTotal_NPS$Sexo)

################ VERSION CON CAUSALES RECATEGORIZADAS E INDICADOR NUEVO/CONOCIDO

library(readxl)
Nuevo_vs_Conocido_NPS_hasta_2019_Q3 <- read_excel("D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Nuevo vs Conocido NPS hasta 2019-Q3.xlsx", 
                                                  sheet = "Nuevo vs Conocido", col_types = c("text", 
                                                                                             "text", "date", "text", "date", "text", 
                                                                                             "text", "text", "text", "text", "text"))
View(Nuevo_vs_Conocido_NPS_hasta_2019_Q3)


ConsolidadoTotal_NPS_NvC=sqldf("select t1.*, t2.Ind_conocido_nuevo as N_o_C from ConsolidadoTotal_NPS t1 
  left join Nuevo_vs_Conocido_NPS_hasta_2019_Q3 t2 
  on t1.ID_cliente||t1.Mes_evaluado||t1.Year=t2.ID_cliente||t2.Mes_evaluado||t2.Year")

I=table(paste0(ConsolidadoTotal_NPS_NvC$Year,ConsolidadoTotal_NPS_NvC$Mes_evaluado,ConsolidadoTotal_NPS_NvC$ID_cliente))

## identificar llaves duplicadas:

I[I>1]

ConsolidadoTotal_NPS_NvC$llave=paste0(ConsolidadoTotal_NPS_NvC$Year,ConsolidadoTotal_NPS_NvC$Mes_evaluado,ConsolidadoTotal_NPS_NvC$ID_cliente)

## Eliminado "duplicados" por llaves repetidas: esto es un error para comentar
attach(ConsolidadoTotal_NPS_NvC)
ConsolidadoTotal_NPS_NvC=ConsolidadoTotal_NPS_NvC[!duplicated(llave),]
detach(ConsolidadoTotal_NPS_NvC)

ConsolidadoTotal_NPS_NvC$Ind_conocido_nuevo=ifelse(((is.na(ConsolidadoTotal_NPS_NvC$Ind_conocido_nuevo)==T)|(ConsolidadoTotal_NPS_NvC$Ind_conocido_nuevo=="No aplica")),
                                                   ConsolidadoTotal_NPS_NvC$N_o_C,ConsolidadoTotal_NPS_NvC$Ind_conocido_nuevo)


I=table(ConsolidadoTotal_NPS_NvC$llave)
I[I>1]

## Borrando la variable 
ConsolidadoTotal_NPS_NvC <- within(ConsolidadoTotal_NPS_NvC, {
  N_o_C <- NULL 
})

## Convirtiendo campos tipo caracter a factor:

mydata=ConsolidadoTotal_NPS_NvC
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
ConsolidadoTotal_NPS_NvC=mydata

## Agregando campos indicadores para detractores, pasivos y promotores:
attach(ConsolidadoTotal_NPS_NvC)
ConsolidadoTotal_NPS_NvC$Ind_promotor=ifelse(likert_recom_NPS=="Promotores",1,0)
ConsolidadoTotal_NPS_NvC$Ind_pasivo=ifelse(likert_recom_NPS=="Pasivos",1,0)
ConsolidadoTotal_NPS_NvC$Ind_detractor=ifelse(likert_recom_NPS=="Detractores",1,0)
detach(ConsolidadoTotal_NPS_NvC)



save(list ="ConsolidadoTotal_NPS_NvC" ,file = "D:\\OneDrive - Muebles Jamar\\2019\\Medicion indicadores mercadeo\\Matrices encuestas NPS SAC\\Consolidado_NPS_2019Q3.RData")


# 
# library(xlsx)
# 
# jgc <- function()
# {
#   .jcall("java/lang/System", method = "gc")
# } 
# 
# Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_221") ##"C:\Program Files\Java\jre1.8.0_221"
# options(java.parameters = "-Xmx2g")
# memory.limit(2000000)
# 
# save(list ="ConsolidadoTotal_NPS_NvC" ,file = "D:\\OneDrive - Muebles Jamar\\2019\\Medicion indicadores mercadeo\\Matrices encuestas NPS SAC\\Consolidado_NPS_2019Q3.RData")
# 
# library(rJava)
# gc(reset = T)
# jgc()  "D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado Total Matrices NPS hasta 2019-Q3_RECAT.xlsx"

library(openxlsx)

a=loadWorkbook("D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado Total Matrices NPS hasta 2019-Q3.xlsx")

addWorksheet(a,"nov 2019 v4")
writeDataTable(a, sheet = "nov 2019 v4",ConsolidadoTotal_NPS_NvC)
saveWorkbook(a, "D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado Total Matrices NPS hasta 2019-Q3.xlsx", overwrite = TRUE) 


###############################################################################

load("D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado_NPS_2019Q3.RData")

#### Función para calcular el NPS

# attach(ConsolidadoTotal_NPS_NvC)
# Datos=as.factor(ConsolidadoTotal_NPS_NvC[,"likert_recom_NPS"])
# detach(ConsolidadoTotal_NPS_NvC)
# 
# Datos=Datos[Datos!="Promotores"]
# 
# rm(Datos)

## El objeto Datos debe ser un factor que incluya todos los niveles, aun los NO usados

NPS=function(Datos){
  #Datos=as.factor(Datos)
  (Tabla=100*table(Datos)/sum(table(Datos)))
  
  (NPS=round(Tabla["Promotores"]-Tabla["Detractores"],1))
  names(NPS)="NPS"     #  paste(Datos$)
  return(NPS)
}

#### NPS departamental por trimestre:

attach(ConsolidadoTotal_NPS_NvC)
NPS_trim_dept=aggregate(likert_recom_NPS,list(Region=Departamento,Trimestre=Trimestre),NPS,drop=FALSE)
detach(ConsolidadoTotal_NPS_NvC)

NPS_trim_dept_wide=as.matrix(reshape(NPS_trim_dept, v.names = "x", idvar = "Region",
        timevar = "Trimestre", direction = "wide")[,-1])

row.names(NPS_trim_dept_wide)=levels(NPS_trim_dept$Region) # asigna nombre a las filas

## sustituye los NA's por CEROS

for (i in 1:nrow(NPS_trim_dept_wide)) {
  for (j in 1:ncol(NPS_trim_dept_wide)) {
    if (is.na(NPS_trim_dept_wide[i,j])==T) {
      NPS_trim_dept_wide[i,j]=0
    }
  }
}


#### tamaño relativo de muestra departamental por trimestre:

library(Rcmdr)

Peso_trim_dept=local({
  .Table <- xtabs(~Trimestre + Departamento, data=ConsolidadoTotal_NPS_NvC)
  cat("\nFrequency table:\n")
  print(.Table)
  cat("\nColumn percentages:\n")
  print(rowPercents(.Table))
})[,1:10]

## NPS total trimestral:

round(0.01*diag(Peso_trim_dept%*%NPS_trim_dept_wide),1)

## NPS total trimestral con pesos de otro trimestre:

round(0.01*diag(Peso_trim_dept["2018-Q2",]%*%NPS_trim_dept_wide[,"x.2018-Q4"]),1)





##########################################################################################
################ VERSION CON CAUSALES RECATEGORIZADAS E INDICADOR NUEVO/CONOCIDO #########
##########################################################################################

# Consolidado_NPS_RECAT <- read_excel("D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Consolidado Total Matrices NPS hasta 2019-Q3_RECAT.xlsx", 
#                                              sheet = "version nov 2019", col_types = c("text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text", "text", 
#                                                                             "text", "text", "text", "text"))
# 


# attach(Consolidado_NPS_RECAT)
# Consolidado_NPS_RECAT$Ocupacion=ifelse(Ocupacion=="Desempleado","DESEMPLEADO",Ocupacion)
# Consolidado_NPS_RECAT$Ocupacion=ifelse(Ocupacion=="Empleado/a","EMPLEADO",Ocupacion)
# Consolidado_NPS_RECAT$Ocupacion=ifelse(Ocupacion=="Hogar","HOGAR",Ocupacion)
# Consolidado_NPS_RECAT$Ocupacion=ifelse(Ocupacion=="Pensionado","PENSIONADO",Ocupacion)
# detach(Consolidado_NPS_RECAT)

# Consolidado_NPS_RECAT_V2=cbind(Consolidado_NPS_RECAT,CamposAdd[,c(1,5)])
# 
# Consolidado_NPS_RECAT_V2$ID_CHECK=ifelse(Consolidado_NPS_RECAT_V2$CLIENTE_CODIGO==Consolidado_NPS_RECAT_V2$ID_cliente,TRUE,FALSE)
# 
# 
# 
# 
# library(sqldf)



# ConsultaCamposAdd="SELECT
#   CLIENTES.CLIENTE_CODIGO AS CLIENTE_CODIGO,
#   ZONA_GEOGRAFICA.ESTRATO AS ESTR,
#   trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360) as AGE,
#   CLIENTES.CLIENTES_SEXO AS SEX,
#   CLIENTES.CLIENTE_OCUP AS JOB
# FROM
#   ZONA_GEOGRAFICA,
#   CLIENTES
# WHERE
#   ( CLIENTES.CLIENTE_ZONA=ZONA_GEOGRAFICA.ZONA_CODIGO  )
#   AND  (
#   CLIENTES.CLIENTE_CODIGO  =  '1102371503'
#   )"
# 
# 
# jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
# sqlQuery(jdbcConnection,ConsultaCamposAdd, dec=",",stringsAsFactors=F) 
# odbcClose(jdbcConnection)



attach(ConsolidadoTotal_NPS_NvC)
aggregate(likert_recom_NPS,list(clasifi=likert_recom_NPS,Trimestre=Trimestre,Region=Departamento)
                        ,table,drop=FALSE)
detach(ConsolidadoTotal_NPS_NvC)

















