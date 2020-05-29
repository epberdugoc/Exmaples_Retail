
library(radiant)
data("diamonds")
library(Rcmdr)
by(diamonds[,c("price","carat")],diamonds$color, cor)


suppressPackageStartupMessages(library(synthpop))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sampling))
suppressPackageStartupMessages(library(partykit))
mycols <- c("darkmagenta", "turquoise")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
myseed <- 20190110

# filtering the dataset
original.df <- SD2011 %>% dplyr::select(sex, age, socprof, income, marital,
                                        depress, sport, nofriend, smoke, nociga, alcabuse, bmi)
head(original.df)

library(Hmisc)

describe(original.df$nociga)

# setting continuous variable NA list
cont.na.list <- list(income = c(NA, -8), nofriend = c(NA, -8), nociga = c(NA, -8))

describe(original.df$nociga)

( m <- matrix(1:12, 3, 4) )
div.3 <- m %% 3 == 0
which(div.3)
which(div.3, arr.ind = TRUE)
rownames(m) <- paste("Case", 1:3, sep = "_")
which(m %% 5 == 0, arr.ind = TRUE)

dim(m) <- c(2, 2, 3); m
which(div.3, arr.ind = FALSE)
which(div.3, arr.ind = TRUE)


require(RODBC)

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW
ti1 <- Sys.time()
CamposAdicionales=sqlQuery(jdbcConnection,paste("select  clientes.clientes_segmento, clientes.cliente_tipo, clientes.cliente_tipocliente
                                                from segmentacion_360 s, clientes where s.fecha_corte = '" ,"2019-03-17", "' and s.cliente_codigo = '22521903'", sep=""), dec=",") #and rownum=1
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)


jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW
ti1 <- Sys.time()
CamposAdicionales=sqlQuery(jdbcConnection,paste("select s.cliente_codigo, clientes.clientes_segmento, clientes.cliente_tipo, clientes.cliente_tipocliente
                                                from segmentacion_360 s, clientes where s.fecha_corte = '" ,FechaCorte, "' and s.cliente_codigo = clientes.cliente_codigo and s.cliente_codigo = '22521903'", sep=""), dec=",") #and rownum=1
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)



attach(SegmentacionCorporativa)
Prueba=data.frame("CLIENTE_CODIGO"= CLIENTE_CODIGO,"SEGMENTO"=SegmentacionCorporativa$SEGMENT_CORP_PRINC, "VALOR_CLIENTE"=Valor_Cliente,
                                    "VALOR_CLIENTE_MICRO"=Valor_Cliente_Micro,"FECHA_CORTE"=as.character(FechaCorte), "MICRO.SEGMENTO"=SegmentacionCorporativa$MICRO_SEGMENTO)[]

detach(SegmentacionCorporativa)

attach(SegmentacionCorporativa)
Prueba=data.frame("CLIENTE_CODIGO"= CLIENTE_CODIGO, "VALOR_CLIENTE"=Valor_Cliente,
                                    "VALOR_CLIENTE_MICRO"=Valor_Cliente_Micro,"FECHA_CORTE"=as.character(as.Date(Sys.Date(),"Y%-m%-d%")))[1,]
detach(SegmentacionCorporativa)


jdbcConnection<- odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart", readOnly=F)#youneed
ti1 <- Sys.time()
sqlSave(jdbcConnection,Prueba, 
        tablename ="clasificacion_grupos_360_cp",append = T,safer=T, test =T )
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)

Prueba$FECHA_CORTE=as.character.factor(Prueba$FECHA_CORTE)



############################################

load("D:/OneDrive - Muebles Jamar/Segmentacion Corpotativa/Modelo_Segmentacion_YouNeed/Archivos/Insumos_Marzo_2019.RData")

attach(SegmentacionCorporativa_abril_2019)
dim(SegmentacionCorporativa_abril_2019[FECHA_MAX_COMPRA=="2019-04-18",])[1]
detach(SegmentacionCorporativa_abril_2019)


attach(SegmentacionCorporativa_abril_2019)
SegmentacionCorporativa_abril_2019[months.Date(FECHA_MAX_COMPRA)=="abril","FECHA_MAX_COMPRA", drop=F]
detach(SegmentacionCorporativa_abril_2019)

###### RESHAPE PRESUPUESTO ########################################################

wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time", direction = "wide")



long=reshape(wide, idvar = "Subject", varying = list(2:12),
             v.names = "conc", direction = "long")
#####################################################################################

PPTO_WIDE_CREDITO=Dataset[Dataset$ALTERNATIVA=="CREDITO",c(2,4:187)]

PPTO_LONG_CREDITO=reshape(PPTO_WIDE_CREDITO, idvar = "ALMACEN", varying = list(2:ncol(PPTO_WIDE_CREDITO)),
                  v.names = "Presupuesto", direction = "long")


PPTO_WIDE_CONTADO=   Dataset[Dataset$ALTERNATIVA=="CONTADO",c(2,4:187)]

PPTO_LONG_CONTADO=reshape(PPTO_WIDE_CONTADO, idvar = "ALMACEN", varying = list(2:ncol(PPTO_WIDE_CONTADO)),
                          v.names = "Presupuesto", direction = "long")


  as.Date("2019-07-01")

require(writexl)
write_xlsx(PPTO_LONG_CONTADO, path = 'D:/OneDrive - Muebles Jamar/Segmentacion Corpotativa/PPTO_LONG_CONTADO.xlsx')

#########################################################################################

Raiz_Cuadrada=function(x){
  
 y <<- sqrt(x) ## global variable
 
 assign("Y",sqrt(x),envir = .GlobalEnv)
  
  return(list(y,Y))
  
}

Raiz_Cuadrada(4)



VectorFecha=strsplit(as.character.Date(FechaCorte), "-")[[1]]

paste(VectorFecha[3],VectorFecha[2],VectorFecha[1],sep="")

as.numeric(paste(VectorFecha[3],VectorFecha[2],VectorFecha[1],sep=""))

prueba <- within(prueba, {
  TOTAL_NUM <- as.numeric(TOTAL) 
})

attach(get(NombreTablaBalance))
(is.na(SEGMENTO_ANTERIOR[1])==T)&(months.Date(FECHA_PRIMERA_COMPRA[1])==months.Date(as.Date.character(FechaCorte)))
detach(get(NombreTablaBalance))

data=rbind(matrix(rep("nc",165),165,1,byrow=TRUE),matrix(rep("sc",70),70,1,byrow=TRUE))
data=cbind.data.frame(data,c(rep(1,100), rep(2,50), rep(3,15), rep(1,30),rep(2,40)),
                      1000*runif(235))
names(data)=c("state","region","income")
# 
table(data$region,data$state)

strata(data, stratanames=NULL, size)


s=strata(data,c("region","state"),size=c(10,5), method="srswor")



DataTest=data.frame(x=seq(-5,5) )

DataTest_neg=subset(DataTest,x<0) 

DataTest_pos=DataTest[DataTest$x>0,,drop=F]

which(DataTest_pos$x==3)

########################################################################
################ COMPOSICION SEGMENTOS CORPORATIVOS  ###################
########################################################################

require(sqldf)

SegmentacionCorporativa_junio_2019$TIPO_CLIENTE_CC=recode( SegmentacionCorporativa_junio_2019$TIPO_CLIENTE,
                                             "'SOLO CONTADO'='CONTADO';else='CREDITO'")


Composicion_Segmentos=sqldf("select SEGMENT_CORP_PRINC as SEGMENTO_CORPORATIVO, TIPO_CLIENTE_CC, count(CLIENTE_CODIGO) as N_CLIENTES from
      SegmentacionCorporativa_junio_2019 group by SEGMENT_CORP_PRINC, TIPO_CLIENTE_CC")


library(xlsx)

write.xlsx2(x=Composicion_Segmentos,file="D:\\OneDrive - Muebles Jamar\\Segmentacion Corpotativa\\Actualizacion 2019\\Composicion_Segmentos.xlsx"
            ,sheetName="Composicion_CC",append=TRUE ,row.names=FALSE)


Composicion_Segmentos_credito<- as.data.frame(xtabs(~SEGMENT_CORP_PRINC+CLIENTES_SEGMENTO, 
                                       data=SegmentacionCorporativa_junio_2019))


write.xlsx2(x=Composicion_Segmentos_credito,file="D:\\OneDrive - Muebles Jamar\\Segmentacion Corpotativa\\Actualizacion 2019\\Composicion_Segmentos.xlsx"
            ,sheetName="Composicion_Credito",append=TRUE ,row.names=FALSE)





require(stats)
by(warpbreaks[, 1:2], warpbreaks[,"tension"], summary)
by(warpbreaks[, 1],   warpbreaks[, -1], summary, simplify = T)
by(warpbreaks, warpbreaks[,"tension"],
   function(x) lm(breaks ~ wool, data = x))

## now suppose we want to extract the coefficients by group
tmp <- with(warpbreaks,
            by(warpbreaks, tension,
               function(x) lm(breaks ~ wool, data = x)))
sapply(tmp, coef)


library(RODBC)
#select CEDULA FROM mercadeo_allinone_mes where ALMACEN_AGR='JAMAR SUR' and CEDULA in (select cliente_codigo from clientes_campanas where cliente_campana='NO_ENVIAR')
# and CEDULA not in (select cliente_codigo from clientes_campanas where cliente_campana='NO_ENVIAR')

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW
ti1 <- Sys.time()
CED_JS=sqlQuery(jdbcConnection,
paste("select CEDULA FROM mercadeo_allinone_mes where ALMACEN_AGR='JAMAR SUR'
", sep=""), dec=",",stringsAsFactors=F) 
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW
ti1 <- Sys.time()
CED_NE=sqlQuery(jdbcConnection,
paste("select cliente_codigo from clientes_campanas where cliente_campana='NO_ENVIAR'", sep=""), dec=",",stringsAsFactors=F) 
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)

library(sqldf)



CED_JS_NE=sqldf("select CEDULA from CED_JS where trim(CEDULA) not in (select trim(CLIENTE_CODIGO) from CED_NE)")

#CED_JS_NE=sqldf("select CEDULA from CED_JS left JOIN CED_NE ON CED_JS.CEDULA=CED_NE.cliente_codigo")

y=rep(FALSE,length(CED_NE$CLIENTE_CODIGO))

system.time({
for (i in 1:length(CED_NE$CLIENTE_CODIGO)) {
  y[i]=any(CED_JS$CEDULA==CED_NE$CLIENTE_CODIGO[i])
}
})

#####################################################
#### PARTICIPACIÓN BARRIOS JAMAR TRINITARIAS ########
#####################################################


jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW
ti1 <- Sys.time()
BARRIOS_JAMAR_SUR_BO=sqlQuery(jdbcConnection,
paste("
SELECT
  AGENCIAS.AGENCIA_DESCRIPCION,
      ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
      ZONA_GEOGRAFICA.BARR_DESCRIPCION,
      count(distinct FACTURAS.FACT_CLIENTE),
      SUM(FACTURAS.FACT_VLRVTA)
      FROM
      AGENCIAS,
      ZONA_GEOGRAFICA,
      FACTURAS,
      CLIENTES
      WHERE
      ( FACTURAS.FACT_AGENCIA=AGENCIAS.AGENCIA_CODIGO  )
      AND  ( FACTURAS.FACT_CLIENTE=CLIENTES.CLIENTE_CODIGO  )
      AND  ( CLIENTES.CLIENTE_ZONA=ZONA_GEOGRAFICA.ZONA_CODIGO  )
      AND  ( AGENCIAS.AGENCIA_DESCRIPCION  =  'JAMAR SUR'      )
      AND  (ZONA_GEOGRAFICA.BARR_DESCRIPCION is not null)
      GROUP BY
      AGENCIAS.AGENCIA_DESCRIPCION, 
      ZONA_GEOGRAFICA.CIUD_DESCRIPCION, 
      ZONA_GEOGRAFICA.BARR_DESCRIPCION", sep=""), dec=",",stringsAsFactors=F) 
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)



jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
ti1 <- Sys.time()
BARRIOS_JAMAR_SUR_AIO=sqlQuery(jdbcConnection,
                              paste("select BARRIO
                                 from mercadeo_allinone_mes
                                 where ALMACEN_AGR='JAMAR SUR' and BARRIO is not NULL
                                 group by BARRIO", sep=""), dec=",",stringsAsFactors=F) 
ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))
odbcClose(jdbcConnection)

library(sqldf)

cRUCE=sqldf("select t1.*, t2.* from BARRIOS_JAMAR_SUR_AIO t1
       left join BARRIOS_JAMAR_SUR_BO t2 on t1.BARRIO=BARR_DESCRIPCION")


write.xlsx2(x=BARRIOS_JAMAR_SUR_BO,file="D:\\OneDrive - Muebles Jamar\\2019\\VENTA BARRIOS TRINITARIAS.xlsx",sheetName="Base_con_direcciones",append=TRUE ,row.names=FALSE)







mi_funcion=function(x=0,y=1){
  z=x+y
  
  return(z)
}


mi_funcion(2,5)

n=25;x=0;p=80/2000

factorial(n)/(factorial(x)*factorial(n-x))

0.96^(25)

dim(Insumos_agosto_2019[(Insumos_agosto_2019$FECHA_MAX_COMPRA>as.POSIXct("2019-07-18"))&(Insumos_agosto_2019$FECHA_MIN_COMPRA==Insumos_agosto_2019$FECHA_MAX_COMPRA),])



SegmentacionCorporativa_agosto_2019_FILTERED=SegmentacionCorporativa_agosto_2019[SegmentacionCorporativa_agosto_2019$TIPO_CLIENTE=="SOLO CREDITO",]

######### 

library(samplesize4surveys)

ss4p(N=67482,P = 0.775,conf = .95,me = 0.03)


###############################################################

library(sqldf)

Participacion_OP_MES=sqldf("select 
                            Fecha
                            ,OP_13_meses_sin_financiacion.[Numero de cuotas],sum(OP_13_meses_sin_financiacion.[Valor Venta Oferta]) as total_OP_plan_VO
                            ,sum(OP_13_meses_sin_financiacion.[Valor  Comprometida]) as total_OP_plan_VF
                            from OP_13_meses_sin_financiacion group by Fecha,OP_13_meses_sin_financiacion.[Numero de cuotas]")

Participacion_facturacion_MES=sqldf("select Fecha, OP_13_meses_sin_financiacion.[Numero de cuotas],sum(OP_13_meses_sin_financiacion.[Valor Venta Oferta]) as total_fact_plan_VO ,sum(OP_13_meses_sin_financiacion.[Valor  Comprometida]) as total_fact_plan_VF
      from OP_13_meses_sin_financiacion where Estado='FACTURADA' group by Fecha,OP_13_meses_sin_financiacion.[Numero de cuotas]")



attach(Participacion_OP_MES)
for (i in 1:nrow(Participacion_OP_MES)) {
  
  Participacion_OP_MES$total_OP_dia_VO[i]=sum(Participacion_OP_MES[Fecha==Fecha[i],3])
  Participacion_OP_MES$total_OP_dia_VF[i]=sum(Participacion_OP_MES[Fecha==Fecha[i],4])
  
}

detach(Participacion_OP_MES)

library(xlsx)
write.xlsx2(x=Participacion_OP_MES,file="D:/OneDrive - Muebles Jamar/2019/OP 13 meses sin financiacion.xlsx"
            ,sheetName="Participacion_OP_MES",append=TRUE ,row.names=FALSE)


attach(Participacion_facturacion_MES)
for (i in 1:nrow(Participacion_facturacion_MES)) {
  
  Participacion_facturacion_MES$total_fact_dia_VO[i]=sum(Participacion_OP_MES[Fecha==Fecha[i],3])
  Participacion_facturacion_MES$total_fact_dia_VF[i]=sum(Participacion_OP_MES[Fecha==Fecha[i],4])
  
}

detach(Participacion_facturacion_MES)


Participacion_OP_MES$Fecha=as.POSIXct(Participacion_OP_MES$Fecha) %m-% days(1)

library(xlsx)
write.xlsx2(x=Participacion_facturacion_MES,file="D:/OneDrive - Muebles Jamar/2019/OP 13 meses sin financiacion.xlsx"
            ,sheetName="Participacion_fact_MES",append=TRUE ,row.names=FALSE)




## where OP_13_meses_sin_financiacion.[Numero de cuotas]=13

# sum((select OP_13_meses_sin_financiacion.[Valor  Comprometida] from OP_13_meses_sin_financiacion
# )) as OP_TOTAL_MES


########################################################################################
######### clientes ORO y AÑORADOS que cumplieron en septiembre y reaizaron OP ##########
########################################################################################

## Conectarse a TCP:

Consulta_OP="select n_ide,REM,emi,EST 
from rem_enc 
where 
emi BETWEEN to_date('01-10-2019', 'dd-mm-yyyy') and to_date('18-10-2019', 'dd-mm-yyyy')
and TVEN in ('CR','OR','TJ','CO','PT','NO')"

require(RODBC)

jdbcConnection=odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart")# DATANEW
system.time({Clientes_OP=sqlQuery(jdbcConnection,Consulta_OP, dec=",") })
odbcClose(jdbcConnection)

Clientes_OP=within(Clientes_OP,{N_IDE=as.factor(as.character(N_IDE))})

library(Rcmdr)

Clientes_OP$EST <- with(Clientes_OP, factor(EST, levels=c('F','D','G','A','C','X'), ordered=TRUE))

library(readxl)
Clientes_Oro <- read_excel("D:/OneDrive - Muebles Jamar/2019/BD Clientes Oro y Añorados Cumpleaños Septiembre.xlsx", 
                           col_types = c("text", "text", "text", "text", "numeric", "text", "blank"))

library(sqldf)

Clientes_Oro_OP=sqldf("select t1.*, t2.REM, t2.EMI, t2.EST from Clientes_Oro t1 left join Clientes_OP t2 on t1.CEDULA=t2.N_IDE")

mydata=Clientes_Oro_OP
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
Clientes_Oro_OP=mydata
rm(list=c("mydata"))

Clientes_Oro_OP$EST <- with(Clientes_Oro_OP, factor(EST, levels=c('F','D','G','A','C','X'), ordered=TRUE))

Clientes_Oro_OP = with(Clientes_Oro_OP,Clientes_Oro_OP[order(CEDULA,EST, decreasing=FALSE),])

library(xlsx)
write.xlsx2(x=Clientes_Oro_OP,file="D:/OneDrive - Muebles Jamar/2019/BD Clientes Oro y Anhorados Cumpleanhos Septiembre.xlsx",sheetName="Clientes_Oro_OP",append=TRUE ,row.names=FALSE)


library(readxl)
BD_Clientes_Anhorados_Septiembre <- read_excel("D:/OneDrive - Muebles Jamar/2019/BD Clientes Oro y Anhorados Cumpleanhos Septiembre.xlsx", 
                                               sheet = "ANHORADOS", col_types = c("text", 
                                                                                  "text", "text", "text", "numeric","text", "blank"))

Clientes_Anhorados_OP=sqldf("select t1.*, t2.REM, t2.EMI, 
t2.EST from BD_Clientes_Anhorados_Septiembre t1 left join Clientes_OP t2 on t1.CEDULA=t2.N_IDE")

write.xlsx2(x=Clientes_Anhorados_OP,file="D:/OneDrive - Muebles Jamar/2019/BD Clientes Oro y Anhorados Cumpleanhos Septiembre.xlsx",sheetName="Clientes_Anhorados_OP",append=TRUE ,row.names=FALSE)


###################################################################################
##### INCLUIR UN CAMPO NUEVO A UN ARCHIVO EXTERNO A PARTIR DE LA BD ORACLE: #######
###################################################################################


library(readxl)
Anhorados_telemercadeo_Sept <- read_excel("D:/OneDrive - Muebles Jamar/2019/Anhorados telemercadeo que no reclamaron obsequio - Sept.xlsx", 
                                          sheet = "Hoja1", col_types = c("text", 
                                                                         "text", "text", "text", "date", "text", 
                                                                         "text", "date", "text", "text"))
View(Anhorados_telemercadeo_Sept)

library(RODBC)

Anhorados_telemercadeo_Sept$TELEFONOS = rep("",nrow(Anhorados_telemercadeo_Sept))
Query=ifelse(is.na(Anhorados_telemercadeo_Sept$CEDULA)==FALSE,paste("select CLIENTES_TELEFONOS from CLIENTES where CLIENTE_CODIGO='", Anhorados_telemercadeo_Sept$CEDULA,"'", sep = "" ),Anhorados_telemercadeo_Sept$TELEFONOS)

### Función para ejecutar Query

Consulta=function(query){
  #require(RODBC) 
  if(query!=""){
    OCUPACION=as.character(sqlQuery(DWJAMAR_Connection,query)[1,1])
  }else{
    OCUPACION=query
  }
  return(OCUPACION)
}

## PRUEBA CON UN SOLO REGISTRO
DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
library(RODBC)
Consulta(Query[1])
#S=lapply(Query,Consulta)
odbcClose(DWJAMAR_Connection)

DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
library(RODBC)
Anhorados_telemercadeo_Sept$TELEFONOS=unlist(lapply(Query,Consulta))
odbcClose(DWJAMAR_Connection)

library(xlsx)
write.xlsx2(x=Anhorados_telemercadeo_Sept,file="D:/OneDrive - Muebles Jamar/2019/Anhorados telemercadeo que no reclamaron obsequio - Sept.xlsx",sheetName="TELEFONOS2",append=TRUE ,row.names=T)


#######################################################################
####### CRECIMIENTO PPTO PARA UN MES  vs VENTA REAL AÑO ANTERIOR ######
#######################################################################

### Extracción del presupuesto:

Consulta_PPTO="select ut.unidad_ano, ut.unidad_nombre_mes, ppto.ppto_fact_agencia, ag.agencia_descripcion
       /*,ppto.ppto_fact_tipo_venta*/
       /*,tc.tcre_codigo*/
       ,tc.tcre_tipo
       ,round(sum(ppto.ppto_fact_valor_venta_neta)/1000000,1) as ppto_vn
       ,round(sum(ppto.ppto_fact_valor_venta_oferta)/1000000,1) as ppto_vo    
       ,round(sum(ppto.ppto_fact_valor_venta_full)/1000000,1) as ppto_vf
  from gi_fact_ppto_facturas ppto
       ,gi_agencias ag
       ,gi_unidad_tiempo ut
       ,gi_tipo_credito   tc
 where ppto.ppto_fact_unidad = 20191130
   and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
   and tc.tcre_id= ppto.ppto_fact_tipo_venta
   and ppto.ppto_fact_agencia = ag.agencia_id
   and ppto.ppto_fact_unidad=ut.unidad_id
   and ag.agencia_pais=1
 group by ut.unidad_ano,ut.unidad_nombre_mes,ppto.ppto_fact_agencia,ag.agencia_descripcion,
 /*ppto.ppto_fact_tipo_venta,*//*tc.tcre_codigo,*/tc.tcre_tipo"

library(RODBC)

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
PPTO_MES_TIENDAS=sqlQuery(jdbcConnection,Consulta_PPTO, dec=",",stringsAsFactors=F) 
odbcClose(jdbcConnection)

#### VENTAS AÑO ANTERIOR

Consulta_ventas="
select t.unidad_ano
,t.unidad_mes
--t.unidad_num_dia_mes, -- para el día del mes
--tc.tcre_codigo,  
,a.agencia_id
,a.agencia_descripcion    
,tc.tcre_tipo
-- count(distinct fact.fact_cliente) Can_Clientes,
,round(sum(fact.fact_valor_venta_neta) / 1000000,1) Venta_Neta
,round(sum(fact.fact_valor_venta_oferta) / 1000000,1) Venta_Oferta
,round(sum(fact.fact_valor_venta_full) / 1000000,1) Venta_Full
from gi_fact_facturas@datamart fact,
gi_unidad_tiempo@datamart t,
gi_tipo_credito@datamart  tc,
gi_agencias@datamart      a
where fact.fact_unidad = t.unidad_id
and fact.fact_tipo_venta = tc.tcre_id
and fact.fact_agencia = a.agencia_id
and t.unidad_ano in (2018)--,2017,2016,2019)--,2016,2017)
and t.unidad_mes in (/*1,2,3,*/11 /*5, 6, 7, 8, 9,10,11,12*/)
and a.agencia_pais = 1
--and  t.unidad_num_dia_mes in (2,3,4,5,6,7)
and a.agencia_descripcion not in ('PUBLICIDAD','OPERACIONES','UP STREAM','MARCA') -- Ojo, estos cuatro deben excluirse para que la cifra anual coincida con la TABLEAU server
--and a.agencia_descripcion in ('SANTA MARTA')--('CONCESIONES HIPER JAMAR','EXPERIMENTOS')
and tc.tcre_codigo in ('CR', 'OR', 'TJ', 'CO', 'PT', 'NO') -- Las tres últimas son para contado
group by t.unidad_ano
,t.unidad_mes,a.agencia_id
,a.agencia_descripcion 
--, t.unidad_num_dia_mes
--,tc.tcre_codigo
,tc.tcre_tipo      
--,a.agencia_descripcion
order by t.unidad_ano"

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
VENTAS_MES_TIENDAS=sqlQuery(jdbcConnection,Consulta_ventas, dec=",",stringsAsFactors=F) 
odbcClose(jdbcConnection)

### CRUCE DE LAS DOS TABLAS:

library(sqldf)

PPTO_vs_VENTA=sqldf("
select t1.UNIDAD_ANO as YEAR,t1.UNIDAD_NOMBRE_MES as MES,t1.AGENCIA_DESCRIPCION as TIENDA,t1.TCRE_TIPO as TIPO_VENTA,
t1.PPTO_VN as PPTO_VN_2019,t2.VENTA_NETA as VENTA_NETA_2018,t1.PPTO_VN - t2.VENTA_NETA as VAR_MILLONES,t1.PPTO_VN/t2.VENTA_NETA-1 
as VAR_PORCENTUAL from PPTO_MES_TIENDAS t1 
left join VENTAS_MES_TIENDAS t2 on t1.PPTO_FACT_AGENCIA||t1.TCRE_TIPO=t2.AGENCIA_ID||t2.TCRE_TIPO 
order by t1.AGENCIA_DESCRIPCION, t1.TCRE_TIPO 
                    ")
library(Rcmdr)

PPTO_vs_VENTA_MT <- na.omit(PPTO_vs_VENTA)


library(xlsx)
write.xlsx2(x=PPTO_vs_VENTA_MT,
file="D:\\OneDrive - Muebles Jamar\\Reporte de cumplimiento diario\\PRESUPUESTO TIENDAS 2019.xlsx",sheetName="NOVIEMBRE",append=TRUE ,row.names=T)


######################################################################################################
################## VENTA POR ZONAS GEOGRÁFICAS: PARTICIPACIÓN DE CADA TIENDA POR BARRIO ##############
######################################################################################################

ParticipacionTiendasPorBarrio_QRY="
select distinct ut.unidad_ano,
 --ag.agencia_pais,
 --ut.unidad_mes,
 --ut.unidad_fecha,
 --ut.unidad_nombre_dia,
 --tc.tcre_codigo alt_vta,
 --a.accion_codigo a_s,
 --zg.estrato,
 --cli2.cliente_ocup,
 --cli2.cliente_calfdatacredito,
 --op.fact_ord_num_cuotas,
 --op.fact_ord_estado,
 --op.fact_ord_hora,
 --zg.dpto_descripcion,
 zg.ciud_descripcion
  ,zg.zona_codigo
 ,zg.barr_descripcion 
 ,ag.agencia_descripcion
 --count(distinct op.fact_ord_cliente) Cant_Clientes,
 --sum(op.fact_valor_venta_oferta/*fact_ord_valor_venta_oferta*/) / 1000000 Fac_Oferta,
 ,ROUND(sum(op.fact_valor_venta_neta/*fact_ord_valor_venta_neta*/) / 1000000,6) Fac_Neta
 --sum(op.fact_valor_venta_full/*fact_ord_valor_venta_full*/) / 1000000 Fac_Full
 from /*gi_fact_orden_pedido*/ gi_fact_facturas op,
 gi_unidad_tiempo     ut,
 --gi_tipo_credito      tc,
 gi_agencias          ag,
 --gi_accion_sugerida   a,
 gi_clientes          cli,
 clientes             cli2,
 zona_geografica      zg
 where op.fact_unidad = ut.unidad_id
 --and op.fact_ord_tipo_venta = tc.tcre_id
 and op.fact_agencia = ag.agencia_id -- Esta tabla contiene el pais para efectos de filtrar Colombia
 --and op.fact_ord_accion_sugerida = a.accion_id
 and op.fact_cliente = cli.cliente_id
 and cli.cliente_codigo = cli2.cliente_codigo
 and cli2.cliente_zona=zg.zona_codigo
 --and ag.agencia_zona = zg.zona_id
 and ut.unidad_ano in (/*2019,*/ 2019/*, 2017*/)
 --and ut.unidad_mes in (6)
 and ag.agencia_pais = 1
 --and op.fact_ord_estado='FACTURADA'
 --and zg.ciud_descripcion ='BARRANQUILLA'
 --and zg.estrato in   ('3', '2', '4')
 --and ag.agencia_descripcion='PRINCIPAL'
 and ag.agencia_descripcion in 
 ('BOSA','CUATRO VIENTOS','ENSUENO CC','LA PLAZUELA','LAS AMERICAS CC','SAN FELIPE','SAN FELIPE CONCESIONES','SINCELEJO','SINCELEJO CONCESIONES','SUBA')  /*('PUBLICIDAD', 'OPERACIONES', 'UP STREAM', 'MARCA','E-COMMERCE')*/
 and zg.ciud_descripcion IN  ('CARTAGENA', 'BOGOTA', 'BOGOTA, D.C.', 'BOGOTÁ D.C', 'CARTAGENA DE INDIAS','SINCELEJO')
 and zg.barr_descripcion is not NULL
 group by ut.unidad_ano,
 --ag.agencia_pais,
 --ut.unidad_mes,
 --ut.unidad_fecha,
 --ut.unidad_nombre_dia,
 --tc.tcre_codigo /*alt_vta*/,
 --a.accion_codigo /*a_s*/,
 --zg.estrato
 --cli2.cliente_ocup,
 --cli2.cliente_calfdatacredito,
 --op.fact_ord_num_cuotas,
 --op.fact_ord_estado,
 --op.fact_ord_hora,
 --zg.dpto_descripcion,
 zg.ciud_descripcion
  ,zg.zona_codigo
 ,zg.barr_descripcion  
 ,ag.agencia_descripcion       
 --,zg.barr_descripcion
 ORDER BY zg.ciud_descripcion
 ,zg.barr_descripcion
 ,ROUND(sum(op.fact_valor_venta_neta/*fact_ord_valor_venta_neta*/)/1000000,6) desc"

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
ParticipacionTiendasPorBarrio_tbl=sqlQuery(jdbcConnection,ParticipacionTiendasPorBarrio_QRY, dec=",",stringsAsFactors=T) 
odbcClose(jdbcConnection)

describe(ParticipacionTiendasPorBarrio_tbl)
levels(ParticipacionTiendasPorBarrio_tbl$CIUD_DESCRIPCION)[1]="BOGOTA, D.C."
levels(ParticipacionTiendasPorBarrio_tbl$AGENCIA_DESCRIPCION)[7]="SAN FELIPE"
levels(ParticipacionTiendasPorBarrio_tbl$AGENCIA_DESCRIPCION)[8]="SINCELEJO"

### Tabla de totales por barrios:

library(sqldf)
ParticipacionPorBarrio_tbl=sqldf("select UNIDAD_ANO,CIUD_DESCRIPCION,ZONA_CODIGO,BARR_DESCRIPCION,
                                 sum(FAC_NETA) as VENTA_NETA_TOT from ParticipacionTiendasPorBarrio_tbl group by
                                 UNIDAD_ANO,CIUD_DESCRIPCION,ZONA_CODIGO,BARR_DESCRIPCION")
#i=1
ParticipacionTiendasPorBarrio_tbl$Participacion=rep(0,dim(ParticipacionTiendasPorBarrio_tbl)[1])
for (i in 1:dim(ParticipacionTiendasPorBarrio_tbl)[1]) {
  ParticipacionTiendasPorBarrio_tbl$Participacion[i]=ParticipacionTiendasPorBarrio_tbl[i,"FAC_NETA"]/ParticipacionPorBarrio_tbl[(ParticipacionPorBarrio_tbl$ZONA_CODIGO==ParticipacionTiendasPorBarrio_tbl$ZONA_CODIGO[i]),"VENTA_NETA_TOT"]
}

ParticipacionTiendasPorBarrio_tbl$ranking=rep(0,dim(ParticipacionTiendasPorBarrio_tbl)[1])

for (i in 1:length(levels(ParticipacionTiendasPorBarrio_tbl$BARR_DESCRIPCION))) {
  ParticipacionTiendasPorBarrio_tbl[ParticipacionTiendasPorBarrio_tbl$ZONA_CODIGO==levels(ParticipacionTiendasPorBarrio_tbl$ZONA_CODIGO)[i],c("ranking")]=order(ParticipacionTiendasPorBarrio_tbl[ParticipacionTiendasPorBarrio_tbl$ZONA_CODIGO==levels(ParticipacionTiendasPorBarrio_tbl$ZONA_CODIGO)[i],c("Participacion")],decreasing = T)

}

ParticipacionTiendasPorBarrio_tbl <- within(ParticipacionTiendasPorBarrio_tbl, {
  ranking <- as.factor(ranking)
})

setwd("D:/OneDrive - Muebles Jamar/2020")

wb=createWorkbook()
addWorksheet(wb,"BARRIOS")
writeData(wb, sheet = "BARRIOS", x = ParticipacionTiendasPorBarrio_tbl)
saveWorkbook(wb,file="PARTICIPACIÓN DE CADA TIENDA POR BARRIO.xlsx",overwrite = T)





# prueba_ranking=head(ParticipacionTiendasPorBarrio_tbl,8)
# 
# prueba_ranking <- droplevels(prueba_ranking)
# 
# length(levels(prueba_ranking$BARR_DESCRIPCION))
# 
# prueba_ranking$ranking=rep(0,8)
# 
# i=2
# 
# for (i in 1:length(levels(prueba_ranking$BARR_DESCRIPCION))) {
#   prueba_ranking[prueba_ranking$BARR_DESCRIPCION==levels(prueba_ranking$BARR_DESCRIPCION)[i],c("ranking")]=order(prueba_ranking[prueba_ranking$BARR_DESCRIPCION==levels(prueba_ranking$BARR_DESCRIPCION)[i],c("Participacion")],decreasing = T)
#   
# }



# prueba_ranking[prueba_ranking$BARR_DESCRIPCION==levels(prueba_ranking$BARR_DESCRIPCION)[i],c("ranking")]
# 
# order(prueba_ranking[prueba_ranking$BARR_DESCRIPCION==levels(prueba_ranking$BARR_DESCRIPCION)[i],c("Participacion")], decreasing =F)



######################################################################################################
################## Coparación presupuestos vs venta neta real para MAYO en 2016 - 2019  ##############
######################################################################################################

## Paquetes:

library(RODBC)
library(lubridate)
library(sqldf)

###  Parámetros: #############

### Fecha: Por defecto el día anterior a la fecha actual de ejcución 

FechaCorte=as.character(Sys.Date() %m-% days(1))
VectorFecha=strsplit(FechaCorte, "-")[[1]]

### Datos de Conexion:
servidor = "DATANEW_JAMAR" # Nombre de la conexión ODBC como se creó en el equipo donde se va a ejecutar
usuario =  "dwjamar"
clave =    "dwjamar"


## Presupuesto AÑOs anteriores:

PPTO_ANO_ANTERIOR_TIENDAS_QUERY=paste0("select ut.unidad_ano,
                                       ag.agencia_descripcion as NOMBRE_TIENDA_PPTO,
                                       tc.tcre_tipo,
                                       ag.agencia_codigo      as COD_AGENCIA,
                                       ut.unidad_mes,
                                       round(sum(ppto.ppto_fact_valor_venta_neta) / 1000000, 1) as PPTO_VENTA_NETA
                                       from gi_fact_ppto_facturas ppto,
                                       gi_agencias           ag,
                                       gi_unidad_tiempo      ut,
                                       gi_tipo_credito       tc
                                       where ut.unidad_ano <=2019
                                       and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                                       and tc.tcre_id = ppto.ppto_fact_tipo_venta
                                       and ppto.ppto_fact_agencia = ag.agencia_id
                                       and ppto.ppto_fact_unidad = ut.unidad_id
                                       and ag.agencia_pais = 1
                                       and ag.agencia_codigo not in ('Z1','Z2')
                                       group by ut.unidad_ano, 
                                       ag.agencia_descripcion,
                                       tc.tcre_tipo,
                                       ag.agencia_codigo,
                                       ut.unidad_mes")

DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
PPTO_ANO_ANTERIOR_TIENDAS_TBL=sqlQuery(DWJAMAR_Connection,PPTO_ANO_ANTERIOR_TIENDAS_QUERY,dec=",")
odbcClose(DWJAMAR_Connection)

###################################################################
############## An Introduction to emojifont package: ##############
###################################################################

library(emojifont)
library(ggplot2)

search_emoji('smile')

ggplot() + geom_emoji("rose", color='steelblue') + theme_void()

x = seq(0, 8*pi, length=60)
y = exp(x/5)+x*sin(x)
ggplot() + geom_emoji('heartbeat', x=x, y=y, size=7)





library(colorspace, pos=17)
with(iris, pie(table(Species), labels=levels(Species), xlab="", ylab="", 
               main="Species", col=palette()[2:4]))



####### RETICULATE:
library(reticulate)

```{python, eval=TRUE}
import numpy as np
x = np.pi
y = np.sin(x/4)
print(y)
```





