#### INVERCOMOLBIA ALIANZAS


## Lectura de datos:

library(readxl)
Datos_alianzas <- read_excel("D:/OneDrive - Muebles Jamar/2019/Alianzas/2 BASES JAMAR PROYECTOS - JUNIO - JULIO 2019.xlsx")
View(Datos_alianzas)

## MACROVARIABLES:

ID_col="Cedula"
Mes=""

## conversion de las variables tipo caracter a FACTOR:

mydata=Datos_alianzas

mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)

Datos_alianzas=mydata

rm(list=c("mydata"))

## Balance de valores perdidos para Clientes_ID
library(Hmisc)

Clientes_ID=Datos_alianzas[,ID_col]
names(Clientes_ID)="CLIENTE_CODIGO"

describe(Clientes_ID)

## Eliminando cedulas perdidas
Clientes_ID=na.omit(Clientes_ID)

## Inserción de registros:

TablaMadre="Clientes_ID"
TablaObjetivo="clientes_campanas"
Cliente_Campanha="ALIANZAS_JUN_JUL_2019"
#i=1
#rm(i)
library(RODBC)
ti1 <- Sys.time()
attach(get(TablaMadre))
for (i in 1:dim(get(TablaMadre))[1]) {
  jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
  
  
  sqlQuery(jdbcConnection, 
           paste("insert into ",TablaObjetivo," (CLIENTE_CODIGO,CLIENTE_CAMPANA) values 
                 ('",CLIENTE_CODIGO[i] ,"','",Cliente_Campanha,"')",sep="")
           , dec=",")
  
  
  odbcClose(jdbcConnection)
  
}
detach(get(TablaMadre))

ti2 <- Sys.time()
(delta_t=as.numeric(difftime(ti2, ti1), units="mins"))

### COMPROBACIÓN:

# CONSULTA:
jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
sqlQuery(jdbcConnection, 
         paste("select count(CLIENTE_CODIGO) from clientes_campanas where CLIENTE_CAMPANA='ALIANZAS_JUN_JUL_2019' ",sep="")
         , dec=",")
odbcClose(jdbcConnection)

## CRUCE:

Query_alianzas="
SELECT
CLIENTES.CLIENTE_CODIGO,
CLIENTES.CLIENTE_NOMBRE,
UNIDAD_TIEMPO.UNIDAD_FECHA,
ALTERNATIVA_VENTA.ALT_CODIGO,
ORDEN_PEDIDO.ORDEN_ESTADO,
sum(ORDEN_PEDIDO.Orden_ingreso)
FROM
CLIENTES,
UNIDAD_TIEMPO,
ALTERNATIVA_VENTA,
ORDEN_PEDIDO
WHERE
( ORDEN_PEDIDO.ORDEN_UNIDAD=UNIDAD_TIEMPO.UNIDAD_CODIGO  )
AND  ( ORDEN_PEDIDO.ORDEN_CLIENTE=CLIENTES.CLIENTE_CODIGO  )
AND  ( ORDEN_PEDIDO.ORDEN_ALTVTA=ALTERNATIVA_VENTA.ALT_CODIGO  )
AND  (
UNIDAD_TIEMPO.UNIDAD_FECHA  BETWEEN  '01-09-2018 12:00:00' AND '18-10-2018 12:00:00'
AND  CLIENTES.CLIENTE_CODIGO  IN  (SELECT
CLIENTES_CAMPANAS.CLIENTE_CODIGO
FROM
CLIENTES_CAMPANAS
WHERE
( 
CLIENTES_CAMPANAS.CLIENTE_CAMPANA  =  'CRUCE ALIANZAS OCT 22 2018'
)
)
AND  ORDEN_PEDIDO.ORDEN_ESTADO  =  'FACTURADA'
)
GROUP BY
CLIENTES.CLIENTE_CODIGO, 
CLIENTES.CLIENTE_NOMBRE, 
UNIDAD_TIEMPO.UNIDAD_FECHA, 
ALTERNATIVA_VENTA.ALT_CODIGO, 
ORDEN_PEDIDO.ORDEN_ESTADO"



