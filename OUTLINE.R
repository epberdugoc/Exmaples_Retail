
TIPO_CLIENTE.2=as.character(SegmentacionCorporativa_junio_2019$TIPO_CLIENTE)

attach(SegmentacionCorporativa_junio_2019)
TIPO_CLIENTE.2=ifelse((TIPO_CLIENTE=='MIXTO')&(PORC_CONTADO>50),'SOLO CONTADO',TIPO_CLIENTE.2)
TIPO_CLIENTE.2=ifelse((TIPO_CLIENTE=='MIXTO')&(PORC_CONTADO<=50),'SOLO CREDITO',TIPO_CLIENTE.2)
detach(SegmentacionCorporativa_junio_2019)

TIPO_CLIENTE.2=as.factor(TIPO_CLIENTE.2)

SEGMETO_TIPO=data.frame('seg'=SegmentacionCorporativa_junio_2019$SEGMENT_CORP_PRINC,TIPO_CLIENTE.2)


Dataset <-   readXL("D:/OneDrive - Muebles Jamar/Users/eberdugo/Documents/LISTADO NO FACTURADO CON DIRECCIÓN JUNIO.xlsx",
                    rownames=FALSE, header=TRUE, na="", sheet="Junio 13", 
                    stringsAsFactors=TRUE)


library(sqldf)

sqldf("select ESTADO_OP, sum(VALOR_OP) from  Total_OP_Brilla group by  ESTADO_OP")

######### CONSULTA 

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
Info_OROS=sqlQuery(jdbcConnection, 
         paste("SELECT
  CLIENTES.CLIENTE_CODIGO,
  CLIENTES.CLIENTE_DIR
FROM
  CLIENTES
WHERE   
  CLIENTES.CLIENTE_CODIGO IN (select CLIENTE_CODIGO from clientes_campanas where cliente_campana = 'ORO_SAN_FELIPE_24_31_JUL_2019')
  ",sep="")
         , dec=",")
odbcClose(jdbcConnection)

Info_OROS$CLIENTE_CODIGO=as.character(Info_OROS$CLIENTE_CODIGO)

library(sqldf)

BaseFinal=sqldf("select t1.*,t2.CLIENTE_DIR from EVENTO_ORO_SAN_FELIPE t1 
      left join Info_OROS t2 on  t1.CEDULA=t2.CLIENTE_CODIGO")

write.xlsx2(x=BaseFinal,file="D:\\OneDrive - Muebles Jamar\\2019\\BD TELE CR EVENTO ORO SAN FELIPE 24 AL 31 JUL 2019.xlsx",sheetName="Base_con_direcciones",append=TRUE ,row.names=FALSE)


############## FINES DE SEMANA COLCHONES ################
library(readxl)
Fines_de_semana_colchones <- read_excel("D:/OneDrive - Muebles Jamar/2019/Fines de semana colchones.xlsx", 
                                        col_types = c("date", "text", "numeric", 
                                                      "numeric"))
View(Fines_de_semana_colchones)


Fecha_agrupada=rep(Fines_de_semana_colchones$Fecha[2],dim(Fines_de_semana_colchones)[1])


Fecha_agrupada=ifelse(Fines_de_semana_colchones$Nombre_Dia=='VIE',(Fines_de_semana_colchones$Fecha+86400),Fecha_agrupada)
Fecha_agrupada=ifelse(Fines_de_semana_colchones$Nombre_Dia=='SAB',(Fines_de_semana_colchones$Fecha),Fecha_agrupada)
Fecha_agrupada=ifelse(Fines_de_semana_colchones$Nombre_Dia=='DOM',(Fines_de_semana_colchones$Fecha-86400),Fecha_agrupada)

Fecha_agrupada=as.Date(Fecha_agrupada/86400, origin = "1970-01-01")

Fines_de_semana_colchones$Fecha_agrupada=Fecha_agrupada

library(sqldf)

Ranking_FDS_colch=sqldf("select Fecha_agrupada, sum([No_ Clientes_OP]) AS N_clientes, 
      sum(Valor_Venta_Oferta) AS Venta_Oferta, sum(Valor_Venta_Oferta)/sum([No_ Clientes_OP]) AS TICKET_MEDIO from Fines_de_semana_colchones group by Fecha_agrupada")

write.xlsx2(x=Ranking_FDS_colch,file="D:/OneDrive - Muebles Jamar/2019/Fines de semana colchones.xlsx",sheetName="Ranking_OP",append=TRUE ,row.names=FALSE)


######### BARRIOS DE INFLUENCIA NUEVA TIENDA BUCARAMANGA #######

library(readxl)
Barrios_de_influencia_nueva_tienda_Bucaramanga <- read_excel("D:/OneDrive - Muebles Jamar/2019/Barrios de influencia nueva tienda Bucaramanga.xls", 
                                                             sheet = "Zona de influencia")
View(Barrios_de_influencia_nueva_tienda_Bucaramanga)

library(RODBC)

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
CLIENTES_BUC=sqlQuery(jdbcConnection, 
                   "select TIPO_BD,SEGMENTO_ME,DPTO,CIUDAD,BARRIO,DIRECCION,CEDULA,NOMBRE_COMPLETO,CELULAR,CEL1,EMAIL
                    from mercadeo_allinone_mes
                   where CIUDAD='BUCARAMANGA' and CEDULA  
not in (select CLIENTE_CODIGO from clientes_campanas  where cliente_campana='NO_ENVIAR' and CLIENTE_CODIGO is not NULL)"
                   , dec=",")
odbcClose(jdbcConnection)

library(sqldf)

CLIENTES_BUC_ZI=sqldf("select * from CLIENTES_BUC where trim(BARRIO) in (select [Nombre del Barrio] from Barrios_de_influencia_nueva_tienda_Bucaramanga)")


library(xlsx)
write.xlsx2(x=CONSOLIDADO_NPS_ATLTCO_y_BOLIV,file="D:/OneDrive - Muebles Jamar/Prueba_R..xlsx",sheetName="CLIENTES",append=TRUE ,row.names=FALSE, password = "Abdbsdjdd")


system.time({  length(rnorm(100))   })



####### SPARK ######

cars <- copy_to(sc, mtcars)

library(DBI)
dbGetQuery(sc, "SELECT count(*) FROM mtcars")


###### cálculos:


(ppto_TOT_ene_2020_MT=20104+603)
(ppto_TOT_ene_2019_MT=18496)

(ppto_CR_ene_2020_MT=12748+340)

(ppto_CO_ene_2020_MT=ppto_TOT_ene_2020_MT-ppto_CR_ene_2020_MT)

(VN_TOT_ene_2019_MT = 19980)

######################################################################################
####### OP GENERADAS EN FERIA DEL CRÉDITO 2019, BENEFICIO 25% DCTO CONTADO ###########
######################################################################################



OP_QRY="SELECT
  UNIDAD_TIEMPO.UNIDAD_FECHA,
TO_CHAR(TRIM(CLIENTES.CLIENTE_CODIGO)) AS CLIENTE_CODIGO,
CLIENTES.CLIENTE_NOMBRE,
ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
CLIENTES.CLIENTE_ESTCIVIL,
ORDEN_PEDIDO.ORDEN_NUMERO,
ALTERNATIVA_VENTA.ALT_CODIGO,
CLIENTES.CLIENTE_OCUP,
ORDEN_PEDIDO.ORDEN_ESTADO,
sum(ORDEN_PEDIDO.Orden_ingreso) as VALOR_VO
FROM
UNIDAD_TIEMPO,
CLIENTES,
ZONA_GEOGRAFICA,
ORDEN_PEDIDO,
ALTERNATIVA_VENTA
WHERE
( CLIENTES.CLIENTE_ZONA=ZONA_GEOGRAFICA.ZONA_CODIGO  )
AND  ( ORDEN_PEDIDO.ORDEN_UNIDAD=UNIDAD_TIEMPO.UNIDAD_CODIGO  )
AND  ( ORDEN_PEDIDO.ORDEN_CLIENTE=CLIENTES.CLIENTE_CODIGO  )
AND  ( ORDEN_PEDIDO.ORDEN_ALTVTA=ALTERNATIVA_VENTA.ALT_CODIGO  )
AND  (
UNIDAD_TIEMPO.UNIDAD_FECHA  BETWEEN  '01-04-2019' AND '30-06-2019'
)
GROUP BY
UNIDAD_TIEMPO.UNIDAD_FECHA, 
CLIENTES.CLIENTE_CODIGO, 
CLIENTES.CLIENTE_NOMBRE, 
ZONA_GEOGRAFICA.DPTO_DESCRIPCION, 
ZONA_GEOGRAFICA.CIUD_DESCRIPCION, 
CLIENTES.CLIENTE_ESTCIVIL, 
ORDEN_PEDIDO.ORDEN_NUMERO, 
ALTERNATIVA_VENTA.ALT_CODIGO, 
CLIENTES.CLIENTE_OCUP, 
ORDEN_PEDIDO.ORDEN_ESTADO"

## Tabla de OP's

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
OP_FeriaDelCredito_2019_TBL=sqlQuery(jdbcConnection,OP_QRY, dec=",", as.is=T)
odbcClose(jdbcConnection)

## Importar tabla con los nombres de los clientes a quienes se les envió el correo dirigido:

library(readxl)
Bd_contado_dcto_25_por_ciento_FERIA_DEL_CREDITO_2019 <- read_excel("Bd contado dcto 25 por ciento FERIA DEL CREDITO 2019.xlsx", 
                                                                   col_types = c("text", "text"))
View(Bd_contado_dcto_25_por_ciento_FERIA_DEL_CREDITO_2019)

## Cruce con las OP:

library(sqldf)

Activacion_OP_FeriaDelCredito_2019_TBL=sqldf(
  "select t1.*,t2.CEDULA 
   from  OP_FeriaDelCredito_2019_TBL t1
   inner join Bd_contado_dcto_25_por_ciento_FERIA_DEL_CREDITO_2019 t2
   on t1.CLIENTE_CODIGO=t2.CEDULA
  "
)

setwd("D:/OneDrive - Muebles Jamar/2020")
library(xlsx)
write.xlsx2(x=Activacion_OP_FeriaDelCredito_2019_TBL,file="Bd contado dcto 25 por ciento FERIA DEL CREDITO 2019.xlsx",sheetName="ACTIVACION",append=TRUE ,row.names=FALSE)



############ RESHAPE: ######################################

## an example that isn't longitudinal data
state.x77 <- as.data.frame(state.x77)
long <- reshape(state.x77, idvar = "state", ids = row.names(state.x77),
                times = names(state.x77), timevar = "Characteristic",
                varying = list(names(state.x77)), direction = "long")

reshape(long, direction = "wide")

reshape(long, direction = "wide", new.row.names = unique(long$state))


########################################################################################################
######################### DIA DE LA SEMANA EQUIVALENTE AÑO ANTEIOR #####################################
########################################################################################################

FechaCorte= as.character(Sys.Date() %m-% days(1))   #as.character(Sys.Date() %m-% days(1)) %m-% years(1) %m-% months(1) 
(VectorFecha=strsplit(FechaCorte, "-")[[1]])


Fecha_Equivalente=function( fecha_referencia=as.character( Sys.Date() %m-% days(1)) ){
  
fecha_objetivo=as.Date.character(fecha_referencia)
  
require(lubridate)

Mes_actual=data.frame("fecha"=seq(floor_date(fecha_objetivo, "month"),length.out = days_in_month(fecha_objetivo),by=1 ),
           "dia_semana"=weekdays(seq(floor_date(fecha_objetivo, "month"),length.out = days_in_month(fecha_objetivo),by=1 ))
           ) 

attach(Mes_actual) #"fecha" ,warn.conflicts = T
  (Secuencia_dias= Mes_actual[dia_semana == weekdays(fecha_objetivo),    ])
detach(Mes_actual)

(indice=which(Secuencia_dias$fecha==fecha_objetivo))


Mes_anterior=data.frame("fecha"=seq(floor_date(fecha_objetivo %m-% years(1), "month"),length.out = days_in_month(fecha_objetivo%m-% years(1)),by=1 ),
                      "dia_semana"=weekdays(seq(floor_date(fecha_objetivo%m-% years(1), "month"),length.out = days_in_month(fecha_objetivo%m-% years(1)),by=1 ))
)


attach(Mes_anterior) # , warn.conflicts = T
(fecha_arranque=min(Mes_anterior[dia_semana == weekdays(fecha_objetivo),"fecha"    ]))
detach(Mes_anterior)

print(cbind(Mes_actual,Mes_anterior))

fecha_equivalente = fecha_arranque %m+% days(7*(indice-1))

return(fecha_equivalente)

}

# (comparacion=cbind(Mes_actual,Mes_anterior)) 

#weekdays(seq(floor_date(Sys.Date() %m-% years(1), "month"),length.out = days_in_month(Sys.Date()),by=1 ))



#################  REFORMULACIÓN Y CORRECIÓN FINAL ##################################

## NOTA: Se requiere tener instalado el paquete "lubridate"

Fecha_Equivalente=function( fecha_referencia=as.character( Sys.Date() %m-% days(1)) ){
  
                   fecha_objetivo=as.Date.character(fecha_referencia)
  
                   require(lubridate)


                   (fecha_arranque_pasada= fecha_objetivo %m-% years(1) %m-% days(1))


                    periodo_anterior=data.frame("fecha"=seq(fecha_arranque_pasada,length.out = 8,by=1 ),
                        "dia_semana"=weekdays(seq(fecha_arranque_pasada,length.out = 8,by=1 ))
                      )


                        attach(periodo_anterior) # , warn.conflicts = T
                        (fecha_equivalente_pasada=min(periodo_anterior[dia_semana == weekdays(fecha_objetivo),"fecha"]))
                        detach(periodo_anterior)
                      
                    return(c(fecha_equivalente_pasada))

}

### ejemplos: Las fechas deben ir tipo texto en el formato año-mes-dia

Fecha_Equivalente("2020-03-21"); weekdays(Fecha_Equivalente("2020-03-21"))

Fecha_Equivalente("2020-02-29");weekdays(Fecha_Equivalente("2020-02-29"))

########################################################################################################################


