
########### AUTOMATIZACIÓN CONSULTAS DE TRÁFICO ###############

### librerias requeridas:

library(RODBC)
library(lubridate)
library(sqldf)

##############################
###  Parámetros: #############
##############################

### Fecha: Por defecto el día anterior a la fecha actual de ejcución 

FechaCorte="2020-03-24"    #as.character(Sys.Date() %m-% days(1))   #as.character(Sys.Date() %m-% days(1)) %m-% years(1) %m-% months(1) 
(VectorFecha=strsplit(FechaCorte, "-")[[1]])

### Datos de Conexion:
servidor = "DATANEW_JAMAR" # Nombre de la conexión ODBC como se creó en el equipo donde se va a ejecutar
usuario =  "dwjamar"
clave =    "dwjamar"

###### TABLA MISMAS TIENDAS #####################

MISMAS_TIENDAS_QRY=paste0(
          "select ag.agencia_id
                  ,ag.AGENCIA_CODIGO
                  ,ag.agencia_descripcion
                  from gi_fact_ppto_facturas@datamart ppto
                  ,gi_agencias@datamart ag
                  ,gi_unidad_tiempo@datamart ut
                  ,gi_tipo_credito@datamart   tc
                  where ut.unidad_mes=",VectorFecha[2],"
                  and ut.unidad_ano=",VectorFecha[1],"
                  --ppto.ppto_fact_unidad = 20200229 -- ultimo día del mes cuyo presupuesto se quiere conocer
                  and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                  and  (ag.agencia_descripcion in
                  ( 
                  select ag.agencia_descripcion 
                  from gi_fact_ppto_facturas@datamart ppto,
                  gi_agencias@datamart           ag,
                  gi_unidad_tiempo@datamart      ut,
                  gi_tipo_credito@datamart       tc
                  where ppto.ppto_fact_unidad = 20190228 -- ultimo día del mes cuyo presupuesto se quiere conocer
                  and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                  and tc.tcre_id = ppto.ppto_fact_tipo_venta
                  and ppto.ppto_fact_agencia = ag.agencia_id
                  and ppto.ppto_fact_unidad = ut.unidad_id
                  and ag.agencia_pais = 1
                  group by ag.agencia_descripcion
                  ) or ag.agencia_id=329)  
                  and tc.tcre_id= ppto.ppto_fact_tipo_venta
                  and ppto.ppto_fact_agencia = ag.agencia_id
                  and ppto.ppto_fact_unidad=ut.unidad_id
                  and ag.agencia_pais=1
                  group by 
                  ag.agencia_id,
                  ag.AGENCIA_CODIGO,
                  ag.agencia_descripcion ORDER BY ag.agencia_descripcion
            
                  --UNION select ag.agencia_nombre, ag.agencia_id from  gi_agencias ag where 
          ")


DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
(MISMAS_TIENDAS=sqlQuery(DWJAMAR_Connection,MISMAS_TIENDAS_QRY))
odbcClose(DWJAMAR_Connection)



############### PRESUPUESTO DEL MES POR MODALIDAD DE COMPRA Y TIPO DE VENTA ## as.character(as.numeric(VectorFecha[1])-1)

PPTO_TOTAL_QUERY=paste0("select ut.unidad_ano,
                        ut.unidad_nombre_mes,
                        --ppto.ppto_fact_tipo_venta,
                        --tc.tcre_codigo,
                        tc.tcre_tipo,
                        round(sum(ppto.ppto_fact_valor_venta_neta) / 1000000, 1) as ppto_vn,
                        round(sum(ppto.ppto_fact_valor_venta_oferta) / 1000000, 1) as ppto_vo,
                        round(sum(ppto.ppto_fact_valor_venta_full) / 1000000, 1) as ppto_vf
                        from gi_fact_ppto_facturas@datamart ppto,
                        gi_agencias@datamart           ag,
                        gi_unidad_tiempo@datamart      ut,
                        gi_tipo_credito@datamart       tc
                        where 
                            --ppto.ppto_fact_unidad = 20200131 -- ultimo día del mes cuyo presupuesto se quiere conocer
                        ut.unidad_mes=",VectorFecha[2],"
                        and ut.unidad_ano=",VectorFecha[1],"
                        and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                        --and ag.agencia_descripcion in
                        and tc.tcre_id = ppto.ppto_fact_tipo_venta
                        and ppto.ppto_fact_agencia = ag.agencia_id
                        and ppto.ppto_fact_unidad = ut.unidad_id
                        and ag.agencia_pais = 1   
                        group by ut.unidad_ano,
                        ut.unidad_nombre_mes,
                        --tc.tcre_codigo,
                        tc.tcre_tipo")

DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
sqlQuery(DWJAMAR_Connection,PPTO_TOTAL_QUERY,dec=",")
odbcClose(DWJAMAR_Connection)

################# PPTO TODAS LAS TIENDAS ######################################### 

PPTO_ACTUAL_TIENDAS_QUERY=paste0("select ut.unidad_ano,
                                 ut.unidad_nombre_mes,
                                 --ppto.ppto_fact_tipo_venta,
                                 --tc.tcre_codigo,
                                 tc.tcre_tipo,
                                 ag.agencia_descripcion,
                                 ag.agencia_id,
                                 ag.AGENCIA_CODIGO,
                                 round(sum(ppto.ppto_fact_valor_venta_neta) / 1000000, 1) as ppto_vn
                                 --,round(sum(ppto.ppto_fact_valor_venta_oferta) / 1000000, 1) as ppto_vo,
                                 --round(sum(ppto.ppto_fact_valor_venta_full) / 1000000, 1) as ppto_vf
                                 from gi_fact_ppto_facturas@datamart ppto,
                                 gi_agencias@datamart           ag,
                                 gi_unidad_tiempo@datamart      ut,
                                 gi_tipo_credito@datamart       tc
                                 where 
                                 --ppto.ppto_fact_unidad = 20200131 -- ultimo día del mes cuyo presupuesto se quiere conocer
                                 ut.unidad_mes=",VectorFecha[2],"
                                 and ut.unidad_ano in ('",VectorFecha[1],"','",as.character(as.numeric(VectorFecha[1])-1),"')
                                 and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                                 --and ag.agencia_descripcion in
                                 and tc.tcre_id = ppto.ppto_fact_tipo_venta
                                 and ppto.ppto_fact_agencia = ag.agencia_id
                                 and ppto.ppto_fact_unidad = ut.unidad_id
                                 and ag.agencia_pais = 1   
                                 group by ut.unidad_ano,
                                 ut.unidad_nombre_mes,
                                 --tc.tcre_codigo,
                                 ag.agencia_descripcion,
                                 ag.agencia_id,
                                 ag.AGENCIA_CODIGO,
                                 tc.tcre_tipo
                              ORDER BY ag.agencia_descripcion")

DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
PPTO_TIENDAS=sqlQuery(DWJAMAR_Connection,PPTO_ACTUAL_TIENDAS_QUERY)
odbcClose(DWJAMAR_Connection)
#PPTO_TIENDAS[PPTO_TIENDAS$AGENCIA_ID==90,c("AGENCIA_DESCRIPCION","AGENCIA_ID","AGENCIA_CODIGO")]
PPTO_TIENDAS[PPTO_TIENDAS$AGENCIA_ID==90,c("AGENCIA_ID")]= PPTO_TIENDAS[PPTO_TIENDAS$AGENCIA_ID==329,c("AGENCIA_ID")]
PPTO_TIENDAS[PPTO_TIENDAS$AGENCIA_CODIGO=="A4",c("AGENCIA_CODIGO")]= PPTO_TIENDAS[PPTO_TIENDAS$AGENCIA_CODIGO=="C5",c("AGENCIA_CODIGO")]
PPTO_TIENDAS$ANTIGUEDAD_TIENDA=ifelse(PPTO_TIENDAS$AGENCIA_ID %in% MISMAS_TIENDAS$AGENCIA_ID,"MISMA_TIENDA","NUEVA O CERRADA")
(PPTO_TIENDAS)

mydata=PPTO_TIENDAS
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
PPTO_TIENDAS=mydata
rm(list=c("mydata"))

######## PPTO TOTAL MES EN CURSO:
(PPTO_TOTAL_AÑO=sqldf("select UNIDAD_ANO, UNIDAD_NOMBRE_MES, SUM(PPTO_VN) AS PPTO_VN FROM PPTO_TIENDAS GROUP BY UNIDAD_ANO, UNIDAD_NOMBRE_MES"))

round(100*(PPTO_TOTAL_AÑO[2,3]/PPTO_TOTAL_AÑO[1,3]-1),1)


######## PPTO MISMAS TIENDAS MES EN CURSO:
(PPTO_TOTAL_AÑO_MT=sqldf("select UNIDAD_ANO, UNIDAD_NOMBRE_MES,ANTIGUEDAD_TIENDA , SUM(PPTO_VN) AS PPTO_VN FROM PPTO_TIENDAS
      WHERE ANTIGUEDAD_TIENDA='MISMA_TIENDA' GROUP BY UNIDAD_ANO, UNIDAD_NOMBRE_MES,ANTIGUEDAD_TIENDA"))

round(100*(PPTO_TOTAL_AÑO_MT[2,4]/PPTO_TOTAL_AÑO_MT[1,4]-1),1)

########################### MISMAS TIENDAS PARA EL MES EN CURSO #############################
# 
# PPTO_QUERY=paste0("select ut.unidad_ano, ut.unidad_nombre_mes, ppto.ppto_fact_agencia, ag.agencia_descripcion
#                   /*,ppto.ppto_fact_tipo_venta*/
#                   /*,tc.tcre_codigo*/
#                   --,tc.tcre_tipo
#                   ,round(sum(ppto.ppto_fact_valor_venta_neta)/1000000,1) as ppto_vn
#                   --,round(sum(ppto.ppto_fact_valor_venta_oferta)/1000000,1) as ppto_vo    
#                   --,round(sum(ppto.ppto_fact_valor_venta_full)/1000000,1) as ppto_vf
#                   from gi_fact_ppto_facturas@datamart ppto
#                   ,gi_agencias@datamart ag
#                   ,gi_unidad_tiempo@datamart ut
#                   ,gi_tipo_credito@datamart   tc
#                   where ppto.ppto_fact_unidad = 20200229 -- ultimo día del mes cuyo presupuesto se quiere conocer
#                   and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
#                   and ag.agencia_descripcion in
#                   ( 
#                   select ag.agencia_descripcion 
#                   from gi_fact_ppto_facturas@datamart ppto,
#                   gi_agencias@datamart           ag,
#                   gi_unidad_tiempo@datamart      ut,
#                   gi_tipo_credito@datamart       tc
#                   where ppto.ppto_fact_unidad = 20190228 -- ultimo día del mes cuyo presupuesto se quiere conocer
#                   and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
#                   and tc.tcre_id = ppto.ppto_fact_tipo_venta
#                   and ppto.ppto_fact_agencia = ag.agencia_id
#                   and ppto.ppto_fact_unidad = ut.unidad_id
#                   and ag.agencia_pais = 1
#                   group by ag.agencia_descripcion
#                   )       
#                   and tc.tcre_id= ppto.ppto_fact_tipo_venta
#                   and ppto.ppto_fact_agencia = ag.agencia_id
#                   and ppto.ppto_fact_unidad=ut.unidad_id
#                   and ag.agencia_pais=1
#                   group by ut.unidad_ano,ut.unidad_nombre_mes,ppto.ppto_fact_agencia,ag.agencia_descripcion")
# 
# DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
# sqlQuery(DWJAMAR_Connection,PPTO_QUERY)
# odbcClose(DWJAMAR_Connection)




######## TRÁFICO TOTAL ACUMULADO MES: ########################################
  
Taf_mes_query=paste0("select 
                     año, 
                     unidad_mes, 
                     agencia_comparable_traf, sum(entradas_act) as trafico_actual,
                     sum(entradas_ant)as trafico_anterior, 
                     (sum(entradas_act)/sum(entradas_ant)-1) as var_trafico
                     from gi_fact_agregada_trafico@datamart 
                     where 
                     agencia_comparable_traf='S'
                       and pais_descripcion='COLOMBIA' 
                       --and agencia_descripcion  not in ('PLAZA DEL SOL', 'METROPOLITANO') 
                       --and unidad_mes in ('NOV')
                       and fecha BETWEEN to_date('",as.character.Date(floor_date(as.Date(FechaCorte), "month")),"', 'yyyy-mm-dd') and to_date('",FechaCorte,"', 'yyyy-mm-dd')
                     group by 
                     año,unidad_mes, agencia_comparable_traf 
                     ")

  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  sqlQuery(DWJAMAR_Connection,Taf_mes_query)
  odbcClose(DWJAMAR_Connection)

  ######## TRÁFICO TOTAL ACUMULADO MES por AGENCIA: ################################ 1001831678
  
  ##VectorFecha=strsplit(FechaCorte, "-")[[1]]  format(as.Date(FechaCorte),"%d-%m-%Y")
  
  Taf_mes_agencia_query=paste0("select 
                       año, 
                       unidad_mes,
                       MAX(FECHA) AS FECHA_CORTE,
                       agencia_descripcion as agencia,
                       agencia_comparable_traf, sum(entradas_act) as trafico_actual,
                       sum(entradas_ant)as trafico_anterior, 
                       (sum(entradas_act)/sum(entradas_ant)-1) as var_trafico
                       from gi_fact_agregada_trafico@datamart 
                       where 
                       agencia_comparable_traf='S'
                       and pais_descripcion='COLOMBIA' 
                       --and agencia_descripcion  not in ('PLAZA DEL SOL', 'METROPOLITANO') 
                       --and unidad_mes in ('NOV')
                       and fecha BETWEEN to_date('",as.character.Date(floor_date(as.Date(FechaCorte), "month")),"', 'yyyy-mm-dd') and to_date('",FechaCorte,"', 'yyyy-mm-dd')
                       group by 
                       año,unidad_mes,agencia_descripcion, agencia_comparable_traf 
                       ")
  
  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  (Taf_mes_agencia=sqlQuery(DWJAMAR_Connection,Taf_mes_agencia_query,dec=","))
  odbcClose(DWJAMAR_Connection)
  
  View(Taf_mes_agencia)
  
  ######## VARIACIÓN DIARIA TRÁFICO por AGENCIA: ######################################
  
  
  Taf_dia_agencia_query=paste0(
    "select agencia_descripcion as AGENCIA,
       fecha,
    DIA_SEMANA,
    agencia_comparable_traf,sum(entradas_act) as trafico_actual,
    
    (select /*tra2.agencia_descripcion || '|' || */sum(tra2.entradas_act)
    from gi_fact_agregada_trafico@datamart tra2
    where tra2.agencia_codigo = tra1.agencia_codigo
    and tra2.pais_descripcion = 'COLOMBIA'
    --and tra2.agencia_descripcion not in ('PLAZA DEL SOL', 'METROPOLITANO' /*,'BOSA'*/)
    and tra2.agencia_descripcion in
    (select agencia_descripcion
    from gi_fact_agregada_trafico@datamart
    --where año = '2019'
    and agencia_comparable_traf = 'S'
    and pais_descripcion = 'COLOMBIA'
    --and unidad_mes in ( /*\*'MAR'*\ 'ABR','MAY',*/ 'NOV')
    group by agencia_descripcion)
    and tra2.fecha = to_date('22-11-2018', 'dd-mm-yyyy')) as trafico_anterior,--FECHA CORTE + 1    
    
    (sum(entradas_act)/ /*as trafico_actual,*/
    (select /*tra2.agencia_descripcion || '|' || */sum(tra2.entradas_act)
    from gi_fact_agregada_trafico@datamart tra2
    where tra2.agencia_codigo = tra1.agencia_codigo
    and tra2.pais_descripcion = 'COLOMBIA'
    and tra2.agencia_descripcion not in
    ('PLAZA DEL SOL', 'METROPOLITANO' /*,'BOSA'*/)
    and tra2.agencia_descripcion in
    (select agencia_descripcion
    from gi_fact_agregada_trafico@datamart
    where año = '2019'
    and agencia_comparable_traf = 'S'
    and pais_descripcion = 'COLOMBIA'
    and unidad_mes in ( /*\*'MAR'*\ 'ABR','MAY',*/ 'NOV')
    group by agencia_descripcion)
    and tra2.fecha = to_date('22-11-2018', 'dd-mm-yyyy')--FECHA CORTE + 1
    /*group by tra2.agencia_descripcion*/)-1) VARIACION /*TRAFICO_MISMO_DIA*/
    from gi_fact_agregada_trafico@datamart tra1
    where tra1.pais_descripcion = 'COLOMBIA'
    --and tra1.agencia_descripcion in ('LA PLAZUELA')/* not in    ('PLAZA DEL SOL', 'METROPOLITANO' \*,'BOSA'*\)*/
    and tra1.agencia_descripcion in
    (select agencia_descripcion
    from gi_fact_agregada_trafico@datamart
    where año = '2019'
    and agencia_comparable_traf = 'S'
    and pais_descripcion = 'COLOMBIA'
    and unidad_mes in ( /*\*'MAR'*\ 'ABR','MAY',*/ 'NOV')
    group by agencia_descripcion)
    and (tra1.fecha = to_date('21-11-2019', 'dd-mm-yyyy'))-- FECHA de CORTE
    group by tra1.agencia_codigo,
    tra1.agencia_descripcion,
    tra1.agencia_comparable_traf,
    tra1.fecha,
    tra1.DIA_SEMANA
    order by tra1.agencia_descripcion"
  )
  
  
  
  
  
##################################################################
##### SEGUIMIENTO OP, TRÁFICO DÍA VS DÍA    ###################### 
##################################################################


  
  


