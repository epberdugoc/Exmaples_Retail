
#######################################################################
############ Comparación presupuestos y ventas 2020 - 2019 ############ 
#######################################################################


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

###### TABLA MISMAS TIENDAS #####################

MISMAS_TIENDAS_QRY=paste0(
  "select ag.agencia_id
  ,ag.AGENCIA_CODIGO
  ,ag.agencia_descripcion
  from gi_fact_ppto_facturas@datamart ppto
  ,gi_agencias@datamart ag
  ,gi_unidad_tiempo@datamart ut
  ,gi_tipo_credito@datamart   tc
  where ppto.ppto_fact_unidad = 20200131 -- ultimo día del mes cuyo presupuesto se quiere conocer
  and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
  and  (ag.agencia_descripcion in
  ( 
  select ag.agencia_descripcion 
  from gi_fact_ppto_facturas@datamart ppto,
  gi_agencias@datamart           ag,
  gi_unidad_tiempo@datamart      ut,
  gi_tipo_credito@datamart       tc
  where ppto.ppto_fact_unidad = 20190131 -- ultimo día del mes cuyo presupuesto se quiere conocer
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



## Archivo consolidado en formato WIDE para el presupuesto del año vigente:

library(readxl)
PO_Ventas_Consolidado_2020 <- read_excel("D:/OneDrive - Muebles Jamar/2020/PO Ventas Consolidado 2020 V1 SIN FORMULAS.xlsx", 
                                                         sheet = "CONSOLIDADO PPTO 2020 VN", col_types = c("text", 
                                                                                                           "skip", "text", "text", "text", "numeric", 
                                                                                                           "numeric", "numeric", "numeric", 
                                                                                                           "numeric", "numeric", "numeric", 
                                                                                                           "numeric", "numeric", "numeric", 
                                                                                                           "numeric", "numeric"))
##View(PO_Ventas_Consolidado_2020)


## presupuesto del año vigente en formato LONG:

PPTO_LONG_2020=reshape(PO_Ventas_Consolidado_2020, idvar = c("UNIDAD_ANO","NOMBRE_TIENDA_PPTO","TCRE_TIPO","COD_AGENCIA"),
varying = list((ncol(PO_Ventas_Consolidado_2020)-11):ncol(PO_Ventas_Consolidado_2020)),timevar = "UNIDAD_MES",
v.names = "PPTO_VENTA_NETA", direction = "long",
new.row.names=1:(2*length(table(PO_Ventas_Consolidado_2020$COD_AGENCIA))*length(list(5:ncol(PO_Ventas_Consolidado_2020))[[1]])))


PPTO_LONG_2020$PPTO_VENTA_NETA=round(PPTO_LONG_2020$PPTO_VENTA_NETA,1)
PPTO_LONG_2020$VENTA_NETA_FUSION=0

## Presupuesto AÑO anterior:

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
                                       where ut.unidad_ano =",as.character(as.numeric(VectorFecha[1])-1), "
                                       and ppto.ppto_fact_agencia <> 101 -- para excluir UP STREAM
                                       and tc.tcre_id = ppto.ppto_fact_tipo_venta
                                       and ppto.ppto_fact_agencia = ag.agencia_id
                                       and ppto.ppto_fact_unidad = ut.unidad_id
                                       and ag.agencia_pais = 1
                                       group by ut.unidad_ano, 
                                       ag.agencia_descripcion,
                                       tc.tcre_tipo,
                                       ag.agencia_codigo,
                                       ut.unidad_mes")

DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
PPTO_ANO_ANTERIOR_TIENDAS_TBL=sqlQuery(DWJAMAR_Connection,PPTO_ANO_ANTERIOR_TIENDAS_QUERY,dec=",")
odbcClose(DWJAMAR_Connection)


#### VENTA NETA REAL AÑO ANTERIOR, POR MESES, AGENCIAS Y ATERNATIVAS:

# VN_ANO_ANTERIOR_TIENDAS_QUERY=paste0("
#                                      select t.unidad_ano
#                                      --,t.unidad_fecha      
#                                      --t.unidad_num_dia_mes, -- para el día del mes
#                                      --tc.tcre_codigo,
#                                      --,zg.zona_dpto_descrip         
#                                      ,a.agencia_descripcion as COD_AGENCIA
#                                      ,a.agencia_codigo
#                                      ,t.unidad_mes    
#                                      ,tc.tcre_tipo
#                                      -- count(distinct fact.fact_cliente) Can_Clientes,
#                                      ,round(sum(fact.fact_valor_venta_neta) / 1000000,1) VENTA_NETA_FUSION
#                                      --,round(sum(fact.fact_valor_venta_oferta) / 1000000,1) Venta_Oferta
#                                      --,round(sum(fact.fact_valor_venta_full) / 1000000,1) Venta_Full
#                                      from gi_fact_facturas@datamart fact,
#                                      gi_unidad_tiempo@datamart t,
#                                      gi_tipo_credito@datamart  tc,
#                                      gi_zona_geografica      zg,
#                                      gi_agencias@datamart      a
#                                      where fact.fact_unidad = t.unidad_id
#                                      and fact.fact_tipo_venta = tc.tcre_id
#                                      and fact.fact_agencia = a.agencia_id
#                                      and a.agencia_zona=zg.zona_id
#                                      --and t.unidad_fecha in (to_date('21-11-2019', 'dd-mm-yyyy'),to_date('22-11-2018', 'dd-mm-yyyy'))
#                                      and t.unidad_ano in (2019)--,2017,2016,2019)--,2016,2017)
#                                      --and t.unidad_mes in (/*1,2,3,*/1 /*5, 6, 7, 8, 9,10,11,12*/)
#                                      and a.agencia_pais = 1
#                                      --and  t.unidad_num_dia_mes in (2,3,4,5,6,7)
#                                      and a.agencia_descripcion not in ('PUBLICIDAD','OPERACIONES','UP STREAM','MARCA','METROPOLITANO','PLAZA DEL SOL') -- Ojo, estos cuatro deben excluirse para que la cifra anual coincida con la TABLEAU server
#                                      --and a.agencia_descripcion in ('SANTA MARTA')--('CONCESIONES HIPER JAMAR','EXPERIMENTOS')
#                                      and tc.tcre_codigo in ('CR', 'OR', 'TJ', 'CO', 'PT', 'NO') -- Las tres últimas son para contado
#                                      and fact.fact_garantia=0
#                                      group by 
#                                      t.unidad_ano
#                                      --,t.unidad_fecha
#                                      
#                                      --t.unidad_num_dia_mes, -- para el día del mes
#                                      --tc.tcre_codigo,
#                                      --,zg.zona_dpto_descrip 
#                                      ,t.unidad_mes 
#                                      ,a.agencia_codigo
#                                      ,a.agencia_descripcion    
#                                      ,tc.tcre_tipo      
#                                      --,a.agencia_descripcion
#                                      order by t.unidad_ano
#                                      , t.unidad_mes
#                                      , tc.tcre_tipo
#                                      ")
# 
# DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
# VN_ANO_ANTERIOR_TIENDAS_TBL=sqlQuery(DWJAMAR_Connection,VN_ANO_ANTERIOR_TIENDAS_QUERY)
# odbcClose(DWJAMAR_Connection)
# 
# 
# 
# 
# 
# 
# library(Rcmdr)
# 
# PresupuestoVentas <- readXL("D:\\OneDrive - Muebles Jamar\\2020\\PO Ventas Consolidado 2020 V1 SIN FORMULAS.xlsx",
#                             rownames=FALSE, header=TRUE, na="", sheet="PPTO formato long", 
#                             stringsAsFactors=TRUE)


PPTO_ANO_ANTERIOR_TIENDAS_TBL <- within(PPTO_ANO_ANTERIOR_TIENDAS_TBL, {
  UNIDAD_MES <- as.factor(UNIDAD_MES)
})

library(RODBC)


VN_Tiendas_QRY="
select t.unidad_ano,
       t.unidad_mes,
       --tc.tcre_codigo,
       tc.tcre_tipo,
      a.agencia_descripcion,
      a.AGENCIA_CODIGO,
       --count(distinct fact.fact_cliente) Can_Clientes,
       ROUND(sum(fact.fact_valor_venta_neta) / 1000000,1) AS VENTA_NETA_FUSION
       --sum(fact.fact_valor_venta_oferta) / 1000000 Venta_Oferta
       --sum(fact.fact_valor_venta_full) / 1000000 Venta_Full
  from gi_fact_facturas fact,
       gi_unidad_tiempo t,
       gi_tipo_credito  tc,
       gi_agencias      a
where fact.fact_unidad = t.unidad_id
   and fact.fact_tipo_venta = tc.tcre_id
   and fact.fact_agencia = a.agencia_id
   and t.unidad_ano in (2019)
   and t.unidad_mes in (1,2,3,4, 5, 6, 7, 8, 9,10,11,12) --
   and fact.fact_garantia=0
   and a.agencia_pais = 1
   --and a.agencia_descripcion not in ('CONCESIONES HIPER JAMAR')
   and tc.tcre_codigo in ('CR', 'OR', 'TJ', 'CO', 'PT', 'NO') -- Las tres últimas son para contado
group by t.unidad_ano,tc.tcre_tipo, t.unidad_mes
      --tc.tcre_codigo,
      ,a.agencia_descripcion,a.AGENCIA_CODIGO
"

jdbcConnection<- odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")# DATANEW and BARRIO is not NULL
VN_Tiendas_tbl=sqlQuery(jdbcConnection,VN_Tiendas_QRY, dec=",",stringsAsFactors=T) 
odbcClose(jdbcConnection)

VN_Tiendas_tbl <- within(VN_Tiendas_tbl, {
  UNIDAD_MES <- as.factor(UNIDAD_MES)
})

# setwd("D:/OneDrive - Muebles Jamar/2020")
# library(xlsx)
# write.xlsx2(x=VN_Tiendas_tbl,file="PO Ventas Consolidado 2020 V1 SIN FORMULAS.xlsx",sheetName="VN REAL 2019",append=TRUE ,row.names=FALSE)


library(sqldf)

## NOTA: para armar la llave compuesta, todas las variables concatenadas deben ser tipo caracter

PresupuestoVentas_real=sqldf("select t1.*,t2.VENTA_NETA_FUSION from PPTO_ANO_ANTERIOR_TIENDAS_TBL t1 
                             left join VN_Tiendas_tbl t2 on
trim(t1.COD_AGENCIA)||trim(t1.TCRE_TIPO)||trim(t1.UNIDAD_MES)=trim(t2.AGENCIA_CODIGO)||trim(t2.TCRE_TIPO)||trim(t2.UNIDAD_MES)
                             order by t1.COD_AGENCIA, t1.TCRE_TIPO 
                             ")

## V..... 

PresupuestoVentas=rbind(PPTO_LONG_2020,PresupuestoVentas_real)

PresupuestoVentas$COD_AGENCIA=as.factor(PresupuestoVentas$COD_AGENCIA)

## Eliminando datos de CR 27 a partir de agosto de 2019
library(lava)
attach(PresupuestoVentas)
PresupuestoVentas_test=PresupuestoVentas[!((COD_AGENCIA=="A4")&(UNIDAD_MES %in% c("8","9","10","11","12"))),]
detach(PresupuestoVentas)

levels(PresupuestoVentas_test$COD_AGENCIA)[13]="C5"

PresupuestoVentas=sqldf("select t1.UNIDAD_ANO,
                t2.AGENCIA_DESCRIPCION AS AGENCIA,
                --COD_AGENCIA,
                t1.UNIDAD_MES,
                t1.TCRE_TIPO,
                sum(t1.VENTA_NETA_FUSION) as VENTA_NETA_FUSION,
                sum(t1.PPTO_VENTA_NETA)   as PPTO_VENTA_NETA
        from    PresupuestoVentas_test t1
        inner join MISMAS_TIENDAS t2
        on t1.COD_AGENCIA= t2.AGENCIA_CODIGO
        --where   t1.COD_AGENCIA in (select AGENCIA_CODIGO from MISMAS_TIENDAS)
                              
        group by t1.UNIDAD_ANO,
                t2.AGENCIA_DESCRIPCION,
                --COD_AGENCIA,
                t1.UNIDAD_MES,
                t1.TCRE_TIPO
        ")

PresupuestoVentas$VAR_PPTO=0
PresupuestoVentas$VAR_PPTO_VENTA=0

#i=960

for (i in 1:nrow(PresupuestoVentas)){
  
  if(PresupuestoVentas$UNIDAD_ANO[i]=="2020"){
    
    PresupuestoVentas$VAR_PPTO[i]=round(100*(PresupuestoVentas$PPTO_VENTA_NETA[i]/PresupuestoVentas[(PresupuestoVentas$UNIDAD_ANO=="2019")&(PresupuestoVentas$AGENCIA == PresupuestoVentas$AGENCIA[i])&(PresupuestoVentas$UNIDAD_MES==PresupuestoVentas$UNIDAD_MES[i])&(PresupuestoVentas$TCRE_TIPO == PresupuestoVentas$TCRE_TIPO[i]),"PPTO_VENTA_NETA"]-1),1)
    PresupuestoVentas$VAR_PPTO_VENTA[i]=round(100*(PresupuestoVentas$PPTO_VENTA_NETA[i]/PresupuestoVentas[(PresupuestoVentas$UNIDAD_ANO=="2019")&(PresupuestoVentas$AGENCIA == PresupuestoVentas$AGENCIA[i])&(PresupuestoVentas$UNIDAD_MES==PresupuestoVentas$UNIDAD_MES[i])&(PresupuestoVentas$TCRE_TIPO == PresupuestoVentas$TCRE_TIPO[i]),"VENTA_NETA_FUSION"]-1),1)
    
  }
  
}

names(PresupuestoVentas)


(resumen.año=sqldf("select UNIDAD_ANO, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO"))
resumen.año <- within(resumen.año, {
  Year <- as.factor(UNIDAD_ANO)
})

windows()
require("ggplot2")
#y = resumen.año$ppto_venta_neta/1000
.df <- data.frame(x = resumen.año$UNIDAD_ANO, y=resumen.año$ppto_venta_neta/1000)
#.df <- as.data.frame(with(.df, table(x)))
.plot <- ggplot(data = .df, aes(x = x,y=y , label = y)) + 
  geom_bar(width = 0.7, stat = "identity", fill = "darkgreen") + 
  xlab("AÑO") + 
  ylab("Presupuesto venta neta (miles de millones)") + 
  theme_bw(base_size = 14, base_family = "sans")+ ylim(0, 300)+ 
  geom_text(aes(label=round(y,1)), vjust=-0.95) +
  ggtitle("Compración presupuestos  2020 - 2019")
print(.plot)
#rm(.df, .plot)

resumen.año.VN = resumen.año[,-3]
resumen.año.VN[2,2]=resumen.año[2,3]

.df <- data.frame(x = resumen.año.VN$UNIDAD_ANO, y=resumen.año.VN$venta_neta/1000)
.plot <- ggplot(data = .df, aes(x = x,y=y)) + 
  geom_bar(width = 0.3, stat = "identity", fill = "red") + 
  xlab("AÑO") + 
  ylab("venta neta (miles de millones)") + geom_step() +
  theme_bw(base_size = 14, base_family = "sans")+ ylim(0, 270)+ 
  geom_text(aes(label=round(y,1)), vjust=-0.95) +
  ggtitle("Compración presupuesto 2020 contra venta neta real 2019")
print(.plot)




(ResumenAñoAlternativas=sqldf("select UNIDAD_ANO as Year, TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, Alternativa"))

ResumenAñoAlternativas <- within(ResumenAñoAlternativas, {
  Year <- as.factor(Year)
})

.df <- data.frame(x = ResumenAñoAlternativas$Year, s = 
                    ResumenAñoAlternativas$Alternativa, y=ResumenAñoAlternativas$ppto_venta_neta/1000)

.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("Año") + 
  ylab("Presupuesto venta neta (miles de millones)") + geom_text(aes(label=round(y,1)), vjust=-0.95)+
   ylim(0, 200) + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines")) +
  ggtitle("Presupuestos venta neta 2020 - 2019 por alternativa")
print(.plot)

ResumenAñoAlternativas[3:4,3]=ResumenAñoAlternativas[3:4,4]

.df <- data.frame(x = ResumenAñoAlternativas$Year, s = 
                    ResumenAñoAlternativas$Alternativa, y=ResumenAñoAlternativas$venta_neta/1000)

.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("Año") + 
  ylab("Venta neta (miles de millones)") + geom_text(aes(label=round(y,1)), vjust=-0.95)+
  ylim(0, 180) + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines")) +
  ggtitle("Varaición del presupuesto 2020 contra la veta real de 2019")
print(.plot)


(resumen.mes=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES"))

resumen.mes$var_ppto_vn=0

##i=24 rm(i)

for(i in 1:nrow(resumen.mes)){
  
  if(resumen.mes$Year[i]=="2020"){
   
    resumen.mes$var_ppto_vn[i]=round(100*(resumen.mes$ppto_venta_neta[i]/resumen.mes[(resumen.mes$Year=="2019")&(resumen.mes$Mes==resumen.mes$Mes[i]),"ppto_venta_neta"]-1),1) 
   
  }
}



resumen.mes$var_ppto_vs_ventas=0

for(i in 1:nrow(resumen.mes)){
  
  if(resumen.mes$Year[i]=="2020"){
    
    resumen.mes$var_ppto_vs_ventas[i]=round(100*(resumen.mes$ppto_venta_neta[i]/resumen.mes[(resumen.mes$Year=="2019")&(resumen.mes$Mes==resumen.mes$Mes[i]),"venta_neta"]-1),1) 
    
  }
}

(resumen.mes_2019=resumen.mes[resumen.mes$Year=="2020",])


# .df <- data.frame(x = as.numeric(as.character(resumen.mes_2019$Mes)), y = resumen.mes_2019$var_ppto_vn)
# .df <- .df[order(.df$x), ]
# .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
#   geom_point() + 
#   geom_line(size = 1) + 
#   scale_y_continuous(expand = c(0.01, 0),limits=c(-25,25)) + 
#   xlab("Mes") + geom_text(aes(label=round(y,1)), vjust=-0.05, hjust=-0.35) + 
#   ylab("Variación porcentual (%)") +
#   theme_bw(base_size = 14, base_family = "sans") + geom_hline(aes(yintercept = 0), colour="blue")+
#   ggtitle("Varaición mensual presupuesto venta neta 2020 vs 2019") +
#   scale_x_discrete(name ="Mes", limits=1:12)
#   print(.plot)
# # ylim(-15, 15)+

  
  
  .df <- data.frame(x = as.numeric(as.character(resumen.mes_2019$Mes)), y = resumen.mes_2019$var_ppto_vs_ventas)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0),limits=c(0,18)) + 
    xlab("Mes") + geom_text(aes(label=round(y,1)), vjust=-0.05, hjust=-0.35) + 
    ylab("Variación porcentual (%)") +
    theme_bw(base_size = 14, base_family = "sans") + geom_hline(aes(yintercept = 10), colour="blue")+
    ggtitle("Variación mensual presupuesto 2020 vs venta neta 2019") +
    scale_x_discrete(name ="Mes", limits=1:12)
  print(.plot)

  
(resumen.mes_2019.alternativa=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES, Alternativa"))
  
  resumen.mes_2019.alternativa$var_ppto_vs_ventas=0
   
  # for(i in 23:44){
  #   resumen.mes_2019.alternativa$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.alternativa$ppto_venta_neta[i]/resumen.mes_2019.alternativa$venta_neta[i-22]-1)
  # }
  
  
  for(i in 1:nrow(resumen.mes_2019.alternativa)){
    
    if(resumen.mes_2019.alternativa$Year[i]=="2020"){
      
      resumen.mes_2019.alternativa$var_ppto_vs_ventas[i]=
        round(100*(resumen.mes_2019.alternativa$ppto_venta_neta[i]
        /resumen.mes_2019.alternativa[(resumen.mes_2019.alternativa$Year=="2019")&(resumen.mes_2019.alternativa$Mes==resumen.mes_2019.alternativa$Mes[i])&(resumen.mes_2019.alternativa$Alternativa==resumen.mes_2019.alternativa$Alternativa[i]),"venta_neta"]-1),1) 
      
    }
  }
  
  
  
  ## mensual por alternativa
 
  resumen.mes_2019.alternativa=resumen.mes_2019.alternativa[resumen.mes_2019.alternativa=="2020",] 
  resumen.mes_2019$Mes=as.numeric(as.character(resumen.mes_2019$Mes))
  
  .df <- data.frame(x = as.numeric(resumen.mes_2019.alternativa$Mes), y = resumen.mes_2019.alternativa$var_ppto_vs_ventas, z = 
                      resumen.mes_2019.alternativa$Alternativa)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0), limits = c(-20,20)) + 
    xlab("Mes") + 
    ylab("Varaición porcentual (%)") + 
    labs(colour = "Alternativa", shape = "Alternativa") + 
    theme_bw(base_size = 14, base_family = "sans") + 
    theme(legend.position = "right") + geom_hline(aes(yintercept = 0), colour="black")+
    geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35)+
    scale_x_discrete(name ="Mes", limits=1:12)
  print(.plot)
  
  ### venta presupuesto neta total 2020 vs venta neta real 2019 por tienda
  
(resumen.mes_2019.tiendas=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,AGENCIA, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES, AGENCIA"))
  resumen.mes_2019.tiendas$var_ppto_vs_ventas=0 

  # for(i in 210:418){
  #   resumen.mes_2019.tiendas$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.tiendas$ppto_venta_neta[i]/
  #                                                         resumen.mes_2019.tiendas$venta_neta[i-209]-1)
  # }
  # 
  # resumen.mes_2019.tiendas=resumen.mes_2019.tiendas[210:418,]
  # 
  # resumen.mes_2019.tiendasComparables=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA!="JAMAR BOSA",]

  
  for(i in 1:nrow(resumen.mes_2019.tiendas)){
    
    if(resumen.mes_2019.tiendas$Year[i]=="2020"){
      
      resumen.mes_2019.tiendas$var_ppto_vs_ventas[i]=
        round(100*(resumen.mes_2019.tiendas$ppto_venta_neta[i]
                   /resumen.mes_2019.tiendas[(resumen.mes_2019.tiendas$Year=="2019")
                  &(resumen.mes_2019.tiendas$Mes==resumen.mes_2019.tiendas$Mes[i])
                  &(resumen.mes_2019.tiendas$AGENCIA==resumen.mes_2019.tiendas$AGENCIA[i]),"venta_neta"]-1),1) 
      
    }
  }
  
  windows()
  
  ### Eliminando las tiendas de 'BUCARAMANGA CENTRO' y 'LA PLAZUELA': 
  
  resumen.mes_2019.18tiendas=resumen.mes_2019.tiendas[(resumen.mes_2019.tiendas$Year=="2020")&(resumen.mes_2019.tiendas$AGENCIA %ni% c('BUCARAMANGA CENTRO','LA PLAZUELA','FLORIDA CC')),] 
  
  
  .df <- data.frame(x = as.numeric(resumen.mes_2019.18tiendas$Mes), y = resumen.mes_2019.18tiendas$var_ppto_vs_ventas, 
                    s = resumen.mes_2019.18tiendas$AGENCIA)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0), limits = c(min(.df$y),max(.df$y)))  + 
    facet_wrap( ~ s) + 
    xlab("Mes") +  
    ylab("Varaición porcentual (%)") +
    theme_bw(base_size = 14, base_family = "sans") + 
    theme(panel.spacing = unit(0.3, "lines")) + geom_hline(aes(yintercept = 0), colour="blue")+
    ggtitle("Variación mensual presupuesto 2020 vs venta neta real 2019 por mismas tiendas") +
    scale_x_discrete(name ="Mes", limits=1:12)
    print(.plot)
  
    ## Para las 3 tiendas atípicas: 
    
    resumen.mes_2019.Bosa=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA %in% c('BUCARAMANGA CENTRO','LA PLAZUELA','FLORIDA CC'),]
  
    names(resumen.mes_2019.Bosa)
    
    .df <- data.frame(x = as.numeric(resumen.mes_2019.Bosa$Mes), y = resumen.mes_2019.Bosa$var_ppto_vs_ventas, 
                      s = resumen.mes_2019.Bosa$AGENCIA)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0), limits = c(min(.df$y),max(.df$y)))  + 
      facet_wrap( ~ s) + 
      xlab("Mes") +  
      ylab("Varaición porcentual (%)") +
      theme_bw(base_size = 14, base_family = "sans") + 
      theme(panel.spacing = unit(0.3, "lines")) + geom_hline(aes(yintercept = 0), colour="blue")+
      ggtitle("Variación mensual presupuesto 2020 vs venta neta real 2019 (tiendas atípicas)") +
      scale_x_discrete(name ="Mes", limits=1:12)
    print(.plot)
    # 
    # PresupuestoVentas2019=PresupuestoVentas[PresupuestoVentas$UNIDAD_ANO==2019,]
    # 
    # PresupuestoVentas2019 <- within(PresupuestoVentas2019, {
    #   UNIDAD_MES <- as.factor(UNIDAD_MES)
    # })
    # 
    # (sqldf("select Year, Mes, avg(venta_neta), count(*), avg(ppto_venta_neta) 
    #                from [resumen.mes_2019.tiendasComparables] group by Year, Mes"))
    # 
    
    
    # (sqldf("select Year, avg(venta_neta), count(*), avg(ppto_venta_neta) 
    #                from [resumen.mes_2019.Bosa] group by Year"))
    
    
    # resumen.mes_2018.Bosa=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA=="JAMAR BOSA",]
    # 
    # attach(resumen.mes_2018.Bosa)
    # resumen.mes_2019.Q1.Bosa= resumen.mes_2018.Bosa[(Year==2019)&(Mes<4),]
    # resumen.mes_2019.Q1.Bosa$var_ppto_vs_ventas=100*(ppto_venta_neta/579-1)
    # detach(resumen.mes_2018.Bosa)
    # 
    # .df <- data.frame(x = resumen.mes_2019.Q1.Bosa$Mes, y = resumen.mes_2019.Q1.Bosa$var_ppto_vn_promedio)
    # .df <- .df[order(.df$x), ]
    # .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
    #   geom_point() + 
    #   geom_line(size = 1) + 
    #   scale_y_continuous(expand = c(0.01, 0), limits = c(0, 30)) + 
    #   ylab("Varaición porcentual (%)") + 
    #   labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 agencia Bosa (meses no comparables)") + 
    #   ggthemes::theme_base(base_size = 14, base_family = "sans") +
    #   geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.65, hjust=0.05)+
    #   scale_x_discrete(name ="Mes", limits=1:3)
    # print(.plot)
    # 
    resumen.mes_2019.tiendas.alternativa=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,AGENCIA,TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO,AGENCIA, UNIDAD_MES,TCRE_TIPO ")
    
    resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas=0 
    
    
    # i=960
    for(i in 1:nrow(resumen.mes_2019.tiendas.alternativa)){
      
      if(resumen.mes_2019.tiendas.alternativa$Year[i]=="2020"){
        
        resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas[i]=
          round(100*(resumen.mes_2019.tiendas.alternativa$ppto_venta_neta[i]
                     /resumen.mes_2019.tiendas.alternativa[(resumen.mes_2019.tiendas.alternativa$Year=="2019")
                     &(resumen.mes_2019.tiendas.alternativa$Mes==resumen.mes_2019.tiendas.alternativa$Mes[i])
                     &(resumen.mes_2019.tiendas.alternativa$AGENCIA==resumen.mes_2019.tiendas.alternativa$AGENCIA[i])&
                      (resumen.mes_2019.tiendas.alternativa$Alternativa==resumen.mes_2019.tiendas.alternativa$Alternativa[i]),"venta_neta"]-1),1) 
        
      }
    }
    
    # resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas=0
    # 
    # for(i in 419:836){
    #   resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.tiendas.alternativa$ppto_venta_neta[i]/
    #                                                                     resumen.mes_2019.tiendas.alternativa$venta_neta[i-418]-1) 
    # }
    # 
    #attach(resumen.mes_2019.tiendas.alternativa)
    resumen.mes_2019.tiendas.comparables.alternativa=resumen.mes_2019.tiendas.alternativa[(resumen.mes_2019.tiendas.alternativa$Year=="2020")&(resumen.mes_2019.tiendas.alternativa$AGENCIA 
        %ni% c('BUCARAMANGA CENTRO','LA PLAZUELA','FLORIDA CC','HIPER JAMAR CONCESIONES') ),]
    #detach(resumen.mes_2019.tiendas.alternativa)
    
    
    .df <- data.frame(x = as.numeric(resumen.mes_2019.tiendas.comparables.alternativa$Mes), y = 
                        resumen.mes_2019.tiendas.comparables.alternativa$var_ppto_vs_ventas, z = 
                        resumen.mes_2019.tiendas.comparables.alternativa$Alternativa, t = resumen.mes_2019.tiendas.comparables.alternativa$AGENCIA)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0)) + 
      facet_wrap( ~ t) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(colour = "Alternativa", shape = "Alternativa") + 
      #labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 por agencias y alternativas (excepto Bosa)") + 
      theme_bw(base_size = 14, base_family = "sans") + 
      theme(panel.spacing = unit(0.3, "lines"), legend.position = "right")+
      scale_x_discrete(name ="Mes", limits=1:12)+ geom_hline(aes(yintercept = 0), colour="black")
    print(.plot)
    
    #### para las 4 tiendas atípicas
    
    resumen.mes_2019.tiendas.atipicas.alternativa=resumen.mes_2019.tiendas.alternativa[(resumen.mes_2019.tiendas.alternativa$Year=="2020")&(resumen.mes_2019.tiendas.alternativa$AGENCIA 
                                                                                                                                               %in% c('BUCARAMANGA CENTRO','LA PLAZUELA','FLORIDA CC','HIPER JAMAR CONCESIONES') ),]
    #detach(resumen.mes_2019.tiendas.alternativa)
    
    
    .df <- data.frame(x = as.numeric(resumen.mes_2019.tiendas.atipicas.alternativa$Mes), y = 
                        resumen.mes_2019.tiendas.atipicas.alternativa$var_ppto_vs_ventas, z = 
                        resumen.mes_2019.tiendas.atipicas.alternativa$Alternativa, t = resumen.mes_2019.tiendas.atipicas.alternativa$AGENCIA)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0)) + 
      facet_wrap( ~ t) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(colour = "Alternativa", shape = "Alternativa") + 
      #labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 por agencias y alternativas (excepto Bosa)") + 
      theme_bw(base_size = 14, base_family = "sans") + 
      theme(panel.spacing = unit(0.3, "lines"), legend.position = "right")+
      scale_x_discrete(name ="Mes", limits=1:12)+ geom_hline(aes(yintercept = 0), colour="black")
    print(.plot)
    
    
    setwd("D:/OneDrive - Muebles Jamar/2020")
    library(xlsx)
    write.xlsx2(x=resumen.mes_2019.tiendas.alternativa,file="PO Ventas Consolidado 2020 V1 SIN FORMULAS.xlsx",sheetName="PPTO vs VN",append=TRUE ,row.names=FALSE)

    
    