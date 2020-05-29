##### LIBRERIAS #####
library(xlsx)
library(lubridate)
library(RODBC)


##### MACRO VARIABLES #####
# Directorio de almacenamiento:

Ruta="D:/OneDrive - Muebles Jamar/2019/Medicion indicadores mercadeo/Matrices encuestas NPS SAC/Bases 2019"
setwd(Ruta)

# Conexiones ODBC:

Conexion_1="DATANEW_JAMAR"
USER_1="dwjamar"
PWD_1="dwjamar"

Conexion_2="TCP_JAMAR"
USER_2 = "datamart"
PWD_2  = "datamart"

# Fecha inicial mes anterior

Fecha_Inicio = "01-09-2019" ## Primer día del mes que se quiere generar
Fecha_Inicio_Banco = format(as.Date(Fecha_Inicio,format = "%d-%m-%Y") %m-% months(6),"%d-%m-%Y")

# Fecha fin mes anterior

Fecha_Fin = format(as.Date(Fecha_Inicio,format = "%d-%m-%Y") %m+% months(1) %m-% days(1),"%d-%m-%Y")

# Vector Fechas

VectorFecha=strsplit(Fecha_Inicio, "-")[[1]]
VectorFechaBanco = strsplit(Fecha_Inicio_Banco, "-")[[1]]



#################################################################
######################## BD RETAIL ##############################
#################################################################

BD_RETAIL = paste("SELECT CLIENTES.CLIENTE_CODIGO Cliente,
       CLIENTES.CLIENTE_NOMBRE Nombre_Completo,
                  CLIENTES.CLIENTE_PRINOM Primer_Nombre,
                  CLIENTES.CLIENTE_CELULAR Celular,
                  CLIENTES.CLIENTES_TELEFONOS Telefonos,
                  CLIENTES.CLIENTE_TEL Tel,
                  CLIENTES.CLIENTES_EMAIL email,
                  CLIENTES.CLIENTES_SEXO sexo,
                  CLIENTES.CLIENTE_ESTCIVIL estado_civil,
                  CLIENTES.CLIENTES_SEGMENTO segmento_credito,
                  clientes.cliente_seg_corp segmentacion_corporativa,
                  ZONA_GEOGRAFICA.DPTO_DESCRIPCION dpto,
                  ZONA_GEOGRAFICA.CIUD_DESCRIPCION ciudad,
                  ZONA_GEOGRAFICA.BARR_DESCRIPCION barrio,
                  CLIENTES.CLIENTE_DIR Direccion,
                  ZONA_GEOGRAFICA.ESTRATO estrato,
                  UNIDAD_TIEMPO.UNIDAD_FECHA fecha,
                  facturas.fact_numero Numero_fact,
                  agencias.agencia_descripcion,
                  ALTERNATIVA_VENTA.ALT_CODIGO alternativa,
                  VENDEDORES.VEND_CODIGO Codigo_vendedor,
                  VENDEDORES.VEND_DESCRIPCION Vendedor,
                  decode((select Count(distinct fact_numero)
                  from facturas, unidad_tiempo
                  where unidad_tiempo.unidad_codigo = facturas.fact_unidad
                  and unidad_tiempo.unidad_fecha <
                  to_date('",Fecha_Inicio,"', 'dd-mm-yyyy')
                  and fact_cliente = cliente_codigo),
                  0,
                  'Nuevo',
                  'Conocido') Tipo_Cliente,
                  0 Tiene_Queja_Reclamo,
                  (select nvl(count(distinct facturas.fact_numero), 0)
                  from facturas, unidad_tiempo
                  where unidad_tiempo.unidad_codigo = facturas.fact_unidad
                  and unidad_tiempo.unidad_fecha <
                  to_date('",Fecha_Inicio,"', 'dd-mm-yyyy')
                  and cliente_codigo = fact_cliente) Numero_compras,
                  Round(SUM(FACTURAS.FACT_VLRVTA) / 1.16, 2) venta_sin_iva
                  FROM CLIENTES,
                  ZONA_GEOGRAFICA,
                  UNIDAD_TIEMPO,
                  ALTERNATIVA_VENTA,
                  VENDEDORES,
                  FACTURAS,
                  AGENCIAS
                  WHERE (FACTURAS.FACT_AGENCIA = AGENCIAS.AGENCIA_CODIGO)
                  AND (FACTURAS.FACT_ALTVTA = ALTERNATIVA_VENTA.ALT_CODIGO)
                  AND (FACTURAS.FACT_CLIENTE = CLIENTES.CLIENTE_CODIGO)
                  AND (FACTURAS.FACT_UNIDAD = UNIDAD_TIEMPO.UNIDAD_CODIGO)
                  AND (VENDEDORES.VEND_CODIGO = FACTURAS.FACT_VENDEDOR)
                  AND (CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO)
                  AND ZONA_GEOGRAFICA.DPTO_DESCRIPCION IN ('ATLANTICO', 'BOLIVAR', 'CESAR', 'MAGDALENA', 'ANTIOQUIA', 'SANTANDER', 'CUNDINAMARCA')
                  AND (FACTURAS.FACT_ESTADO IN ('ENTREGADA') AND
                  UNIDAD_TIEMPO.UNIDAD_ANO = ",VectorFecha[3],"AND UNIDAD_TIEMPO.UNIDAD_MES = ",VectorFecha[2]," and
                  clientes.cliente_codigo in
                  (select s.cliente_codigo
                  from segmentacion_360 s
                  group by s.cliente_codigo
                  having sum(s.id_qr) = 0) AND
                  ALTERNATIVA_VENTA.ALT_CODIGO IN ('CO', 'CR', 'OR', 'PT', 'TJ') and
                  cliente_codigo in
                  (SELECT distinct ORDEN_PEDIDO.ORDEN_CLIENTE
                  FROM UNIDAD_TIEMPO, ORDEN_PEDIDO
                  WHERE (ORDEN_PEDIDO.ORDEN_UNIDAD = UNIDAD_TIEMPO.UNIDAD_CODIGO)
                  AND (UNIDAD_TIEMPO.UNIDAD_ANO = ",VectorFecha[3]," AND
                  UNIDAD_TIEMPO.UNIDAD_MES = ",VectorFecha[2]," AND
                  orden_altvta IN ('CO', 'CR', 'OR', 'PT', 'TJ') and
                  orden_pedido.orden_estado = 'FACTURADA') --ALTERNATIVA_VENTA.ALT_CODIGO 
                  ))
                  GROUP BY CLIENTES.CLIENTE_CODIGO,
                  CLIENTES.CLIENTE_NOMBRE,
                  CLIENTES.CLIENTE_PRINOM,
                  CLIENTES.CLIENTE_CELULAR,
                  CLIENTES.CLIENTES_TELEFONOS,
                  CLIENTES.CLIENTE_TEL,
                  CLIENTES.CLIENTE_DIR,
                  CLIENTES.CLIENTES_SEXO,
                  CLIENTES.CLIENTE_ESTCIVIL,
                  CLIENTES.CLIENTES_SEGMENTO,
                  CLIENTES.CLIENTES_EMAIL,
                  agencias.agencia_descripcion,
                  ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
                  ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
                  ZONA_GEOGRAFICA.BARR_DESCRIPCION,
                  ZONA_GEOGRAFICA.ESTRATO,
                  UNIDAD_TIEMPO.UNIDAD_FECHA,
                  ALTERNATIVA_VENTA.ALT_CODIGO,
                  VENDEDORES.VEND_CODIGO,
                  VENDEDORES.VEND_DESCRIPCION,
                  clientes.cliente_seg_corp,
                  facturas.fact_numero",sep = "")




system.time({
DWJAMAR_Connection =  odbcConnect(Conexion_1, uid = USER_1, pwd = PWD_1)
BD_SAC_1 =  sqlQuery(DWJAMAR_Connection,BD_RETAIL)
odbcClose(DWJAMAR_Connection)
}
)

### set.seed(12345)
BD_SAC_1 = with(BD_SAC_1,BD_SAC_1[order(CLIENTE,FECHA, decreasing=TRUE),])

attach(BD_SAC_1)
BD_SAC_1=BD_SAC_1[!duplicated(CLIENTE),]
detach(BD_SAC_1)
  
#length(unique(BD_SAC_1$CLIENTE))
#dim(BD_SAC_1[unique(BD_SAC_1$CLIENTE),])
BD_SAC_1$ALEATORIO = runif(nrow(BD_SAC_1))
BD_SAC_1 = with(BD_SAC_1,BD_SAC_1[order(DPTO,ALEATORIO, decreasing=TRUE),])

gc(reset = T)
write.xlsx2(BD_SAC_1, file=paste(VectorFecha[2],". BD Piloto Encuestas SAC - ",months.Date(as.POSIXct(Fecha_Inicio)),".xlsx", sep = ""), sheetName = "BD_RETAIL",
            row.names = FALSE, append = TRUE)

###rm(list = c("BD_SAC_1.1","BD_SAC_1.2","BD_SAC_1.3","BD_SAC_1.4"))

#################################################################
######################## BD NONBANK #############################
#################################################################


BD_BANCO = paste("SELECT CLIENTES.CLIENTE_CODIGO Cliente,
                 CLIENTES.CLIENTE_NOMBRE Nombre_Completo,
                 CLIENTES.CLIENTE_PRINOM Primer_Nombre,
                 CLIENTES.CLIENTE_CELULAR Celular,
                 CLIENTES.CLIENTES_TELEFONOS Telefonos,
                 CLIENTES.CLIENTE_TEL Tel,
                 CLIENTES.CLIENTES_EMAIL email,
                 CLIENTES.CLIENTES_SEXO sexo,
                 CLIENTES.CLIENTE_ESTCIVIL estado_civil,
                 CLIENTES.CLIENTES_SEGMENTO segmento_credito,
                 clientes.cliente_seg_corp segmentacion_corporativa,
                 ZONA_GEOGRAFICA.DPTO_DESCRIPCION dpto,
                 ZONA_GEOGRAFICA.CIUD_DESCRIPCION ciudad,
                 ZONA_GEOGRAFICA.BARR_DESCRIPCION barrio,
                 CLIENTES.CLIENTE_DIR Direccion,
                 ZONA_GEOGRAFICA.ESTRATO estrato,
                 UNIDAD_TIEMPO.UNIDAD_FECHA fecha,
                 facturas.fact_numero Numero_fact,
                 agencias.agencia_descripcion,
                 ALTERNATIVA_VENTA.ALT_CODIGO alternativa,
                 VENDEDORES.VEND_CODIGO Codigo_vendedor,
                 VENDEDORES.VEND_DESCRIPCION Vendedor,
                 decode((select Count(distinct fact_numero)
                 from facturas, unidad_tiempo
                 where unidad_tiempo.unidad_codigo = facturas.fact_unidad
                 and unidad_tiempo.unidad_fecha <
                 to_date('",Fecha_Inicio_Banco,"', 'dd-mm-yyyy')
                 and fact_cliente = cliente_codigo),
                 0,
                 'Nuevo',
                 'Conocido') Tipo_Cliente,
                 0 Tiene_Queja_Reclamo,
                 (select nvl(count(distinct facturas.fact_numero), 0)
                 from facturas, unidad_tiempo
                 where unidad_tiempo.unidad_codigo = facturas.fact_unidad
                 and unidad_tiempo.unidad_fecha <
                 to_date('",Fecha_Inicio_Banco,"', 'dd-mm-yyyy')
                 and cliente_codigo = fact_cliente) Numero_compras,
                 (select cartera.cart_tramo_nuevo || ';' || cartera.cart_ctacar || ';' ||
                 cartera.cart_nrocuovenini ||';' || cartera.cart_tipcre
                 from cartera, unidad_tiempo
                 where unidad_tiempo.unidad_codigo = cartera.cart_unidad
                 and unidad_tiempo.unidad_mes = ",month(today()),"
                 and unidad_tiempo.unidad_ano = ",year(today()),"
                 and cliente_codigo = cart_cliente
                 and rownum = 1) Datos_Cartera,
                 FACTURAS.FACT_NROCUOTAS,
                 Round(SUM(FACTURAS.FACT_VLRVTA) / 1.16, 2) venta_sin_iva
                 FROM CLIENTES,
                 ZONA_GEOGRAFICA,
                 UNIDAD_TIEMPO,
                 ALTERNATIVA_VENTA,
                 VENDEDORES,
                 FACTURAS,
                 AGENCIAS
                 WHERE (FACTURAS.FACT_AGENCIA = AGENCIAS.AGENCIA_CODIGO)
                 AND (FACTURAS.FACT_ALTVTA = ALTERNATIVA_VENTA.ALT_CODIGO)
                 AND (FACTURAS.FACT_CLIENTE = CLIENTES.CLIENTE_CODIGO)
                 AND (FACTURAS.FACT_UNIDAD = UNIDAD_TIEMPO.UNIDAD_CODIGO)
                 AND (VENDEDORES.VEND_CODIGO = FACTURAS.FACT_VENDEDOR)
                 AND (CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO)
                 AND ZONA_GEOGRAFICA.DPTO_DESCRIPCION IN ('ATLANTICO', 'BOLIVAR', 'CESAR', 'MAGDALENA', 'ANTIOQUIA', 'SANTANDER', 'CUNDINAMARCA')
                 AND (FACTURAS.FACT_ESTADO IN ('ENTREGADA') AND
                 UNIDAD_TIEMPO.UNIDAD_ANO = ",VectorFechaBanco[3]," AND UNIDAD_TIEMPO.UNIDAD_MES = ",VectorFechaBanco[2]," and
                 clientes.cliente_codigo in
                 (select s.cliente_codigo
                 from segmentacion_360 s
                 group by s.cliente_codigo
                 having sum(s.id_qr) = 0) AND
                 ALTERNATIVA_VENTA.ALT_CODIGO IN ('CR', 'OR', 'TJ') and
                 cliente_codigo in
                 (SELECT distinct ORDEN_PEDIDO.ORDEN_CLIENTE
                 FROM UNIDAD_TIEMPO, ORDEN_PEDIDO
                 WHERE (ORDEN_PEDIDO.ORDEN_UNIDAD = UNIDAD_TIEMPO.UNIDAD_CODIGO)
                 AND (UNIDAD_TIEMPO.UNIDAD_ANO = ",VectorFechaBanco[3]," AND
                 UNIDAD_TIEMPO.UNIDAD_MES = ",VectorFechaBanco[2]," AND
                 orden_altvta IN ('CR', 'OR', 'TJ') and
                 orden_pedido.orden_estado = 'FACTURADA') --ALTERNATIVA_VENTA.ALT_CODIGO 
                 ))
                 GROUP BY CLIENTES.CLIENTE_CODIGO,
                 CLIENTES.CLIENTE_NOMBRE,
                 CLIENTES.CLIENTE_PRINOM,
                 CLIENTES.CLIENTE_CELULAR,
                 CLIENTES.CLIENTES_TELEFONOS,
                 CLIENTES.CLIENTE_TEL,
                 CLIENTES.CLIENTE_DIR,
                 CLIENTES.CLIENTES_SEXO,
                 CLIENTES.CLIENTE_ESTCIVIL,
                 CLIENTES.CLIENTES_SEGMENTO,
                 CLIENTES.CLIENTES_EMAIL,
                 agencias.agencia_descripcion,
                 ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
                 ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
                 ZONA_GEOGRAFICA.BARR_DESCRIPCION,
                 ZONA_GEOGRAFICA.ESTRATO,
                 UNIDAD_TIEMPO.UNIDAD_FECHA,
                 ALTERNATIVA_VENTA.ALT_CODIGO,
                 VENDEDORES.VEND_CODIGO,
                 VENDEDORES.VEND_DESCRIPCION,
                 clientes.cliente_seg_corp,
                 facturas.fact_numero,
                 FACTURAS.FACT_NROCUOTAS",sep = "")

system.time({
  DWJAMAR_Connection =  odbcConnect(Conexion_1, uid = USER_1, pwd = PWD_1)
  BD_SAC_2 =  sqlQuery(DWJAMAR_Connection,BD_BANCO)
  odbcClose(DWJAMAR_Connection)
}
)

### set.seed(12345)
BD_SAC_2$ALEATORIO = runif(nrow(BD_SAC_2))

BD_SAC_2 = with(BD_SAC_2,BD_SAC_2[order(DPTO,ALEATORIO, decreasing=TRUE),])


gc(reset = T)
write.xlsx2(BD_SAC_2, file=paste(VectorFecha[2],". BD Piloto Encuestas SAC - ",months.Date(as.POSIXct(Fecha_Inicio)),".xlsx", sep = ""), sheetName = "BD_BANCO",
            row.names = FALSE, append = TRUE)


#################################################################
######################## BD QUEJAS ##############################
#################################################################


BD_QUEJAS = paste("select n_ide,
                  c_pqr,
                  fecharec,
                  CASE
                  WHEN NVL((select Count(distinct fact_numero)
                  from facturas@datamart, unidad_tiempo@datamart
                  where unidad_tiempo.unidad_codigo = facturas.fact_unidad
                  and fact_estado = 'ENTREGADA'
                  and unidad_tiempo.unidad_fecha <
                  to_date(fecharec, 'dd-mm-yyyy')
                  and fact_cliente = N_IDE having
                  Count(distinct fact_numero) > 1),
                  0) > 0 THEN
                  'Conocido'
                  ELSE
                  'Nuevo'
                  END TIPO_CLIENTE,
                  CLIENTES.CLIENTE_NOMBRE,
                  CLIENTES.CLIENTE_PRINOM,
                  ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
                  ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
                  ZONA_GEOGRAFICA.BARR_DESCRIPCION,
                  CLIENTES.CLIENTE_DIR,
                  CLIENTES.CLIENTE_CELULAR,
                  CLIENTES.CLIENTES_EMAIL,
                  CLIENTES.CLIENTES_SEGMENTO,
                  CLIENTES.cliente_seg_corp,
                  CLIENTES.CLIENTES_SEXO,
                  CLIENTES.CLIENTE_ESTCIVIL,
                  CLIENTES.CLIENTE_OCUP,
                  ZONA_GEOGRAFICA.ESTRATO,
                  trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360) EDAD,
                  obs,
                  obs_sol,
                  fechasol,
                  causal,
                  c_proced,
                  c_agr,
                  cue,
                  pqr_causal.descri,
                  pqr_causal.aplicacion,
                  pqr_causal.tipo
                  from reclamoc r, pqr_causal, CLIENTES@datamart, ZONA_GEOGRAFICA@datamart
                  where pqr_causal.c_emp = r.c_emp
                  and pqr_causal.c_causal = r.causal
                  and n_ide = CLIENTES.CLIENTE_CODIGO
                  and CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO
                  AND ZONA_GEOGRAFICA.DPTO_DESCRIPCION IN ('ATLANTICO', 'BOLIVAR', 'CESAR', 'MAGDALENA', 'ANTIOQUIA', 'SANTANDER', 'CUNDINAMARCA')
                  and pqr_causal.tipo in ('Q')
                  and to_date(r.fechasol, 'dd-mm-yyyy') BETWEEN
                  to_date('",Fecha_Inicio,"', 'dd-mm-yyyy') AND
                  to_date('",Fecha_Fin,"', 'dd-mm-yyyy')",sep = "")


system.time({
  DWJAMAR_Connection =  odbcConnect(Conexion_2, uid = USER_2, pwd = PWD_2)
  BD_SAC_3 =  sqlQuery(DWJAMAR_Connection,BD_QUEJAS)
  odbcClose(DWJAMAR_Connection)
}
)

BD_SAC_3$ALEATORIO = runif(nrow(BD_SAC_3))

BD_SAC_3 = with(BD_SAC_3,BD_SAC_3[order(DPTO_DESCRIPCION,ALEATORIO, decreasing=TRUE),])


gc(reset = T)
write.xlsx2(BD_SAC_3, file=paste(VectorFecha[2],". BD Piloto Encuestas SAC - ",months.Date(as.POSIXct(Fecha_Inicio)),".xlsx", sep = ""), sheetName = "BD_QUEJAS",
            row.names = FALSE, append = TRUE)


#################################################################
######################## BD RECLAMOS ############################
#################################################################


BD_RECLAMOS = paste("SELECT ST_ENC.CLTE CEDULA,
       JAMAR.TRAE_NOMBRE('JA', ST_ENC.CLTE) NOMBRE_CLIENTE,
                    CLIENTES.CLIENTE_NOMBRE,
                    CLIENTES.CLIENTE_PRINOM,
                    ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
                    ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
                    ZONA_GEOGRAFICA.BARR_DESCRIPCION,
                    CLIENTES.CLIENTE_DIR,
                    CLIENTES.CLIENTE_CELULAR,
                    CLIENTES.CLIENTES_EMAIL,
                    CLIENTES.CLIENTES_SEGMENTO,
                    CLIENTES.cliente_seg_corp,
                    CLIENTES.CLIENTES_SEXO,
                    CLIENTES.CLIENTE_ESTCIVIL,
                    CLIENTES.CLIENTE_OCUP,
                    ZONA_GEOGRAFICA.ESTRATO,
                    trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360) EDAD,
                    ST_ENC.DEP,
                    ST_ENC.CIU,
                    JAMAR.FVCREPTELTIPO('JA', ST_ENC.CLTE, 'CELULAR', 2) CELULARES_CLIENTE,
                    DECODE((TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') - TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY')), 1, 'SI', 2, 'SI', 0, 'SI', 3, 'SI', 'NO') PRIORITARO,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') - TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY') DIAS_GARANTIA,
                    JAMAR.TRAE_NOMBRE(ST_ENC.C_EMP, MAX(ST_VISITA.TEC)) NOMBRE_TECNICO,
                    ST_DET.CLASE,
                    ST_ENC.AGE AGENCIA,
                    ST_ENC.SER SERVICIO,
                    ST_ENC.TIPO_SER TIPO_SERVICIO,
                    ST_ENC.PRIORITARIO,
                    ST_ENC.EST,
                    CAUSAS_CES.DESCRI DESCRIPCION_CAUSA,
                    FUENTES_CES.DESCRI DESCRIPCION_FUENTE,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') FECHA_GENERACION,
                    TO_CHAR(ST_ENC.FEC_SOL, 'YYYY') AÑO_SOLUCION,
                    TO_CHAR(ST_ENC.FEC_SOL, 'Month') NES_SOLUCION,
                    TO_CHAR(ST_ENC.FEC_SOL, 'DD') DIA_SOLUCION,
                    MAX(ST_DET.FEC_SOL) FECHA_SOLUCION,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_SOL, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') DIAS_SOLUCION,
                    DECODE(MIN(ST_DET.TIPO_SOL) || '-' || MAX(ST_DET.TIPO_SOL),'CAS-CAS','CAS',
                    'CAM-CAM', 'CAM', 'TAL-TAL', 'TAL', 'DEV-DEV', 'DEV', 'OTRAS:' || MIN(ST_DET.TIPO_SOL) || '-' || MAX(ST_DET.TIPO_SOL)) SOLUCION,
                    FAC_ENC.FAC FACTURA,
                    FAC_ENC.TVEN TIPO_VENTA,
                    decode((select Count(distinct fact_numero)
                    from facturas@DATAMART
                    where facturas.fact_unidad < FAC_ENC.EMI
                    and fact_estado = 'ENTREGADA'
                    and fact_cliente = CLIENTES.CLIENTE_CODIGO),
                    0,
                    'Nuevo',
                    'Conocido') Tipo_Cliente,
                    ARTICULO.NOM NOMBRE_ARTICULO,
                    ARTICULO.C_CAT CATEGORIA_ARTICULO,
                    ST_ENC.REABIERTO REABIERTO
                    FROM JAMAR.ST_DANOS    ST_DANOS,
                    JAMAR.ST_DET      ST_DET,
                    JAMAR.ST_ENC      ST_ENC,
                    JAMAR.ST_VISITA   ST_VISITA,
                    JAMAR.CAUSAS_CES  CAUSAS_CES,
                    JAMAR.FUENTES_CES FUENTES_CES,
                    JAMAR.FAC_ENC     FAC_ENC,
                    JAMAR.ARTICULO    ARTICULO,
                    CLIENTES@datamart, ZONA_GEOGRAFICA@datamart
                    WHERE ((ST_DET.C_EMP = ST_DANOS.C_EMP AND ST_DET.AGE = ST_DANOS.AGE AND
                    ST_DET.SER = ST_DANOS.SER AND ST_DET.CSC = ST_DANOS.CSC) AND
                    (ST_ENC.C_EMP = ST_DET.C_EMP AND ST_ENC.AGE = ST_DET.AGE AND
                    ST_ENC.SER = ST_DET.SER) AND
                    (ST_ENC.C_EMP = ST_VISITA.C_EMP AND ST_ENC.AGE = ST_VISITA.AGE AND
                    ST_ENC.SER = ST_VISITA.SER) AND (CAUSAS_CES.C_EMP = ST_DANOS.C_EMP AND
                    CAUSAS_CES.FUENTE = ST_DANOS.FUE AND
                    CAUSAS_CES.CAUSA = ST_DANOS.CAUSAL) AND
                    (FUENTES_CES.C_EMP = CAUSAS_CES.C_EMP AND
                    FUENTES_CES.FUENTE = CAUSAS_CES.FUENTE) AND
                    (FUENTES_CES.C_EMP = ST_DANOS.C_EMP AND
                    FUENTES_CES.FUENTE = ST_DANOS.FUE) AND
                    (FAC_ENC.C_EMP = ST_ENC.C_EMP AND FAC_ENC.C_AGR = ST_ENC.AGE AND
                    FAC_ENC.PER = ST_ENC.PER_FAC AND FAC_ENC.FAC = ST_ENC.FAC) AND
                    (ARTICULO.C_EMP = ST_DET.C_EMP AND ARTICULO.COD = ST_DET.COD))
                    and ST_ENC.CLTE = CLIENTES.CLIENTE_CODIGO
                    and CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO
                    AND ZONA_GEOGRAFICA.DPTO_DESCRIPCION IN ('ATLANTICO', 'BOLIVAR', 'CESAR', 'MAGDALENA', 'ANTIOQUIA', 'SANTANDER', 'CUNDINAMARCA')
                    AND (CAUSAS_CES.DESCRI NOT IN
                    (('ERROR EN SISTEMA DECALIFICACION DE VIAJE')))
                    AND (CAUSAS_CES.DESCRI NOT IN (('SERVICIO TOMADO POR ERROR')))
                    AND (CAUSAS_CES.DESCRI NOT IN
                    ((UPPER('CONTACTO NO EFECTIVO CON CLIENTE'))))
                    AND (ST_ENC.TIPO_SER NOT IN (('DDESPACHO')))
                    AND (ST_ENC.FEC_SOL between TO_DATE('",Fecha_Inicio,"', 'dd-mm-yyyy') and TO_DATE('",Fecha_Fin,"', 'dd-mm-yyyy'))
                    AND (ST_ENC.EST = 'C')
                    AND (ST_DET.C_EMP = 'JA')
                    AND ST_ENC.TIPO_SER NOT IN ('ARMADO') 
                    GROUP BY TO_CHAR(ST_ENC.FEC_SOL, 'DD'),
                    TO_DATE(TO_CHAR(ST_ENC.FEC_SOL, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY'),
                    JAMAR.TRAE_NOMBRE('JA', ST_ENC.CLTE),
                    CLIENTES.CLIENTE_NOMBRE,
                    CLIENTES.CLIENTE_PRINOM,
                    ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
                    ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
                    ZONA_GEOGRAFICA.BARR_DESCRIPCION,
                    CLIENTES.CLIENTE_DIR,
                    CLIENTES.CLIENTE_CELULAR,
                    CLIENTES.CLIENTES_EMAIL,
                    CLIENTES.CLIENTES_SEGMENTO,
                    CLIENTES.cliente_seg_corp,
                    CLIENTES.CLIENTES_SEXO,
                    CLIENTES.CLIENTE_ESTCIVIL,
                    CLIENTES.CLIENTE_OCUP,
                    ZONA_GEOGRAFICA.ESTRATO,
                    trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360),
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY'),
                    DECODE((TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'),
                    'DD-MM-YYYY') - TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY')),1,'SI',2,'SI',0,'SI',3,'SI','NO'),
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY'),
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY'),
                    JAMAR.FVCREPTELTIPO('JA', ST_ENC.CLTE, 'CELULAR', 2),
                    ST_DET.CLASE,
                    ST_ENC.C_EMP,
                    ST_ENC.AGE,
                    ST_ENC.SER,
                    ST_ENC.CLTE,
                    ST_ENC.EST,
                    ST_ENC.DEP,
                    ST_ENC.CIU,
                    CAUSAS_CES.DESCRI,
                    FUENTES_CES.DESCRI,
                    TO_CHAR(ST_ENC.FEC_SOL, 'YYYY'),
                    TO_CHAR(ST_ENC.FEC_SOL, 'Month'),
                    ST_ENC.PRIORITARIO,
                    ST_ENC.TIPO_SER,
                    JAMAR.FDTFECHENTALLER(ST_DET.C_EMP, ST_DET.SER, ST_DET.AGE),
                    ARTICULO.NOM,
                    ARTICULO.C_CAT,
                    ST_ENC.REABIERTO,
                    FAC_ENC.FAC,
                    FAC_ENC.TVEN,
                    FAC_ENC.EMI,
                    CLIENTES.CLIENTE_CODIGO
                    ORDER BY ST_ENC.SER ASC,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY') ASC,
                    ST_ENC.DEP ASC,
                    ST_ENC.PRIORITARIO ASC,
                    DECODE((TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'),
                    'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY')),
                    1,
                    'SI',
                    2,
                    'SI',
                    0,
                    'SI',
                    3,
                    'SI',
                    'NO') ASC,
                    ST_ENC.CIU ASC,
                    ST_ENC.AGE ASC,
                    ST_ENC.CLTE ASC,
                    JAMAR.TRAE_NOMBRE('JA', ST_ENC.CLTE) ASC,
                    ST_ENC.EST ASC,
                    ST_ENC.REABIERTO ASC,
                    DECODE(MIN(ST_DET.TIPO_SOL) || '-' || MAX(ST_DET.TIPO_SOL),
                    'CAS-CAS',
                    'CAS',
                    'CAM-CAM',
                    'CAM',
                    'TAL-TAL',
                    'TAL',
                    'DEV-DEV',
                    'DEV',
                    'OTRAS:' || MIN(ST_DET.TIPO_SOL) || '-' ||
                    MAX(ST_DET.TIPO_SOL)) ASC,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_SOL, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') ASC,
                    MAX(ST_DET.FEC_SOL) ASC,
                    TO_DATE(TO_CHAR(ST_ENC.FEC_GEN, 'DD-MM-YYYY'), 'DD-MM-YYYY') -
                    TO_DATE(TO_CHAR(FAC_ENC.EMI, 'DD-MM-YYYY'), 'DD-MM-YYYY') ASC,
                    JAMAR.FDTFECHENTALLER(ST_DET.C_EMP, ST_DET.SER, ST_DET.AGE) ASC,
                    ARTICULO.C_CAT ASC,
                    JAMAR.TRAE_NOMBRE(ST_ENC.C_EMP, MAX(ST_VISITA.TEC)) ASC,
                    JAMAR.FVCREPTELTIPO('JA', ST_ENC.CLTE, 'CELULAR', 2) ASC,
                    ST_DET.CLASE ASC,
                    FUENTES_CES.DESCRI ASC,
                    ARTICULO.NOM ASC,
                    CAUSAS_CES.DESCRI ASC",sep = "")


system.time({
  DWJAMAR_Connection =  odbcConnect(Conexion_2, uid = USER_2, pwd = PWD_2)
  BD_SAC_4 =  sqlQuery(DWJAMAR_Connection,BD_RECLAMOS)
  odbcClose(DWJAMAR_Connection)
}
)

BD_SAC_4$ALEATORIO = runif(nrow(BD_SAC_4))

BD_SAC_4 = with(BD_SAC_4,BD_SAC_4[order(DPTO_DESCRIPCION,ALEATORIO, decreasing=TRUE),])


gc(reset = T)
write.xlsx2(BD_SAC_4, file=paste(VectorFecha[2],". BD Piloto Encuestas SAC - ",months.Date(as.POSIXct(Fecha_Inicio)),".xlsx", sep = ""), sheetName = "BD_RECLAMOS",
            row.names = FALSE, append = TRUE)



###rm(list = c("BD_SAC_1","BD_SAC_2","BD_SAC_3","BD_SAC_4","BD_BANCO","BD_QUEJAS","BD_RECLAMOS","BD_RETAIL"))
rm(list = c("BD_SAC_1","BD_SAC_2","BD_SAC_3"))
rm(data)
gc(reset = T)
