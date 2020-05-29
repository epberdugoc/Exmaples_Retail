options(java.parameters="-Xmx1g")
##### LIBRERIAS #####
library(xlsx)
library(lubridate)
library(RODBC)
library(sqldf)
library(openxlsx)


# Datos de Conexion

servidor = "DATANEW32" # Nombre de la conexión ODBC como se creó en el equipo donde se va a ejecutar
usuario =  "dwjamar"
clave =    "dwjamar"

# Datos Ruta de Guardado Archivos

ruta = "D:\\OneDrive - Muebles Jamar\\hmedina\\Documents\\02. Mercadeo\\05. Victor Antequera\\Ciclos\\01. Colombia\\Nuevos Oro\\2019"
setwd(ruta)

# Datos de Fecha (Se debe colocar siempre 4 dias despues del corte del mes anterior)

Fecha_Ejecuta = format(floor_date(Sys.Date()%m-% months(1), unit = "months")%m+% days(3),"%d-%m-%Y")
Fecha_Cartera = format(floor_date(Sys.Date()%m-% months(1), unit = "months")%m+% days(2),"%d-%m-%Y")


### SE CREA BD CON LOS CICLOS ANTERIORES 

system.time({
  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  CICLOS_ANTERIORES =  sqlQuery(DWJAMAR_Connection,"SELECT CLIENTE_CODIGO, CAMPO1 CONSECUTIVO
                                FROM CLIENTES_CAMPANAS
                                WHERE CLIENTE_CAMPANA  = 'BD_NUEVO_ORO'", dec=".")
  odbcClose(DWJAMAR_Connection)
}
)

### SE CONSULTA EL ULTIMO CONSECUTIVO INGRESADO

ULTIMO_CONSECUTIVO =  max(na.omit(CICLOS_ANTERIORES$CONSECUTIVO))


#################################################################
######################## BD NUEVOS ORO ##########################
#################################################################

### SE CREA VARIABLE CON LA CONSULTA

SQL_NORO = paste("SELECT CLIENTES.CLIENTE_CODIGO CLIENTE,
       TRIM(CLIENTES.CLIENTE_NOMBRE) NOMBRE_COMPLETO,
       TRIM(CLIENTES.CLIENTE_PRINOM) || ' ' ||
       DECODE(SUBSTR(TRIM(CLIENTES.CLIENTE_SEGNOM), 1, 1),
              '',
              '',
              SUBSTR(TRIM(CLIENTES.CLIENTE_SEGNOM), 1, 1) || '. ') ||
       TRIM(CLIENTES.CLIENTE_PRIAPE) || ' ' ||
       SUBSTR(TRIM(CLIENTES.CLIENTE_SEGAPE), 1, 1) || '.' NOMBRE_TARJETA,
       CLIENTES.CLIENTE_CELULAR CELULAR,
       CLIENTES.CLIENTES_EMAIL EMAIL,
       ZONA_GEOGRAFICA.DPTO_DESCRIPCION DPTO,
       ZONA_GEOGRAFICA.CIUD_DESCRIPCION CIUDAD,
       ZONA_GEOGRAFICA.BARR_DESCRIPCION BARRIO,
       CLIENTES.CLIENTE_DIR DIRECCION,
       CLIENTES.CLIENTES_SEXO SEXO,
       CLIENTES.CLIENTE_OCUP OCUPACION,
       CLIENTES.CLIENTE_FNAC FECHA_NAC,
       ZONA_GEOGRAFICA.ESTRATO ESTRATO,
       CLIENTES.CLIENTE_HIJOS HIJOS,
       CLIENTES.CLIENTE_FECACTSEG FECHA_SEGMENTACION,
       trunc((sysdate - CLIENTES.CLIENTE_FNAC) / 360) EDAD,
       (SELECT MIN(UNIDAD_TIEMPO.UNIDAD_FECHA) FECHA
          FROM UNIDAD_TIEMPO, CARTERA
         WHERE UNIDAD_TIEMPO.UNIDAD_CODIGO = CARTERA.CART_UNIDAD
           AND CARTERA.CART_SEGMENTO = 'ORO'
           AND TO_DATE(UNIDAD_TIEMPO.UNIDAD_FECHA, 'DD-MM-YYYY') <
               '",Fecha_Cartera,"'
           AND CARTERA.CART_CLIENTE = CLIENTES.CLIENTE_CODIGO) FECHA_FUE_ORO
  FROM CLIENTES, ZONA_GEOGRAFICA
 WHERE CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO
   AND CLIENTES.CLIENTES_SEGMENTO = 'ORO'
   AND trunc(CLIENTES.CLIENTE_FECACTSEG) >=
       to_date('",Fecha_Ejecuta,"', 'dd-mm-yyyy')
   AND ZONA_GEOGRAFICA.DPTO_DESCRIPCION IN
       ('ANTIOQUIA',
        'ATLANTICO',
        'BOLIVAR',
        'CESAR',
        'CUNDINAMARCA',
        'CORDOBA',
        'GUAJIRA',
        'MAGDALENA',
        'SANTANDER',
        'SUCRE')
   AND (select cartera.cart_agencia
          from cartera
         where cart_unidad = to_char(sysdate - 1, 'ddmmyyyy')
           and clientes.cliente_codigo = cart_cliente
           and rownum = 1) not in ('12', '70', '74', '46')
",sep = "")

### SE ABRE LA CONEXION ODBC Y SE EJECUTA LA CONSULTA

system.time({
  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  BD_NORO =  sqlQuery(DWJAMAR_Connection,SQL_NORO, dec=".", as.is=T )
  odbcClose(DWJAMAR_Connection)
}
)


### QUITAMOS LOS QUE YA FUERON ORO

attach(BD_NORO)

BD_NORO = BD_NORO[is.na(FECHA_FUE_ORO)==T,1:16]

detach(BD_NORO)

### QUITAMOS LOS QUE YA LES ENVIAMOS TARJETAS

BD_NORO = sqldf("SELECT * FROM BD_NORO WHERE CLIENTE NOT IN (SELECT CLIENTE_CODIGO FROM CICLOS_ANTERIORES)")


### CREAMOS LOS CAMPOS DE LA ULTIMA COMPRA


BD_NORO$AGENCIA = "JAMAR" 
BD_NORO$ALTERNATIVA =  "TR"
BD_NORO$VENDEDOR = "HIDALGO MEDINA"
BD_NORO$FECHA = as.Date("01-01-1900",format = "%d-%m-%Y")


### SE ORDENA POR DPTO, CIUDAD, BARRIO Y DIRECCION 

BD_NORO = with(BD_NORO,BD_NORO[order(DPTO,CIUDAD,BARRIO,DIRECCION, decreasing=FALSE),])


### CREAMOS EL CAMPO CONSECUTIVO Y LO ASOCIAMOS A CADA CLIENTE

CONSECUTIVO = (ULTIMO_CONSECUTIVO + 1):(ULTIMO_CONSECUTIVO + nrow(BD_NORO))

BD_NORO = cbind(CONSECUTIVO,BD_NORO)


### LLENAMOS LOS DATOS DE LA ULTIMA COMPRA PARA CADA CLIENTE

system.time({
  for (i in 1:dim(BD_NORO)[1]){
    
      DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
      ULTIMA_COMPRA =  sqlQuery(DWJAMAR_Connection,paste("SELECT AGENCIAS.AGENCIA_DESCRIPCION,
       ALTERNATIVA_VENTA.ALT_CODIGO,
       VENDEDORES.VEND_DESCRIPCION,
       Max(UNIDAD_TIEMPO.UNIDAD_FECHA) FECHA
  FROM AGENCIAS, ALTERNATIVA_VENTA, VENDEDORES, UNIDAD_TIEMPO, FACTURAS
 WHERE FACTURAS.FACT_AGENCIA = AGENCIAS.AGENCIA_CODIGO
   AND ALTERNATIVA_VENTA.ALT_CODIGO = FACTURAS.FACT_ALTVTA
   AND FACTURAS.FACT_UNIDAD = UNIDAD_TIEMPO.UNIDAD_CODIGO
   AND VENDEDORES.VEND_CODIGO = FACTURAS.FACT_VENDEDOR
   AND ALTERNATIVA_VENTA.ALT_CODIGO IN ('CR', 'OR', 'TJ')
   AND FACTURAS.FACT_ESTADO = 'ENTREGADA'
   AND FACTURAS.FACT_CLIENTE = '", BD_NORO$CLIENTE[i],"'
   AND ROWNUM = 1
 GROUP BY AGENCIAS.AGENCIA_DESCRIPCION,
          ALTERNATIVA_VENTA.ALT_CODIGO,
          VENDEDORES.VEND_DESCRIPCION
 ORDER BY Max(UNIDAD_TIEMPO.UNIDAD_FECHA) DESC", sep = "" )   ) 
      odbcClose(DWJAMAR_Connection)
      BD_NORO$AGENCIA[i]= as.character(ULTIMA_COMPRA[1,1])
      BD_NORO$ALTERNATIVA[i]= as.character(ULTIMA_COMPRA[1,2])
      BD_NORO$VENDEDOR[i]= as.character(ULTIMA_COMPRA[1,3])
      BD_NORO$FECHA[i]= ULTIMA_COMPRA[1,4]
    
  }
})


### CREAR ARCHIVO

write.xlsx2(BD_NORO, file=paste(month(Sys.Date()), ". Nuevos Oros - ", months(Sys.Date())," ",year(Sys.Date()),".xlsx",sep = ""), sheetName = "Entregado",
            row.names = FALSE, append = TRUE)


## Creamos Vector PAra Subir a BO

attach(BD_NORO)

SUBIR = ifelse(is.na(CLIENTE)==FALSE,paste0("insert into CLIENTES_CAMPANAS (CLIENTE_CODIGO, CLIENTE_CAMPANA, CAMPO1, CAMPO2) values ('",CLIENTE,"','BD_NUEVO_ORO','",CONSECUTIVO,"','",format(Sys.Date(),"%B-%Y"),"')"))

detach(BD_NORO)


## Creamos la funcion consulta

Consulta=function(query){
  sqlQuery(DWJAMAR_Connection,query)
}

## Subimos los datos a BO

DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
S = lapply(SUBIR,Consulta)
odbcClose(DWJAMAR_Connection)


## Borramos Todo
rm(list = ls(all=T))
