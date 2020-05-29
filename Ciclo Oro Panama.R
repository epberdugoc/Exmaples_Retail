options(java.parameters="-Xmx1g")
##### LIBRERIAS #####
library(xlsx)
library(lubridate)
library(RODBC)
library(sqldf)
library(openxlsx)


# Datos de Conexion

servidor = "datanew" # Nombre de la conexión ODBC como se creó en el equipo donde se va a ejecutar
usuario =  "dwpanama"
clave =    "dwpanama"

# Datos Ruta de Guardado Archivos

ruta = "D:\\OneDrive - Muebles Jamar\\hmedina\\Documents\\02. Mercadeo\\05. Victor Antequera\\Ciclos\\02. Panama\\Oro\\2019"
setwd(ruta)

#################################################################
######################## BD ORO ##############################
#################################################################

### SE CREA VARIABLE CON LA CONSULTA

SQL_ORO = paste("SELECT CLIENTES.CLIENTE_CODIGO CLIENTE,
       CARTERA.CART_CTACAR CUENTA_CARTERA,
       AGENCIAS.AGENCIA_CODIGO AGENCIA_CARTERA,
       cartera.CART_TIPOCUENTA,
       CLIENTES.CLIENTES_SEGMENTO SEGMENTO,
       CLIENTES.CLIENTE_NOMBRE NOMBRE,
       CLIENTES.CLIENTE_PRINOM PRIMER_NOMBRE,
       CLIENTES.CLIENTE_CELULAR CELULAR,
       CLIENTES.CLIENTE_TEL TELEFONO,
       ZONA_GEOGRAFICA.DPTO_DESCRIPCION DEPARTAMENTO,
       ZONA_GEOGRAFICA.CIUD_DESCRIPCION CIUDAD,
       ZONA_GEOGRAFICA.BARR_DESCRIPCION BARRIO,
       CLIENTES.CLIENTE_DIR DIRECCION,
       AVG(DECODE(CARTERA.CART_TOTAL,
                  0,
                  0,
                  1 - (CARTERA.CART_SALDO / CARTERA.CART_TOTAL))) POR_PAGO_DEUDA,
       sum(CARTERA.CART_TOTAL) VALOR_TOTAL,
       sum(CARTERA.CART_SALDO) SALDO,
       sum(CARTERA.CART_CUOTA) VALOR_CUOTA,
       sum(CARTERA.CART_TOTAL) - sum(CARTERA.CART_SALDO) HAN_PAGADO
  FROM CLIENTES, ZONA_GEOGRAFICA, CARTERA, AGENCIAS, UNIDAD_TIEMPO
 WHERE (CLIENTES.CLIENTE_ZONA = ZONA_GEOGRAFICA.ZONA_CODIGO)
   AND (CARTERA.CART_AGENCIA = AGENCIAS.AGENCIA_CODIGO)
   AND (CLIENTES.CLIENTE_CODIGO = CARTERA.CART_CLIENTE)
   AND (UNIDAD_TIEMPO.UNIDAD_CODIGO = CARTERA.CART_UNIDAD)
   AND (CLIENTES.CLIENTES_SEGMENTO IN ('ORO') AND
       UNIDAD_TIEMPO.UNIDAD_ANO = ",year(Sys.Date())," AND UNIDAD_TIEMPO.UNIDAD_MES = ",month(Sys.Date()),")
    GROUP BY CLIENTES.CLIENTES_SEGMENTO,
          CLIENTES.CLIENTE_CODIGO,
          CLIENTES.CLIENTE_NOMBRE,
          CLIENTES.CLIENTE_CELULAR,
          CLIENTES.CLIENTE_TEL,
          CLIENTES.CLIENTE_DIR,
          ZONA_GEOGRAFICA.DPTO_DESCRIPCION,
          ZONA_GEOGRAFICA.CIUD_DESCRIPCION,
          ZONA_GEOGRAFICA.BARR_DESCRIPCION,
          CLIENTES.CLIENTE_PRINOM,
          CARTERA.CART_CTACAR,
          AGENCIAS.AGENCIA_CODIGO,
          cartera.CART_TIPOCUENTA
HAVING AVG(DECODE(CARTERA.CART_TOTAL, 0, 0, 1 - (CARTERA.CART_SALDO / CARTERA.CART_TOTAL))) between 0.60 and 1
",sep = "")

### SE ABRE LA CONEXION ODBC Y SE EJECUTA LA CONSULTA

system.time({
  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  BD_ORO =  sqlQuery(DWJAMAR_Connection,SQL_ORO, dec=".")
  odbcClose(DWJAMAR_Connection)
}
)


### SE CREA COLUMNA PORC_PAGO 

BD_ORO$PORC_PAGO = round(BD_ORO$HAN_PAGADO / BD_ORO$VALOR_TOTAL,3)


### SE ORDENA POR CEDULA Y PORC_PAGO PARA BORRAR DUPLICADOS

BD_ORO = with(BD_ORO,BD_ORO[order(CLIENTE,PORC_PAGO, decreasing=FALSE),])

### SE ELEMINAN LAS CEDULAS REPETIDAS DEJANDO EL PORC_PAGO MAS BAJO

attach(BD_ORO)
BD_ORO = BD_ORO[!duplicated(CLIENTE),]
detach(BD_ORO)


### SE CREA LA MATRIX DE LOS LIMITES DE PORCENTAJES DE PAGO PARA CADA HOJA

LIMITES = matrix(c(0.6,0.8,0.8,0.9,0.9,1.01),byrow = T, ncol = 2)

### TABLA CON LOS RESUMENES PARA CADA BD

RESUMEN = data.frame("ITEM" = c("De 60 a 79.9 ","De 80 a 89.9 ","De 90 a 100 "), "CANTIDAD" = 0,
                     stringsAsFactors = F)


### CICLO PARA CREAR ARCHIVO Y HOJAS CON CADA BD

attach(BD_ORO)
for (i in 1:3) {
  DATOS_TEMP = BD_ORO[(PORC_PAGO>=LIMITES[i,1])&(PORC_PAGO<LIMITES[i,2]),]
  
  RESUMEN$CANTIDAD[i] = nrow(DATOS_TEMP)
  
  write.xlsx2(DATOS_TEMP, file=paste(month(Sys.Date()), ". BD CICLO ORO ", months(Sys.Date())," ",year(Sys.Date()),".xlsx",sep = ""), sheetName = paste("BD ",100*LIMITES[i,1]," A ", 100*LIMITES[i,2]-1),
              row.names = FALSE, append = TRUE)
  
}
detach(BD_ORO)


### SE ESCRIBE LA TABLA RESUMEN EN EL ARCHIVO

write.xlsx2(RESUMEN, file=paste(month(Sys.Date()), ". BD CICLO ORO ", months(Sys.Date())," ",year(Sys.Date()),".xlsx",sep = ""), sheetName = "RESUMEN",
            row.names = FALSE, append = TRUE )


### ORDENAMIENTO DE LAS HOJAS DEL ARCHIVO

a=loadWorkbook(paste(month(Sys.Date()), ". BD CICLO ORO ", months(Sys.Date())," ",year(Sys.Date()),".xlsx",sep = ""))
worksheetOrder(a) <- c(4,1,2,3)
saveWorkbook(a,paste(month(Sys.Date()), ". BD CICLO ORO ", months(Sys.Date())," ",year(Sys.Date()),".xlsx",sep = ""),overwrite = T)


rm(list = ls(all=T))