##### LIBRERIAS #####
library(xlsx)
library(lubridate)
library(RODBC)
#library(samplesize4surveys)
library(sqldf)
library(excel.link)

##### MACRO VARIABLES #####
# Datos de Conexion

servidor = "DATANEW_JAMAR" # Nombre de la conexión ODBC como se creó en el equipo donde se va a ejecutar
usuario =  "dwjamar"
clave =    "dwjamar"

# Datos Ruta de Guardado Archivos

ruta = "D:\\OneDrive - Muebles Jamar\\2019\\Telemercadeo\\OCTUBRE\\BASES"


### SE CREA VARIABLE CON LA CONSULTA ALMACENES

SQL_ALM = paste("select AG.AGENCIA_DESCRIPCION,ZG.DPTO_DESCRIPCION from agencias AG,ZONA_GEOGRAFICA ZG where AG.AGENCIA_ZONA=ZG.ZONA_CODIGO",sep = "")

# Descomentarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
system.time({
   DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
   ALMACENES =  sqlQuery(DWJAMAR_Connection,SQL_ALM)
   odbcClose(DWJAMAR_Connection)
}
)

#################################################################
#################### BD TELEMERCADEO CR #########################
#################################################################

### SE CREA VARIABLE CON LA CONSULTA ROW_NUMBER() OVER(partition by almacen_agr order by almacen_agr) AS fila,

BD_CREDITO = paste("Select 'CR' tipo_bd,
       VENDEDOR as ULTIMO_VENDEDOR,
       aleatorio,
       Cedula,
       trim(almacen_agr) almacen,
       Segmento_me,
       Nombre_completo,
       case
         when Primer_nombre is null then
          trim(substr(trim(Nombre_completo),
                      1,
                      instr(trim(Nombre_completo), ' ', 1, 1) - 1))
         else
          Primer_nombre
       End Primer_Nombre,
       Celular,
       Cel1,
       Cel2,
       cel3,
       Ciudad,
       trim(Dpto) Dpto,
       barrio,
       ultimo_prod_mas_caro,
       ultima_compra,
       OCUPACION,
       EDAD,
       SEXO,
       CLIENTE_HIJOS,
       ESTADO_CIVIL,
       ESTRATO,
       SALAS,
       COLCHONES,
       DOBLE,
       COMEDORES,
       INFANTIL,
       JUVENIL
  from mercadeo_allinone_mes
 where celu_duplicado is null
   and celular is not null
   and primer_nombre is not null
   and tipo_bd <> 'CO'
   and TRIM(segmento_me) in ('CLIENTE NUEVO',
   'CLIENTE NUEVO_SS',
   'EJEMPLAR',
   'NUEVOS ZAFIRO',
   'ORO',
   'ORO_SS',
   'ORO_SS_RIESGO',
   'PEJEMPLAR',
   'PORO',
   'PORO_SS',
   'PRENTABLE',
   'RENTABLE',
   'RENTABLE_SS',
   'ZAFIRO')
   and almacen_agr is not null
   and dpto in ('ANTIOQUIA',
                'ATLANTICO',
                'BOLIVAR',
                'CESAR',
                'CORDOBA',
                'GUAJIRA',
                'SUCRE',
                'SANTANDER',
                'MAGDALENA',
                'CUNDINAMARCA')
 order by almacen_agr",sep = "")

### SE ABRE LA CONEXION ODBC Y SE EJECUTA LA CONSULTA

system.time({
  DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
  BD_CR =  sqlQuery(DWJAMAR_Connection,BD_CREDITO,  dec=",")
  odbcClose(DWJAMAR_Connection)
}
)

## EXCLUYENDO CLIENTES QUE RESIDEN EN DEPARTAMENTOS DONDE NO HAY TIENDAS UBIDACAS

BD_CR = sqldf("select BD_CR.*,ALMACENES.DPTO_DESCRIPCION
              from BD_CR left join ALMACENES on BD_CR.ALMACEN=ALMACENES.AGENCIA_DESCRIPCION
               where BD_CR.DPTO = ALMACENES.DPTO_DESCRIPCION")


### SE ORDENA POR ALMACEN Y ALEATORIO PARA LA MUESTRA

BD_CR = with(BD_CR,BD_CR[order(ALMACEN, ALEATORIO, decreasing=TRUE),])


#################################################################
#################### BD TELEMERCADEO CO #########################
#################################################################

### SE CREA VARIABLE CON LA CONSULTA  Select ROW_NUMBER() OVER(partition by almacen_agr order by almacen_agr) AS fila,

BD_CONTADO = paste("select 'CO' tipo_bd,
       VENDEDOR as ULTIMO_VENDEDOR,
       aleatorio,
       Cedula,
       trim(almacen_agr) almacen,
       Segmento_me,
       Nombre_completo,
       case
         when Primer_nombre is null then
          trim(substr(trim(Nombre_completo),
                      1,
                      instr(trim(Nombre_completo), ' ', 1, 1) - 1))
         else
          Primer_nombre
       End Primer_Nombre,
       Celular,
       Cel1,
       Cel2,
       cel3,
       Ciudad,
       trim(Dpto) Dpto,
       barrio,
       ultimo_prod_mas_caro,
       ultima_compra,
       OCUPACION,
       EDAD,
       SEXO,
       CLIENTE_HIJOS,
       ESTADO_CIVIL,
       ESTRATO,
       SALAS,
       COLCHONES,
       DOBLE,
       COMEDORES,
       INFANTIL,
       JUVENIL
  from mercadeo_allinone_mes
 where celu_duplicado is null
   and celular is not null
   and primer_nombre is not null
   and tipo_bd = 'CO'
   and TRIM(segmento_me) NOT IN ('ACTIVECAMPAIGN','SUSCRIBETE')
   and almacen_agr is not null
   and dpto in ('ANTIOQUIA',
                'ATLANTICO',
                'BOLIVAR',
                'CESAR',
                'CORDOBA',
                'GUAJIRA',
                'SUCRE',
                'SANTANDER',
                'MAGDALENA',
                'CUNDINAMARCA')
 order by almacen_agr",sep = "")

### SE ABRE LA CONEXION ODBC Y SE EJECUTA LA CONSULTA

system.time({
   DWJAMAR_Connection =  odbcConnect(servidor, uid = usuario, pwd = clave)
   BD_CO =  sqlQuery(DWJAMAR_Connection,BD_CONTADO,  dec="," )
   odbcClose(DWJAMAR_Connection)
}
)

## EXCLUYENDO CLIENTES QUE RESIDEN EN DEPARTAMENTOS DONDE NO HAY TIENDAS UBIDACAS

BD_CO = sqldf("select BD_CO.*,ALMACENES.DPTO_DESCRIPCION
              from BD_CO left join ALMACENES on BD_CO.ALMACEN=ALMACENES.AGENCIA_DESCRIPCION
               where BD_CO.DPTO = ALMACENES.DPTO_DESCRIPCION")


### SE ORDENA POR ALMACEN Y ALEATORIO PARA LA MUESTRA

BD_CO = with(BD_CO,BD_CO[order(ALMACEN, ALEATORIO, decreasing=TRUE),])

### GENERAR PASSWORDS ALEATORIOS:  
# Descomentarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr
PASSWORDS=data.frame(AGENCIA=levels(BD_CO$ALMACEN), "Password"="")

attach(PASSWORDS)
PASSWORDS$Password=as.character(PASSWORDS$Password)
for (i  in 1:nrow(PASSWORDS)) {
  PASSWORDS$Password[i]=paste0(sample(c(LETTERS,letters,0:9),size = 5,replace = T ),collapse ="")

}
detach(PASSWORDS)

### GENERACION DE ARCHIVOS Y ASIGNACION DE CONTRASEÑAS
BD_TOTAL=rbind(BD_CR,BD_CO)

rm(list = c("BD_CR","BD_CO"))

#i=1
setwd(ruta)

attach(BD_TOTAL)
for (i in 1:length(levels(ALMACEN))) {
   DATOS_TEMP = head(BD_TOTAL[(ALMACEN==levels(ALMACEN)[i])&(TIPO_BD=="CR"),],2500)
   
   write.xlsx2(DATOS_TEMP, file=paste("Telemercadeo_",months(Sys.Date()),"_",year(Sys.Date()),"_",levels(ALMACEN)[i],".xlsx",sep = ""), sheetName = "CREDITO",
               row.names = FALSE, append = TRUE)
   
   
   DATOS_TEMP = head(BD_TOTAL[(ALMACEN==levels(ALMACEN)[i])&(TIPO_BD=="CO"),],2500)
   
   write.xlsx2(DATOS_TEMP, file=paste("Telemercadeo_",months(Sys.Date()),"_",year(Sys.Date()),"_",levels(ALMACEN)[i],".xlsx",sep = ""), sheetName = "CONTADO",
               row.names = FALSE, append = TRUE)
   #gc(reset = T)
   xl.workbook.open(paste("Telemercadeo_",months(Sys.Date()),"_",year(Sys.Date()),"_",levels(ALMACEN)[i],".xlsx",sep = ""))
   xl.workbook.save(paste("Telemercadeo_",months(Sys.Date()),"_",year(Sys.Date()),"_",levels(ALMACEN)[i],".xlsx",sep = ""),password = PASSWORDS[PASSWORDS$AGENCIA==levels(ALMACEN)[i],2])
   xl.workbook.close(paste("Telemercadeo_",months(Sys.Date()),"_",year(Sys.Date()),"_",levels(ALMACEN)[i],".xlsx",sep = ""))

}
detach(BD_TOTAL)

write.xlsx2(PASSWORDS, file=paste("PASSWORDS_",months(Sys.Date()),"_",year(Sys.Date()),".xlsx",sep = ""), sheetName = "CLAVES",
            row.names = FALSE, append = TRUE)








