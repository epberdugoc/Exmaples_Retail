
#############  CAPACITACION INTERNA R ####################

library(RODBC)

# Crear canal para la connexión:
DWJAMAR_Connection<-  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")

#Obtener información a cerca de la conexion.
odbcGetInfo(DWJAMAR_Connection)

## cONSULTAR TABLAS EN LA BASE
sqlTables(DWJAMAR_Connection, tableType = "TABLE")$TABLE_NAME

#

sqlColumns(DWJAMAR_Connection, "CLIENTES")$COLUMN_NAME

consulta1="select * from CLIENTES where rownum<= 100"

DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
Datos=  sqlQuery(DWJAMAR_Connection,consulta1) 
odbcClose(DWJAMAR_Connection)


library(sqldf)

(resultado=sqldf("select CLIENTE_CODIGO,CLIENTE_NOMBRE,CLIENTE_ESTCIVIL  FROM Datos WHERE CLIENTE_ESTCIVIL='C' "))

class(resultado)

library(readxl)
Alianzas <- read_excel("D:/OneDrive - Muebles Jamar/2019/Alianzas/2 BASES JAMAR PROYECTOS - JUNIO - JULIO 2019.xlsx")



################# EJEMPLO:  
Alianzas$OCUPACION = rep("",nrow(Alianzas))

#rm(i)

t1=system.time({
for (i in 1:dim(Alianzas)[1]){
  
  if(is.na(Alianzas$Cedula[i])==FALSE){
    
    DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
    OCUPACION=  sqlQuery(DWJAMAR_Connection,paste("select CLIENTE_OCUP from CLIENTES where CLIENTE_CODIGO='", Alianzas$Cedula[i],"'", sep = "" )   ) 
    odbcClose(DWJAMAR_Connection)
    Alianzas$OCUPACION[i]=as.character(OCUPACION[1,1])
  }
}
})

# system.time({ 
#   rnomr(100000)
#   })


######################################################################
############## FUNCIONES DEFINIDAS POR EL USUARIO ####################
######################################################################

### otra forma más rápida:

library(readxl)
setwd("D:/OneDrive - Muebles Jamar/2019/Alianzas/")
Alianzas <- read_excel("2 BASES JAMAR PROYECTOS - JUNIO - JULIO 2019.xlsx")


library(RODBC)

Alianzas$OCUPACION = rep("",nrow(Alianzas))
Query=ifelse(is.na(Alianzas$Cedula)==FALSE,paste("select CLIENTE_OCUP from CLIENTES where CLIENTE_CODIGO='", Alianzas$Cedula,"'", sep = "" ),Alianzas$OCUPACION)

### 

Consulta=function(query){
  #require(RODBC) 
  if(query!=""){
  OCUPACION=as.character(sqlQuery(DWJAMAR_Connection,query)[1,1])
  }else{
    OCUPACION=query
  }
  return(OCUPACION)
}

DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
library(RODBC)
Consulta(Query[14])
#S=lapply(Query,Consulta)
odbcClose(DWJAMAR_Connection)

DWJAMAR_Connection =  odbcConnect("DATANEW_JAMAR", uid = "dwjamar", pwd = "dwjamar")
library(RODBC)
t2=system.time({Alianzas$OCUPACION=unlist(lapply(Query,Consulta))})
odbcClose(DWJAMAR_Connection)


##############################################################
######### Creacion y manpulación de matrices #################
##############################################################

## Crear un dataframe "vacío" 
datos=data.frame()

## Editar el data frame, insertando manualmente los naturales del 1 al 9, en forma horizontal:

fix(datos) ## al cerrar la ventana cuadriculada se gurdan los cambios

datos ## Imprime una "aparente" matriz de 3x3

class(datos) ## Confirma que aunque tenga aspecto de matriz; es un objeto de la classe data.frame

## Si se intenta aplicar una función o método para la clase MATRICES, arroja un error:

det(datos) ## intentando calcular el determinante de la matriz: ERROR!! 

t(datos) ## intentando calcular la transpuesta de una matriz: ERROR!! 

## Podemos utilizar un método de "transformación" de clases, que convierte el objeto datos a la clase
## de las matrices:

A=as.matrix(datos); class(A) ## Efectivamente, el objeto A pertenec a la clase de las MATRICES

## Otra forma: En la línea anterior creamos un nuevo objeto, conservando el original. A continuación se modifica 
## la clase y el objeto original cambia.

as(datos,"matrix")

## Ahora podemos aplicar los métodos que antes prodcian errores:

det(A) ## Prácticamente CERO

(B=t(A)) ## Calculando la transpuesta y almacenandola en otro objeto B

B%*%A ## Producto de matricial

# Otra forma: B%*%A es realmente t(A)%*%A. Esta operación recibe el nombre de PRODUCTO CRUZADO y
# cuenta con una función dedicada:

crossprod(A)

B*A ## Producto elemento a alemento (solo para matrices de la misma dimensión)


##############################################################
######### TABLAS PIVOTE CON LA FUNCIÓN RESHAPE ###############
##############################################################

# Archivo consolidado en formato WIDE para el presupuesto del año vigente. 
# descargar desde 
# https://organizacionjamar-my.sharepoint.com/:u:/g/personal/eberdugo_mueblesjamar_com_co/EVhay33mC9NDq5y2hMfqG48BIwvBlkBthXdLPP7tFoCVLw?e=2WCWjs
# Y luego cargar:

# Ruta de acceso al área de trabajo descargada:
ruta="D:/OneDrive - Muebles Jamar/2019/Capacitacion Ciencia de datos/PO_Ventas_Consolidado_2020.RData"

# Carga del área de trabajo

load(ruta)
# Visualización de la tabla en formato WIDE:
View(PO_Ventas_Consolidado_2020)

## Convierte todas las variables tipo "cahracter" a factor:

mydata=PO_Ventas_Consolidado_2020
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
PO_Ventas_Consolidado_2020=mydata
rm(list=c("mydata"))

### EJERCICIO # 1: Transformar la tabla inicial de su formato WIDE al formato LONG en el cual las columnas de los meses se combiene en una sola
### llamada presupuesto y crear una columna llamada UNIDAD_MES para diferenciar el componente longitudinal (tiempo).

# Consultar la ayuda sobre la función reshape:
help(reshape)

######## Preparación de insumos:

## Nombre de la colummna fusionada:
rep_fact="UNIDAD_MES"

Col_unica="PPTO_VENTA_NETA"

## Llamar con la función get()
Nombre_Tabla="PO_Ventas_Consolidado_2020"          

## Combianción de variables que diferencia registros únicos
Llave_unica= c("UNIDAD_ANO","NOMBRE_TIENDA_PPTO","TCRE_TIPO","COD_AGENCIA")       

## Lista de columnas (posiciones o nombres) a fusionar desde el formato WIDE al LONG

# opción 1: posicional
Columnas_fusion=list((ncol(get(Nombre_Tabla))-11):ncol(get(Nombre_Tabla)))  

# opción 2: nombres
meses=c("Ene" ,"Feb" ,"Mar" ,"Abr", "May" ,"Jun", "Jul" ,"Ago" ,"Sep", "Oct" ,"Nov", "Dic")

Columnas_fusion = list(meses)

## Etiquetado de filas:

# opción 1: Automática usanso la llave única.Arroja varios WARNINGS, pero la función se ejecuta
Nombres_filas=NULL

# opción 1: Personalizada, en este caso usando NÚMEROS NATURALES.
Nombres_filas=1:(nrow(get(Nombre_Tabla))*length(unlist(Columnas_fusion)))

PPTO_LONG_2020=reshape(get(Nombre_Tabla), 
                       idvar = Llave_unica,
                       varying = Columnas_fusion,
                       timevar = rep_fact,
                       v.names = Col_unica,
                       direction = "long",
                       new.row.names=Nombres_filas
                       ,times = meses ## para cambiar el nombre de los meses desde números a letras
                       )

mydata=PPTO_LONG_2020
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
PPTO_LONG_2020=mydata
rm(list=c("mydata"))

View(PPTO_LONG_2020)


## Reodenando los niveles del factor "UNIDAD_MES" de forma cronológica Enero a Diciembre:

PPTO_LONG_2020$UNIDAD_MES <- with(PPTO_LONG_2020, factor(UNIDAD_MES, levels=c('Ene','Feb','Mar','Abr','May',
                                                                              'Jun','Jul','Ago','Sep','Oct','Nov','Dic'), ordered=TRUE))



### EJERCICIO # 2: Devolvel la tabla PPTO_LONG_2020 de formato LONG a WIDE 

### TAREAAAAAAAAAAAAAAAAAAAAAA

names(PPTO_LONG_2020)

PPTO_WIDE_2020=reshape(PPTO_LONG_2020, idvar = c("UNIDAD_ANO","NOMBRE_TIENDA_PPTO","TCRE_TIPO","COD_AGENCIA" ), 
                       direction = "wide", timevar = "UNIDAD_MES",v.names="PPTO_VENTA_NETA"
                      , sep="_"
                       )



###################################################################
#### BD Clientes Soledad Con Compras Hasta 2017: CLUSTERING #######
###################################################################


### Base con filtros:

library(readxl)

setwd("D:/OneDrive - Muebles Jamar/2020/SELECCION DE MUESTRAS/Telemercadeo clientes sin compras en SOLEDAD")


BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado <- read_excel("BD Clientes Soledad Con Compras Hasta 2018 Act 27 Feb -Entregado.xlsx", 
                                                                   sheet = "DATOS FILTRADOS", col_types = c("text", 
                                                                                                            "text", "text", "text", "text", "text", 
                                                                                                            "numeric", "date", "numeric", "text", 
                                                                                                            "text", "text", "text", "text", "text", 
                                                                                                            "text", "text", "text", "date", "text", 
                                                                                                            "text", "date", "text", "text", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric"))
View(BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado)
###

#' Load useful packages
library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(Rtsne)

#' Load data
##df_total <- read_csv2("bank.csv")

#attach(df)
df=BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado[,c("SEXO","ESTADO_CIVIL","EDAD","NO_HIJOS","OCUPACION","ESTRATO","ALT_VTA_ULTIMA_COMPRA","AGENCIA_ULTIMA_COMPRA","N_PRODUCTOS","N_QR" )]
#detach(df)

mydata=df
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
df=mydata
rm(list=c("mydata"))

#' Compute Gower distance
system.time({ gower_dist <- daisy(df, metric = "gower")})

gower_mat <- as.matrix(gower_dist)

#' Print most similar clients
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

#In business situation, we usually search for a number of clusters both meaningful and easy to remember,
#i.e. 2 to 8 maximum. The silhouette figure helps us identify the best option(s).

sil_width <- c(NA)

for(i in 13:20){  
  assign(paste0("pam_fit_",i),pam(gower_dist, diss = TRUE, k = i))
  sil_width[i] <- get(paste0("pam_fit_",i))$silinfo$avg.width  
}

##
windows()
plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)

##Summary of each cluster
#k <- 10
#pam_fit_10 <- pam(gower_dist, diss = TRUE, k)

Resumen_Cluster=function(k){
  require(dplyr)
  assign(paste0("pam_results_",k),{ df %>%
      mutate(cluster = get(paste0("pam_fit_",k))$clustering) %>%
      group_by(cluster) %>%
      do(the_summary = summary(.))})
  
  get(paste0("pam_results_",k))$the_summary
}

Resumen_Cluster(12)



# #### GAP  
# library(factoextra)
# set.seed(123)
# fviz_nbclust(x=gower_mat, FUN=cluster::pam, method = "gap_stat", nboot = 5,k.max=2,diss=T)
# +
#   labs(subtitle = "Gap statistic method")

# library(NbClust)
# nb <- NbClust(data=df ,diss=gower_dist,distance=NULL ,min.nc = 2,
#               max.nc = 10, method = "centroid")




###Visualization in a lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))







