
############ TABLAS DE FRECUENCIAS   ##################################

######## Tablas no agrupadas para variables numéricas:

library(descriptr)

DatosSincelejo2018_II$Numero.de.Pedidos_cat=as.factor(as.character( DatosSincelejo2018_II$Numero.de.Pedidos ))

ds_freq_table(DatosSincelejo2018_II,Numero.de.Pedidos_cat)

library(Rcmdr)

library(RcmdrPlugin.KMggplot2)

with(DatosSincelejo2018_II, discretePlot(Numero.de.Pedidos, scale="percent",by=Estado,
                                         xlab="Número de pedidos", ylab="Porcentaje de clientes", 
                                         main="Distribución porcentual para el número de pedidos en Sincelejo 2018 II"))


########## Tablas agrupadas para variables numéricas: 

library(descriptr)

ds_freq_table(Vendedores2018,Valor.Venta.Oferta,bins = 8)

