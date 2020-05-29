##### TABLAS DE FRECUENCIAS PARA VARAIABLES CATEGÓRICAS:

## Tabla para la variable estrato:


Tabla_de_freq=ds_freq_table(DatosSincelejo2018_II,Estrato_ord)

plot(ds_freq_table(DatosSincelejo2018_II,Estrato_ord))

as.data.frame(Tabla_de_freq$ftable)

write_xlsx(as.data.frame(Tabla_de_freq$ftable)  ,"D:\\OneDrive - Muebles Jamar\\2019\\Capacitacion Ciencia de datos\\Tabla_De_frecuencias_estrato.xlsx")
