
#### Selección muestra:
setwd("D:/OneDrive - Muebles Jamar/2019/Muestra para Paola")

library(Rcmdr)

MarcoMuestral <- 
  readXL("D:/OneDrive - Muebles Jamar/2019/Muestra para Paola/BD Clientes Estudio - Paola Daza.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="BD", stringsAsFactors=TRUE)



MarcoMuestral <- within(MarcoMuestral, {
  ESTRATO <- as.factor(ESTRATO)
})

## Eliminar ceros y perdidos:

MarcoMuestral[MarcoMuestral$EDAD==0,"CLIENTE_NOMBRE"]

MarcoMuestral=MarcoMuestral[is.na(MarcoMuestral$EDAD)==F,]


MarcoMuestral <- within(MarcoMuestral, {
  EDAD_CAT <- as.factor(EDAD)
})

## Eliminar edades superiores a 85 años:

MarcoMuestral <-MarcoMuestral[MarcoMuestral$EDAD<=85,]

#DatosPrueba=data.frame(x=rnorm(10))

#within.data.frame(DatosPrueba,rango)

DatosPrueba=MarcoMuestral
DatosPrueba$x <- with(DatosPrueba, EDAD)


for (i in 1:length(DatosPrueba$x)) {
  
  if(DatosPrueba$x[i]< 26){DatosPrueba$Edad_cat[i]="Entre 18 y 25"}
  if((26 <=DatosPrueba$x[i])& (DatosPrueba$x[i]<35)){DatosPrueba$Edad_cat[i]="Entre 26 y 35"}
  if(36<=DatosPrueba$x[i]){DatosPrueba$Edad_cat[i]="De 36 en adelante"} 
  
}
DatosPrueba <- within(DatosPrueba, {
  x <- NULL 
})

Estratos=round(500*Objeto/100,0)

Sample_09012019_1513 <- StrataSample(data = DatosPrueba, strata1 = 15, 
                                     sstotal= 500, ppstype = "pro", nminimum = 100, neyvar = )
View(Sample_09012019_1513)

library(writexl)

write_xlsx(Sample_09012019_1513,"D:\\OneDrive - Muebles Jamar\\2019\\Muestra para Paola\\FinalSample.xlsx")

muestra500=Sample_09012019_1513


library(sqldf)

names(muestra500)[c(19)] <- c("EDAD_NOMINAL")

MarcoMuestral_2=sqldf("select * from MarcoMuestral where CLIENTE_CODIGO not in (
                      select CLIENTE_CODIGO from muestra500)")


Sample_11012019_1537 <- StrataSample(data = MarcoMuestral_2, strata1 = 15, sstotal= 1000, ppstype = "pro", 
                                     nminimum = 100, neyvar = )


muestra1000=Sample_11012019_1537

write_xlsx(muestra1000,"D:\\OneDrive - Muebles Jamar\\2019\\Muestra para Paola\\SecondSample1000.xlsx")

###################################################################################
##################### EDUCACIÓN FINANCIERA ########################################
###################################################################################

libra





