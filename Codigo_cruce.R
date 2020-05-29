##############################################
###### Cruce de informaci�n ##################
##############################################



library(Rcmdr)

Encuesta.financiacion <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - AN�LISIS/Encuestas en punto de financiaci�n (respuestas).xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="respuestas", 
         stringsAsFactors=TRUE)


info_add_BD <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - AN�LISIS/Informaci�n adicional encuestas II.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="BD general", 
         stringsAsFactors=TRUE)

info_add_BD <- within(info_add_BD, {
  CLIENTE_CODIGO <- as.factor(CLIENTE_CODIGO)
})


Si_compraron <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - AN�LISIS/Si y No compraron (an�lisis).xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="si compraron (respuestas)", 
         stringsAsFactors=TRUE)
names(Si_compraron)[c(32)] <- c("CEDULA")

library(sqldf)

TEST=sqldf('SELECT Si_compraron.*, info_add_BD.* FROM Si_compraron LEFT JOIN info_add_BD ON Si_compraron.CEDULA= info_add_BD.CLIENTE_CODIGO')


sqldf('SELECT CLIENTE_CODIGO FROM TEST')


TEST[,c("CLIENTE_CODIGO", "CEDULA")]


SI_COMPRARON_ADD=TEST[is.na(TEST$CLIENTE_CODIGO)==F,-62]

library(writexl)

write_xlsx(SI_COMPRARON_ADD,"D:\\Muebles Jamar\\Andres Sencherman Dachner - AN�LISIS\\SI_COMPRARON_ADD.xlsx")

########## AN�LISIS DESCRIPTIVO Y EXPLORATORIO ##############

library(Rcmdr)









