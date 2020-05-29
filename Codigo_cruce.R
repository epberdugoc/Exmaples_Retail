##############################################
###### Cruce de información ##################
##############################################



library(Rcmdr)

Encuesta.financiacion <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - ANÁLISIS/Encuestas en punto de financiación (respuestas).xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="respuestas", 
         stringsAsFactors=TRUE)


info_add_BD <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - ANÁLISIS/Información adicional encuestas II.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="BD general", 
         stringsAsFactors=TRUE)

info_add_BD <- within(info_add_BD, {
  CLIENTE_CODIGO <- as.factor(CLIENTE_CODIGO)
})


Si_compraron <- 
  readXL("D:/Muebles Jamar/Andres Sencherman Dachner - ANÁLISIS/Si y No compraron (análisis).xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="si compraron (respuestas)", 
         stringsAsFactors=TRUE)
names(Si_compraron)[c(32)] <- c("CEDULA")

library(sqldf)

TEST=sqldf('SELECT Si_compraron.*, info_add_BD.* FROM Si_compraron LEFT JOIN info_add_BD ON Si_compraron.CEDULA= info_add_BD.CLIENTE_CODIGO')


sqldf('SELECT CLIENTE_CODIGO FROM TEST')


TEST[,c("CLIENTE_CODIGO", "CEDULA")]


SI_COMPRARON_ADD=TEST[is.na(TEST$CLIENTE_CODIGO)==F,-62]

library(writexl)

write_xlsx(SI_COMPRARON_ADD,"D:\\Muebles Jamar\\Andres Sencherman Dachner - ANÁLISIS\\SI_COMPRARON_ADD.xlsx")

########## ANÁLISIS DESCRIPTIVO Y EXPLORATORIO ##############

library(Rcmdr)










