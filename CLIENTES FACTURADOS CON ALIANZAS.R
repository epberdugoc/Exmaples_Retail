#### CLIENTES FACTURADOS CON ALIANZAS  #########

library(Rcmdr)

ClientesAlianzas <- readXL("D:/OneDrive - Muebles Jamar/Alianzas/BASES JAMAR ALIANZAS - Abril de 2019.xlsx",
                           rownames=FALSE, header=TRUE, na="", 
                           sheet="InformeDeBancos_NegociosEInvers", stringsAsFactors=TRUE)

FacturacionAbril_2019 <- 
  readXL("D:/OneDrive - Muebles Jamar/Alianzas/BASES JAMAR ALIANZAS - Abril de 2019.xlsx",
         rownames=FALSE, header=TRUE, na="", sheet="Facturación abril 2019", 
         stringsAsFactors=TRUE)

FacturacionAbril_2019 <- 
  within(FacturacionAbril_2019, {
    Codigo.del.Cliente <- 
      as.factor(Codigo.del.Cliente)
  })

library(sqldf)

CruceWhere=sqldf("select t1.Proyecto, t1.Cedula,t2.[Codigo.del.cliente] , t2.[Nombre.del.cliente], t2.[Codigo.Alternativa.Venta] 
      from ClientesAlianzas t1, FacturacionAbril_2019 t2 where t1.Cedula=t2.[Codigo.del.cliente]")


CruceInner=sqldf("select t1.Proyecto, t1.Cedula, t2.[Nombre.del.cliente], t2.[Codigo.Alternativa.Venta] 
      from ClientesAlianzas t1 inner join FacturacionAbril_2019 t2 on t1.Cedula=t2.[Codigo.del.cliente]")

CruceLeft=sqldf("select t1.Proyecto, t1.Cedula, t2.[Nombre.del.cliente], t2.[Codigo.Alternativa.Venta] 
      from ClientesAlianzas t1 left join FacturacionAbril_2019 t2 on t1.Cedula=t2.[Codigo.del.cliente]",stringsAsFactors=T)


CruceLeft$Codigo.Alternativa.Venta=as.factor(CruceLeft$Codigo.Alternativa.Venta)





