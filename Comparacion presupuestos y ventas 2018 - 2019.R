
### Comparación presupuestos y ventas 2018 - 2019

library(Rcmdr)

PresupuestoVentas <- readXL("D:/OneDrive - Muebles Jamar/Venta oferta mensual por agencia y alternativa 2018.xls",
                            rownames=FALSE, header=TRUE, na="", sheet="Venta Neta", 
                            stringsAsFactors=TRUE)

library(sqldf)

## V.....
names(PresupuestoVentas)

(resumen.año=sqldf("select UNIDAD_ANO, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO"))
resumen.año <- within(resumen.año, {
  Year <- as.factor(UNIDAD_ANO)
})

require("ggplot2")
#y = resumen.año$ppto_venta_neta/1000
.df <- data.frame(x = resumen.año$UNIDAD_ANO, y=resumen.año$ppto_venta_neta/1000)
#.df <- as.data.frame(with(.df, table(x)))
.plot <- ggplot(data = .df, aes(x = x,y=y , label = y)) + 
  geom_bar(width = 0.7, stat = "identity", fill = "darkgreen") + 
  xlab("AÑO") + 
  ylab("Presupuesto venta neta (miles de millones)") + 
  theme_bw(base_size = 14, base_family = "sans")+ ylim(0, 300)+ 
  geom_text(aes(label=round(y,1)), vjust=-0.95) +
  ggtitle("Compración presupuestos venta neta 2018 - 2019 (corte noviembre)")
print(.plot)
#rm(.df, .plot)

resumen.año.VN = resumen.año[,-3]
resumen.año.VN[2,2]=resumen.año[2,3]

.df <- data.frame(x = resumen.año.VN$UNIDAD_ANO, y=resumen.año.VN$venta_neta/1000)
.plot <- ggplot(data = .df, aes(x = x,y=y)) + 
  geom_bar(width = 0.3, stat = "identity", fill = "red") + 
  xlab("AÑO") + 
  ylab("venta neta (miles de millones)") + geom_step() +
  theme_bw(base_size = 14, base_family = "sans")+ ylim(0, 270)+ 
  geom_text(aes(label=round(y,1)), vjust=-0.95) +
  ggtitle("Compración presupuesto de venta neta 2019 contra venta neta 2018 (corte noviembre)")
print(.plot)




(ResumenAñoAlternativas=sqldf("select UNIDAD_ANO as Year, TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, Alternativa"))

ResumenAñoAlternativas <- within(ResumenAñoAlternativas, {
  Year <- as.factor(Year)
})

.df <- data.frame(x = ResumenAñoAlternativas$Year, s = 
                    ResumenAñoAlternativas$Alternativa, y=ResumenAñoAlternativas$ppto_venta_neta/1000)

.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("Año") + 
  ylab("Presupuesto venta neta (miles de millones)") + geom_text(aes(label=round(y,1)), vjust=-0.95)+
   ylim(0, 180) + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines")) +
  ggtitle("Presupuestos venta neta 2018 - 2019 por alternativa")
print(.plot)

ResumenAñoAlternativas[3:4,3]=ResumenAñoAlternativas[3:4,4]

.df <- data.frame(x = ResumenAñoAlternativas$Year, s = 
                    ResumenAñoAlternativas$Alternativa, y=ResumenAñoAlternativas$venta_neta/1000)

.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("Año") + 
  ylab("Venta neta (miles de millones)") + geom_text(aes(label=round(y,1)), vjust=-0.95)+
  ylim(0, 180) + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines")) +
  ggtitle("Varaición del presupuesto 2019 contra la veta neta de 2018")
print(.plot)


(resumen.mes=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES"))

resumen.mes$var_ppto_vn=rep(0,22)

for(i in 12:22){
  resumen.mes$var_ppto_vn[i]=100*(resumen.mes$ppto_venta_neta[i]/resumen.mes$ppto_venta_neta[i-11]-1)
}

resumen.mes$var_ppto_vs_ventas=rep(0,22)

for(i in 12:22){
  resumen.mes$var_ppto_vs_ventas[i]=100*(resumen.mes$ppto_venta_neta[i]/resumen.mes$venta_neta[i-11]-1)
}

(resumen.mes_2019=resumen.mes[12:22,])


.df <- data.frame(x = resumen.mes_2019$Mes, y = resumen.mes_2019$var_ppto_vn)
.df <- .df[order(.df$x), ]
.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_point() + 
  geom_line(size = 1) + 
  scale_y_continuous(expand = c(0.01, 0),limits=c(-15,12)) + 
  xlab("Mes") + geom_text(aes(label=round(y,1)), vjust=-0.05, hjust=-0.35) + 
  ylab("Variación porcentual (%)") +
  theme_bw(base_size = 14, base_family = "sans") + geom_hline(aes(yintercept = 0), colour="blue")+
  ggtitle("Varaición mensual presupuesto venta neta 2019 vs 2018") +
  scale_x_discrete(name ="Mes", limits=1:11)
  print(.plot)
# ylim(-15, 15)+

  
  
  .df <- data.frame(x = resumen.mes_2019$Mes, y = resumen.mes_2019$var_ppto_vs_ventas)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0),limits=c(0,18)) + 
    xlab("Mes") + geom_text(aes(label=round(y,1)), vjust=-0.05, hjust=-0.35) + 
    ylab("Variación porcentual (%)") +
    theme_bw(base_size = 14, base_family = "sans") + geom_hline(aes(yintercept = 10), colour="blue")+
    ggtitle("Variación mensual presupuesto 2019 vs venta neta 2018") +
    scale_x_discrete(name ="Mes", limits=1:11)
  print(.plot)

  
(resumen.mes_2019.alternativa=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES, Alternativa"))
  
  resumen.mes_2019.alternativa$var_ppto_vs_ventas=rep(0,44)
   
  for(i in 23:44){
    resumen.mes_2019.alternativa$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.alternativa$ppto_venta_neta[i]/resumen.mes_2019.alternativa$venta_neta[i-22]-1)
  }
 
  resumen.mes_2019.alternativa=resumen.mes_2019.alternativa[23:44,] 
  
  
  .df <- data.frame(x = resumen.mes_2019.alternativa$Mes, y = resumen.mes_2019.alternativa$var_ppto_vs_ventas, z = 
                      resumen.mes_2019.alternativa$Alternativa)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0), limits = c(-5,25)) + 
    xlab("Mes") + 
    ylab("Varaición porcentual (%)") + 
    labs(colour = "Alternativa", shape = "Alternativa") + 
    theme_bw(base_size = 14, base_family = "sans") + 
    theme(legend.position = "right") + geom_hline(aes(yintercept = 0), colour="black")+
    geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35)+
    scale_x_discrete(name ="Mes", limits=1:12)
  print(.plot)
  
  ### venta presupuesto neta total 2019 vs venta neta real 2018
  
(resumen.mes_2019.tiendas=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,AGENCIA, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO, UNIDAD_MES, AGENCIA"))
  resumen.mes_2019.tiendas$var_ppto_vs_ventas=rep(0,418)  

  for(i in 210:418){
    resumen.mes_2019.tiendas$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.tiendas$ppto_venta_neta[i]/
                                                          resumen.mes_2019.tiendas$venta_neta[i-209]-1)
  }
  
  resumen.mes_2019.tiendas=resumen.mes_2019.tiendas[210:418,]
  
  resumen.mes_2019.tiendasComparables=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA!="JAMAR BOSA",]

  
  
  .df <- data.frame(x = resumen.mes_2019.tiendasComparables$Mes, y = resumen.mes_2019.tiendasComparables$var_ppto_vs_ventas, 
                    s = resumen.mes_2019.tiendasComparables$AGENCIA)
  .df <- .df[order(.df$x), ]
  .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
    geom_point() + 
    geom_line(size = 1) + 
    scale_y_continuous(expand = c(0.01, 0)) + 
    facet_wrap( ~ s) + 
    xlab("Mes") +  
    ylab("Varaición porcentual (%)") +
    theme_bw(base_size = 14, base_family = "sans") + 
    theme(panel.spacing = unit(0.3, "lines")) +
    ggtitle("Variación mensual presupuesto 2019 vs venta neta real 2018 por agencias (excepto Bosa)") +
    scale_x_discrete(name ="Mes", limits=1:11)
    print(.plot)
  
    resumen.mes_2019.Bosa=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA=="JAMAR BOSA",]
  
  
    
    .df <- data.frame(x = resumen.mes_2019.Bosa$Mes[4:11], y = resumen.mes_2019.Bosa$var_ppto_vs_ventas[4:11])
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
      geom_area(alpha = 0.3) + 
      scale_y_continuous(expand = c(0.01, 0), limits =c(-2,800) ) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 agencia Bosa") + 
      ggthemes::theme_foundation(base_size = 14, base_family = "sans") +
      scale_x_discrete(name ="Mes", limits=4:11) +
    geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35)
    print(.plot)
  
    PresupuestoVentas2019=PresupuestoVentas[PresupuestoVentas$UNIDAD_ANO==2019,]
    
    PresupuestoVentas2019 <- within(PresupuestoVentas2019, {
      UNIDAD_MES <- as.factor(UNIDAD_MES)
    })
    
    (sqldf("select Year, Mes, avg(venta_neta), count(*), avg(ppto_venta_neta) 
                   from [resumen.mes_2019.tiendasComparables] group by Year, Mes"))
    
    
    
    # (sqldf("select Year, avg(venta_neta), count(*), avg(ppto_venta_neta) 
    #                from [resumen.mes_2019.Bosa] group by Year"))
    
    
    resumen.mes_2018.Bosa=resumen.mes_2019.tiendas[resumen.mes_2019.tiendas$AGENCIA=="JAMAR BOSA",]
    
    attach(resumen.mes_2018.Bosa)
    resumen.mes_2019.Q1.Bosa= resumen.mes_2018.Bosa[(Year==2019)&(Mes<4),]
    resumen.mes_2019.Q1.Bosa$var_ppto_vs_ventas=100*(ppto_venta_neta/579-1)
    detach(resumen.mes_2018.Bosa)
    
    .df <- data.frame(x = resumen.mes_2019.Q1.Bosa$Mes, y = resumen.mes_2019.Q1.Bosa$var_ppto_vn_promedio)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0), limits = c(0, 30)) + 
      ylab("Varaición porcentual (%)") + 
      labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 agencia Bosa (meses no comparables)") + 
      ggthemes::theme_base(base_size = 14, base_family = "sans") +
      geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.65, hjust=0.05)+
      scale_x_discrete(name ="Mes", limits=1:3)
    print(.plot)
    
    (resumen.mes_2019.tiendas.alternativa=sqldf("select UNIDAD_ANO as Year,UNIDAD_MES as Mes,AGENCIA,TCRE_TIPO as Alternativa, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO,AGENCIA, UNIDAD_MES,TCRE_TIPO "))
    
    resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas=rep(0,836)
    
    for(i in 419:836){
      resumen.mes_2019.tiendas.alternativa$var_ppto_vs_ventas[i]=100*(resumen.mes_2019.tiendas.alternativa$ppto_venta_neta[i]/
                                                                        resumen.mes_2019.tiendas.alternativa$venta_neta[i-418]-1)
    }
    
    #attach(resumen.mes_2019.tiendas.alternativa)
    resumen.mes_2019.tiendas.comparables.alternativa=resumen.mes_2019.tiendas.alternativa[(resumen.mes_2019.tiendas.alternativa$Year=="2019")&(resumen.mes_2019.tiendas.alternativa$AGENCIA!="JAMAR BOSA"),]
    #detach(resumen.mes_2019.tiendas.alternativa)
    
    
    .df <- data.frame(x = resumen.mes_2019.tiendas.comparables.alternativa$Mes, y = 
                        resumen.mes_2019.tiendas.comparables.alternativa$var_ppto_vs_ventas, z = 
                        resumen.mes_2019.tiendas.comparables.alternativa$Alternativa, t = resumen.mes_2019.tiendas.comparables.alternativa$AGENCIA)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0)) + 
      facet_wrap( ~ t) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(colour = "Alternativa", shape = "Alternativa") + 
      #labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 por agencias y alternativas (excepto Bosa)") + 
      theme_bw(base_size = 14, base_family = "sans") + 
      theme(panel.spacing = unit(0.3, "lines"), legend.position = "right")+
      scale_x_discrete(name ="Mes", limits=1:11)+ geom_hline(aes(yintercept = 0), colour="black")
    print(.plot)
    
    
    resumen.mes_2019.Bosa.alternativa=resumen.mes_2019.tiendas.alternativa[(resumen.mes_2019.tiendas.alternativa$Year=="2019")&(resumen.mes_2019.tiendas.alternativa$AGENCIA=="JAMAR BOSA"),]
    
    resumen.mes_2019.Bosa.alternativa[seq(2,6,2),7]=100*(resumen.mes_2019.Bosa.alternativa[seq(2,6,2),6]/489.8-1)
    
    resumen.mes_2019.Bosa.alternativa[seq(1,5,2),7]=100*(resumen.mes_2019.Bosa.alternativa[seq(1,5,2),6]/89.4-1)
    
    
    
    .df <- data.frame(x = resumen.mes_2019.Bosa.alternativa$Mes, y = resumen.mes_2019.Bosa.alternativa$var_ppto_vs_ventas, z = 
                        resumen.mes_2019.Bosa.alternativa$Alternativa)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y, colour = z, shape = z)) + 
      geom_point() + 
      geom_line(size = 1) + 
      scale_y_continuous(expand = c(0.01, 0), limits = c(-10,850)) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(colour = "Alternativa", shape = "Alternativa") + geom_text(aes(label= paste(round(y,1),"%")  ), vjust=-0.65, hjust=0.05) +
      theme_bw(base_size = 14, base_family = "sans") + 
      theme(legend.position = "right") +
      scale_x_discrete(name ="Mes", limits=1:11)
    print(.plot)
    
  
  ## Reformulación del gráfico para bosa en venta neta total (Crédito + Contado)
    
    resumen.mes_2019.Bosa[1:3,6]=resumen.mes_2019.Q1.Bosa[1:3,7]
    
    .df <- data.frame(x = resumen.mes_2019.Bosa$Mes, y = resumen.mes_2019.Bosa$var_ppto_vs_ventas)
    .df <- .df[order(.df$x), ]
    .plot <- ggplot(data = .df, aes(x = x, y = y)) + 
      geom_area(alpha = 0.3) + 
      scale_y_continuous(expand = c(0.01, 0), limits =c(-2,800) ) + 
      xlab("Mes") + 
      ylab("Varaición porcentual (%)") + 
      labs(title = "Variación mensual presupuesto 2019 vs venta neta real 2018 agencia Bosa") + 
      theme_bw(base_size = 14, base_family = "sans") +
      scale_x_discrete(name ="Mes", limits=1:11) +
      geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35)
    print(.plot)
    
    sqldf("select UNIDAD_ANO, sum(VENTA_NETA_FUSION) as venta_neta, sum(PPTO_VENTA_NETA) as ppto_venta_neta 
                   from PresupuestoVentas group by UNIDAD_ANO")
    
    