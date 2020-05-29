#### CRECIMIENTO EN OP (VENTA OFERTA)


library(Rcmdr)

VentaOfertaContadoOP <- 
  readXL("D:/OneDrive - Muebles Jamar/VENTA OFERTA SAB 15NOV2018.xls", 
         rownames=FALSE, header=TRUE, na="", sheet="CONTADO", stringsAsFactors=TRUE)

library(sqldf)

A=sqldf("select Fecha, [Nombre.Dia], [Nombre.de.la.Agencia],sum([No..Clientes.OP]), sum([Valor.Venta.Oferta]) from VentaOfertaContadoOP group by Fecha, 
      [Nombre.Dia], [Nombre.de.la.Agencia] HAVING  [Nombre.de.la.Agencia] NOT IN ('JAMAR BOSA','JAMAR DE LA CUESTA','JAMAR SUR','JAMAR PLAZA DEL SOL',
        'JAMAR METROPOLITANO')")
A <- droplevels(A)
names(A) <- make.names(names(A))

names(A) <- make.names(names(A))
A <- with(A, A[order(Nombre.de.la.Agencia, Fecha, decreasing=FALSE), ])

A$ALTERNATIVA <- with(A, rep("CONTADO",dim(A)[1]))


VentaOfertaCreditoOP <- 
  readXL("D:/OneDrive - Muebles Jamar/VENTA OFERTA SAB 15NOV2018.xls", 
         rownames=FALSE, header=TRUE, na="", sheet="CREDITO", stringsAsFactors=TRUE)


B=sqldf("select Fecha, [Nombre.Dia], [Nombre.de.la.Agencia],sum([No..Clientes.OP]), sum([Valor.Venta.Oferta]) from VentaOfertaCreditoOP group by Fecha, 
      [Nombre.Dia], [Nombre.de.la.Agencia] HAVING  [Nombre.de.la.Agencia] NOT IN ('JAMAR BOSA','JAMAR DE LA CUESTA','JAMAR SUR','JAMAR PLAZA DEL SOL',
        'JAMAR METROPOLITANO','JAMAR CONCESIONES', 'JAMAR CONCESIONES SANTA MARTA')")
B <- droplevels(B)
names(B) <- make.names(names(B))

names(B) <- make.names(names(B))
B <- with(B, B[order(Nombre.de.la.Agencia, Fecha, decreasing=FALSE), ])

B$ALTERNATIVA <- with(B, rep("CREDITO",dim(B)[1]))

VentaOfertaAlternativasOP=rbind(A,B)

VentaOfertaAlternativasOP$ALTERNATIVA=as.factor(VentaOfertaAlternativasOP$ALTERNATIVA)


VentaOfertaAlternativasOP$CrecimientoVO= rep(0, dim(VentaOfertaAlternativasOP)[1])

names(VentaOfertaAlternativasOP)[c(4,5)] <- c("NumClientes","VentaOferta")

for(i in 1:(dim(VentaOfertaAlternativasOP)[1]/2)){
  VentaOfertaAlternativasOP$CrecimientoVO[2*i]=  100*(VentaOfertaAlternativasOP$VentaOferta[2*i]/ VentaOfertaAlternativasOP$VentaOferta[2*i-1]-1)
}


require("ggplot2")
.df <- data.frame(x = VentaOfertaAlternativasOP$ALTERNATIVA, s = 
                    VentaOfertaAlternativasOP$Nombre.de.la.Agencia, y=VentaOfertaAlternativasOP$CrecimientoVO)
.df <- as.data.frame(with(.df, .df[VentaOfertaAlternativasOP$CrecimientoVO!=0,]))
.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("ALTERNATIVA") + 
  ylab("Percent") + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines"))+
  geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35)
print(.plot)


VentaOfertaTotalOP=sqldf("select Fecha, [Nombre.Dia], [Nombre.de.la.Agencia],sum(NumClientes), sum(VentaOferta) as VentaOferta from VentaOfertaAlternativasOP group by Fecha, 
      [Nombre.Dia], [Nombre.de.la.Agencia] HAVING  [Nombre.de.la.Agencia] NOT IN ('JAMAR BOSA','JAMAR DE LA CUESTA','JAMAR SUR','JAMAR PLAZA DEL SOL',
        'JAMAR METROPOLITANO','JAMAR CONCESIONES', 'JAMAR CONCESIONES SANTA MARTA')")


VentaOfertaTotalOP$CrecimientoVO= rep(0, dim(VentaOfertaTotalOP)[1])


for(i in 18:34){
  VentaOfertaTotalOP$CrecimientoVO[i]=  100*(VentaOfertaTotalOP$VentaOferta[i]/ VentaOfertaTotalOP$VentaOferta[i-17]-1)
}


require("ggplot2")
.df <- data.frame(x = VentaOfertaTotalOP$Nombre.Dia, y=VentaOfertaTotalOP$CrecimientoVO, s=VentaOfertaTotalOP$Nombre.de.la.Agencia)
.df <- as.data.frame(with(.df, .df[VentaOfertaTotalOP$CrecimientoVO!=0,]))
.plot <- ggplot(data = .df, aes(x = x, y = y)) + 
  geom_bar(width = 0.9, stat = "identity") + 
  scale_y_continuous(expand = c(0.01, 0)) + 
  facet_wrap( ~ s) + 
  xlab("") + 
  ylab("Percent") + 
  theme_bw(base_size = 14, base_family = "sans") + 
  theme(panel.spacing = unit(0.3, "lines"))+
  geom_text(aes(  label= paste(round(y,1),"%")  ), vjust=-0.05, hjust=-0.35) + geom_hline(aes(yintercept = 0), colour="red")
print(.plot)

###############################################

knime.in=kIn

R<-knime.in

#summary(knime.in)

K_min <- knime.flow.in[["K_min"]]
K_max <- knime.flow.in[["K_max"]]

library("clValid");
library("kohonen");

intern <- clValid(R, K_min:K_max, clMethods = c("som"), validation = c("internal"),maxitems = 3000, metric =
                    "euclidean", method = "average", neighbSize = 2)

summary(intern);

op <- par(no.readonly = TRUE);
oppala <- par(mfrow = c(1, 4), mar = c(4, 4, 3, 1));
plot(intern, legend = FALSE);
plot(nClusters(intern), measures(intern, "Dunn")[ , , 1], type = "n",axes = F, xlab = "", ylab = "");
legend("center", clusterMethods(intern), col = 1:6, lty = 1:6,pch = paste(1:6));
par(op);







