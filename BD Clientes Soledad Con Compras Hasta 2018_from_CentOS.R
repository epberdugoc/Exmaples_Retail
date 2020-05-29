
#### BD Clientes Soledad Con Compras Hasta 2018

### Base con filtros:

setwd("~/Emilio/CLUSTER SOLEDAD")


library(readxl)
BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado <- read_excel("BD Clientes Soledad Con Compras Hasta 2018 - Entregado.xlsx", 
                                                                   sheet = "DATOS FILTRADOS", col_types = c("text", 
                                                                                                            "text", "blank", "blank", "text", 
                                                                                                            "text", "numeric", "blank", "blank", 
                                                                                                            "numeric", "text", "text", "text", 
                                                                                                            "text", "text", "text", "text", "text", 
                                                                                                            "text", "date", "text", "text", "date", 
                                                                                                            "text", "text", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric", 
                                                                                                            "numeric", "numeric", "numeric"))






BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado$N_PRODUCTOS= rowSums((BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado)[22:38])

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
df=BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado[(BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado$FECHA_ULT_COMPRA
                                                   >=as.POSIXct("2015-01-01"))&(BD_Clientes_Soledad_Con_Compras_Hasta_2018_Entregado$N_PRODUCTOS>0),c("SEXO","ESTADO_CIVIL","EDAD","NO_HIJOS","OCUPACION","ESTRATO","ALT_VTA_ULT_COMPRA","AGENCIA_ULT_COMPRA","N_PRODUCTOS" )]

#detach(df)

mydata=df
mydata[sapply(mydata, is.character)] <- lapply(mydata[sapply(mydata, is.character)], as.factor)
df=mydata
rm(list=c("mydata"))

#' Compute Gower distance
system.time({gower_dist <- daisy(df, metric = "gower")})

system.time({gower_mat <- as.matrix(gower_dist)})

#' Print most similar clients
df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]), arr.ind = TRUE)[1, ], ]

#' Print most dissimilar clients
df[which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]), arr.ind = TRUE)[1, ], ]

rm(gower_mat)

#In business situation, we usually search for a number of clusters both meaningful and easy to remember,
#i.e. 2 to 8 maximum. The silhouette figure helps us identify the best option(s).

#sil_width <- c(NA)
for(i in 2:10){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

##Summary of each cluster
k <- 8
#system.time({ pam_fit_8 <- pam(gower_dist, diss = TRUE, k)})

pam_results_8 <- df %>%
  mutate(cluster = pam_fit_8$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results_8$the_summary

pam_fit_3=pam_fit

pam_results_3=pam_results

###Visualization in a lower dimensional space
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))






