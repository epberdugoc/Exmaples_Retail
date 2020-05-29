##### Consultas piloto Brilla

library(RODBC)
library(sqldf)

fecha_apertura='21-05-2019'
fecha_min='01-06-2019'
fecha_max_visor='28-08-2019'
fecha_max_OP='27-08-2019'

############ ACUMULADO DESDE APERTURA DEL PILOTO: #############################

##### TOTAL ROJOS #############################

TotalRojosQuery=paste(
  "Select p.* from pilotofacilidad p where /*SALIDA = 'NUEVO' and*/ p.Viable = 'ROJO' and p.agencia = 'B5' 
  and  (p.emi between to_Date('",fecha_apertura,"','dd-mm-yyyy') and to_date('",fecha_max_visor,"','dd-mm-yyyy'))"
  , sep="")

jdbcConnection=odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart")# DATANEW

system.time({TotalRojos=sqlQuery(jdbcConnection,TotalRojosQuery, dec=",") 
})
odbcClose(jdbcConnection)


length(unique(TotalRojos$N_IDE))

### TOTAL CONTACTOS GUARDADOS ##############

ContactosGuardadosQuery=paste(
  "select * /*count(distinct N_IDE)*/ from fac_pilotofabricaexterna where  agencia='B5' and (FECHA BETWEEN to_date('",fecha_apertura,"', 'dd-mm-yyyy') AND
       to_date('",fecha_max_visor,"', 'dd-mm-yyyy'))"
  
  , sep="")

jdbcConnection=odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart")# DATANEW

system.time({ContactosGuardados=sqlQuery(jdbcConnection,ContactosGuardadosQuery, dec=",") 
})
odbcClose(jdbcConnection)

length(unique(ContactosGuardados$N_IDE))

##### TOTAL OP BRILLA #############################


Total_OP_BrillaQuery=paste(
  "SELECT r.n_ide Cedula,
       r.rem orden_pedido,
  r.est estado_op,
  r.c_agr agencia_op,
  r.emi fecha_op,
  R.TOT Valor_OP,
  R.VTACONTADO,
  r.tven,
  r.cuotas,
  r.sep_fac,
  r.cod_convenio,
  (Select Nom_Convenio
  From Convenios
  Where C_Emp = R.C_EMP
  And Cod_Convenio = R.Cod_Convenio) Nom_Convenio,
  r.TVEN    
  FROM rem_enc r
  WHERE 
  (R.Cod_Convenio In ('BR', 'BS') /*OR  r.TVEN in ('CO','PT')*/ ) And
  C_AGR='B5'
  and (r.emi BETWEEN to_date('",fecha_apertura,"', 'dd-mm-yyyy') AND
  to_date('",fecha_max_OP,"', 'dd-mm-yyyy'))"
  
  , sep="")

jdbcConnection=odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart")# DATANEW

system.time({Total_OP_Brilla=sqlQuery(jdbcConnection,Total_OP_BrillaQuery, dec=",") 
})
odbcClose(jdbcConnection)

length(unique(Total_OP_Brilla$CEDULA))

require(writexl)
write_xlsx(Total_OP_Brilla, path ='D:\\OneDrive - Muebles Jamar\\2019\\Piloto Brilla\\OP_BRILLA.xlsx')


############ VARIACIÓN DIARIA: #############################

#####  ROJOS INTERVALO #############################

IntervaloRojosQuery=paste(
  "Select p.* from pilotofacilidad p where /*SALIDA = 'NUEVO' and*/ p.Viable = 'ROJO' and p.agencia = 'B5' 
  and  (p.emi between to_Date('",fecha_min,"','dd-mm-yyyy') and to_date('",fecha_max_visor,"','dd-mm-yyyy'))"
  , sep="")

jdbcConnection=odbcConnect("EMILIO_DSN", uid = "datamart", pwd = "datamart")# DATANEW

system.time({IntervaloRojos=sqlQuery(jdbcConnection,IntervaloRojosQuery, dec=",") 
})
odbcClose(jdbcConnection)


length(unique(IntervaloRojos$N_IDE))







