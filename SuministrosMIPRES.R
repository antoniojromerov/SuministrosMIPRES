#-------------------------------------------------------------
#Análisis de suministro de medicamentos según registros MIPRES
#-------------------------------------------------------------

setwd("/Volumes/Datos/Users/AntonioJose/OneDrive/Trabajo Observatorio/ProyectoR/
      SuministrosMIPRES")

#Instalando las librerias necesarias
#-----------------------------------
install.packages("tidyverse")
library(tidyverse)
library(readr)
library(dplyr)
library(pryr)
library(sparklyr)
library(rmarkdown)

tinytex::install_tinytex()


#Importando los datos
#--------------------
Suministros_20201026 <- read_delim("Suministros_20201026.txt", 
                                     "|", escape_double = FALSE, trim_ws = TRUE)

ReporteEntregas_20201026 <- read_delim("ReporteEntregas_20201026.txt", 
                                         "|", escape_double = FALSE, trim_ws = TRUE)

Entregas_20201026 <- read_delim("Entregas_20201026.txt", 
                                "|", escape_double = FALSE, trim_ws = TRUE)

Programaciones_20201026 <- read_delim("Programaciones_20201026.txt", 
                                      "|", escape_double = FALSE, trim_ws = TRUE)

Direccionamientos_20201026 <- read_delim("Direccionamientos_20201026.txt", 
                                         "|", escape_double = FALSE, trim_ws = TRUE)

Medicamentos_20201026 <- read_delim("Medicamentos_20201026.txt", 
                                    "|", escape_double = FALSE, trim_ws = TRUE)

Prescripciones_20201026 <- read_delim("Prescripciones_20201026.txt", 
                                      "|", escape_double = FALSE, trim_ws = TRUE)

Municipios_DANE <- read_delim("~/Downloads/municipios.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

#Filtran los datos y seleccionando los necesarios para el análisis
#-----------------------------------------------------------------
TiemposSuministros <- filter(Prescripciones_20201026, EstPres == 4)
TiemposSuministros <- select(TiemposSuministros, ID_Prescripcion, FPrescripcion)
TiemposSuministros <- merge(TiemposSuministros, Direccionamientos_20201026, by.x = "ID_Prescripcion", 
                 by.y = "Id_Prescripcion")

#Cambiando el nombre de las columnas
#-----------------------------------
names(TiemposSuministros) = c("Id_Prescripcion","FechaPrescripcion","ConsecutivoTecnologia",
                   "NoEntrega", "FechaDireccionamiento")

#Continuando con la seleccion de datos, uniendo las diferentes bases de datos importadas
#---------------------------------------------------------------------------------------
TiemposSuministros <- merge(TiemposSuministros, Entregas_20201026, by = c("Id_Prescripcion",
                                                    "ConsecutivoTecnologia", "NoEntrega"))
TiemposSuministros <- merge(TiemposSuministros, Programaciones_20201026, by = c("Id_Prescripcion",
                                                          "ConsecutivoTecnologia", 
                                                          "NoEntrega"))
TiemposSuministros <-merge(TiemposSuministros, ReporteEntregas_20201026, by = c("Id_Prescripcion",
                                                          "ConsecutivoTecnologia",
                                                          "NoEntrega"))
TiemposSuministros <- merge(TiemposSuministros, Suministros_20201026, by = c("Id_Prescripcion",
                                                          "ConsecutivoTecnologia",
                                                          "NoEntrega"))

#Convirtiendo las variables de fecha en "Date"
#--------------------------------------------
TiemposSuministros$FechaDireccionamiento <- as.Date(TiemposSuministros$FechaDireccionamiento)
TiemposSuministros$FechaEntrega <- as.Date(TiemposSuministros$FechaEntrega)
TiemposSuministros$FechaPrescripcion <- as.Date(TiemposSuministros$FechaPrescripcion)
TiemposSuministros$FechaProgramacion <- as.Date(TiemposSuministros$FechaProgramacion)
TiemposSuministros$FechaReporteEntrega <- as.Date(TiemposSuministros$FechaReporteEntrega)
TiemposSuministros$FechaSuministro <- as.Date(TiemposSuministros$FechaSuministro)

#Seleccionando datos de prescripción
#-----------------------------------
DataPrescripcion <- select(Prescripciones_20201026, ID_Prescripcion, CodDANEMunIPS, 
                           CodDxPpal, CodEPS)

#Uniendo los datos de prescripción con las otras de TiemposSuministros
#---------------------------------------------------------------------
TiemposSuministros <- merge(TiemposSuministros, DataPrescripcion, by.x = "Id_Prescripcion", 
                            by.y = "ID_Prescripcion")

#Creando una nueva variable para determinar tiempo de respuesta
#--------------------------------------------------------------
TiemposSuministros$Diferencia <- as.double(TiemposSuministros$FechaSuministro - 
                                             TiemposSuministros$FechaPrescripcion)

#Incluyendo el nombre del municipio
#----------------------------------
TiemposSuministros <- merge(TiemposSuministros, Municipios_DANE, by.x = "CodDANEMunIPS", 
                            by.y = "CODIGO_MUNICIPIO")

#Creando un dataframe con sólo las primeras entregas de los medicamentos
#-----------------------------------------------------------------------
PrimeraEntregaSuministros <- filter(TiemposSuministros, NoEntrega == "1")

#Exportando el dataframe de las primeras entregas
#------------------------------------------------
write.csv(PrimeraEntregaSuministros, file = "PrimeraEntregaSuministros.csv")

#Se importa el anterior DF para disminuir el tamaño del environment
#----------------------------------------------------------------------------------------------
PrimeraEntregaSuministros <- read_csv("PrimeraEntregaSuministros.csv", 
                           col_types = cols(FechaDireccionamiento = col_date(format = "%Y-%m-%d"), 
                                            FechaEntrega = col_date(format = "%Y-%m-%d"), 
                                            FechaPrescripcion = col_date(format = "%Y-%m-%d"), 
                                            FechaPrescripcionNew = col_date(format = "%Y-%m-%d"), 
                                            FechaProgramacion = col_date(format = "%Y-%m-%d"), 
                                            FechaReporteEntrega = col_date(format = "%Y-%m-%d"), 
                                            FechaSuministro = col_date(format = "%Y-%m-%d")))


#------------------
#Conectando a spark
#------------------

#Revisando versión disponible e instalando
#-----------------------------------------
spark_available_versions()
spark_install(version = "3.0.0")
print(spark_version(sc=spark_conn))

#Creando configuración para uso ampliado de memoria
#--------------------------------------------------
conf <- spark_config()
conf$spark.executor.memory <- "16G"
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "16G"
conf$spark.memory.fraction <- 0.9

#Conectando al cluster de Spark
#------------------------------
spark_conn <- spark_connect(master="local", config = conf)

# Copiando datos a Spark
#-----------------------
PrimeraEntrega_tbl <- copy_to(spark_conn, PrimeraEntrega)
Municipios_tbl <- copy_to(spark_conn, Municipios_DANE)

#Listando los data frames disponibles en Spark
#---------------------------------------------
src_tbls(spark_conn)

#Viendo qué tan grande es el dataset
#-----------------------------------
dim(PrimeraEntrega_tbl)
dim(Municipios_tbl)

#Viendo qué tan pequeño es el tibble
#-----------------------------------
object_size(PrimeraEntrega_tbl)
object_size(Municipios_tbl)

#---------------------------------------------
#Analizando datos desde Spark
#---------------------------------------------

#Creando dataframe de entregas por municipio
#-------------------------------------------
PrimeraEntrega_mun <- left_join(PrimeraEntrega_tbl, Municipios_tbl, 
                                by = c("CodDANEMunIPS"="CODIGO_MUNICIPIO"))
PrimeraEntrega_mun_DF <- collect(PTDPrimeraEntrega_mun)


#Creando el DF y consultando los tiempos de entrega en el año 2019 y 2020
#------------------------------------------------------------------------
PrimeraEntrega20192020_tbl <- filter(PrimeraEntrega_tbl, year(FechaPrescripcion) >= 2019 
                                         & year(FechaPrescripcion) <=2020)


#Consultado el tiempo promedio de entrega por cada número de entrega
#-------------------------------------------------------------------
PTD_tbl %>%
  group_by(NoEntrega) %>%
  arrange(NoEntrega) %>%
  summarise(
    Promedio_entrega = mean(Diferencia, na.rm = TRUE)
  )

#Creando DF y consultando el tiempo promedio por cada mes de prescripción
#-----------------------------------------------------------------------
TiempoPromedioMensual_tbl <- PTDPrimeraEntrega_tbl %>%
  mutate(yearPresc = year(FechaPrescripcion), monthPresc = month(FechaPrescripcion)) %>%
  group_by(yearPresc, monthPresc) %>%
  arrange(yearPresc, monthPresc) %>%
  summarise(meanDiferencia = mean(Diferencia),
            maxDiferencia = max(Diferencia),
            minDiferencia = min(Diferencia),
            numPresc = n_distinct(Id_Prescripcion)
            )

TiempoPromedioMensual <- collect(TiempoPromedioMensual_tbl)
TiempoPromedioMensual2020 <- filter(TiempoPromedioMensual, yearPresc == 2020)


#Analizando entregas en municipios
#---------------------------------