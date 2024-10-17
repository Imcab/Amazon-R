## Amazon
## Nombre: Imad Jared Cabrera Trejo
## Matricula: A01798415

## Librerías
library(ggplot2)
library(tidyr)
library(boot)
library(car)
library(quantmod)

## 2023

#Function to round millions
MILL <- function(DATA){
  DATA / 1000000
}

#View Data Base
TM23

Ventas23 <- MILL(TM23$`Revenue Share Amount(MX$)(2023 )`) 
Costos23 <- MILL(TM23$`PCOGS(MX$)(2023 )`)
Exportaciones23 <- MILL(TM23$`Shipped Units(2023 )`)
Fabricacion23 <- MILL(TM23$`Total Contra COGS Amt(MX$)(2023 )`)

#View Data Base
TM24

Ventas24 <- MILL(TM24$`Revenue Share Amount(MX$)(YTD )`)
Costos24 <- MILL(TM24$`PCOGS(MX$)(YTD )`)
Exportaciones24 <- MILL(TM24$`Shipped Units(YTD )`)
Fabricacion24 <- MILL(TM24$`Total Contra COGS Amt(MX$)(YTD )`)

# Estadistica Descriptiva

# 2023
summary(TM23)
var(Ventas23)
sd(Ventas23)

var(Costos23)
sd(Costos23)

var(Exportaciones23)
sd(Exportaciones23)

var(Fabricacion23)
sd(Fabricacion23)

# 2024
summary(TM24)
var(Ventas24)
sd(Ventas24)

var(Costos24)
sd(Costos24)

var(Exportaciones24)
sd(Exportaciones24)

var(Fabricacion24)
sd(Fabricacion24)

# Modelos de Regresión: 

ModeloCostos23=lm(Ventas23~Costos23, na.action = na.exclude) 
ModeloCostos24=lm(Ventas24~Costos24, na.action = na.exclude) 
ModeloExportacion23=lm(Ventas23~Exportaciones23, na.action = na.exclude) 
ModeloExportacion24=lm(Ventas24~Exportaciones24, na.action = na.exclude) 

summary(ModeloCostos23)
summary(ModeloCostos24)
summary(ModeloExportacion23)
summary(ModeloExportacion24)

#Para graficar utilizamos ggplot 
# nombre grafica = ggplot(data = Base de Datos, aes (Variable1,Variable2))
# graficar los puntos: nombre grafica = nombre grafica + geom_point ()
# graficar los desviación y función: 
#  nombre grafica = nombre grafica + geom_point () + geom_smooth(method = "lm", colour="Red")

# Modelo Costos23
graf1=ggplot(data = TM23, aes(Ventas23,Costos23))
graf1 + geom_point()
graf1 + geom_point() + geom_smooth(method = "lm", colour="Red")

# Modelo Costos24
graf2=ggplot(data = TM24, aes(Ventas24,Costos24))
graf2 + geom_point()
graf2 + geom_point() + geom_smooth(method = "lm", colour="Red")

# Modelo Exportacion23
graf3=ggplot(data = TM23, aes(Ventas23,Exportaciones23))
graf3 + geom_point()
graf3 + geom_point() + geom_smooth(method = "lm", colour="Red")

# Modelo Fabricacion24
graf3=ggplot(data = TM24, aes(Ventas24,Fabricacion24))
graf3 + geom_point()
graf3 + geom_point() + geom_smooth(method = "lm", colour="Red")

