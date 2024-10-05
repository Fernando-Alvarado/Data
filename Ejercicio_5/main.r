#Ejercicio 5, echo por: Fernando Alvarado Palacios

#Limpiando la consola para poder trabajar mejor
rm(list = ls(all.names = TRUE))
gc()

#Librerias necesarias para el ejercicio
library(dplyr)
library(kableExtra)
library(ggplot2)
library(lmtest) # checar homocedasticidad  
library(car) # checar linealidad
library(broom) # checar normalidad y calcular residuales
library(lawstat) #libreria para checar aleatoriedad
getwd()

data = read.csv("./Datos.csv")