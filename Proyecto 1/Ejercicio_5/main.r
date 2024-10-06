#Problema 5, talba Anova echo por: Fernando Alvarado Palacios

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
library(tidyverse) #Libreria para hacer la manipulacion de los datos


#Estableciendo el directorio de trabajo
getwd()
setwd("C://Users//ferna//Documents//Estadisitica_2//Data//Proyecto 1//Ejercicio_5")#Modificar esta linea en caso de que no se encuentre en la misma carpeta


#Cargando nuestros datos
dataMed = read.csv("./Ejercicio5A.csv") #Archivo CSV con los datos de los medicamentos
str(dataMed) #Cargando una vista previa de nuestros datos
#--Podemos observar que que tenemos 3 variables,  una de tipo numerico y dos de tipo caracter 


#Vamos a modificar los datos de tipo caracter a categoricos para poder trabajar con ellos
dataMed$Med = as.factor(dataMed$Med)
dataMed$Edad = as.factor(dataMed$Edad)
str(dataMed) #Cargando una vista previa de nuestros datos


#Vamos a hacer una filtracion de los dato


#Inciso I, vamos a realizar un analisis exploratorio de los datos, para ellos vamos a realizar un boxplot







#Hacer un analisis exploratorio de los datos, con un boxplot



#-------------------------------------------------------------------------------------------
#Notas:
#-------------------------------------------------------------------------------------------
#En este caso, para ver lsa preubas del modelo tengo que hacer las pruebas para ambos modelos
# El profe, como tenia ambas variables categoricas en una sola columan pudo hacer 1 preuba
#para ambas cosas
#
#
#
