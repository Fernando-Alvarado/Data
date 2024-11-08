#Ejercicio 4 echo por Fernando Alvarado Palacios
#Limpiando la consola para poder trabajar mejor
rm(list = ls(all.names = TRUE))
gc()

#Librerias necesarias para el ejercicio
library(kableExtra)
library(ggplot2) #Graficar
library(lmtest) # checar homocedasticidad  
library(car) # checar linealidad
library(broom) # checar normalidad y calcular residuales
library(lawstat) #libreria para checar aleatoriedad
library(tidyverse) #Libreria para hacer la manipulacion de los datos
library(dplyr) #Meanejo de datos
library(multcomp) #Libreria para hacer pruebas de hipotesis
library(GGally) #Libreria para hacer graficas un poco mas conpleas, para relacionar varias variables a la vez


#Estableciendo el directorio de trabajo
getwd()
setwd("C://Users//ferna//Documents//Estadisitica_2//Data//Proyecto_2//Ejercicio_4")#Modificar esta linea en caso de que no se encuentre en la misma carpeta

#Cargando los datos con los que vamos a trabajar 
data = read.csv("./Ex4A.csv") #Archivo CSV con los datos de la ansiedad

summary(data) #Cargando una vista previa de nuestros datos  

#Transformando la variables al tipo factor 
data$Sexo = factor(data$Sexo)
data$Trat = factor(data$Trat)

str(data) #Cargando una vista previa de nuestros datos
#Donde en este caso, Hobre y Control seran nuestros niveles de referencia 

#1) Empecemos a hacer un Analisis descriptivo de los datos 



plot(data) 
library(GGally)
ggpairs(data)
#Con esta profica podemos empezar a notar unas cosas, como que tenemos 2 variables, categoricias y una continua,
#sexo tiene solo dos niveles mientras que tratamiento tiene 3 niveles y la varible continua al parecer tendria una
#distribucion normal, pero tendriamos que verificarlo 

boxplot(Puntaje ~ Trat * Sexo, data = data,
        main = "Boxplot de  Tratamiento, Sexo vs Puntaje Depresion",
        xlab = "Tratamiento y Sexo",
        ylab = "Puntaje de Depresión",
        col = c("lightblue", "lightgreen"))

#Con esta grafica  podemos ver que el comportamiento de los medicamentes, ambos funcionan, pero al parecer el tratamiento 1 parece funcionar
#mejor tanto en hombres como en mujeres ya que baja mas en media el puntuaje de depresion, pero tendriamos que hacer un analisis mas profundo





