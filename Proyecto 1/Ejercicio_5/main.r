#Problema 5, talba Anova echo por: Fernando Alvarado Palacios

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


#Estableciendo el directorio de trabajo
getwd()
setwd("C://Users//ferna//Documents//Estadisitica_2//Data//Proyecto 1//Ejercicio_5")#Modificar esta linea en caso de que no se encuentre en la misma carpeta


#Cargando nuestros datos
dataMed = read.csv("./Ejercicio5A.csv") #Archivo CSV con los datos de los medicamentos
#Donde Y es un índice de carga viral y Med es una variable con dos niveles dependiendo si se aplicó o no el nuevo medicamento y Edad es la edad del paciente
str(dataMed) #Cargando una vista previa de nuestros datos
#--Podemos observar que que tenemos 3 variables,  una de tipo numerico y dos de tipo caracter 



#Vamos a modificar los datos de tipo caracter a categoricos para poder trabajar con ellos
dataMed$Med = as.factor(dataMed$Med)
dataMed$Edad = as.factor(dataMed$Edad)
str(dataMed) #Cargando una vista previa de nuestros datos


#Vamos a hacer una filtracion de los dato
dataFil = dataMed %>% dplyr::select(Y, Med)  %>% filter (Med %in% c("Si", "No"))  #Filtrando los datos para solo tener dos categorias
levels(dataFil$Med) <- list(Si = "Si", No = "No") #Cambiamos los nombres de las categorias
dataFil 


#Inciso I, vamos a realizar un analisis exploratorio de los datos, para ellos vamos a realizar un boxplot
boxplot(Y ~ Med, data = dataFil, col = "white", outline=FALSE) 
stripchart(Y ~ Med, data = dataFil,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
#--Con esta dispersion de los puntos que tenemos podeos concluir que no estamos teniendo homoceasticidad en nuestros datos (por la disspersion vertical de los puntos)


#Crenado nuestro modelo de regresion lineal
modelo = lm(Y ~ Med, data = dataFil) #Modelo base con el vamos a trbaajar
summary(modelo) #Cargando un resumen de nuestro modelo, con esto podemos ver que Y y Med tienen una relacion


#Verificando los Supuestos del modelo de regresion lineal
plot(modelo, 3)#Checar homocedasticidad, por defecto en R
lmtest::bptest(modelo) 
#-- Con esta prueba de Hipoteis como nuestro p-value es 0.2594, podemos concluir que tenemos homocedasticidad
car::ncvTest(modelo) #Otra funcion para checar homocedasticidad

plot(modelo, 2)#Checar normalidad, por defecto en R
ResiduosModelo = augment(modelo) #Arreglando los residuales de nuestro modelo
head(ResiduosModelo) #Veamos que nos regresa la funcion augment
shapiro.test(ResiduosModelo$.std.resid) 
#El valor de este pruena fue de 0.06098, casi se rechaza, ver si haciendo una transformacion a los datos se puede mejorar

#Otra prueba ms robusta de normalidad en cada grupo y sobre la variable dependiente
dataFil
shapiro.test(dataFil$Y[dataFil$Med=="Si"])
nortest::lillie.test(dataFil$Y[dataFil$Med=="Si"])
tseries::jarque.bera.test(dataFil$Y[dataFil$Med=="Si"])

shapiro.test(dataFil$Y[dataFil$Med=="No"])
nortest::lillie.test(dataFil$Y[dataFil$Med=="No"])
tseries::jarque.bera.test(dataFil$Y[dataFil$Med=="No"])
#Con estas preubas individuales tuvimos un mejor resultado, por lo que podemos concluir que tenemos normalidad en cada grupo y sobre la varible dependiente

#resumen del modelo y tests 
summary(modelo)
AIC(modelo)
BIC(modelo)




#-------------------------------------------------------------------------------------------
#Notas:
#-------------------------------------------------------------------------------------------
#En este caso, para ver lsa preubas del modelo tengo que hacer las pruebas para ambos modelos
# El profe, como tenia ambas variables categoricas en una sola columan pudo hacer 1 preuba
#para ambas cosas
#
#
#
#Creando otro filtro para trabajar con los datos, separando las dos clases (A quienes se les aplico el medicamento y a quienes no)
