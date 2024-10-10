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
library(multcomp) #Libreria para hacer pruebas de hipotesis


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
#--Con este box-plot podemos ver como se estan distribuyendo los datos, podemos ver que los datos de la categoria "Si" estan mas dispersos que los de la categoria "No"



#Inciso II, vamos a escribir nuestra prueba de hipotesis para ver si la carga viral es mayor en los pacientes que se les aplico el medicamento
#   Sea x nuestra varible cstegorica => x=0 si no se aplico el medicamento y x=1 si se aplico el medicamento
#   De nuestro modelo de regresion lineal tendremos que:
#   E(Y|x=0) = b0 y
#   E(Y|x=1) = b0 + b1
#   Si se suministro, tengan mayor nivel de anticuerpos, lo podemos modelar como:
#   E(Y|x=1) >= E(Y|x=0) => b0 + b1 >= b0 => b1 >= 0
#   Por lo que nuestra prueba de hipotesis sera:
#   H_o: b1 <= 0 vs H_a: b1 > 0


#Inciso III, vamos a crear nuestro modelo de regresion lineal y ver si lo que nos dice la farmaceutica es cierto

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

#Ya con estas pruebas podemos concluir que se cumple con los supuestos del modelo de regresion lineal

#resumen del modelo y tests, para ver que tan bueno es 
summary(modelo)
AIC(modelo)
BIC(modelo)



#Creando la prueba de hipotesis para comprobar la prueba la hipotesis de relacion entre la carga viral y el medicamento
#   H_o: b1 <= 0 vs H_a: b1 > 0

MatPrueba1 = matrix(c(0,1), ncol=2, nrow=1) 
c=0
prueba1 = glht(modelo, linfct=MatPrueba1, rhs=c, alternative ="greater")
summary(prueba1)
#Con esta prueba podemos ver que es cierto, si hay una relacion positiva entre la carga viral y la aplicacion de la vacuna
#por lo que en un principio la farmaceutica tiene razon y el medicamento funciona


#Inciso IV, Vemos que el costo del medicamento es considerable, y nos dieron una nueva variable, la edad, vamos a ver si la edad influye en la carga viral
#Primero, vamos a hacer un analisis descriptivo de los datos, para ver si la edad puede influir en el funcionameinto del medicamento
summary(dataMed) #Cargando un resumen de nuestros
ggplot(data = dataMed, aes(x = interaction(Med, Edad), y = Y)) +
  geom_boxplot() +
  labs(x = "Edad y si estaban vacunado", y = "Carga retroviral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
stripchart(Y ~ Med, data = dataMed,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)
#-----------------------Duda de como hacer la comparacion bien 


#Inciso V, vamos a hacer un modelo de regresion y una prueba de hipotesis, para verificar nuestra sospecha de que la edad influye en el funcionamento del medicamento y por consiguiente en la carga viral


#Como tenemos mas personas en el caso donde son >60, vamos a hacer el modelo en base a esa poblacion
dataFil2 = dataMed %>% dplyr::select(Y, Med, Edad)  %>% filter (Edad== ">60")   #Filtrando los datos para solo tener dos categorias
summary(dataFil2) #Cargando un resumen de nuestros


#Volviendo a hacer nuestro modelo de regresion lineal
modeloEdad = lm(Y ~ Med, data = dataFil2) #Modelo base con el vamos a trbaajar
summary(modeloEdad) #Cargando un resumen de nuestro modelo, con esto podemos ver que Y y Med tienen una relacion de nuevo filtrando con la edad


#Checando pruebas del modelo de regresion de nuevo
plot(modeloEdad, 3)#Checar homocedasticidad, por defecto en R
lmtest::bptest(modeloEdad) 
#-- Con esta prueba de Hipoteis como nuestro p-value es 0.4251, podemos concluir que tenemos homocedasticidad
car::ncvTest(modeloEdad) #Otra funcion para checar homocedasticidad

plot(modeloEdad, 2)#Checar normalidad, por defecto en R
ResiduosModeloEdad = augment(modeloEdad) #Arreglando los residuales de nuestro modelo
head(ResiduosModeloEdad) #Veamos que nos regresa la funcion augment
shapiro.test(ResiduosModeloEdad$.std.resid) 
#El valor de este pruena fue de 0.8304, casi se rechaza, ver si haciendo una transformacion a los datos se puede mejorar

#Otra prueba mas robusta de normalidad en cada grupo y sobre la variable dependiente
dataFil2
shapiro.test(dataFil2$Y[dataFil2$Med=="Si"])
nortest::lillie.test(dataFil2$Y[dataFil2$Med=="Si"])
tseries::jarque.bera.test(dataFil2$Y[dataFil2$Med=="Si"])

shapiro.test(dataFil2$Y[dataFil2$Med=="No"])
nortest::lillie.test(dataFil2$Y[dataFil2$Med=="No"])
tseries::jarque.bera.test(dataFil2$Y[dataFil2$Med=="No"])
#Con estas preubas individuales tuvimos un mejor resultado, por lo que podemos concluir que tenemos normalidad en cada grupo y sobre la varible dependiente
#Ya con estas pruebas podemos concluir que se cumple con los supuestos del modelo de regresion lineal


#resumen del modelo y tests, para ver que tan bueno es 
summary(modeloEdad)
AIC(modeloEdad)
BIC(modeloEdad) #Parece curioso, pero este modelo nos dio una mejor puntacion que el anterior que tenia mas datos


#Haciedno la misma preuba de hipotesis que hicimos en el inciso III para ver si el medicamento sigue funcionando en relacion a la carga viral
#Donte tambien estamos comparando H_o: b1 <= 0 vs H_a: b1 > 0  
MatPrueba2 = matrix(c(0,1), ncol=2, nrow=1)
c=0
prueba2 = glht(modeloEdad, linfct=MatPrueba2, rhs=c, alternative ="greater")
summary(prueba2)
#En este caso estamos viendo que se acepto h_0, lo que significa que no hay una relacion positiva entre la carga viral y la aplicacion del medicamento
#Por lo que podriamos concluir que el medicamento no funciona y para "maquillar", los datos, agregaron poblacion joven



