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
library(multcomp) #Libreria para hacer pruebas de hipotesis muiltiples
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
#Donde en este caso, Hombre y Control seran nuestros niveles de referencia 

#1) Empecemos a hacer un Analisis descriptivo de los datos 


 #Hagamos una grafica rapido, para ver la distribucion de nuestros datos y verlos de forma grafica, en este caso grafico el puntaje de depresion
  # por color que tratamiento se le esta aplicando al paciente y con el tamaño del punto el sexo del paciente, para ver si hay alguna relacion entre
ggplot(data, aes(x = 1:nrow(data), y = Puntaje, color = Trat, size = Sexo)) +
  geom_point(alpha = 0.8) +
  labs(title = "Dispersión del Puntaje por Tratamiento y Sexo",
       x = "Índice",
       y = "Puntaje",
       color = "Tratamiento",
       size = "Sexo") +
  scale_color_manual(values = c("red", "green", "blue")) +
  scale_size_manual(values = c(Hombre = 2, Mujer = 4)) +
  theme_minimal()
 

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


#Construyendo el modelo de regresion lineal multiple
#Expresion matematica del modelo
#Puntuaje = Trat + Sexo + Trat*Sexo

levels(data$Trat)#Checando los niveles de nuestras variables categoricas para saber cual va a salir a la hora de hacer nuestro modelo
levels(data$Sexo)

#2) Creemos nuestro modelo de regresion lineal 
#Para ello lo vamos a plantear de la siguiente manera, tenemos dos variables categorcias, una con 2 niveles de referencia y otra con 3 niveles
# PLANTEAMIENTO DEL MODELO 
# Y = {Hombre o mujer}
# X = {Control, Trat1, Trat2}
# entonces podemos hacer las variables dicotomicas de la siguiente manera
# Y_h = {1 si es hombre, 0 eoc} 
# Y_m = {1 si es mujer, 0 eoc}
# X_c = {1 si es control, 0 eoc}
# X_t1 = {1 si es tratamiento 1, 0 eoc}
# X_t2 = {1 si es tratamiento 2, 0 eoc}
# Por lo que el modelo de regresion lineal seria el siguiente, quitando la variables categoricas de hombres y control 
#---------------------------------------------------------------------
# Puntaje = B0 + B1*Y_m + B2*X_t1 +B3*X_t2 + b4*Y_m*X_t1 + B5*Y_m*X_t2
#---------------------------------------------------------------------
modelo <- lm( Puntaje~ Sexo*Trat , data = data)
summary(modelo) #veamos nuestro resumen del modelo de regresion lineal 

length(coef(modelo))
#Vamos a analizar las expresiones de los puntajes promedios para cada nivel de las variables categoricas
#E(puntaje;Trat = Control,Sexo = h) =  B0  
#E(puntaje;Trat = Trat1,Sexo = h)   =  B0 + B2*X_t1  
#E(puntaje;Trat = Trat2,Sexo = h)   =  B0 + B3*X_t2 
#E(puntaje;Trat = Control,Sexo = m) =  B0 + B1*Y_m 
#E(puntaje;Trat = Trat1,Sexo = m)   =  B0 + B1*Y_m + B2*X_t1 + B4*Y_m*X_t1
#E(puntaje;Trat = Trat2,Sexo = m)   =  B0 + B1*Y_m + B3*X_t2 + B5*Y_m*X_t2


#3) Veamos que pasa con nuestra tabla ANOVA
#Planteemos la preuba ANOVA 
#(H0):B1 = B2 = B3 = ... = Bk = 0 vs (Ha):Al menos un Bi ≠ 0, para alguna variable i
summary(modelo) #Nos apoyamos de summary para ver los resultados de la prueba #?????????????????---------------------------------- Tengo dudas, sobre como evaluar la salida individual de las cosas, ya que B_4 me sale que es diferente de cero de forma individual, pero con conjunto con B_1, es cero y tengo una recta parelela
#Con este resumen vemos que se rechaza Ho, en la preuba asociada a la tabla ANOVA, lo que impica que nuestro modelo tiene sentido 
#Lo que implica que el sexo y los tratamientos ayudan a modelas al puntuaje de depresion 


#4) Vamos a ver si el sexo tiene algun efecto en el puntuaje, i.e. al menos paara un tratamiento existe un efecto derivado del sexo

#Modelo sin sexo
modelo_sin_sexo <- lm( Puntaje~ Trat , data = data) #??????--------------------------------------------------------------------------------------------------------------------------------------------------- Asi nada mas quito a la variable categorica sexo?
summary(modelo_sin_sexo) #veamos nuestro resumen del modelo de regresion lineal 

m_2ceros=c(0,0) #vamos a comparar todas nuestras preubas de hipotesis con cero 
#Con esto vamos a ver si existe una igualdad entre pendientes 
#Vamos a comparar las sigueintes pruebas de hipotesis: (Dada una significancia de 0.05)
#(Ho) E(puntaje;Trat = Control,Sexo = h) = E(puntaje;Trat = Control,Sexo = m) vs (Ha) E(puntaje;Trat = Control,Sexo = h) ≠ E(puntaje;Trat = Control,Sexo = m)
#(Ho) B_1 = 0  vs (Ha) B_1 ≠ 0
m1 =c(0)
K4_c = matrix(c(0,1,0,0,0,0), ncol=6, nrow = 1, byrow = TRUE)
summary(glht(modelo, linfct = K4_c, rhs = m1), test= Ftest())
#De esta prueba nos dio que el p-value es de: 0.6572, por lo que no se rechaza (Ho) ???????????????????-----------------------------------------------------------------------------------------------Lo puedo interpretar como que no hay una relacion entre la efectividad de los medicamentos y el sexo de la persona?


#(Ho) E(puntaje;Trat = Trat1,Sexo = h) = E(puntaje;Trat = Trat1,Sexo = m) vs (Ha) E(puntaje;Trat = Trat1,Sexo = h) ≠ E(puntaje;Trat = Trat1,Sexo = m)
#(Ho) B_1 = 0 ^ B_4 = 0 vs (Ha) B_1 ≠ 0 v B_4 ≠ 0
K4_1 = matrix(c(0,1,0,0,0,0,
                0,0,0,0,1,0), ncol=6, nrow = 2, byrow = TRUE)
summary(glht(modelo, linfct = K4_1, rhs = m_2ceros), test= Ftest())
#De esta prueba nos dio que el p-value es de: 0.001023 , por lo que  se rechaza (Ho),  

#Escribimos una prueba simultanea para ver que provoco el rechazo de (Ho)
#Las preubas de hipotesis a Analisas son:
# H0_1: B_1 = 0 vs Ha_1: B_1 ≠ 0
# H0_2: B_4 = 0 vs Ha_2: B_4 ≠ 0
# H0_3: B_1 - B_4 = 0 vs Ha_3: B_1 - B_4 ≠ 0
m_sim = c(0,0,0)
K4_Sim = matrix(c(0,1,0,0,0,0,
                  0,0,0,0,1,0,
                  0,1,0,0,-1,0), ncol=6, nrow = 3, byrow = TRUE) #Matrices para realizar nuestras preubas de hipotesis
#Corriendo nuestra prueba de hipotesis, simultanea
summary(glht(modelo, linfct = K4_Sim, rhs = m_sim))
#Puntaje = B0 + B1*Y_m + B2*X_t1 +B3*X_t2 + b4*Y_m*X_t1 + B5*Y_m*X_t2
#Lo que nos esta diciendo que posiblemente B_4 no es diferente de 0, por lo que las pendientes de ???????----------------------------------------------------------------------------Como analizar este resultado 



#(Ho) E(puntaje;Trat = Trat2,Sexo = h) = E(puntaje;Trat = Trat2,Sexo = m) vs (Ha) E(puntaje;Trat = Trat2,Sexo = h) ≠ E(puntaje;Trat = Trat2,Sexo = m)
#(Ho) B_1 = 0 ^ B_5 = 0 vs (Ha) B_1 ≠ 0 v B_5 ≠ 0
K4_2 = matrix(c(0,1,0,0,0,0,
                0,0,0,0,0,1), ncol=6, nrow = 2, byrow = TRUE)
summary(glht(modelo, linfct = K4_2, rhs = m), test= Ftest())
#De esta prueba nos dio que el p-value es de: 0.895 , por lo que no se rechaza (Ho)

#Con los datos anteriores vamos a hacer un modelo reducido, para poder trabajar mejor 

#5) Ajustando el modelo reducido
#Por el paso de arriba observamos que debemos sacar a b_4, para ello, proponemos el sigueinte modelo 
names(data)

modelo_reducido <- lm(Puntaje ~ Sexo + Trat +I(Sexo*Trat="Trat2") , data = data) #????????????? No se como hacer el modelo reducido

summary(modelo)
summary(modelo_reducido)



