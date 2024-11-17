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
  data %>% group_by(Trat,Sexo) %>%  #Hacienod un analisis descriptivo de nuestros datos, para saber cosas como la media, mediana, varianza y el numero de observaciones
  summarise(Observaciones = n(),
            Media = round(mean(Puntaje),2),
            Mediana = median(Puntaje),
            Varianza = round(var(Puntaje),2))



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

x11()
boxplot(Puntaje ~ Trat * Sexo, data = data,#Un consejo a a la hora de correr esta grafica es que la hagan mas grande, ya que quita los labs del eje x, por que no caben todos y se hace un poco complicado de interpretar
        main = "Boxplot de  Tratamiento, Sexo vs Puntaje Depresion",
        xlab = "Tratamiento y Sexo",
        ylab = "Puntaje de Depresión",
        col = c("lightblue", "lightgreen", "lightsalmon"))
stripchart(Puntaje~Trat+Sexo, data = datos,
        method = "jitter",
        pch = 19,
        col = 2:4,
        vertical = TRUE,
        add = TRUE)


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


#Tratemos de ver nuestro modelo de una forma mas grafica
data$Predicciones <- predict(modelo)

# Crear el gráfico
ggplot(data, aes(x = Trat, y = Predicciones, color = Sexo, group = Sexo)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +  # Líneas de medias por Sexo
  stat_summary(fun = mean, geom = "point", size = 3) +  # Puntos de medias por Sexo
  labs(title = "Gráfico de Interacción: Sexo y Tratamiento",
       x = "Tratamiento",
       y = "Puntaje promedio predicho") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Mejora estética con colores

length(coef(modelo))


#3) Veamos que pasa con nuestra tabla ANOVA

summary(modelo)

B0 <- modelo[["coefficients"]][["(Intercept)"]]
B1 <- modelo[["coefficients"]][["SexoMujer"]]
B2 <- modelo[["coefficients"]][["TratTrat1"]]
B3 <- modelo[["coefficients"]][["TratTrat2"]]
B4 <- modelo[["coefficients"]][["SexoMujer:TratTrat1"]]
B5 <- modelo[["coefficients"]][["SexoMujer:TratTrat2"]]


#Vamos a analizar las expresiones de los puntajes promedios para cada nivel de las variables categoricas

# E(puntaje; Trat = Control, Sexo = h) = B0
cat("E(puntaje; Trat = Control, Sexo = h) =", B0, "\n")
# E(puntaje; Trat = Trat1, Sexo = h) = B0 + B2
cat("E(puntaje; Trat = Trat1, Sexo = h) =", B0 + B2, "\n")
# E(puntaje; Trat = Trat2, Sexo = h) = B0 + B3
cat("E(puntaje; Trat = Trat2, Sexo = h) =", B0 + B3, "\n")
# E(puntaje; Trat = Control, Sexo = m) = B0 + B1
cat("E(puntaje; Trat = Control, Sexo = m) =", B0 + B1, "\n")
# E(puntaje; Trat = Trat1, Sexo = m) = B0 + B1 + B2 + B4
cat("E(puntaje; Trat = Trat1, Sexo = m) =", B0 + B1 + B2 + B4, "\n")
# E(puntaje; Trat = Trat2, Sexo = m) = B0 + B1 + B3 + B5
cat("E(puntaje; Trat = Trat2, Sexo = m) =", B0 + B1 + B3 + B5, "\n")


#Planteemos la preuba ANOVA 
#(H0):B1 = B2 = B3 = ... = Bk = 0 vs (Ha):Al menos un Bi ≠ 0, para alguna variable i
summary(modelo) #Nos apoyamos de summary para ver los resultados de la prueba 
#Con este resumen vemos que se rechaza Ho, en la preuba asociada a la tabla ANOVA, lo que impica que nuestro modelo tiene sentido 
#Lo que implica que el sexo y los tratamientos ayudan a modelas al puntuaje de depresion 


#4) Vamos a ver si el sexo tiene algun efecto en el puntuaje, i.e. al menos paara un tratamiento existe un efecto derivado del sexo

#Vamos a ver si el sexo tiene algun efecto en el puntuaje de los individuas 
#Momparando las sigueintes pruebas de hipotesis simultaneas: (Dada una significancia de 0.05)

#(Ho) 1) E(puntaje;Trat = Control,Sexo = h) = E(puntaje;Trat = Control,Sexo = m) vs (Ha) E(puntaje;Trat = Control,Sexo = h) ≠ E(puntaje;Trat = Control,Sexo = m) ^
#     2) E(puntaje;Trat = Trat1,Sexo = h) = E(puntaje;Trat = Trat1,Sexo = m) vs (Ha) E(puntaje;Trat = Trat1,Sexo = h) ≠ E(puntaje;Trat = Trat1,Sexo = m) ^
#     3) E(puntaje;Trat = Trat2,Sexo = h) = E(puntaje;Trat = Trat2,Sexo = m) vs (Ha) E(puntaje;Trat = Trat2,Sexo = h) ≠ E(puntaje;Trat = Trat2,Sexo = m)

#Pasandolo a terminos de nuestras Bi

#(Ho) B_1 = 0  ^                      (Ha) B_1 ≠ 0
#     B_1 + B_4 = 0  ^    vs               B_1 + B_4 ≠ 0
#     B_1 + B_5 = 0   ^                    B_1 + B_5 ≠ 0

#Observemos que podemos resumir esta prueba de hipotesis de la siguiente forma

#(Ho) B_1 = 0  ^  B_4 = 0 ^ B_5 = 0 vs (Ha) B_4 ≠ 0 v B_5 ≠ 0

K4 = matrix(c(0,1,0,0,0,0,
              0,1,0,0,1,0,
              0,1,0,0,0,1), ncol=6, nrow = 2, byrow = TRUE)
summary(glht(modelo, linfct = K4, rhs = c(0,0)), test= Ftest())
#El valor del p-value de prueba de hipotesis es de:  0.006433, por lo que se rechaza (Ho)
#Lo que se puede interpretar como que hay una diferencia entre los puntajes de depresion de los hombres y las mujeres, para al menos un tratamiento
#Por lo que el sexo si tiene un efecto en el puntuaje de depresion de los individuos en al menos un tratamiento

#Veamos que fue lo que provoco el rechazo anterior, comparando las Esperanzas, por seprado
 
#Vamos a hacer todas las posibilidades, comparando las esperanzas, apra ver donde, existe una relacion de Esperanzas, (Con las esperanzas de arriba)
#(Ho)  1) = 2)
#      1) = 3) 
#      2) = 3)

#Pasandolo a terminos de nuestras Bi 

# (Ho) B1 = 0
#      B4 = 0
#      B5 = 0    
#      B4-B5 = 0       

K4_2 = matrix(c(0,1,0,0,0,0,
                0,0,0,0,1,0,
                0,0,0,0,0,1,
                0,0,0,0,1,-1), ncol=6, nrow = 4, byrow = TRUE)
summary(glht(modelo, linfct = K4_2, rhs = c(0,0,0,0)))
#Con esta prueba podemos identificar que B5 y B1 no es diferente de 0



#5) Ajustando el modelo reducido
#Por el paso de arriba observamos que solo se debe salir B5 y B1, para ello, proponemos el sigueinte modelo 
names(data)
# Puntaje = b0 + b1*X_t1 +b2*X_t2 + b3*Y_m*X_t1
modelo_reducido <- lm(Puntaje ~   Trat +I((Trat == 'Trat1')*(Sexo == 'Mujer')), data = data) 
summary(modelo_reducido)

#Con este modelo podemos ver que nuestro modelo de regresion si tiene, sentido y podemos trabajar con el 

#Hagamos un pequeña prueba de hipotesis para ver si nuestro modelo reducido es mejor que el modelo orignal 
anova(modelo_reducido, modelo) #Esta prueba nos dice que el modelo reducido es mejor que el modelo original
#Dando las expreciones del nuevo modelo de regresion reducido (De nuevo deje expresadas las variables de donde viene, para poder hacer un mejor un mejor desarrollo de los siginetes pasos)

#

b0 <- modelo_reducido[["coefficients"]][["(Intercept)"]]
b1 <- modelo_reducido[["coefficients"]][["TratTrat1"]]
b2 <- modelo_reducido[["coefficients"]][["TratTrat2"]]
b3 <- modelo_reducido[["coefficients"]][["I((Trat == \"Trat1\") * (Sexo == \"Mujer\"))"]]



#Vamos a analizar las expresiones de los puntajes promedios para cada nivel de las variables categoricas
# Vamos a analizar las expresiones de los puntajes promedios para cada nivel de las variables categóricas

# E(puntaje; Trat = Control, Sexo = h) = b0
cat("E(puntaje; Trat = Control, Sexo = h) =", b0, "\n")
# E(puntaje; Trat = Trat1, Sexo = h) = b0 + b1
cat("E(puntaje; Trat = Trat1, Sexo = h) =", b0 + b1, "\n")
# E(puntaje; Trat = Trat2, Sexo = h) = b0 + b2
cat("E(puntaje; Trat = Trat2, Sexo = h) =", b0 + b2, "\n")
# E(puntaje; Trat = Control, Sexo = m) = b0
cat("E(puntaje; Trat = Control, Sexo = m) =", b0, "\n")
# E(puntaje; Trat = Trat1, Sexo = m) = b0 + b1 + b3
cat("E(puntaje; Trat = Trat1, Sexo = m) =", b0 + b1 + b3, "\n")
# E(puntaje; Trat = Trat2, Sexo = m) = b0 + b2
cat("E(puntaje; Trat = Trat2, Sexo = m) =", b0 + b2, "\n")



#Obs: para los sigueintes incisos donde hare una comparacion sobre el desempeño de los medicamentos, usare el modelo sin reducier
#ya que por la prueba anova que comparaba ambos modelos, salio que el mejor modelo era el que no estaba reducido 

#Obs: Como intepretacion personal, para comparar si un medicamento es mejor,dividire la prueba de hipotesis en base al sexo, 
#y luego hare una prueba simultanea, para ver que efectivamente el medicamento esta siendo mejor


#6) Prueba de Hipotesis,  Nuevo tratameinto (Tratamiendo 2), tiene un mejor desempeño (a esto me refiero de que el medicamento baje los niveles de ansiedad en los pacientes)
#Haciendo la prueba de hip
#E(puntaje;Trat = Trat2,Sexo = h) < E(puntaje;Trat = Trat1,Sexo = h) y  E(puntaje;Trat = Trat2,Sexo = h) < E(puntaje;Trat = Control,Sexo = h) ^
#E(puntaje;Trat = Trat2,Sexo = m) < E(puntaje;Trat = Trat1,Sexo = m) y  E(puntaje;Trat = Trat2,Sexo = m) < E(puntaje;Trat = Control,Sexo = m)

#Expresando la preuba de hipotesis (Ho)

# (Ho):  b2-b1 >= 0 ^
#        b2 >= 0  ^
#        b2-b1-b3 >= 0

summary(modelo_reducido)

k_6 =matrix(c(0,0,1,0,
              0,-1,1,0,
              0,-1,1,-1), ncol=4, nrow = 4, byrow = TRUE)


summary(glht(modelo_reducido, linfct =k_6 , rhs = c(0,0,0, 0), alternative = "less"))


#Conclusiones:
#Como conclusion podemos observar que unicamente el Tratamiento 2 (El nuevo) solamente es mejor que el control, tanto en hombres, como en mujeres, ya que se rechazo estadisticamente que B2 >=0.
#Por lo que el el tratamiento 2, no es el mejor tratameinto de todo nuestro estudio, ya que en promedio no es que el mas baja los niveles de ansiedad en los pacientes


#7) Preuba de Hipotesis, Tratamiento Actual (Tratamiento 1), tiene un mejor desempeño
#Haciendo la prueba de hip, para los hombres
#E(puntaje;Trat = Trat1,Sexo = h) < E(puntuaje;Trat = Trat2,Sexo = h) y E(puntaje;Trat = Trat1,Sexo = h) < E(puntaje;Trat = Control,Sexo = h) y
#E(puntaje;Trat = Trat1,Sexo = m) < E(puntuaje;Trat = Trat2,Sexo = m) y E(puntaje;Trat = Trat1,Sexo = m) < E(puntaje;Trat = Control,Sexo = m)

#Expresando la preuba de hipotesis (Ho)

# (Ho):  b1-b2 >= 0 ^
#        b1 >= 0 ^
#        b1+b3-b2 >= 0 ^
#        b1+b3 >= 0



k_7 =matrix(c(0,1,-1,0,
              0,1,0,0,
              0,1,-1,1,
              0,1,0,1), ncol=4, nrow = 4, byrow = TRUE)

summary(glht(modelo_reducido, linfct =k_7 , rhs = c(0,0,0,0), alternative = "less"))


#Conclusiones:
#Con estoy prueba de hipotesis, obeservamos que se rechazaron todas nuestras preubas de hipotesis (Ho), con lo que podemos conclir que el medicamente viejo (Tratamiento 1) 
#es mejor que el medicamento nuevo (Tratamiento 2) y el control, tanto en hombres como en mujeres, ya que en promedio baja mas los niveles de ansiedad en los pacientes.

