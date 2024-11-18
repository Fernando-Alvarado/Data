#Ejercicio 4 echo por Fernando Alvarado Palacios

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


#Estableciendo el directorio de trabajo
getwd()
setwd("C://Users//ferna//Documents//Estadisitica_2//Data//Proyecto_2//Ejercicio_5")#Modificar esta linea en caso de que no se encuentre en la misma carpeta

#Cargando los datos
data <- read.csv("ex5.csv")
summary(data)

#1) Realizando un analisis descriptivo de los datos
data %>% group_by(Trat) %>%  #Hacienod un analisis descriptivo de nuestros datos, para saber cosas como la media, mediana, varianza y el numero de observaciones
  summarise(Observaciones = n(),
            Media = round(mean(Ant),2),
            Mediana = median(Ant),
            Varianza = round(var(Ant),2))
#Observnado el analisis descriptivo de nuestros datos, viendo la media, mediana, varianza y el numero de observaciones, de los pacientes con y sin el medicamento

#"Ant","Trat","Edad"

#Hagamos una grafica rapido, para ver la distribucion de nuestros datos y verlos de forma grafica, en este caso grafico el puntaje de depresion
  # por color que tratamiento se le esta aplicando al paciente y con el tamaño del punto el sexo del paciente, para ver si hay alguna relacion entre
data$Trat <- factor(data$Trat)

X11()
# Crear la gráfica con nombres en los ejes
ggplot(data, aes(x = Edad, y = Ant, color = Trat)) +
  geom_point(size = 3, alpha = 0.7) + 
  labs(
    title = "Relación entre Edad y Numero de anticuerpos",
    x = "Edad (años)", 
    y = "Num. anticuerpos", 
    color = "Tratamiento"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "bottom"  
  )
#Con esta grafica podemos observar, que si parece existir una relacion entre el numero de anticuerpos y si el paciente tomo o no tomo el medicamento, pero esta diferencia se hace mas notoria
#en pacientes de mator edad

#Para poder observar mas esta diferencia, hagamos un boxplot relacionando, el numero de anticuerpos y si tomaron o no tomaeron el medicamento, pero ahora dividiendolo en solo pacientes de esas edad

#Observando como se ven nuestro datos, dependiendo de la edad del paciente 
ggplot(data, aes(x = Trat, y = Ant, fill = Trat)) +
  geom_boxplot() +
  facet_wrap(~Edad, scales = "free_y") +
  labs(
    title = "Boxplot de Ant por Trat",
    x = "Tratamiento",
    y = "Anticuerpos",
    fill = "Trat"
  ) 

#Con esta grafica, podemos obtener un poco mas de informacion, por ejemplo, no tenemos pacientes de ciertas edades que tomaron el medicaento o que al parecer en el grupo de edad de 16-17 años, 
#no tomar el medicamento pareceria tener un mayor efecto en el numero de anticuerpos



#2) Haciendo el modelo de regresion lineal
levels(data$Trat) #Checando los niveles para ver cual variable categorica se va a tomar como referencia
modelo <- lm(Ant ~ Edad * Trat, data = data)
summary(modelo)
#Vemos que la prueba Anova, la pasa el modelo, lo que significa que nuestro modelo tiene sentido

#"Ant","Trat","Edad"

#3) Expresion de nuestro modelo, y colocando las esperanzas
#Planteamento de nuestro modelo 
# T = {Control, Med}
# E = {Edad}
# Haciendo las variables dicotomicas para nuestro modelo
# T_c = {1 si  Control, 0 eoc}
# T_m = {1 si Med, 0 eoc}
#
#Por lo que nuestro modelo de regresion lineal seria:
#
# Ant = B0 + B1*Edad  + B2*T_m  + B3*Edad*T_m


#Obteniendo los coeficientes de nuestro modelo
B0 <- modelo[["coefficients"]][["(Intercept)"]]
B1 <- modelo[["coefficients"]][["Edad"]]
B2 <- modelo[["coefficients"]][["TratMed"]]
B3 <- modelo[["coefficients"]][["Edad:TratMed"]]


#Expresando las esperanzas de nuestro modelo
# E[Ant | Edas, Trat = Control] = B0 + B1
cat("E[Ant | Edad, Trat = Control] =", B0 + B1, "\n")
# E[Ant | Edas, Trat = Med] = B0 + B2 + B1 + B3
cat("E[Ant | Edad, Trat = Med] =", B0 + B1 + B2 + B3, "\n")


#4) ¿Se puede decir que la edad afecta de la misma forma la generación de anticuerpos en el grupo control que en el grupo que recibe el medicamento?
#Esta prueba la interpreto, como que la pendiente de la regresion lineal, es la misma en ambos grupos, por lo que la hipotesis nula seria que la pendiente es la misma en ambos grupos
#Nuestra prueba de hipotesis seria:
# Ho: E[Ant | Edas, Trat = Control] = E[Ant | Edad, Trat = Med] vs Ha: E[Ant | Edad, Trat = Control] != E[Ant | Edad, Trat = Med]
# (Ho)  B2 + B3 = 0 vs (Ha) B2 + B3 != 0

k = matrix(c(0,0,1,1), ncol = 4, nrow = 1, byrow = TRUE)
summary(glht(modelo, linfct = k, c(0), alternative = "two.sided"))

#Con esta prueba observemos que se rechaza Ho, lo que nos indicaria que la adad, afecta de diferente forma en la generacion de anticuerpos en ambos grupos
#Lo podemos comprar de forma grafica (Grafica de antes, pero con el modelo de regresion lineal)

ggplot(data, aes(x = Edad, y = Ant, color = Trat)) +
  geom_point(size = 3, alpha = 0.7) +  # Puntos observados
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  # Líneas ajustadas del modelo
  labs(
    title = "Relación entre Edad y Número de Anticuerpos",
    x = "Edad (años)", 
    y = "Num. anticuerpos", 
    color = "Tratamiento"
  ) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5),  
    legend.position = "bottom"  
  )


#5) Comente el ajuste del modelo
summary(modelo)
#El modelo se ajusta bien, ya que la prueba Anova, la pasa, lo que nos indica que nuestro modelo tiene sentido
# Y la pruebas por separado de los Bi, con i en {0,1,2,3} son diferentes de 0, lo que nos indico que no habira necesidad de quitar alguna variable de nuestro modelo
# (Intercept)=29.34, se puede ver como el numero de anticuerpos cuadno la edad es 0 (como un punto de partida)
# Edad (B1)= -0.2829, se puede ver como el incremento de anticuerpos por cada unidad en edad
# TratMed (B2)= -2.25730, se puede ver como diferencia en promedio del numero de Anticuerpos entre el grupo de control y el grupo que tomo el medicamento
# Edad:TratMed (B3)= 0.17307, es la interaccion o relacion que puede llegar a tener el medicamento con la edad en la generacion de anticuerpos por cada unidad de edad y que tratamiendo se le dio

#6) “El medicamento funciona aumentando el número de anticuerpos para todos los pacientes entre 25 y 60 años”.
#Para ver esta hipotesis usaremos intervalos de confianza simultaneos, para varios valores de la edad
#"Ant","Trat","Edad"


plot(data$Edad, data$Ant, pch=19, col = c("red", "blue")[data$Trat])
legend("bottomright", levels(data$Trat),
       col = c("red", "blue"), pch = 19, inset = 0.01,  pt.cex=1.5,cex = .9, y.intersp = 1.3 , bty="n")

edad <- seq(from = 25, to = 60, by = .5) #Creando un vector de edades 

length(edad) #71
# Ant = B0 + B1*Edad  + B2*T_m  + B3*Edad*T_m


#Vamos a bajar la confiaza a 90%pues seran inervalos siultaneos
# E[Ant | Edas, Trat = Control] = B0 + B1
#Bnada para el tratamiento de control
KC <- cbind(1, edad, 0,0)

#Banda para el tratamiento de medicamento
# E[Ant | Edas, Trat = Med] = B0 + B2 + B1 + B3
KM<- cbind(1, edad, 1, edad)

K <- rbind(KC, KM)#Haciendo la combinacion de los dos grupos

#Creando nuestro intervalo de confianza simultaneos
fitE <- glht(modelo, linfct = K)
fitci <- confint(fitE, level = 0.90)

 
#Creando las lineas en nuestra grafica
lines(edad, coef(fitE)[1:71], col="red")
lines(edad, fitci$confint[1:71,"upr"], col="red")
lines(edad, fitci$confint[1:71,"lwr"], col="red")

lines(edad, coef(fitE)[72:142], col="blue")
lines(edad, fitci$confint[72:142,"upr"], col="blue")
lines(edad, fitci$confint[72:142,"lwr"], col="blue")


#Combinando los valores de los limites superiores e inferiores, para los intervalos de confianza
cbind(edad, fitci$confint[1:71,"lwr"], fitci$confint[72:142,"upr"])

#Por lo tanto, podemos concluir que en efecto aplicar el medicamento aumenta el numero de anticuerpos en los pacientes
#Por lo que el medicamento funciones en el rando de edades de 25 a 60 años










