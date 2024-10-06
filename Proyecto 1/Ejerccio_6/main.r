#Problema 6 de los Pinguinos, echo por: Fernando Alvarado Palacios

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


#Datos sobre los pinguinos
x = c(79, 93, 100, 105, 85, 101, 96, 96, 109, 70, 71, 87)
y = c(119, 142, 158, 157, 136, 151, 139, 142, 165, 117, 123, 130)

Datos6 = data.frame(cbind(x, y))

kable(t(Datos6)) %>%
  kable_styling(bootstrap_options = "striped", full_width = F)


#Ajusstando el modelo de regresion lineal
modelo = lm(y ~ x, data = Datos6)

summary(modelo)

#-- Con este primer vistazo podemos observar que si tienen relacion las variables x, y en el modelo de regresion lineal

#Graficando el modelo de regresion lineal

grafica_modelo = function(varx) {
  ggplot(Datos6, aes(x = varx, y = y)) + #Creando la grafica de nuetra regresion lineal, agustado con los datos de los pinguinos
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Modelo de regresion lineal",
       x = "x peso del huevo menor",
       y = "y peso del huevo mayor") +
  theme_minimal()
}

grafica_modelo(x)




# Inciso I) Verificando si se cumplen los supuestos del modelo de regresion lineal

#Checando  homocedasticidad por medio de la grafica propia de R

plot(modelo, 3)#Checar homocedasticidad, por defecto en R

#Aplicando la una prueba de hipotesis para la homocedasticidad

lmtest::bptest(modelo)

#-- Con esta prueba de Hipoteis como nuestro p-value es 0.5036, podemos concluir que tenemos homocedasticidad


#Checando linealidad  por medio de la grafica de R

plot(modelo, 1)#Checar linealidad, por defecto en R

#-- Con esta grafica podemos observar que no se esta cumpliendo el supuesto de linealidad, por lo que tendremso que hacer una transformacion de los datos
#-- # se va a hacer a partir de la familia de transformaciones Box-Tidwell
pruebaBox_Tid <- boxTidwell(y~x, data=Datos6) #No pongas el modelo, si no los datos
pruebaBox_Tid
#-- Con esta prueba podemos observar que es necesrio hacer una transformacion de los datos


#Ajustando nuestro modelo de Regresion Lineal con una transformacion  
   #pruebaBox_Tid$estimate[1]  
lambda <- 4 #Valor de lambda obtenido de la prueba de Box-Tidwell

#-- Este valor esta redondeado, para poder tener una mejor interpretacion 

Datos6$x_transformed <- Datos6$x^lambda    # Aplicar la transformación

Datos6
#Volciendo a hacer el modelo de regresion  
modelo_transformado = lm(y ~ x_transformed, data = Datos6)

summary(modelo_transformado) #Checando el resumen del modelo
grafica_modelo(Datos6$x_transformed) #Graficando el modelo de regresion lineal transformado

#Checando  homocedasticidad por medio de la prueba de hipotesis en R

lmtest::bptest(modelo_transformado) 

#Nuestro modelo pasa de nuevo la prueba de homocedasticidad

#Veamos de nuevo lineaalidad por medio de la prueba de hipotesis en R
boxTidwell(y~x_transformed, data=Datos6) 
#-- Con esta prueba podemos observar que ya se cumple el supuesto de linealidad

#Vamos a prbrar normalidad en los residuales, de nuestros modelos

ResiduosModelo = augment(modelo_transformado) #Arreglando los residuales de nuestro modelo
head(ResiduosModelo) #Veamos que nos regresa la funcion augment
shapiro.test(ResiduosModelo$.std.resid)
#-- Con esta prueba como  p-value es 0.3847, podemos concluir que nuestros residuales son normales

plot(modelo_transformado, 2) #Checar normalidad, por defecto en R

#Vamos a verificar aleaotoridad en nuestra muestra

lawstat::runs.test(ResiduosModelo$.std.resid, plot.it = TRUE) #Checar aleatoriedad


# 
#  Con todas estas pruebas podemos concluir que nuestro modelo de regresion lineal cumple con los supuestos de linealidad, homocedasticidad, normalidad y aleatoriedad
#  y ya con esto podemos haeer predicciones con nuestro modelo de regresion lineal
# 









