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

#Estableciendo el directorio de trabajo
getwd()
setwd("C://Users//ferna//Documents//Estadisitica_2//data//Ejercicio_5")#Modificar esta linea en caso de que no se encuentre en la misma carpeta

#Importadno los datos para trabajar
dataSal = read.csv("./Datos.csv")


#Graficando el modelo de regresion lineal, para poder visualizar los datos
grafica_modelo = function(varx, vary) {
  ggplot(dataSal, aes(x = varx, y = vary)) + #Creando la grafica de nuetra regresion lineal, agustado con los datos de los pinguinos
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  labs(title = "Modelo de regresion lineal",
       x = "x peso del huevo menor",
       y = "y peso del huevo mayor") +
  theme_minimal()
}

#Inciso a y b, los junte por que quise dar el modelo, grafica y luegos las respectivas transformaciones y sus respectivas graficas

#Proponiendo un primer modelo de regresion lineal para nuestros datos 
modelo = lm(years ~ salary, data = dataSal)
summary(modelo)

#-- Con este primer vistaso podemos concluir que los datos si tienne una relacion y podemos aplicar un modelo de regresion lineal

grafica_modelo(dataSal$salary, dataSal$years) #grafica del modelo sin transformar

#Verificando si se cumplen los supuestos del modelo de regresion lineal

#Checando linealidad
plot(modelo, 1)#Checar linealidad, por defecto en R, de primera vista parece que no es lineal
#Aplicando una prueba de hip para verificarlo 
boxTidwell(years ~ salary, data=dataSal) #Con esto verificamos que no es lineal y ocupamos aplicar untra trasformacion ln 

#Aplicando otrara trnasformacion a los datos para ver como se veria graficamente 

dataSal$transformed2 = dataSal$salary^2 #Aplicando la transformacion cuadratica a los datos
modeloIncA = lm(years ~ transformed, data = dataSal) #Modelo con la transformacion cuadratica
#Grafica para responder el inciso a, por completo 
grafica_modelo(dataSal$transformed2, dataSal$years) #grafica del modelo sin transformar




#Aplicando la transformacion ln a los datos que es la que nos recomendo la prueba de box-tidwell
dataSal$transformed = log(dataSal$salary) #Aplicando la transformacion ln a los datos
head(dataSal) #Para verificar que se aplico la transformacion


#Proponiendo un segundo modelo de regresion lineal para nuestros datos 
modelo2 = lm(years ~ transformed, data = dataSal)
summary(modelo2)
grafica_modelo(dataSal$transformed, dataSal$years) #grafica del modelo sin transformar


#Volvamos a apliar una prueba de hipotesis para verificar si se cumple la linealidad
boxTidwell(years ~ dataSal$transformed, data=dataSal) #Con esto ya podemos verificar que si es lineal nuestro modelo 


#c) inciso C


#Caculado los parametros de nuestro modelo 
b0 <- coef(modelo2)[1]
b1 <- coef(modelo2)[2]

print(paste("El valor de b0 es: ", b0, " y el valor de b1 es: ", b1))

#
#cuando tienen x=0, exp(b0) es exactamente E[y], y cuando incrementan en
# una unidad a x, se multiplica el valor anterior por una constante de exp(b1), 
# lo que significa que cada año trabajado se incrementa exp(11.3) veces el salario
# cosa que es muy buena por que aumenta mucho tu suelto, el valor de b0, no tiene mucho sientido de forma negtiva, 
# ya que no puedes tener un salario negativo, pero podemos iniciar nuestra regresion en 0 
#

#Por tiempo me carrancie tu codigo, una disuclpa :(((


# d) Gráficas de los datos transformados, con recta ajustada
ggplot(dataSal, aes(dataSal$transformed, dataSal$years)) +
  geom_point() +
  geom_abline(intercept = b0, slope = b1)


# e) Gráficas de los datos no transformados, con curva ajustada
curva_ajustada <- function(x) {exp(b0 + b1*x)} # aquí despejen E[y] de su modelo original,
                                            # por ejemplo, si usaron log(E[y]), debe quedar
                                            # exp(b0 + b1*x)
#Siento que queda muy fea, pero para mi transformacion use logaritmo 


ggplot(dataSal, aes(transformed, years)) +
  geom_point() +
  geom_function(fun = curva_ajustada)


AIC(modelo2)
BIC(modelo2) #Creo que mi modelo no es muy bueno por que tiene un valor muy alto de BICde mas de 400 

predicciones <- predict(modelo2)
ecm <- mean((dataSal$transformed - predicciones)^2)

