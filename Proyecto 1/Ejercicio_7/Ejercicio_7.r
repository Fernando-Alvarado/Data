
rm(list = ls(all.names = TRUE))
gc()

#llamamos a las librerias que usaremos
library(dplyr)
library(ggplot2)
library(psych)
library(latex2exp)
library(lmtest)
library(car)

#Extraemos los datos de: performance
setwd('C:\\Users\\980018031\\OneDrive\\Documentos\\TAREAS NICO')
datos<-read.csv('performance.csv', header = TRUE)

#seleccinamos solo las columnas que nos interesan, en este caso son "x=ell" y "y=api00"
#Además, verificamos el tipo de datos de ambas columnas
Datos <- datos[,c("ell","api00")]
str(Datos)


#Les cambiamos el nombre a ambas columnas para facilitar la interpretacion del codigo
colnames(Datos) <- c("x", "y")




#INCISO 1: AJUSTAR EL MODELO DE REGRESION Y VER SUPUESTOS

#AJUSTAMOS EL MODELO CON LA VARIABLES SIN TRANSFORMAR Y PEDIMOS EL SUMMARY
modelo<- lm(y~x, data=Datos)
summary(modelo)

#GRAFICAMOS LAS VARIABLES X-Y Y LE AGREGAMOS UNA RECTA ROJA QUE MUESTRE EL MODELO DE REGRESION
plot(Datos$x,Datos$y, xlab= "porcentaje de estudiantes de inglés", ylab = "Rendimiento de la escuela",
     col= "blue")

abline(modelo$coefficients, col="red")

#Revisamos homocedasticidad
#Para verificar esto haremos dos cosas, revisar la grafica de las y_gorro obtenidas con el modelo
#vs los residuos estandarizados.
plot(modelo,3)

#Observamos que podemos encerrar la mayoria de los puntos en un rectangulo por lo que podemos intuir que 
#el supuesto de homocedasticidad se cumple, ahora revisemos las pruebas de hipotesis:

# H0: varianza no depende de forma lineal en x vs  Ha: varianza depende de forma lineal en x
# Se busca no rechazar, es decir,
# Asumir que la varianza no depende de forma lineal de x ,i.e., p-value mayor a significancia.
#Esta prueba usa residuales studentilizados

lmtest::bptest(modelo)

# En esta prueba no se rechaza la hipotesis se obtiene que la varianza no depende de x
#sucede lo mismo con las prubeas de hipotesis que usan residuales estandarizados

car::ncvTest(modelo)
car::ncvTest(modelo,~x)

#Al graficar lo anterior y hacer las respetivas pruebas de hipotesis, vemos que
#el supuesto de homoelasticidad se cumple, es decir, la varianza no depende de forma
#lineal de las X, se rechaza la hipotesis de que conforme aumenta X aumenta la varianza


#LINEALIDAD
#Para lienalidad veremos como se comporta el grafico de dispersion de los datos 
#"XvsY" y "Y_GORRO vs E"

plot(Datos$x,Datos$y, xlab= "porcentaje de estudiantes de inglés", ylab = "Rendimiento de la escuela",
     col= "blue")

abline(modelo$coefficients, col="red")

par(mfrow=c(1,1)) 
par(mar=c(4,5,1, 1))
plot(modelo, 1)

#En este caso vemos en la grafica de residuos vs "y_gorro" que los residuos van aumentando
#conforme se va alejando del origen, es decir, conforme aumenta el valor de y_gorro
#los residuos se van haciendo mas grandes. Por lo tanto, NO se cumple el supuesto de linealidad


#NORMALIDAD
par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(modelo, 2)
#Se usan los residuales estandarizados y los cuantiles de una normal. veamos que no se cumple
#el supuesto de normalidad,ya que las colas de la recta se lejan ,ucho de la diagonal



#INCISO 2
#En este caso, tendremos que hacer una trasnformacion en la linealidad de los residuales
#Definimos un proceso para sumar una constante a los datos x ya que no pueden ser <=0
# Definir la constante
constante <- 1

# Verificar que la constante no esté vacía
if (length(constante) == 0) {
  stop("La constante no se ha definido correctamente.")
}
# Calcular la nueva columna
nueva_columna <- Datos$x + constante
# Verificar la longitud de la nueva columna
if (length(Datos$x) != nrow(Datos)) {
  stop("La nueva columna tiene una longitud diferente al número de filas del dataframe.")
}
# Asignamos la nueva columna al dataframe Datos
Datos$nueva_columna <- nueva_columna

# Mostrar el dataframe resultante
print(Datos)
min(Datos$x)


#realizamos una transormacion Box-Tidwell
boxTidwell(y~nueva_columna, data=Datos)

#esta transformacion nos indica que lambda= 1/2

#La salida es el estimador de la potencia   0.43447 aprox 1/2
#Test Ho: lambda=1 vs Ha:lambda != 1.
#Si no se rechaza, entonces es plausible considerar el modelo con X directamente
#Si se rechaza, entonces conviene usar el estimador de lambda=1/2 para transformar X 
#Aquí se rechaza H0.

Datos$Xprima=sqrt(Datos$x)
modelo2=lm(y~Xprima, data=Datos)
summary(modelo2)
#Se rechaza la hipotesis nula, esto nos indica que es necesaria la transformacion

par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(modelo2, 1)
#El supuesto de linealidad se cumple, ahora vemaos que tambien se cumple el supuesto de homocedasticidad

#Homocedasticidad
par(mfrow=c(1,1)) 
par(mar=c(4, 5, 1, 1))
plot(modelo2, 3)

lmtest::bptest(modelo2)

summary(powerTransform(modelo2))
# esto indica que no es necesario hacer otra transformacion. Esto mejora la
#homocedasticidad y la normalidad.

#revisando la normalidad

par(mar=c(4, 5, 1, 1))
par(mfrow=c(1,1))
plot(modelo2, 2)
#observando la grafica vemos que se cumple normalidad, ya que no se alejan demasiado los puntos de la diagonal


# Crear un dataframe para las y_gorror del modelo 1 y las y_gorro del modelo 2
Datos$predicciones <- predict(modelo2)
Datos$y_gorro <- predict(modelo)

# Graficamos los datos originales junto con los dos modelos de regresion
ggplot(Datos, aes(x = x, y = y)) +
  geom_point(color = "black") +  # Puntos originales
  geom_line(aes(y = predicciones), color = "blue") + 
  geom_line(aes(y = y_gorro), color = "red") +# Curva del modelo transformado
  labs(title = "Modelo con Transformación Raíz Cuadrada", x = "X", y = "Y") +
  theme_minimal()

#INCISO 3
summary(modelo2)
anova(modelo2)

#En este caso podemos observar que la R cuadrada nos indica que el 62.56% de la variabilidad
#del rendimiento de los alumnos de la escuelas se explica por el modelo que inclue a la
#X transformada a sqrt(x)

#Para la tabla Anova vemos que:
# Para este caso vemos que el p value es extremadamente bajo, lo que nos indica que
#la variable sqrt(x) tiene un efecto significativo sobre Y.

#INCISO 4

summary(modelo2)
#basta ver el parametro que acompaña a la variable independiente y la grafica, conforme
#va aumentando el porcentaje de niños que requieren clases de ingles la calidad escolar disminuye
#recordemos que cuando se hace una transformacion, lo qu estamos graficando es la mediana de los datos
#esto nos dice que al menos la mitad de los datos se comportan de la misma froma que la mediana.}
#conforme aumenta el porcentaje de niños que necesitan calses de recuperacion de ingles, el nivel de la
#escuela va cayendo.



