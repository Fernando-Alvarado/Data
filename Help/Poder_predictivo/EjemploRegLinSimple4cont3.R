rm(list = ls(all.names = TRUE))
gc()

# Ejemplo. Estimación del poder predictivo de dos modelos
# a) un modelo que no cumple supuestos, pero podría ser la opción más fácil
# b) un modelo que cumple supuestos

setwd("C:\\Users\\`\\Desktop\\MNPyR 2025-1\\Poder predictivo")
Datos = read.csv("ejemplo4.csv")

head(Datos)
str(Datos)

par(mfrow=c(1,2)) 
par(mar=c(4, 5, 1, 1))
library(latex2exp)
plot(Datos$x, Datos$y, xlab = TeX("$x$"), ylab=TeX("$y$") )

# a.
# Modelo 1. Sin transformar variables
fit=lm(y~x, data=Datos)
summary(fit)

# Para usar funciones del paquete boot se ajusta el modelo usando la función glm
# por defaul coincide con el caso normal, pero tiene más argumentos
# para funcionar con modelos lineales generalizados
fitglm=glm(y~x, data=Datos)
summary(fitglm)

# b.
# Modelo 2
# Se transforma X a X^2 para lograr linealidad
Datos$Xprima=Datos$x^2
plot(Datos$Xprima, Datos$y, xlab = TeX("$x^2$"), ylab=TeX("$y$") )
fit2=lm(y~Xprima, data=Datos)
summary(fit2)
fit2glm=glm(y~Xprima, data=Datos)
summary(fit2glm)

# Lo anterior define sólo el ajuste de los modelos
# sólo uno sería usado para la predicción, por ejemplo, usando el estimador de E(y;x)
# La decisión de qué modelo usar se basa en el poder predictivo.
# Eso se calcula a continuación.


# ============  Opcion con caret  ============

# paquete caret ayuda a tomar muestras aleatorias para K-CV o holdout method
library(caret)
set.seed(1234)

# La función createFolds() del paquete caret ayuda a obtener la muestra 
# en cada una de las K divisiones para K-CV.
Datos$unos=1
Datos$unos=factor(Datos$unos) 
Folds=createFolds(Datos$unos, k = 10, list = FALSE)
Folds
table(Folds)


# Vector auxiliar para guardar resultados del modelo 1
SE1=c(NA)
# Vector auxiliar para guardar resultados del modelo 2
SE2=c(NA)
for(kj in 1:10){
  
  # Datos de entrenamiento (con ellos se ajusta el modelo)
  Datos.train=Datos[(Folds==kj)==FALSE,]
  # Datos de test (con ellos se evalua el modelo)
  Datos.test=Datos[Folds==kj,]
  # Ajuste del Modelo 1
  fitkj1=lm(y~x, data=Datos.train)
  # Predicciones del Modelo 1
  predkj1=predict(fitkj1, Datos.test)
  # Guardamos el error cuadrático medio del Modelo 1
  SE1[kj]=sum((Datos.test$y-predkj1)^2)
  
  
  fitkj2=lm(y~Xprima, data=Datos.train)
  predkj2=predict(fitkj2, Datos.test)
  SE2[kj]=sum((Datos.test$y-predkj2)^2)  
}

# Promedio del error cuadrático medio para el Modelo 1
(MSEtest1=sum(SE1)/length(Folds))
# Promedio del error cuadrático medio para el Modelo 2
(MSEtest2=sum(SE2)/length(Folds))

# Lo anterior se debe realizar B veces y se promedia
# Existen dos opciones en R que ya hacen ese proceso
B=200
K=10


# ======== 10-CV. Validación cruzada (boot) ==========
 
library(boot)
set.seed(123)

# Vector auxiliar para guardar los resultados de cada
# proceso de validación cruzada para el Modelo 1
cvLRfit1=rep(NA,B)
for (i in 1:B){
  cvLRfit1[i]=cv.glm(Datos, fitglm, K=K)$delta[1]
}
(mean(cvLRfit1))


set.seed(123)

# Vector auxiliar para guardar los resultados de cada
# proceso de validación cruzada para el Modelo 2
cvLRfit2=rep(NA,B)
for (i in 1:B){
  cvLRfit2[i]=cv.glm(Datos, fit2glm, K=K)$delta[1]
}
(mean(cvLRfit2))


# ========== cvTools ============

# 10-CV. Validación cruzada
library(cvTools)
set.seed(123)
folds <- cvFolds(nrow(Datos), K = K, R = B)

repCV(fit, cost = mspe, folds = folds)

repCV(fit2, cost = mspe, folds = folds)

 