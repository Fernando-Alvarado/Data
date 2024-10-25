
####################
# Poder Predictivo #
# Regresión Lineal #
####################

rm(list = ls(all.names = TRUE))
gc()

library(tidyverse)


## Datos
Datos <- read.csv("ejemplo4.csv")

## Diagrama de dispersión
ggplot(data = Datos,
       mapping = aes(x = x, y =y)) + 
  geom_point(size = 1,
             color = "deeppink") +
  theme_bw()


# Propuesta 1 de modelo, RLS ----------------------------------------------

## Modelo de RLS
Ajuste1 <- lm(formula = y ~ x,
              data = Datos)

summary(Ajuste1)

b01 <- Ajuste1[["coefficients"]][["(Intercept)"]]
b11 <- Ajuste1[["coefficients"]][["x"]]

## Estimaciones puntuales
Modelo1 <- function(x){
  b01 + b11*x
}

ggplot(data = Datos,
       mapping = aes(x = x, y = y)) + 
  geom_point(size = 1,
             color = "deeppink2") +
  geom_function(fun = Modelo1) +
  theme_bw()


# Propuesta 2 de modelo, RLS Box-Tidwell ----------------------------------
# Box-Tidwell sirven para tratar de arreglar problemas con el supuesto de 
# linealidad.

car::boxTidwell(formula = y ~ x,
                data = Datos)

Ajuste2 <- lm(formula = y ~ I(x^2),
              data = Datos)

summary(Ajuste2)

b02 <- Ajuste2[["coefficients"]][["(Intercept)"]]
b12 <- Ajuste2[["coefficients"]][["I(x^2)"]]

Modelo2 <- function(x){
  b02 + b12*(x^2)
}

ggplot(data = Datos,
       mapping = aes(x = x, y = y)) + 
  geom_point(size = 1,
             color = "deeppink3") +
  geom_function(fun = Modelo2) +
  theme_bw()

## Lo anterior define sólo el ajuste de los modelos.
## Sólo uno sería usado para la predicción, por ejemplo, 
## usando el estimador de E(y;x).
##
## La decisión de qué modelo usar se basa en el poder predictivo.
## Eso se calcula a continuación.

# Evaluación del poder predictivo -----------------------------------------


# K-Cross Validation ------------------------------------------------------

## paquete caret ayuda a tomar muestras aleatorias para K-CV o holdout method

library(caret)

## La función createFolds() del paquete caret ayuda a obtener la muestra 
## en cada una de las K divisiones para K-CV.

## Variable auxiliar 
Datos <- Datos %>% 
  mutate(Aux = as.factor(1))

## Partición de los datos

set.seed(1234)
Folds <- createFolds(Datos$Aux, k = 10, list = FALSE)
Folds
table(Folds)

## Vector auxiliar para guardar resultados del modelo 1
SE1 <- c(NA)

## Vector auxiliar para guardar resultados del modelo 2
SE2 <- c(NA)

for(kj in 1:10){
  
  ## Datos de entrenamiento (con ellos se ajusta el modelo)
  Datos.train <- Datos[(Folds==kj)==FALSE,]
  
  ## Datos de test (con ellos se evalua el modelo)
  Datos.test <- Datos[Folds==kj,]
  
  ## Ajuste del Modelo 1
  fitkj1 <- lm(y~x, data=Datos.train)
  
  ## Predicciones del Modelo 1
  predkj1 <- predict(fitkj1, Datos.test)
  
  ## Guardamos el error cuadrático medio del Modelo 1
  SE1[kj] <- sum((Datos.test$y-predkj1)^2)
  
  ## Ajuste del Modelo 2
  fitkj2 <- lm(y~I(x^2), data=Datos.train)
  
  ## Predicciones del Modelo 2
  predkj2 <- predict(fitkj2, Datos.test)
  
  ## Guardamos el error cuadrático medio del Modelo 2
  SE2[kj] <- sum((Datos.test$y-predkj2)^2)  
}

## Promedio del error cuadrático medio para el Modelo 1
sum(SE1)/length(Folds)

## Promedio del error cuadrático medio para el Modelo 2
sum(SE2)/length(Folds)


# Repeated K-Cross Validation ---------------------------------------------

## El proceso de validación cruzada (con 10 pliegues) se repite B veces 
## y se promedian los resultados (error cuadrático medio)

B <- 200
K <- 10

## Existen dos opciones en R que ya hacen ese proceso

### Opción 1:
library(boot)

## Modelo 1 equivalente con la función glm()
Modelo1_glm <- glm(formula = y~x, 
                   data = Datos)

## Modelo 2 equivalente con la función glm()
Modelo2_glm <- glm(formula = y~I(x^2), 
                   data = Datos)

## Semilla
set.seed(1234)

## Vector auxiliar para guardar los resultados de cada
## proceso de validación cruzada para el Modelo 1
cvLRfit1 <- rep(NA,B)

for (i in 1:B){
  cvLRfit1[i] <- cv.glm(data = Datos, 
                        glmfit = Modelo1_glm, 
                        K = K)$delta[1]
}

(mean(cvLRfit1))

## Semilla
set.seed(1234)

## Vector auxiliar para guardar los resultados de cada
## proceso de validación cruzada para el Modelo 2
cvLRfit2 <- rep(NA,B)

for (i in 1:B){
  cvLRfit2[i] <- cv.glm(data = Datos, 
                        glmfit = Modelo2_glm, 
                        K = K)$delta[1]
}

(mean(cvLRfit2))


### Opción 2:
library(cvTools)

## Semilla
set.seed(123)

## Pliegues
folds <- cvFolds(nrow(Datos), K = K, R = B)

repCV(object = Ajuste1, 
      cost = mspe, 
      folds = folds)

Datos$xP <- Datos$x^2

Ajuste3 <- lm(formula = y ~ xP,
              data = Datos)

repCV(object = Ajuste3, 
      cost = mspe, 
      folds = folds)

