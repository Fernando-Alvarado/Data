rm(list = ls(all.names = TRUE))
gc()

#install.packages("ALSM")
data = ALSM::TolucaCompany
head(data) 
#x es el tamaño de un lote de producción
#y las horas de trabajo realizadas para obtener el lote

#Definiendo las matrices
n = dim(data)[1]
X = rep(1, n)
X = cbind(X, data[1])
Y = data[2]

X = as.matrix(X)
head(X)
Y = as.matrix(Y)
head(Y)

#Sustituyendo con lo obtenido
solve(t(X) %*% X) %*% t(X) %*% Y

#Comparando con los datos obtenidos de un ajuste de regresión
fit <- lm(y ~ x, data)
coef(fit)


