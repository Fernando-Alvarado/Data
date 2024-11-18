

rm(list = ls(all.names = TRUE))
gc()

ventas <- c(12,10,15,18,11,11,17,16,14,15,23,20,18,17,27,33,23,26,28)
empaque <- c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,4)

datos <- data.frame(ventas = ventas, empaque = empaque)

datos$empaque <- factor(datos$empaque)

str(datos)
head(datos)


X11()
library(GGally)
ggpairs(datos[,c(1,2)])


#//////////////////////////////////////////////////////INCISO 1, GRAFICA BOXPLOT///////////////////////////////////////////////////
X11()
boxplot(ventas ~ empaque, data = datos, col = "white", outline=FALSE)
stripchart(ventas ~ empaque, data = datos,
           method = "jitter",
           pch = 19,
           col = 2:4,
           vertical = TRUE,
           add = TRUE)

#/////////////////////////////////////////////////////INCISO 2, AJUSTE DE MODELO/////////////////////////////////////////////// 

#Asumimos que se cumplen todos los supuestos y ajustamos el modelo

fit1=lm(ventas ~ empaque, data = datos)
summary(fit1)


#Notamos que el nivel de referencia es el Empaque 1 por lo que las expresiones son:
# E[y]= 13.2 + 1.4x_2 + 6.3x_3 + 14.2x_4
# E[y| el empaque 1]= B_0 = 13.200
# E[y| el empaque 2]= B_0 + B_1 = 13.200 + 1.400
# E[y| el empaque 3]= B_0 + B_2 = 13.200 + 6.300
# E[y| el empaque 4]= B_0 + B_3 = 13.200 + 14.200


#/////////////////////////////////////////////////////////////INCISO 3, PRUEBA F///////////////////////////////////////////////////

#La prueba F asociada a la tabla ANOVA contrasta las siguientes hipotesis:
#Ho: Las tres esperanzas son iguales: B1=0 , B2=0 y B3=0 vs B1 !=0, B2!=0 y B3!=0

drop1(fit1, test="F")

#Se rechaza la prubea de hipotesis, es decir, los bethas son diferentes de cero, lo que nos indica que,
# al menos uno de los empaques tiene un promedio en las ventas diferente. Es estadisticamente significativo el modelo


#///////////////////////////////////////////////////////////////////////INCISO 4/////////////////////////////////////////////////

# Dado que la hipotesis H0 de la prueba F asociada a la tabla ANOVA se rechazó
# vemos que el diseño del empaque sí influye en las ventas promedio.
#las pruebas son: Ho: B_0=B_1=B_2=B_3=0 VS Hi: Al m3nos una diferente de cero


#////////////////////////////////////////INCISO 5, pruebas simultaneas por pares////////////////////////////////////////////////

#Usando: 
# E[y| el empaque 1]= B_0
# E[y| el empaque 2]= B_0 + B_1
# E[y| el empaque 3]= B_0 + B_2
# E[y| el empaque 4]= B_0 + B_3

#Queremos ver que:
#  E[y| el empaque 1]= E[y| el empaque 2]
#  E[y| el empaque 1]= E[y| el empaque 3]
#  E[y| el empaque 1]= E[y| el empaque 4]
#  E[y| el empaque 2] = E[y| el empaque 3]
#  E[y| el empaque 2] = E[y| el empaque 4]
#  E[y| el empaque 3] = E[y| el empaque 4]

library(multcomp)

K=matrix(c(0,1,0,0,
           0,0,1,0,
           0,0,0,1,
           0,1,-1,0,
           0,1,0,-1,
           0,0,1,-1), ncol=4, nrow=6, byrow=TRUE)
m<- c(0,0,0,0,0,0)
summary(glht(fit1, linfct=K, rhs=m ))

summary(glht(fit1, linfct = mcp(empaque = "Tukey")))

# vemos que en ambas pruebas se rechazan:
#  E[y| el empaque 1]= E[y| el empaque 3] 
#  E[y| el empaque 1]= E[y| el empaque 4]
#  E[y| el empaque 2] = E[y| el empaque 4]
#  E[y| el empaque 3] = E[y| el empaque 4]
# lo que nos indica que hay una diferencia estadísticamente significativa 
#entre las ventas de los empaques 3 y 1; 4 y1 ; 4 y 2; 4 y 3


#///////////////////////////////////////INCISO 6,¿Empaque 4 mejor que los demas?////////////////////////////////////////////////////////////

#Podemos usar el inciso anterior.

# Empaque 4 mejor que Empaque 1
#Lo que se quiere, si se puede, se pone en la hipótesis alternativa
# E[y| el empaque 4] = B_0 + B_3 > # E[y| el empaque 1]= B_0
#B_0 + B_3>B_0
#B_3>0
#Aquí Ho: B_3<=0

K=matrix(c(0,0,0,1), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))

# Se rechaza la hipotesis nula Ho: B_3<=0, lo que nos indica que el empaque 4 es mejor que el empaque 1

#Empaque 4 mejor que empaque 2
# E[y| el empaque 4] = B_0 + B_3 >  E[y| el empaque 2]= B_0 + B_1
# B_0 + B_3 > B_0 + B_1
# B_3-B_1>0
# Aquí Ho: B_3-B_1<=0

K=matrix(c(0,-1,0,1), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))

#Se rechaza la hipotesis nula Ho: B_3-B_1<=0, lo que nos indica que el empaque 4 es mejor que el empaque 2

#Empaque 4 mejor que empaque 3
# E[y| el empaque 4] = B_0 + B_3 >  E[y| el empaque 2]= B_0 + B_2
# B_0 + B_3 > B_0 + B_2
# B_3-B_2>0
# Aquí Ho: B_3-B_2<=0

K=matrix(c(0,0,-1,1), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))

#Se rechaza la hipotesis nula Ho: B_3-B_2<=0, lo que nos indica que el empaque 4 es mejor que el empaque 3


K=matrix(c(0,0,0,1,
           0,-1,0,1,
           0,0,-1,1), ncol=4, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(fit1, linfct=K, rhs=m, alternative="greater"))

#al hacer las pruebas de hipotesis simultaneas y una por una vemos que las hipotesis nulas se rechazan
# lo que nos indica que el Empaque 4 es el que aumenta las ventas en comparacion con el resto.






