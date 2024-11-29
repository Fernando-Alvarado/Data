#Codigo ehco por Alvarado Placios Fernando

# Creación del dataframe con los datos de la tabla
ldl_data <- data.frame(
  treat = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
            3, 3, 3, 3, 3, 3, 3, 3, 3, NA, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
  ldl = c(52, 67, 54, 69, 116, 79, 68, 47, 120, 73, 
          36, 34, 47, 125, 30, 31, 30, 59, 33, 98, 
          52, 55, 66, 50, 58, 176, 91, 66, 61, 63,
          62, 71, 41, 118, 48, 82, 65, 72, 49, 63)
)

# Mostrar el dataframe
print(ldl_data)