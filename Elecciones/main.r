#En este archivo haremos el proceso de la base de datos 
setwd("C:/Users/ferna/Documents/Estadisitica_2/Data/Elecciones/Data")

load_libraries  <- function(paquete){ #Funcion para cargar las librerias y ejecutarlas
    if(!require(paquete, character.only = TRUE)){
        install.packages(paquete)
    } # nolint
     library(paquete, character.only = TRUE)
}
#Cargamos las librerias
load_libraries("ggplot2")
load_libraries("gapminder")
load_libraries("dplyr")

#Ruta donde se encuentran nuestros archivos
datos_sen <- read.csv("SEN_2024.csv", sep = ",", header = TRUE) #leemos el archivo de diputaciones # nolint
datos_indicadores <- read.csv("indicadores.csv", header = TRUE, sep = ",") #leemos el archivo de indicadores # nolint

#Primer filtro para seleccionar las columans que nos interesan
datos_sen_selec <- datos_sen %>% select("ID_DISTRITO_FEDERAL", "PAN","PRI","PRD","PVEM","PT","MC","MORENA","PAN.PRI.PRD","PAN.PRI","PAN.PRD","PRI.PRD","PVEM_PT_MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA","TOTAL_VOTOS_CALCULADO") # nolint




#Convertir los char a numericos para poder trabajar con ellos
pasar_char_int <- function(dataframe){
    for (i in 1:ncol(dataframe)){
        if (class(dataframe[,i]) == "character"){
            dataframe[,i] <- as.numeric(dataframe[,i])
        }
    }
    return(dataframe)
}

datos_sen_selec <- pasar_char_int(datos_sen_selec)
datos_indicadores <- pasar_char_int(datos_indicadores)


#vamos a quitar los filas con valores NA de nuestro dataframes, creo que este no era el objetivo pero dejar la funcion para un futuro  
limpiar_na <- function(dataframe) {
  # Usar complete.cases() para filtrar filas sin NA
  clean <- dataframe %>%
    filter(complete.cases(.))
  return(clean)
}


#funcion para reemplazar los valores NA por 0
reempazar_na <- function(dataframe) {
    for(i in 1:ncol(dataframe)){
       dataframe[is.na(dataframe[,i]), i] <- 0
    }
    return(dataframe)
}

datos_sen_selec <- reempazar_na(datos_sen_selec)
datos_indicadores <- reempazar_na(datos_indicadores)

#Quiero ver que con que tipos de datos estoy trabajando en mis dataframes
print((head(datos_sen_selec)))
print((head(datos_indicadores)))




