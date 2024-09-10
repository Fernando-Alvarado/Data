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
#ID_DISTRITO_FEDERAL
#Distrito  


colnames(datos_sen_selec)[colnames(datos_sen_selec) == "ID_DISTRITO_FEDERAL"] <- "Distrito"
#vamos a cambiar el tpo de datos a factor
datos_sen_selec$Distrito <- as.factor(datos_sen_selec$Distrito)
datos_indicadores$Distrito <- as.factor(datos_indicadores$Distrito)

#Ahora vamos a agrupar los datos de las senadurias por distrito para poder hacer el merge ya que tenemos muchso datos repetidos
#datos_sen_agrupados 



datos_sen_agrupados <- datos_sen_selec %>%
  group_by(Distrito) %>%
  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))

#datos_indicadores_agrupados <- datos_indicadores %>%
#  group_by(ID_ENTIDAD) %>%
#  summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
#Vamos a hacer el merge de los dos dataframes

# 1. Merge de los datos de Indicadores.csv y SEN_2024.csv
datos_limpios <- merge(datos_sen_agrupados, datos_indicadores, by = 'Distrito')


#print(head(datos_limpios))


#-------------------------------------------------------------------------------------------------------------
#Con los datos obtenidos vamos a hacer un analisis de los datos 
#-------------------------------------------------------------------------------------------------------------

#Calcular el porcentaje a favor de la coalicion (PVEM-PT-MORENA)

#datos_sen_selec <- datos_sen %>% select("ID_DISTRITO_FEDERAL", "PAN","PRI","PRD","PVEM","PT","MC","MORENA","PAN.PRI.PRD","PAN.PRI","PAN.PRD","PRI.PRD","PVEM_PT_MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA","TOTAL_VOTOS_CALCULADO") # nolint


datos_coalicion <- datos_limpios %>% select("PVEM_PT_MORENA","PVEM","PT","MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA") # nolint
suma_total_votos <- sum(rowSums(datos_limpios %>% select("TOTAL_VOTOS_CALCULADO")))

suma_coalicion <-sum(rowSums(datos_coalicion))

print("Respondiendo pregunta 2")
print(paste("El porcentaje de la coalicion es:", (suma_coalicion / suma_total_votos)*100))


#Respondiendo la pregunta 3 

datos_diagrama <-datos_limpios %>% select("PVEM_PT_MORENA","PVEM","PT","MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA","TOTAL_VOTOS_CALCULADO", "Porcentaje1", "Porcentaje2", "Porcentaje3" )

datos_diagrama$total <- rowSums(datos_diagrama[, c("PVEM_PT_MORENA","PVEM","PT","MORENA","PVEM_PT","PVEM_MORENA","PT_MORENA")])

#Grafica 1

ggplot(datos_diagrama, aes(x = total, y = Porcentaje1)) + 
  geom_point(color = "red") +
  labs(title = "Diagrama de dispersión entre el Total Coalicion y el porcentaje 1", x = "Total", y = "porcentaje 1")

#Grafica 1

ggplot(datos_diagrama, aes(x = total, y = Porcentaje2)) + 
  geom_point(color = "blue") +
  labs(title = "Diagrama de dispersión entre el Total Coalicion y el porcentaje 2", x = "Total", y = "porcentaje 2")

#Grafica 1

ggplot(datos_diagrama, aes(x = total, y = Porcentaje1)) + 
  geom_point(color = "black") +
  labs(title = "Diagrama de dispersión entre el Total Coalicion y el porcentaje 3", x = "Total", y = "porcentaje 3")


#Por tiempo ya no puede hacer los otros incisos, pero les juro que fue trabajo honesto y unas buenas desveladas :( jajajajajajaja

