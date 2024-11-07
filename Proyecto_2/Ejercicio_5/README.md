## Análisis de Efectividad de un Medicamento para Covid-19

Suponga que una empresa farmacéutica está ofreciendo al gobierno un nuevo medicamento para tratar a pacientes con la enfermedad Covid-19. El costo del medicamento es considerable y para tomar una buena decisión se han acercado a usted para analizar los datos que ha compartido la empresa farmacéutica. El archivo `Ex5.csv` contiene la información: `Ant` es el número total de anticuerpos, `Trat` es una variable con dos niveles dependiendo si se aplicó o no el nuevo medicamento. Se sabe que tener mayores anticuerpos evita que se desarrolle una versión grave de la enfermedad y la empresa afirma que eso se logra al aplicar el medicamento, pues los pacientes que recibieron el medicamento tienen más anticuerpos que los que sólo recibieron placebo. También se sabe que la generación de anticuerpos es diferente dependiendo de la edad de los individuos y se sospecha que eso también podría afectar la efectividad del medicamento, así que al diseñar el experimento se seleccionaron al azar 100 personas de 300 que presentaban síntomas leves al iniciar el cuadro de la enfermedad a los que se les administró el medicamento, al resto se les dio sólo seguimiento. En todos los pacientes se capturó la edad y se procuró tener pacientes en el rango entre 16 y 60 años en ambos grupos. No se sospecha de otro aspecto que pudiera modificar la evaluación del medicamento.

*(Para este ejercicio no se requiere verificar supuestos del modelo, asuma que se cumplen)*

### Instrucciones

1. **Análisis descriptivo**: Realice un análisis descriptivo de los datos considerando tanto la información de la edad como de la administración o no del medicamento.

2. **Modelo ajustado**: Ajuste un modelo adecuado para evaluar la efectividad del medicamento ajustando por la edad de los pacientes. Es decir, un modelo que incluya como explicativas las variables edad, la binaria asociada a la administración del medicamento y la interacción obtenida como el producto de estas dos.

3. **Interpretación del modelo**: De acuerdo con el modelo ajustado, indique las expresiones asociadas a la relación de la generación promedio de anticuerpos con la edad en:
    - (a) el grupo control
    - (b) el grupo que recibe el medicamento

4. **Efecto de la edad en anticuerpos**: ¿Se puede decir que la edad afecta de la misma forma la generación de anticuerpos en el grupo control que en el grupo que recibe el medicamento? Realice una prueba de hipótesis apropiada e interprete.

5. **Evaluación del modelo**: Comente sobre el ajuste del modelo incluyendo la interpretación de cada uno de los coeficientes.

6. **Evaluación de la afirmación**: Argumente en contra o a favor de la afirmación: *"El medicamento funciona aumentando el número de anticuerpos para todos los pacientes entre 25 y 60 años"*. Se puede apoyar de pruebas de hipótesis o intervalos de confianza simultáneos.
