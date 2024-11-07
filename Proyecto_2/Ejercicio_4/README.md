# Análisis de Estudio de Tratamiento para Control de Ansiedad

Una institución de investigación realiza un estudio para analizar los efectos de un nuevo tratamiento para controlar los niveles altos de ansiedad. Para eso consideran un puntaje (a mayor valor mayores niveles de ansiedad) y definen un conjunto experimental con 120 individuos en que ese puntaje presentan valores similares al inicio del estudio, 60 son hombres y 60 mujeres. En el mercado se sabe que hay otro tratamiento que se usa comúnmente para este fin, de forma que de forma aleatoria han dividido a los 120 individuos en tres grupos: 40 a los que no se aplicó ningún tratamiento (control), 40 a los que se aplicó el tratamiento actual (Trat1) y 40 a los que se aplicó el nuevo tratamiento (Trat2); 20 hombres y 20 mujeres en cada grupo. Los datos se presentan en el archivo `Ex4A.csv`.

Los investigadores sospechan que para el nuevo tratamiento podría existir un efecto diferenciado de acuerdo con el sexo, por lo que consideraron conveniente incluir esta variable en el análisis.

*(Para este ejercicio no se requiere verificar supuestos del modelo, asuma que se cumplen)*

## Instrucciones

1. **Análisis descriptivo**: Realice un análisis descriptivo de los datos. Dado que las dos covariables son categóricas, incluya un boxplot para cada posible combinación de niveles que se pueden observar en esas dos variables categóricas (`boxplot(Puntaje~Trat+Sexo, ...)`). Comente lo que observe.

2. **Modelo de regresión**: Considerando un modelo de regresión que incluye las dos variables categóricas de forma individual y también su interacción, dé la expresión del puntaje promedio para cada valor de las variables categóricas, es decir: \( E(puntaje | Trat = k, Sexo = l) \), con \( k \in \{Control, Trat1, Trat2\} \) y \( l \in \{Hombre, Mujer\} \); así como la estimación puntual correspondiente.

### Continuación del Análisis

3. **Hipótesis con ANOVA**: Escriba las hipótesis que se contrastan con la tabla ANOVA, calcule esta e interprete. Use \( \alpha = 0.05 \).

4. **Efecto del Sexo**: ¿Se puede considerar que el sexo tiene un efecto en el puntaje, es decir, al menos para un tratamiento existe un efecto diferenciado en el puntaje derivado del sexo de los individuos? Use una prueba F con \( \alpha = 0.025 \). Interprete.

    - **Hint**: Aquí \( H_0 : E(puntaje; Trat = k, Sexo = Hombre) = E(puntaje; Trat = k, Sexo = Mujer) \ \forall \ k \in \{Control, Trat1, Trat2\} \).
    - En caso de no rechazar \( H_0 \), considere el modelo reducido eliminando la variable Sexo; pero si se rechaza \( H_0 \), considere una prueba simultánea que ayude a identificar para qué tratamiento se puede considerar que el sexo tiene un efecto, con los resultados de esa prueba reduzca el modelo si es posible.

5. **Ajuste de Modelo Reducido**: En caso de que en el inciso anterior se haya reducido el modelo, ajuste de nuevo la regresión y dé la expresión del puntaje promedio para cada valor en las variables categóricas: \( E(puntaje; Trat = k, Sexo = l) \), con \( k \in \{Control, Trat1, Trat2\} \) y \( l \in \{Hombre, Mujer\} \); así como estimaciones puntuales.

6. **Prueba de Hipótesis - Nuevo Tratamiento**: Realice una prueba de hipótesis para argumentar en favor o en contra de la hipótesis: *el nuevo tratamiento tiene el mejor desempeño*. Use \( \alpha = 0.05 \).

7. **Prueba de Hipótesis - Tratamiento Actual**: Realice una prueba de hipótesis para argumentar en favor o en contra de la hipótesis: *el tratamiento actual tiene el mejor desempeño*. Use \( \alpha = 0.05 \).

### Nota

Suponga que tiene dos variables categóricas, una con \( K \) niveles y la otra con \( J \) niveles. Para usar el modelo de regresión con interacciones se requiere incluir \( K - 1 \) y \( J - 1 \) variables binarias asociadas a los efectos principales de los niveles, además de \( (K - 1) \times (J - 1) \) variables binarias asociadas a las interacciones. Las variables de las interacciones se construyen como el producto de las \( K - 1 \) y \( J - 1 \) variables binarias que se introducen en el modelo.
