# Análisis de colesterol LDL con diferentes tratamientos

Los datos de la siguiente tabla muestran las mediciones de **low density lipid (LDL) cholesterol**, también conocido como colesterol malo, por las consecuencias asociadas a tener altos niveles de este colesterol. Cuatro tratamientos (`treat:1,2,3,4`) se probaron con la finalidad de reducir los niveles de LDL. Los datos de 39 observaciones aleatorias se muestran en la siguiente tabla (datos `quail` en paquete `Rfit`).

| Cuadro 1: Low density lipid (LDL) cholesterol para 39 observaciones independientes |
| --- |
| **n = 39** |

| treat | ldl  | treat | ldl  | treat | ldl  | treat | ldl  |
|-------|------|-------|------|-------|------|-------|------|
| 1     | 52   | 2     | 36   | 3     | 52   | 4     | 62   |
| 1     | 67   | 2     | 34   | 3     | 55   | 4     | 71   |
| 1     | 54   | 2     | 47   | 3     | 66   | 4     | 41   |
| 1     | 69   | 2     | 125  | 3     | 50   | 4     | 118  |
| 1     | 116  | 2     | 30   | 3     | 58   | 4     | 48   |
| 1     | 79   | 2     | 31   | 3     | 176  | 4     | 82   |
| 1     | 68   | 2     | 30   | 3     | 91   | 4     | 65   |
| 1     | 47   | 2     | 59   | 3     | 66   | 4     | 72   |
| 1     | 120  | 2     | 33   | 3     | 61   | 4     | 49   |
| 1     | 73   | 2     | 98   | 3     |     | 4     | 63   |

## Tareas a realizar:

1. Presente el boxplot de la variable LDL para cada tratamiento (grupo) y comente.
2. Indique si se puede asumir que los datos en cada grupo provienen de una distribución Normal.
3. Dependiendo de la respuesta anterior, realice una prueba adecuada para indicar si es plausible asumir que la varianza es similar en los cuatro grupos.
4. ¿Los cuatro tratamientos proporcionan los mismos valores de LDL? Realice la prueba adecuada con \(\alpha = 0.1\).
5. Si la respuesta a la pregunta del inciso IV es negativa, indique si el tratamiento 2 reduce más los niveles de colesterol en comparación con el resto de tratamientos. Use \(\alpha = 0.1\).
