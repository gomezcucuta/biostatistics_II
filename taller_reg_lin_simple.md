# Taller de Regresión Lineal Simple

## Los datos
```{r}
install.packages("ISwR")
library(ISwR)
data(bp.obese)
?bp.obese
```

## Objetivo 
Estimar un modelo de regresión para estudiar la dependencia  de la presión sistólica en función del cociente entre del peso y el peso ideal según las tablas de vida.

## Preliminares

### Resumen de los datos

```{r}
summary(bp.obese)
```

Responder:
1 Qué porcentaje de mujeres hay en la muestra?

```{r}
summary(bp.obese$sex)
# El 56.86% de la muestra son mujeres.
```

2 Se puede decir que hay más personas con peso superior al ideal que personas con peso inferior?

```{r}
summary(bp.obese$obese)
# Al menos el 75% de las personas tienen un peso superior al ideal.

nrow(bp.obese[bp.obese$obese > 1, ])
# 96 de 102 personas tiene un peso superior al ideal.
```

3 Qué puede decir de la distribución  de la presión sistólica?

```{r}
summary(bp.obese$bp)
hist(bp.obese$bp)
boxplot(bp.obese$bp)
# La presión sistólica presenta una distribución con una ligera desviación a la derecha.
```

### Diagrama de dispersión

```{r, fig=TRUE}
plot(bp~obese,pch = ifelse(sex==1, "F", "M"), data = bp.obese)
```

4 Qué observa en el diagrama de dispersión?

A pesar de que hay unos valores extremos, parece existir una relación lineal entre las dos variables.


### El mismo diagrama utilizando colores en lugar de símbolos

```{r, fig=TRUE}
plot(bp~obese,col = ifelse(sex==1, "brown", "darkgreen"), data = bp.obese,las=1,pch=20)
```

### Diagrama de dispersión para hombres

```{r,fig=TRUE}
plot(bp~obese, col="darkgreen", data = bp.obese[bp.obese$sex==0,],las=1,pch=20)
```

### Diagrama de dispersión para mujeres

```{r,fig=TRUE}
plot(bp~obese, col="darkgreen", data = bp.obese[bp.obese$sex==1,],las=1,pch=20)
```

5 Estimaría un modelo de regresión para cada género?  Por qué?

Sí, porque la fuerza de la asociacion lineal entre los grupos parece no ser la misma; adicionalmente, la magnitud de la relación entre x y y parece ser diferente.

## Estimación del modelo de regresión para ambos géneros

```{r}
reg1 <- lm(bp~obese,data = bp.obese)
reg1
```

6 Escriba la estimación puntual de beta_0

96.82

7 Escriba la estimación puntual de beta_1
23.00

```{r}
summary(reg1)
```

8 El modelo de regresión muestra dependencia lineal entre la presión sistólica y obesidad?  Por qué?

Sí, por cada aumento en una unidad en el índice de obesidad aumenta la presión sistólica en 23.001 unidades; después de tener en cuenta la variabilidad del muestreo, este incremento puede ir de 9.77 a 36.23

9 Se puede decir que la pendiente de la recta poblacional es diferente de cero?  Por qué?

Sí, porque el valor p es inferior al punto de corte standard de 0.05; por lo tanto, puedo decir que el valor del coeficiente beta1 es diferente de cero. 

10 Qué porcentaje de la varianza explica el modelo de regresión?
El 10.64% del variabilidad es explicada por el modelo.

```{r}
anova(reg1)
```

* Dibuje la distribución t asociada a las pruebas de hipótesis junto con las regiones de rechazo para un alfa del 5%.

```{r}
curve (dt(x, 100), xlim = c(-5, 5), las = 1)
abline(v = c(-1.983972, 1.983972), col = "green")
```

* Superponga la distribución normal estándar sobre la gráfica con otro color.

```{r}
curve(dnorm(x), add = TRUE, col= "orange")
```

* Señale en la gráfica la estimación de t para probar que beta_1 es diferente de 0.

```{r}
abline(v = 3.45, col = "blue")
```
* Para que sea todo después de compilado en word:

```{r,fig=TRUE}
# Gráfica de t con 100 grados de libertad
curve (dt(x, 100), xlim = c(-5, 5), las = 1)
abline(v = c(-1.983972, 1.983972), col = "green")
curve(dnorm(x), add = TRUE, col= "orange")
abline(v = 3.45, col = "blue")
```

* Dibuje de nuevo el diagrama de dipersión y superponga la recta de regresión estimada.

```{r,fig=TRUE}
# diagrama de dispersión y recta ajustada
plot(bp~obese,col = ifelse(sex==1, "brown", "darkgreen"), data = bp.obese,las=1,pch=20)
abline(reg1, col = "darkblue")
```

## Diagnósticos del modelo

* Interprete las gráficas obtenidas con la función plot.lm recurriendo a la ayuda de la función y al capítulo 4 del texto.

```{r}
# Obtener la ayuda de la función plot.lm
?plot.lm
```

```{r}
# Obtener las cuatro gráficas c(1:3, 5)
plot(reg1)
```
