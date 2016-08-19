# Taller de Regresión Lineal Simple

## Los datos
```{r}
#install.packages(ISwR)
library(ISwR)
data(bp.obese)
#?bp.obese
```


## Objetivo 
Estimar un modelo de regresi?n para estudiar la dependencia  de la presi?n sist?lica en funci?n del cociente entre del peso y el peso ideal seg?n las tablas de vida.

## Preliminares

### Resumen de los datos

```{r}
summary(bp.obese)
```

Responder:
1. ?Qu? porcentaje de mujeres hay en la muestra?

```{r}
summary(bp.obese$sex)
# El 56.86% de la muestra son mujeres.
```

2. ?Se puede decir que hay m?s personas con peso superior al ideal que personas con peso inferior?.

```{r}
summary(bp.obese$obese)
# Al menos el 75% de las personas tienen un peso superior al ideal.
nrow(bp.obese[bp.obese$obese > 1, ])
# 96 de 102 personas tiene un peso superior al ideal.
```

3. ?Qu? puede decir de la distribuci?n  de la presi?n sist?lica?
```{r}
summary(bp.obese$bp)
hist(bp.obese$bp)
boxplot(bp.obese$bp)
# La presión sistólica presenta una distribución con una ligera desviación a la derecha.
```

### Diagrama de dispersi?n

```{r,fig=TRUE}
plot(bp~obese,pch = ifelse(sex==1, "F", "M"), data = bp.obese)
```

4. ?Qu? observa en el diagrama de dispersi?n?
A pesar de que hay unos valores extremos, parece existir una relación lineal entre las dos variables.

El mismo diagrama utilizando colores en lugar de s?mbolos

```{r}
plot(bp~obese,col = ifelse(sex==1, "brown", "darkgreen"), data = bp.obese,las=1,pch=20)
```

Diagrama de dispersi?n para hombres

```{r,fig=TRUE}
plot(bp~obese, col="darkgreen", data = bp.obese[bp.obese$sex==0,],las=1,pch=20)
# diagrama de dispersi?n para hombres
```

Grafique el diagrama de dispersi?n para mujeres

```{r,fig=TRUE}
plot(bp~obese, col="darkgreen", data = bp.obese[bp.obese$sex==1,],las=1,pch=20)
# diagrama de dispersi?n para mujeres
```

5. ?Estimaria un modelo de regresi?n para cada g?nero?. ?Por que?
Si, porque la fuerza de la asociacion lineal entre los grupos parece no ser la misma; adicionalmente, la magnitud de la relacion entre x y y parece ser diferente.

## Estimaci?n del modelo de regresi?n para ambos g?neros

```{r}
reg1 <- lm(bp~obese,data = bp.obese)
reg1
```

6. Escriba la estimaci?n puntual de $\beta_0$
96.82

7. Escriba la estimaci?n puntual de $\beta_1$
23.00

```{r}
summary(reg1)
```

8. ?El modelo de regresi?n muestra dependencia lineal entre la presi?n sist?lica?  y obesidad?Por qu??.
Si, por cada aumento en una unidad en el indice de obesidad aumenta la presion sistolica en 23.001 unidades; despues de tener en cuenta la variabilidad del muestreo, este incremento puede ir de 9.77 a 36.23   

9. ?Se puede decir que la pendiente de la recta poblacional es diferente de cero?. ?Por qu??.
Si, porque el valor p es inferior al punto de corte standard de 0.05; por lo tanto, puedo decir que el valor del coeficiente beta1 es diferente de cero. 

10. ?Qu? porcentaje de la varianza explica el modelo de regresi?n?.
El 10.64% del variabilidad es explicada por el modelo.

```{r}
anova(reg1)
```

- Dibuje la distribuci?n $t$ asociada a las pruebas de hip?tesis, con las regi?n de rechazo para un $\alpha=5\%$.

```{r}
curve (dt(x, 100), xlim = c(-5, 5), las = 1)
abline(v = c(-1.983972, 1.983972), col = "green")
```

- Superponga la distribuci?n normal est?ndar sobre la gr?fica con otro color.
```{r}
curve(dnorm(x), add = TRUE, col= "orange")
```

- Se?ale en la gr?fica la estimaci?n de $t$ para probar que $\beta_1 \neq 0$
```{r}
abline(v = 3.45, col = "blue")
```

```{r,fig=TRUE}
# gr?fica de t con 100 grados de libertad
curve (dt(x, 100), xlim = c(-5, 5), las = 1)
abline(v = c(-1.983972, 1.983972), col = "green")
curve(dnorm(x), add = TRUE, col= "orange")
abline(v = 3.45, col = "blue")
```

- Dibuje de nuevo el diagrama de dipersi?n y superponga la recta de regresi?n estimada.

```{r,fig=TRUE}
# diagrama de dispersi?n y recta ajustada
plot(bp~obese,col = ifelse(sex==1, "brown", "darkgreen"), data = bp.obese,las=1,pch=20)
abline(reg1, col = "darkblue")
```

## Diagn?sticos del modelo

Interprete las gr?ficas obtenidas con la funci?n plot.lm, recurriendo a la ayuda de la funci?n y al cap?tulo 4 del texto.

```{r}
# Obtener la ayuda de la funcion plot.lm
?plot.lm
```

```{r}
# Obtener las cuatro graficas c(1:3, 5)
plot(reg1)
```
