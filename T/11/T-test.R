
## Ejemplo: pesos de ratones

Supongamos que medimos el **peso de un grupo de ratones** que han sido alimentados con una dieta alta en grasas (puntos negros).  
Queremos saber si el peso de estos ratones **difiere significativamente** del peso promedio conocido de los ratones (línea azul).  

https://shiny-portal.embl.de/shinyapps/app_proxy/e7c7f260-0863-43de-808c-fe9e58010238/images/mouse-weights.png

## Hoja de ruta para el ejemplo

1. **Plantear la hipótesis nula y alternativa**  
  - H₀: los ratones del grupo experimental pesan en promedio lo mismo que el peso conocido  
- H₁: los ratones del grupo experimental pesan diferente  

2. **Elegir un estadístico de prueba**  
  - En la prueba t, este estadístico se llama \(t\)  

3. **Calcular el valor del estadístico** para los datos observados  

4. **Calcular la probabilidad de observar este valor si H₀ fuera verdadera**  
  - En la prueba t, se utiliza la **distribución t**  
  
  5. **Obtener el p-valor** y decidir si **rechazar o no la hipótesis nula**  
  
  ---
  
  ## Peso de los ratones
  
  **Objetivo:** Comparar la media de la muestra con \(\mu_0\)  

- **Hipótesis nula (H₀):** La muestra fue tomada de una población con **peso promedio \(\mu_0\)**  
  - **Hipótesis alternativa (H₁):** La muestra fue tomada de una población donde el **peso promedio es diferente de \(\mu_0\)**
  
  ---
  
  ## Estadístico t
  
  El estadístico t se calcula como:
  
  $$
  t = \frac{\bar{x} - \mu_0}{s / \sqrt{n}}
$$
  
  - \(\bar{x}\): media de la muestra  
- \(\mu_0\): media poblacional bajo H₀  
- \(s\): desviación estándar de la muestra  
- \(n\): tamaño de la muestra  

> t mide la **diferencia entre la media de la muestra y \(\mu_0\)** en unidades del error estándar

---
  
  ## ¿Por qué t es un estadístico útil?
  
  - Si la **diferencia** entre \(\bar{x}\) y \(\mu_0\) es grande en relación con la varianza, hay **más evidencia** contra H₀  
- Un **mayor tamaño de muestra (n)** aumenta t  
- Una **mayor desviación estándar (s)** disminuye t  
- Una **mayor diferencia entre \(\bar{x}\) y \(\mu_0\)** aumenta t (mayor efecto)

En resumen:  
  
  $$
  t = \frac{\bar{x}-\mu_0}{s/\sqrt{n}}
$$
  
  - Si \(|t|\) es grande → más evidencia de que la media poblacional **difere de \(\mu_0\)**
  - \(s\) grande → menor evidencia  
- \(n\) grande → mayor evidencia  
- Diferencia \(\bar{x}-\mu_0\) grande → mayor evidencia

poner imagen balanza

---
  
  ## Estadístico t observado
  
  - Calculamos el estadístico t:
  
  $$
  t = \frac{\bar{x} - \mu_0}{s / \sqrt{n}} = 2.57
$$
  
  - ¿Qué tan probable es observar un valor como 2.57 bajo H₀?
  
  
  library(ggplot2)

# Grados de libertad
df <- 9  # ejemplo, ajustar según la muestra
t_val <- 2.57

# Secuencia de valores t
x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df)

df_t <- data.frame(x=x, y=y)

ggplot(df_t, aes(x=x, y=y)) +
  geom_line() +
  geom_area(data=subset(df_t, x >= t_val), aes(x=x, y=y), fill="red", alpha=0.3) +
  geom_area(data=subset(df_t, x <= -t_val), aes(x=x, y=y), fill="red", alpha=0.3) +
  labs(title="Distribución t", x="t", y="Densidad") +
  geom_vline(xintercept=c(-t_val, t_val), linetype="dashed", color="red") +
  annotate("text", x=c(-t_val, t_val), y=max(y)*0.9, label=c("-2.57", "2.57"), color="red")



---
  ## Distribución t según el Teorema Central del Límite
  
  Para calcular un **p-valor** a partir de t, necesitamos conocer su **probabilidad bajo la hipótesis nula**.  
Es decir, necesitamos saber cómo se comporta t si la dieta **no tiene efecto** y los pesos de los ratones **no difieren realmente de μ₀**.  

Esta distribución se llama **distribución nula**.

---
  
  ## Teorema Central del Límite (TCL)
  
  El TCL establece que **la suma (o promedio) de variables aleatorias independientes tiende a una distribución normal** al aumentar el tamaño de la muestra.

- Mientras más ratones muestreamos, más **normal** será la distribución de la media muestral \(\bar{x}\)  
- El estadístico t es básicamente una media escalada:  
  
  \[
    t = \frac{\bar{x} - \mu_0}{s / \sqrt{n}}
    \]

Por lo tanto, **t tiende a una distribución normal estándar**:
  
  - Media = 0 (bajo H₀, \(\bar{x} \approx \mu_0\))  
- Desviación estándar = 1 (por la escala del error estándar)

---
  
  ## Implicaciones para el p-valor
  
  - Si repetimos el experimento muchas veces, calculando t cada vez, obtendremos **una distribución de t con forma de campana**  
  - Esta distribución nos permite calcular la **probabilidad de observar un t al menos tan extremo como el de nuestra muestra**, es decir, el **p-valor**  
  - Así, el TCL **justifica usar la distribución normal para aproximar la distribución de t**, especialmente para muestras grandes

---
  
  5.1. SINGLE-SAMPLE INFERENCE WITH THE T -DISTRIBUTION 241
5.1.2 Using the ttt-distribution for tests and confidence intervals for a popu-
  lation mean

With the ability to conveniently calculate t? for any sample size or associated α via computing
software, the t-distribution can be used by default over the normal distribution. The rule of thumb
that n > 30 qualifies as a large enough sample size to use the normal distribution dates back to
when it was necessary to rely on distribution tables


---
  
  
  For a sample of size n with sample mean x and standard deviation s, two-sided
confidence intervals with confidence coefficient 100(1 − α)% have the form
x ± t?
  df × SE,
where SE is the standard error of the sample mean (s/√n) and t?
  df is the point on a t-distribution
with n − 1 degrees of freedom and area (1 − α/2) to e left.
A one-sided interval with the same confidence coefficient will have the form
x + t?
  df × SE (one-sided upper confidence interval), or
x − t?
  df × SE (one-sided lower confidence interval),
except that in this case t?
  df is the point on a t-distribution with n − 1 degrees of freedom and area
(1 − α) to its lef


---
  
  Ejemplo

EXAMPLE 5.3
Dolphins are at the top of the oceanic food chain; as a consequence, dangerous substances such
as mercury tend to be present in their organs and muscles at high concentrations. In areas where
dolphins are regularly consumed, it is important to monitor dolphin mercury levels. This example
uses data from a random sample of 19 Risso’s dolphins from the Taiji area in Japan.2 Calculate the
95% confidence interval for average mercury content in Risso’s dolphins from the Taiji area using
the data in Figure 5.6.
The observations are a simple random sample consisting of less than 10% of the population, so
independence of the observations is reasonable. The summary statistics in Figure 5.6 do not suggest
any skew or outliers; all observations are within 2.5 standard deviations of the mean. Based on this
evidence, the approximate normality assumption seems reasonable.
Use the t-distribution to calculate the confidence interval:
  x ± t?
  df × SE = x ± t?
  18 × s/√n
= 4.4 ± 2.10 × 2.3/√19
= (3.29, 5.51) μg/wet g.
The t? point can be read from the t-table on page 239, in the column with area totaling 0.05 in the
two tails (third column) and the row with 18 degrees of freedom. Based on these data, one can be
95% confident the average mercury content of muscles in Risso’s dolphins is between 3.29 and 5.51
μg/wet gram.
Alternatively, the t? point can be calculated in R with the function qt, which returns a value of
2.1009.


n x s minimum maximum
19 4.4 2.3 1.7 9.2


---
  5.1. SINGLE-SAMPLE INFERENCE WITH THE T -DISTRIBUTION 243
GUIDED PRACTICE 5.4
The FDA’s webpage provides some data on mercury content of various fish species.3 From a sample
of 15 white croaker (Pacific), a sample mean and standard deviation were computed as 0.287 and
0.069 ppm (parts per million), respectively. The 15 observations ranged from 0.18 to 0.41 ppm.
Assume that these observations are independent. Based on summary statistics, does the normality
assumption seem reasonable? If so, calculate a 90% confidence interval for the average mercury
content of white croaker (Pacific).4
EXAMPLE 5.5

---
  
  The FDA’s webpage provides some data on mercury content of various fish species.3 From a sample
of 15 white croaker (Pacific), a sample mean and standard deviation were computed as 0.287 and
0.069 ppm (parts per million), respectively. The 15 observations ranged from 0.18 to 0.41 ppm.
Assume that these observations are independent. Based on summary statistics, does the normality
assumption seem reasonable? If so, calculate a 90% confidence interval for the average mercury
content of white croaker (Pacific).4


There are no obvious outliers; all observations are within 2 standard deviations of the mean. If there is skew, it is not
evident. There are no red flags for the normal model based on this (limited) information. x ± t?
  14 × SE → 0.287 ± 1.76 ×
0.0178 → (0.256, 0.318). We are 90% confident that the average mercury content of croaker white fish (Pacific) is between
0.256 and 0.318 ppm


---
  According to the EPA, regulatory action should be taken if fish species are found to have a mercury
level of 0.5 ppm or higher. Conduct a formal significance test to evaluate whether the average
mercury content of croaker white fish (Pacific) is different from 0.50 ppm. Use α = 0.05.
The FDA regulatory guideline is a ‘one-sided’ statement; fish should not be eaten if the mercury
level is larger than a certain value. However, without prior information on whether the mercury in
this species tends to be high or low, it is best to do a two-sided test.
State the hypotheses: H0 : μ = 0.5 vs HA : μ , 0.5. Let α = 0.05.
Calculate the t-statistic:
  t = x − μ0
SE = 0.287 − 0.50
0.069/√15 = −11.96

The probability that the absolute value of a t-statistic with 14 df is smaller than -11.96 is smaller
than 0.01. Thus, p < 0.01. There is evidence to suggest at the α = 0.05 significance level that the
average mercury content of this fish species is lower than 0.50 ppm, since x is less than 0.50.

