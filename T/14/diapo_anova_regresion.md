---
class: animated, fadeIn

## Del ANOVA a la regresión

En la regresión (**próximo tema**) la idea es la misma: **observación = predicción + residuo**. Lo que cambia es la predicción: en vez de una media por grupo, una **recta**.

```{r, echo = F, fig.width = 12, fig.height = 4.6}
p2 <- ggplot(bird, aes(x = log(Mass), y = log(MaxAbund))) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#DE1144", linewidth = 1.3) +
  labs(title = "Regresión: x numérica",
       subtitle = "El modelo predice una recta",
       x = "log(Masa corporal)", y = NULL) +
  theme_classic(base_size = 15)
p1 + labs(title = "ANOVA: x categórica", subtitle = "El modelo predice una media por grupo") + p2
```

> En `R`, los dos se ajustan con la misma función: `lm(y ~ x)`

