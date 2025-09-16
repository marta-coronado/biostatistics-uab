
# 02
## **La gramática de los gráficos**

---
  layout: true
class: animated, fadeIn

---
  .pull-left[<img src="img/Ggplot2_hex_logo.png"></img>]

.pull-right[
  <br>
    <br>
    `ggplot2` es un popular paquete de visualización de datos de código abierto para el **lenguaje de programación R**.
  
  <br>
    Fue desarrollado por **Hadley Wickham** y se basa en la **gramática de gráficos**, que proporciona un marco coherente y sistemático para crear una amplia gama de visualizaciones de datos.
  
  <br><b>
    1. Capas (Layering)
  2. Flexibilidad
  3. Reproducibilidad
  4. Comunidad
]

---
  
  # La gramática de los gráficos
  
  .pull-left[
    #### Gramática original
    <i class="fas fa-book"></i> Wilkinson, Leland. The grammar of graphics. Springer Science & Business Media, 2006.
  ]

.pull-right[
  <center>
    <img src="img/books_ggplot.png" width="80%"></img>
]

--
  
  #### Adaptado a `R` en el paquete `ggplot2`
  
  <i class="fas fa-book"></i> Hadley Wickham. ggplot2: elegant graphics for data analysis. Springer, 2009.

"La gramática nos dice que un **gráfico estadístico** es un **mapeo** de **datos** a **atributos estéticos** (color, forma, tamaño) de **objetos geométricos** (puntos, líneas, barras). El gráfico también puede contener **transformaciones estadísticas** de los datos y se dibuja en un **sistema de coordenadas específico**. Es la **combinación de estos componentes independientes lo que forma un gráfico**."

---
  
  ## Sintaxis 
  
  En `ggplot2` hay diferentes componentes que podemos añadir a un gráfico:
  
  .pull-left[
    ```r
    ggplot(data = <DATA>,
           mapping = aes(<MAPPINGS>)) +
      
      <GEOM_FUNCTION>(stat = <STAT>,
                      position = <POSITION>) +
      
      <SCALE_FUNCTION>() +
      
      <COORDINATE_FUNCTION>() +
      
      <FACET_FUNCTION>() +
      
      <THEME_FUNCTION>
      ```
    
  ]


<img src="img/ggplot2layers2019_8_10D11_28_23.png" width="52%" style="position: absolute; right: 50px; top: 25%;">
  <center>
  <p style="position:absolute; bottom:125px; right:200px;">
  Grammar of Graphics:<br>A layered approach to elegant visuals
</p>
  
  ---
  
  ## Componentes
  
  La gramática por capas define un gráfico como una combinación de:
  
  - __Capas__
+ Datos
+ Mapeo estético
+ Objetos geométricos
+ Transformación estadística
+ Ajuste de posición
- __Escalas__
- __Sistema de coordenadas__
- __Especificación del facetting__
- __Tema__ (no en la gramática original)

<img src="img/ggplot2layers2019_8_10D11_28_23.png" width="52%" style="position: absolute; right: 50px; top: 25%;">
  
  <center>
  <p style="position:absolute; bottom:125px; right:200px;">
  Grammar of Graphics:<br>A layered approach to elegant visuals

---
  ---
  
  ### Datos ordenados (tidy data)
  
  Data frames con una observación por fila y una variable por columna.

<center>
  <img src="img/tidy-1.png" alt="Bokeh plots" width="900"/>
  </center>
  
  ---
  
  ### Datos ordenados (tidy data)
  
  Dos tipos de estructuras de datos ordenados:
  
  * **Formato ancho** (el más común): en un formato ancho, las múltiples medidas de una sola observación se almacenan en una sola fila.

```{r, eval = TRUE, echo = F}
options(width = 300)

wide_df <- data.frame(
  Student = c("A", "B", "C"),
  Math = c(99, 73, 12),
  Literature = c(45, 78, 96),
  PE = c(56, 55, 57)
)
wide_df
```


* **Formato largo**: cada fila corresponde a una medida de una observación.



```{r, eval = TRUE, echo = F}
library(tidyverse)
long_df <- pivot_longer(
  wide_df,
  cols = c(Math, Literature, PE),
  names_to = "Subject",
  values_to = "Score"
)
long_df
```


---
  ### Datos ordenados (tidy data)
  
  Existen funciones útiles para cambiar del formato ancho al formato largo:
  
  ```{r eval = TRUE, echo = TRUE}
# Paquete tidyverse, función pivot_longer
library(tidyverse)
long_df <- pivot_longer(
  wide_df,
  cols = c(Math, Literature, PE), # o  cols = -c(Student),
  names_to = "Subject",
  values_to = "Score"
)
```

  
---
  class: animated, fadeIn
# Visualització amb `ggplot2`

## Sobre `ggplot2`
.pull-left[
  `ggplot2` és un paquet de visualització de dades per a R ([Wickham 2009](https://www.springer.com/gp/book/9780387981413)). Va ser creat per Hadley Wickham, implementant la **Gramàtica de Gràfics** de Leland Wilkinson —un esquema general per a la visualització de dades que descompon els gràfics en components semàntics ([Wilkinson 2010](https://onlinelibrary.wiley.com/doi/full/10.1002/wics.118)).
]

.pull-right[
  ```{r, echo = FALSE, fig.width = 6, fig.height = 4, fig.align = "center"}
  # Simulate data
  set.seed(170513)
  n <- 5000
  d <- data.frame(a = rnorm(n))
  d$b <- .4 * (d$a + rnorm(n))
  
  # Compute first principal component
  d$pc <- predict(prcomp(~a+b, d))[,1]
  
  # Plot
  ggplot(d, aes(a, b, color = pc)) +
    geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = 0.4) +
    theme_minimal() +
    scale_color_gradient(low = "#0091FF", high = "#F0650E") + labs(x = "", y = "") +
    scale_x_continuous(breaks = round(seq(-2,2, by = 1),0))
  ```
  
]

---
  
  ## Com fer boxplots avançats?
  
  
  
  .pull-left[
    
    Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.
    
    ```{r, echo = F, eval = T}
    surv_raw %>% 
      group_by(outcome) %>% 
      summarise(
        n_rows  = n())
    ```
    
    Primer de tot, indiquem les dades (el **dataframe**):
      
      ```{r, echo = T, eval = F}
    ggplot(data = surv_raw)  #<<
    ```
    
  ]

.pull-right[
  ```{r, echo = FALSE, fig.height=6, fig.width=6}
  ggplot(data = surv_raw)
  
  ```
  
]

---
  
  ## Com fer boxplots avançats?
  
  .pull-left[
    
    Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.
    
    Desprès, indiquem les variables que fem servir per l'eix `x` i `y`:


```{r, echo = T, eval = F}
ggplot(data = surv_raw,
       mapping = aes(x = outcome, y = age))  #<<
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw,
       mapping = aes(x = outcome, y = age))  #<<
```

]


---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Indiquem de quin tipus és el gràfic, en aquest cas, farem un boxplot:

```{r, echo = T, eval = F}
ggplot(data = surv_raw,
       mapping = aes(x = outcome, y = age)) +
  geom_boxplot() #<<
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw,
       mapping = aes(x = outcome, y = age)) +
  geom_boxplot() #<<

```

]



---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Podem fer servir la funció `drop_na()` per eliminar els casos NA (no tenim informació de l'edat):
  
  ```{r, echo = T, eval = F}
library(tidyr) #<<

ggplot(data = surv_raw %>% drop_na(outcome), #<<
       mapping = aes(x = outcome, y = age)) +
  geom_boxplot() 
```

  ]

.pull-right[
  ```{r, echo = FALSE, fig.height=6, fig.width=6}
  library(tidyr)
  
  ggplot(data = surv_raw  %>% drop_na(outcome),
         mapping = aes(x = outcome, y = age)) +
    geom_boxplot() 
  ```
  
]


---
  
  ## Com fer boxplots avançats?
  
  .pull-left[
    
    Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.
    
    Amb `fill`, indiquem que volem colorejar segons el grup d'`outcome`.

```{r, echo = T, eval = F}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + #<<
  geom_boxplot() 
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw  %>% drop_na(outcome),
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) +
  geom_boxplot() 
```

]

---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Amb `scale_fill_manual`, indiquem que de manera manual, donarem els colors que volem fer servir:

```{r, echo = T, eval = F}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age, 
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      #<<
    values = c("Death" = "#bf5300",  #<<
              "Recover" = "#11118c")) #<<
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age, 
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      #<<
    values = c("Death" = "#bf5300",  #<<
              "Recover" = "#11118c")) #<<

```

]

---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Podem aplicar un tema amb `theme_*`:

```{r, echo = T, eval = F}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() #<<
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() #<<

```

]

Pots consultar els temes disponibles a: https://ggplot2.tidyverse.org/reference/ggtheme.html

---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Amb `labs()`, indiquem els títols pels eixos i un de general.


```{r, echo = T, eval = F}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",#<<
       title = "Violin plot by outcome") #<<
```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",#<<
       title = "Violin plot by outcome") #<<

```

]

---

## Com fer boxplots avançats?

.pull-left[

Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Amb `theme()`, podem aplicar **qualsevol** modificació al gràfic (avançat!). Per exemple, indicar que no volem la llegenda.

```{r, echo = T, eval = F}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",
       title = "Violin plot by outcome") +
  theme(legend.position = "none") #<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",
       title = "Violin plot by outcome") +
  theme(legend.position = "none") 
```

]

---

## Com fer boxplots avançats?

.pull-left[


Volem representar l'edat (`age`) dels grups d'`outcome` de la nostra taula `surv_raw`.

Podem afegir la significància amb el paquet `ggsignif`. 


```{r, echo = T, eval = F}
library(ggsignif)

ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",
       title = "Violin plot by outcome") +
  theme(legend.position = "none") +
  geom_signif(comparisons = #<<
                list(c("Death", "Recover")),#<<
              test = "wilcox.test")#<<

```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}
ggplot(data = surv_raw %>% drop_na(outcome), 
       mapping = aes(x = outcome, y = age,
                     fill = outcome)) + 
  geom_boxplot() +
    scale_fill_manual(      
    values = c("Death" = "#bf5300",  
              "Recover" = "#11118c")) +
  theme_minimal() +
  labs(y = "Age", x = "Outcome",
       title = "Violin plot by outcome") +
  theme(legend.position = "none") +
  geom_signif(comparisons =
                list(c("Death", "Recover")), #<<
              test = "wilcox.test") #<<
```

]

---


## Com fer un histograma avançat?


.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F}

ggplot(data = surv_raw) #<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw)


```

]

---

## Com fer un histograma avançat?


.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset)) #<<

```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset)) #<<
```

]



---

## Com fer un histograma avançat?



.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual",#<<
                     palette = 1)#<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual",
                     palette = 1) #<<


```

]

Hem utilitzat una paleta de colors pre-existent. Pots consultar-les a: https://ggplot2.tidyverse.org/reference/scale_brewer.html

---

## Com fer un histograma avançat?



.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() #<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() #<<


```

]


---


## Com fer un histograma avançat?


.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")#<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")#<<


```

]


---


## Com fer un histograma avançat?

.pull-left[
Volem fer una corba epidèmica per cada hospital.

```{r, echo = T, eval = F}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Onset date", y = "",#<<
       title = "Epidemic curve")#<<


```

]

.pull-right[
```{r, echo = FALSE, fig.height=6, fig.width=6}

ggplot(data = surv_raw) +
    geom_histogram(aes(x = date_onset,
                       fill = hospital)) +
   scale_fill_brewer(type = "qual", palette = 1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "Onset date", y = "", title = "Epidemic curve")#<<


```

]

layout: false
class: left, bottom, inverse, animated, bounceInDown

# 03
## **Librerías especializadas**


---
layout: true
class: animated, fadeIn
---

# Bibliotecas y software especializados

- Paquetes de software integrados
- Javascript
    + [BioJS](https://biojs.net)
- Bibliotecas de R
    + Repositorios especializados [Bioconductor](https://bioconductor.org)
    + Extensiones de [`ggplot2`](https://exts.ggplot2.tidyverse.org/)
    + [`htmlwidgets`](http://gallery.htmlwidgets.org/), algunos utilizando bibliotecas de BioJS

---

## Estructuras anatómicas: `gganatogram`

https://github.com/jespermaag/gganatogram

```{r echo = F}
# Install from github
# devtools::install_github("jespermaag/gganatogram")
# sample data set
organ_df <- data.frame(organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"), 
 type = c("circulation", "circulation",  "nervous system", "nervous system", "digestion", "digestion", "digestion"), 
 colour = c("red", "red", "purple", "purple", "orange", "orange", "orange"), 
 value = c(10, 5, 1, 8, 2, 5, 5),
 stringsAsFactors=F)
```

```{r echo=TRUE, fig.height=4.7, fig.width=7.8}
library(gganatogram)
gganatogram(data=organ_df, fillOutline='#a6bddb', organism='human',
sex='female', fill="value") +
  scale_fill_gradient(low = "white", high = "red") + 
  facet_wrap(~ type) 
```

---
  
  ## Logos: `ggseqlogo`
  
  https://omarwagih.github.io/ggseqlogo/ 
  
  ```{r, warning=F, echo = F}
# Install from CRAN
# install.packages("ggseqlogo")
## Or install from github
## devtools::install_github("omarwagih/ggseqlogo")

library(ggseqlogo)
# Sample data
data(ggseqlogo_sample)
# str(seqs_dna)
```

```{r echo=TRUE, fig.height=3.5, fig.width=4.5, warning=F, fig.show='hold'}
library(ggseqlogo)
ggplot() + geom_logo(seqs_dna$MA0002.1) +
  theme_logo() + labs(title = "My TFBS profile")
ggplot() +
  annotate(geom = "rect", xmin = 2.5, xmax = 4.5,
           ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_logo(seqs_dna$MA0008.1, method = "probability") +
  theme_logo() 
```

---
  
  ## Estructuras de genes: `gggenes` y `gggenomes`
  
  https://github.com/wilkox/gggenes - 
  https://thackl.github.io/gggenomes/
  
  .pull-left[
    ```{r eval = F}
    library(gggenes)
    ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
      geom_feature(
        data = example_features,
        aes(x = position, y = molecule, forward = forward)
      ) +
      geom_feature_label(
        data = example_features,
        aes(x = position, y = molecule, label = name, forward = forward)
      ) +
      geom_gene_arrow() +
      geom_blank(data = example_dummies) +
      facet_wrap(~ molecule, scales = "free", ncol = 1) +
      scale_fill_brewer(palette = "Set3") +
      theme_genes()
    
    ```
  ]

.pull-right[
  ```{r eval = T, echo = F, fig.height=7,fig.width=8}
  library(gggenes)
  ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
    geom_feature(
      data = example_features,
      aes(x = position, y = molecule, forward = forward)
    ) +
    geom_feature_label(
      data = example_features,
      aes(x = position, y = molecule, label = name, forward = forward)) +
    geom_gene_arrow() +
    geom_blank(data = example_dummies) +
    facet_wrap(~ molecule, scales = "free", ncol = 1) +
    scale_fill_brewer(palette = "Set3") +
    theme_genes()
  
  ```
]

---
  
  # Resumen
  
  - La mayoría de los gráficos exploratorios y de comunicación básicos en biología se pueden lograr con herramientas generales de gráficos estadísticos.
- La complejidad y las características de algunos datos biológicos requieren herramientas especializadas.
+ Si los requisitos son estáticos, las extensiones de `ggplot2` pueden ser útiles.
+ Si los requisitos son interactivos, `htmlwidgets` puede ser útil. 
+ Revisa las herramientas utilizadas en estudios similares.



