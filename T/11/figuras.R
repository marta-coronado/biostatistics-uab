# =============================================================================
# figuras.R  --  Figuras de la sesión "Inferencia estadística" (Bioestadística)
# Se carga desde el Rmd con:  source("R/figuras.R")
# Todas las funciones devuelven un objeto ggplot (o patchwork).
# =============================================================================
library(ggplot2)
library(patchwork)

# ---- Paleta -----------------------------------------------------------------
col_esp  <- "#1B8A1B"  # verde   (curva dieta especial)
col_nor  <- "#C2185B"  # magenta (curva dieta normal)
rel_esp  <- "#4CD44C"  # verde   (relleno de círculos)
rel_nor  <- "#F0459A"  # magenta (relleno de círculos)
col_fill <- "#CFE6FA"  # azul claro (solapamiento / distribución común)
col_h0   <- "#1F5FA8"  # azul (distribución de H0)

# ---- Utilidades internas ------------------------------------------------------

# Eje x dibujado a mano (línea, marcas y etiquetas)
eje_x <- function(ymax, marcas = seq(5, 45, by = 5), etiquetas = marcas, xmax = 50) {
  list(
    annotate("segment", x = 0, xend = xmax, y = 0, yend = 0, linewidth = 0.5),
    annotate("segment", x = marcas, xend = marcas,
             y = 0, yend = -0.05 * ymax, linewidth = 0.5),
    annotate("label", x = etiquetas, y = -0.14 * ymax, label = etiquetas,
             size = 4.5, fill = "white", label.size = 0,
             label.padding = unit(0.15, "lines"))
  )
}

# Títulos de las dos curvas
titulos_curvas <- function(x_esp, x_nor, y, size = 5, hj_esp = 0.5, hj_nor = 0.5) {
  list(
    annotate("text", x = x_esp, y = y, label = "Dieta especial", hjust = hj_esp,
             colour = col_esp, fontface = "bold", size = size),
    annotate("text", x = x_nor, y = y, label = "Dieta normal", hjust = hj_nor,
             colour = col_nor, fontface = "bold", size = size)
  )
}

# Observaciones: flechas punteadas hasta el eje + círculos
# obs: data.frame con columnas x y grupo ("esp" / "nor")
capa_obs <- function(obs, ymax) {
  y_circ <- -0.42 * ymax
  list(
    geom_segment(data = obs,
                 aes(x = x, xend = x, y = y_circ + 0.09 * ymax, yend = -0.1 * ymax),
                 linetype = "dotted", linewidth = 1),
    geom_segment(data = obs,
                 aes(x = x, xend = x, y = -0.09 * ymax, yend = -0.015 * ymax),
                 arrow = arrow(length = unit(0.22, "cm"), type = "closed"),
                 linewidth = 0.8),
    geom_point(data = obs, aes(x = x, y = y_circ, fill = grupo),
               shape = 21, size = 6, stroke = 1, colour = "grey20"),
    scale_fill_manual(values = c(esp = rel_esp, nor = rel_nor), guide = "none")
  )
}

# Curva normal solo en su media +- 4 sd
curva_normal <- function(mu, sd, x = seq(0, 50, length.out = 1000)) {
  d <- data.frame(x = x, y = dnorm(x, mu, sd))
  subset(d, x >= mu - 4 * sd & x <= mu + 4 * sd)
}

tema_curvas <- function() {
  list(scale_x_continuous(limits = c(0, 50), expand = c(0, 0)),
       theme_void(base_size = 14),
       theme(plot.margin = margin(10, 10, 10, 10)))
}

# ---- 1. Dos curvas bien separadas -------------------------------------------
# obs = NULL: solo curvas; con obs: añade las observaciones bajo el eje
fig_separadas <- function(obs = NULL, mu_esp = 14, mu_nor = 36, sd = 2.5) {
  ymax <- dnorm(0, sd = sd)
  ylim <- if (is.null(obs)) c(-0.25 * ymax, 1.2 * ymax) else c(-0.62 * ymax, 1.2 * ymax)

  p <- ggplot() +
    geom_line(data = curva_normal(mu_esp, sd), aes(x, y),
              colour = col_esp, linewidth = 1.6, lineend = "round") +
    geom_line(data = curva_normal(mu_nor, sd), aes(x, y),
              colour = col_nor, linewidth = 1.6, lineend = "round") +
    titulos_curvas(mu_esp, mu_nor, ymax * 1.08)
  if (!is.null(obs)) p <- p + capa_obs(obs, ymax)
  p + eje_x(ymax) + coord_cartesian(ylim = ylim, expand = FALSE) + tema_curvas()
}

# ---- 2. Curvas separadas + distribución única (H0) --------------------------
fig_h0 <- function(mu_esp = 14, mu_nor = 36, sd = 2.5, mu_all = 25, sd_all = 4.5) {
  x  <- seq(0, 50, length.out = 1000)
  ymax <- dnorm(0, sd = sd)
  # La curva azul se reescala para que su pico iguale al de las otras dos
  all <- data.frame(x = x, y = dnorm(x, mu_all, sd_all) * (sd_all / sd))
  all <- subset(all, abs(x - mu_all) <= 3.2 * sd_all)

  ggplot() +
    geom_line(data = curva_normal(mu_esp, sd), aes(x, y),
              colour = col_esp, linewidth = 1.6, lineend = "round") +
    geom_line(data = curva_normal(mu_nor, sd), aes(x, y),
              colour = col_nor, linewidth = 1.6, lineend = "round") +
    geom_line(data = all, aes(x, y), colour = col_h0, linewidth = 1.6, lineend = "round") +
    titulos_curvas(mu_esp, mu_nor, ymax * 1.08) +
    annotate("text", x = mu_all, y = ymax * 1.08,
             label = "Una sola distribución (H0)",
             colour = col_h0, fontface = "bold", size = 5) +
    eje_x(ymax) +
    coord_cartesian(ylim = c(-0.25 * ymax, 1.2 * ymax), expand = FALSE) +
    tema_curvas()
}

# ---- 3. Curvas que solapan --------------------------------------------------
# Sirve para: solape sin observaciones, solape con observaciones,
# ejemplo de d = 1.5 (mu 20 / 30, sd 6.5) y de d = 0.1 (mu 24.8 / 25.2, sd 4)
fig_solape <- function(obs = NULL, mu_esp = 24, mu_nor = 26.5, sd = 3.5,
                       etiquetas = seq(5, 45, by = 5)) {
  x  <- seq(0, 50, length.out = 1000)
  ymax <- dnorm(0, sd = sd)
  df <- data.frame(x = x, a = dnorm(x, mu_esp, sd), b = dnorm(x, mu_nor, sd))
  df$solape <- pmin(df$a, df$b)
  ylim <- if (is.null(obs)) c(-0.25 * ymax, 1.25 * ymax) else c(-0.62 * ymax, 1.25 * ymax)

  p <- ggplot(df, aes(x)) +
    geom_ribbon(aes(ymin = 0, ymax = solape), fill = col_fill) +
    geom_line(data = curva_normal(mu_esp, sd), aes(x, y), colour = col_esp,
              linewidth = 1.6, lineend = "round") +
    geom_line(data = curva_normal(mu_nor, sd), aes(x, y), colour = col_nor,
              linewidth = 1.6, lineend = "round") +
    titulos_curvas(mu_esp - 1, mu_nor + 1, ymax * 1.1, hj_esp = 1, hj_nor = 0)
  if (!is.null(obs)) p <- p + capa_obs(obs, ymax)
  p + eje_x(ymax, etiquetas = etiquetas) + coord_cartesian(xlim = c(0, 50), ylim = ylim, expand = FALSE) + tema_curvas()
}

# ---- 4. Una única distribución común (no hay diferencia real) ---------------
fig_comun <- function(obs, mu = 25, sd = 3) {
  x  <- seq(11, 39, length.out = 500)
  df <- data.frame(x = x, dens = dnorm(x, mu, sd))
  ymax <- dnorm(mu, mu, sd)

  ggplot(df, aes(x)) +
    geom_area(aes(y = dens), fill = col_fill) +
    geom_line(aes(y = dens), colour = col_esp, linewidth = 1.6) +
    geom_line(aes(y = dens), colour = col_nor, linewidth = 1.6, alpha = 0.6) +
    annotate("text", x = mu - 5, y = ymax * 1.18, label = "Dieta especial",
             colour = col_esp, fontface = "bold", size = 5) +
    annotate("text", x = mu + 5, y = ymax * 1.18, label = "Dieta normal",
             colour = col_nor, fontface = "bold", size = 5) +
    annotate("segment", x = mu - 4, xend = mu - 2.2, y = ymax * 1.08,
             yend = dnorm(mu - 2.2, mu, sd) + 0.02 * ymax,
             arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 1) +
    annotate("segment", x = mu + 4, xend = mu + 2.2, y = ymax * 1.08,
             yend = dnorm(mu + 2.2, mu, sd) + 0.02 * ymax,
             arrow = arrow(length = unit(0.25, "cm"), type = "closed"), linewidth = 1) +
    capa_obs(obs, ymax) +
    eje_x(ymax) +
    coord_cartesian(ylim = c(-0.62 * ymax, 1.3 * ymax), expand = FALSE) +
    tema_curvas()
}

# ---- 5. Muestra pequeña: puntos + medias + p-valor calculado ----------------
# El p-valor se calcula con una t de Student (varianzas iguales), así la
# etiqueta siempre coincide con los datos que se dibujan.
fig_muestra <- function(y_esp, y_nor) {
  p <- t.test(y_esp, y_nor, var.equal = TRUE)$p.value
  p_txt <- paste0("p-value = ", formatC(p, format = "f", digits = ifelse(p < 0.01, 4, 2)))

  datos  <- data.frame(grupo = factor(rep(c("esp", "nor"), each = 3),
                                      levels = c("esp", "nor")),
                       y = c(y_esp, y_nor))
  medias <- aggregate(y ~ grupo, data = datos, FUN = mean)

  ggplot(datos, aes(x = grupo, y = y)) +
    geom_segment(data = medias,
                 aes(x = as.numeric(grupo) - 0.35, xend = as.numeric(grupo) + 0.35,
                     y = y, yend = y, colour = grupo), linewidth = 1.6) +
    geom_point(aes(fill = grupo), shape = 21, size = 6, stroke = 1.1, colour = "grey20") +
    scale_fill_manual(values   = c(esp = rel_esp, nor = rel_nor)) +
    scale_colour_manual(values = c(esp = col_esp, nor = col_nor)) +
    scale_y_continuous(limits = c(0, 8.5), breaks = c(1, 3, 5, 7), expand = c(0, 0)) +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    labs(x = p_txt) +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          axis.ticks  = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 16, margin = margin(t = 8)))
}

# ---- 6. Efecto del tamaño de muestra (3 paneles apilados) -------------------
# Cada marca es la media de una muestra de tamaño n; se repite k veces.
fig_n <- function(n_vals = c(1, 2, 10), mu_esp = 24, mu_nor = 26.5, sd = 3.5,
                  k = 12, semilla = 123) {
  set.seed(semilla)
  z_esp <- rnorm(k)
  z_nor <- rnorm(k)
  ymax  <- dnorm(0, sd = sd)
  c_esp <- curva_normal(mu_esp, sd)
  c_nor <- curva_normal(mu_nor, sd)

  panel <- function(n) {
    m_esp <- data.frame(x = mu_esp + z_esp * sd / sqrt(n))
    m_nor <- data.frame(x = mu_nor + z_nor * sd / sqrt(n))
    ggplot() +
      geom_line(data = c_esp, aes(x, y), colour = col_esp, linewidth = 1.6, lineend = "round") +
      geom_line(data = c_nor, aes(x, y), colour = col_nor, linewidth = 1.6, lineend = "round") +
      geom_segment(data = m_nor, aes(x = x, xend = x),
                   y = 0.03 * ymax, yend = 0.4 * ymax,
                   colour = col_nor, alpha = 0.45, linewidth = 2.5) +
      geom_segment(data = m_esp, aes(x = x, xend = x),
                   y = -0.03 * ymax, yend = -0.4 * ymax,
                   colour = col_esp, alpha = 0.45, linewidth = 2.5) +
      annotate("segment", x = 0, xend = 50, y = 0, yend = 0, linewidth = 0.6) +
      annotate("segment", x = seq(5, 45, by = 5), xend = seq(5, 45, by = 5),
               y = 0, yend = -0.06 * ymax, linewidth = 0.6) +
      annotate("text", x = 3, y = ymax * 1.05, hjust = 0,
               label = paste0("Tamaño muestral = ", n),
               fontface = "bold", size = 5.5) +
      coord_cartesian(xlim = c(0, 50), ylim = c(-0.55 * ymax, 1.2 * ymax), expand = FALSE) +
      theme_void(base_size = 14) +
      theme(plot.margin = margin(4, 10, 4, 10))
  }
  wrap_plots(lapply(n_vals, panel), ncol = 1)
}

# ---- 7. Potencia en función de n (por grupo) --------------------------------
fig_potencia_n <- function(d = 1.5, n_max = 20, alpha = 0.05, objetivo = 0.8) {
  n   <- 2:n_max
  pot <- sapply(n, function(k) power.t.test(n = k, delta = d, sd = 1, sig.level = alpha)$power)
  df  <- data.frame(n = n, potencia = pot)
  n_req <- ceiling(power.t.test(delta = d, sd = 1, sig.level = alpha, power = objetivo)$n)

  ggplot(df, aes(n, potencia)) +
    geom_hline(yintercept = objetivo, linetype = "dashed", colour = "grey40") +
    geom_line(colour = col_h0, linewidth = 1.4) +
    geom_point(colour = col_h0, size = 2.5) +
    geom_point(data = df[df$n == n_req, ], colour = col_nor, size = 5) +
    annotate("text", x = n_req + 0.6, y = df$potencia[df$n == n_req] - 0.09,
             label = paste0("n = ", n_req, " por grupo"),
             colour = col_nor, fontface = "bold", hjust = 0, size = 5) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.8, 1)) +
    labs(x = "Tamaño de muestra por grupo (n)", y = "Potencia",
         title = paste0("d = ", d, ", α = ", alpha)) +
    theme_classic(base_size = 14)
}
