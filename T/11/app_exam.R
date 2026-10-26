library(shiny)
library(ggplot2)

# ------------------------------------------------------------------
# Juego: ¿cómo cambian el efecto, la desviación y n la potencia?
#
# Diseño: dos grupos independientes, t-test bilateral, n por grupo.
# Todos los cálculos usan power.t.test(), igual que en las diapositivas.
#
# Las funciones de los gráficos están fuera del server para poder
# reutilizarlas (por ejemplo, para generar las figuras del examen).
# Con bn = TRUE los gráficos no dependen del color (para imprimir).
# ------------------------------------------------------------------

# Colores (los mismos que en las diapositivas)
col_ctrl  <- "#C2185B"  # magenta: grupo control
col_trat  <- "#1B8A1B"  # verde:   grupo tratamiento
col_h0    <- "#2C7BB6"
col_ha    <- "#D95F02"
col_alpha <- "#D7191C"
col_beta  <- "#1A9641"
col_power <- "#1c5253"

# Grises para la versión en blanco y negro
gris_alpha <- "black"
gris_beta  <- "grey82"
gris_power <- "grey50"

# Potencia de un t-test de dos muestras (bilateral) con n por grupo
potencia <- function(n, delta, sigma, alpha) {
  power.t.test(n = n, delta = delta, sd = sigma, sig.level = alpha,
               strict = TRUE)$power
}

# n por grupo necesario para alcanzar la potencia objetivo (redondeado hacia arriba)
n_necesaria <- function(delta, sigma, alpha, objetivo) {
  if (delta == 0) return(NA)
  if (potencia(2, delta, sigma, alpha) >= objetivo) return(2)
  n <- tryCatch(power.t.test(delta = delta, sd = sigma, sig.level = alpha,
                             power = objetivo, strict = TRUE)$n,
                error = function(e) NA)
  ceiling(n)
}

categoria_d <- function(d) {
  if (d < 0.2) "trivial" else if (d < 0.5) "pequeño" else if (d < 0.8) "medio" else "grande"
}

# Mismo eje x en los dos gráficos: depende solo de Δ y σ (no de n),
# así al cambiar n se ve cómo se estrechan las curvas de abajo
eje_x <- function(delta, sigma) c(-4 * sigma, delta + 4 * sigma)

# ---- Gráfico 1: poblaciones ------------------------------------------
grafico_poblaciones <- function(delta, sigma, bn = FALSE, base_size = 15) {
  eje <- eje_x(delta, sigma)
  x <- seq(eje[1], eje[2], length.out = 600)
  df <- rbind(
    data.frame(x = x, y = dnorm(x, 0, sigma),     grupo = "Control"),
    data.frame(x = x, y = dnorm(x, delta, sigma), grupo = "Tratamiento")
  )
  g <- ggplot(df, aes(x, y, colour = grupo, fill = grupo, linetype = grupo)) +
    geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 1.3)
  if (bn) {
    g <- g +
      geom_vline(xintercept = c(0, delta), linetype = c("solid", "22"),
                 colour = "grey40", linewidth = 0.5) +
      scale_colour_manual(values = c(Control = "black", Tratamiento = "black"), name = NULL) +
      scale_fill_manual(values = c(Control = "grey60", Tratamiento = "grey60"), name = NULL) +
      scale_linetype_manual(values = c(Control = "solid", Tratamiento = "22"), name = NULL)
  } else {
    g <- g +
      geom_vline(xintercept = c(0, delta), colour = c(col_ctrl, col_trat),
                 linetype = "dashed") +
      scale_colour_manual(values = c(Control = col_ctrl, Tratamiento = col_trat), name = NULL) +
      scale_fill_manual(values = c(Control = col_ctrl, Tratamiento = col_trat), name = NULL) +
      scale_linetype_manual(values = c(Control = "solid", Tratamiento = "solid"),
                            name = NULL, guide = "none")
  }
  g +
    labs(x = "Valor de la variable", y = "Densidad",
         subtitle = sprintf("Δ = %.1f   σ = %.1f   →   d = %.2f", delta, sigma, delta / sigma)) +
    coord_cartesian(xlim = eje) +
    scale_y_continuous(labels = function(y) sprintf("%.2f", y)) +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "top", legend.key.width = unit(2.5, "lines"),
          panel.grid.minor = element_blank())
}

# ---- Gráfico 2: distribución de la diferencia de medias ---------------
# En blanco y negro, cada región sombreada lleva una etiqueta
# (etiquetas = textos para α, β y potencia; p. ej. c("1", "2", "3") en un examen)
grafico_muestral <- function(delta, sigma, n, alpha, bn = FALSE, base_size = 15,
                             etiquetas = c("α", "β", "1 − β"), xlim = NULL) {
  eje  <- eje_x(delta, sigma)
  if (is.null(xlim)) xlim <- eje            # por defecto, el mismo eje que las poblaciones
  df_t <- 2 * n - 2
  se   <- sigma * sqrt(2 / n)               # error estándar de x̄1 − x̄2
  ncp  <- delta / se
  crit <- qt(1 - alpha / 2, df_t) * se      # valor crítico en unidades originales

  lim <- max(abs(eje), crit + se)           # calculamos un poco más allá del eje visible
  x  <- seq(-lim, lim, length.out = 4000)
  df <- data.frame(x = x,
                   h0 = dt(x / se, df_t) / se,
                   ha = suppressWarnings(dt(x / se, df_t, ncp = ncp)) / se)
  rechazo <- abs(df$x) >= crit
  df$alpha_y <- ifelse(rechazo, df$h0, 0)
  df$power_y <- ifelse(rechazo, df$ha, 0)
  df$beta_y  <- ifelse(rechazo, 0, df$ha)

  nombres <- c("Error tipo I (α)", "Error tipo II (β)", "Potencia (1 − β)")
  rellenos <- if (bn) c(gris_alpha, gris_beta, gris_power) else c(col_alpha, col_beta, col_power)
  names(rellenos) <- nombres
  lineas <- c("Bajo H0 (no hay diferencia)", "Bajo HA (diferencia = Δ)")

  g <- ggplot(df, aes(x)) +
    geom_ribbon(aes(ymin = 0, ymax = power_y, fill = nombres[3]), alpha = if (bn) 0.9 else 0.55) +
    geom_ribbon(aes(ymin = 0, ymax = beta_y,  fill = nombres[2]), alpha = if (bn) 0.9 else 0.45) +
    geom_ribbon(aes(ymin = 0, ymax = alpha_y, fill = nombres[1]), alpha = if (bn) 1 else 0.8) +
    scale_fill_manual(values = rellenos, name = NULL)

  if (bn) {
    g <- g +
      geom_line(aes(y = h0, linetype = lineas[1]), linewidth = 1.1) +
      geom_line(aes(y = ha, linetype = lineas[2]), linewidth = 1.1) +
      scale_linetype_manual(values = setNames(c("solid", "22"), lineas), name = NULL) +
      geom_vline(xintercept = c(-crit, crit), linetype = "dotted", linewidth = 0.8) +
      guides(fill = "none")
    nota_crit <- "punteadas"

    # Etiquetas de las regiones: cada etiqueta apunta al centro de su región
    centro <- function(y) {
      if (sum(y) == 0) return(c(NA, NA))
      xc <- sum(df$x * y) / sum(y)
      c(xc, approx(df$x, y, xc)$y / 2)
    }
    pico <- max(df$h0, df$ha)
    ancho <- diff(xlim)
    c_beta   <- centro(df$beta_y)
    c_power  <- centro(df$power_y)
    c_alpha  <- centro(ifelse(df$x < 0, df$alpha_y, 0))      # cola izquierda
    x_a2     <- crit + 0.3 * se                                # cola derecha
    c_alpha2 <- c(x_a2, dt(x_a2 / se, df_t) / se / 2)
    et <- data.frame(
      etiqueta = etiquetas,
      xl = c(c_alpha[1] - 0.14 * ancho, c_beta[1] - 0.02 * ancho, c_power[1] + 0.14 * ancho),
      yl = pico * 1.30,
      xp = c(c_alpha[1], c_beta[1], c_power[1]),
      yp = c(c_alpha[2], c_beta[2], c_power[2]))
    et <- rbind(et, data.frame(etiqueta = etiquetas[1], xl = et$xl[1], yl = et$yl[1],
                               xp = c_alpha2[1], yp = c_alpha2[2]))
    et <- et[!is.na(et$xp), ]
    g <- g +
      geom_segment(data = et, aes(x = xl, y = yl, xend = xp, yend = yp),
                   linewidth = 0.4, colour = "black") +
      geom_label(data = et[!duplicated(et$etiqueta), ], aes(x = xl, y = yl, label = etiqueta),
                 size = base_size / 3, fontface = "bold", label.size = 0.4,
                 label.padding = unit(0.25, "lines"), fill = "white") +
      expand_limits(y = pico * 1.42)
  } else {
    g <- g +
      geom_line(aes(y = h0, colour = lineas[1]), linewidth = 1.2) +
      geom_line(aes(y = ha, colour = lineas[2]), linewidth = 1.2) +
      scale_colour_manual(values = setNames(c(col_h0, col_ha), lineas), name = NULL) +
      geom_vline(xintercept = c(-crit, crit), linetype = "dashed")
    nota_crit <- "líneas discontinuas"
  }

  g +
    labs(x = expression(bar(x)[tratamiento] - bar(x)[control]), y = "Densidad",
         subtitle = sprintf("EE = σ·√(2/n) = %.2f%srechazamos H0 si |diferencia| > %.2f (%s)",
                            se, if (bn) "\n" else "   ·   ", crit, nota_crit)) +
    coord_cartesian(xlim = xlim) +
    scale_y_continuous(labels = function(y) sprintf("%.2f", y)) +
    theme_minimal(base_size = base_size) +
    theme(legend.position = "top", legend.box = "vertical",
          legend.key.width = unit(2.5, "lines"),
          panel.grid.minor = element_blank())
}

ui <- fluidPage(
  titlePanel("Distribuciones poblacionales vs. muestrales: ¿cómo cambian el efecto, la desviación y n la potencia?"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("delta", "Diferencia de medias Δ:",
                  min = 0, max = 5, value = 1, step = 0.1),
      sliderInput("sigma", "Desviación estándar σ:",
                  min = 0.5, max = 5, value = 2, step = 0.1),
      sliderInput("n", "Tamaño de muestra n (por grupo):",
                  min = 2, max = 100, value = 10, step = 1),
      radioButtons("alpha", "Nivel de significación α:",
                   choices = c("0.01" = 0.01, "0.05" = 0.05, "0.10" = 0.10),
                   selected = 0.05, inline = TRUE),
      sliderInput("objetivo", "Potencia objetivo:",
                  min = 0.5, max = 0.99, value = 0.8, step = 0.01),
      checkboxInput("bn", "Blanco y negro (para imprimir)", value = FALSE),
      hr(),
      helpText("Dos grupos independientes, t-test bilateral.",
               "Los cálculos son los de power.t.test().")
    ),

    mainPanel(
      h3("Distribuciones poblacionales"),
      plotOutput("popPlot", height = "260px"),
      h3("Distribuciones muestrales"),
      p("Distribución de la diferencia de medias si repitiéramos el experimento muchas veces."),
      plotOutput("samplePlot", height = "340px"),
      uiOutput("resumen")
    )
  )
)

server <- function(input, output, session) {

  alpha <- reactive(as.numeric(input$alpha))
  d     <- reactive(input$delta / input$sigma)
  pot   <- reactive(potencia(input$n, input$delta, input$sigma, alpha()))
  n_obj <- reactive(n_necesaria(input$delta, input$sigma, alpha(), input$objetivo))

  # ---- Resumen numérico --------------------------------------------
  output$resumen <- renderUI({
    texto_n <- if (is.na(n_obj())) {
      "Si Δ = 0 no hay diferencia real: la potencia no aplica (solo rechazamos H0 por error, con probabilidad α)."
    } else {
      sprintf("Para una potencia de %.2f necesitas <b>n = %d por grupo</b>.",
              input$objetivo, n_obj())
    }
    color_pot <- if (input$bn) "black" else if (pot() >= input$objetivo) col_beta else col_alpha
    HTML(sprintf(
      "<div style='font-size:1.15em; line-height:1.7;'>
         Cohen's <i>d</i> = Δ / σ = %.1f / %.1f = <b>%.2f</b> (efecto %s)<br>
         Con n = %d por grupo, la potencia es
         <b style='color:%s;'>%.3f</b> (el %.1f%% de los experimentos darían p &lt; %s)<br>
         %s
       </div>",
      input$delta, input$sigma, d(), categoria_d(d()),
      input$n, color_pot, pot(), 100 * pot(), input$alpha,
      texto_n))
  })

  output$popPlot <- renderPlot({
    grafico_poblaciones(input$delta, input$sigma, bn = input$bn)
  })

  output$samplePlot <- renderPlot({
    grafico_muestral(input$delta, input$sigma, input$n, alpha(), bn = input$bn)
  })
}

shinyApp(ui = ui, server = server)
