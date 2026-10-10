library(shiny)
library(ggplot2)

# ------------------------------------------------------------------
# Juego: ¿cómo cambian el efecto, la desviación y n la potencia?
#
# Diseño: dos grupos independientes, t-test bilateral, n por grupo.
# Todos los cálculos usan power.t.test(), igual que en las diapositivas.
# ------------------------------------------------------------------

# Colores (los mismos que en las diapositivas)
col_ctrl  <- "#C2185B"  # magenta: grupo control
col_trat  <- "#1B8A1B"  # verde:   grupo tratamiento
col_h0    <- "#2C7BB6"
col_ha    <- "#D95F02"
col_alpha <- "#D7191C"
col_beta  <- "#1A9641"
col_power <- "#1c5253"

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

  # Mismo eje x en los dos gráficos: depende solo de Δ y σ (no de n),
  # así al cambiar n se ve cómo se estrechan las curvas de abajo
  eje_x <- reactive(c(-4 * input$sigma, input$delta + 4 * input$sigma))

  # ---- Resumen numérico --------------------------------------------
  output$resumen <- renderUI({
    texto_n <- if (is.na(n_obj())) {
      "Si Δ = 0 no hay diferencia real: la potencia no aplica (solo rechazamos H0 por error, con probabilidad α)."
    } else {
      sprintf("Para una potencia de %.2f necesitas <b>n = %d por grupo</b>.",
              input$objetivo, n_obj())
    }
    color_pot <- if (pot() >= input$objetivo) col_beta else col_alpha
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

  # ---- Panel 1: poblaciones ----------------------------------------
  output$popPlot <- renderPlot({
    delta <- input$delta; sigma <- input$sigma
    x <- seq(eje_x()[1], eje_x()[2], length.out = 600)
    df <- rbind(
      data.frame(x = x, y = dnorm(x, 0, sigma),     grupo = "Control"),
      data.frame(x = x, y = dnorm(x, delta, sigma), grupo = "Tratamiento")
    )
    ggplot(df, aes(x, y, colour = grupo, fill = grupo)) +
      geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.15, colour = NA) +
      geom_line(linewidth = 1.3) +
      geom_vline(xintercept = c(0, delta), colour = c(col_ctrl, col_trat),
                 linetype = "dashed") +
      scale_colour_manual(values = c(Control = col_ctrl, Tratamiento = col_trat), name = NULL) +
      scale_fill_manual(values = c(Control = col_ctrl, Tratamiento = col_trat), name = NULL) +
      labs(x = "Valor de la variable", y = "Densidad",
           subtitle = sprintf("Δ = %.1f   σ = %.1f   →   d = %.2f", delta, sigma, d())) +
      coord_cartesian(xlim = eje_x()) +
      scale_y_continuous(labels = function(y) sprintf("%.2f", y)) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })

  # ---- Panel 2: distribución de la diferencia de medias -------------
  output$samplePlot <- renderPlot({
    delta <- input$delta; sigma <- input$sigma; n <- input$n
    df_t <- 2 * n - 2
    se   <- sigma * sqrt(2 / n)               # error estándar de x̄1 − x̄2
    ncp  <- delta / se
    crit <- qt(1 - alpha() / 2, df_t) * se    # valor crítico en unidades originales

    lim <- max(abs(eje_x()), crit + se)       # calculamos un poco más allá del eje visible
    x  <- seq(-lim, lim, length.out = 2000)
    df <- data.frame(x = x,
                     h0 = dt(x / se, df_t) / se,
                     ha = suppressWarnings(dt(x / se, df_t, ncp = ncp)) / se)
    rechazo <- abs(df$x) >= crit
    df$alpha_y <- ifelse(rechazo, df$h0, 0)
    df$power_y <- ifelse(rechazo, df$ha, 0)
    df$beta_y  <- ifelse(rechazo, 0, df$ha)

    ggplot(df, aes(x)) +
      geom_ribbon(aes(ymin = 0, ymax = power_y, fill = "Potencia (1 − β)"), alpha = 0.55) +
      geom_ribbon(aes(ymin = 0, ymax = beta_y,  fill = "Error tipo II (β)"), alpha = 0.45) +
      geom_ribbon(aes(ymin = 0, ymax = alpha_y, fill = "Error tipo I (α)"),  alpha = 0.8) +
      geom_line(aes(y = h0, colour = "Bajo H0 (no hay diferencia)"), linewidth = 1.2) +
      geom_line(aes(y = ha, colour = "Bajo HA (diferencia = Δ)"), linewidth = 1.2) +
      geom_vline(xintercept = c(-crit, crit), linetype = "dashed") +
      scale_fill_manual(values = c("Error tipo I (α)"  = col_alpha,
                                   "Error tipo II (β)" = col_beta,
                                   "Potencia (1 − β)"  = col_power), name = NULL) +
      scale_colour_manual(values = c("Bajo H0 (no hay diferencia)" = col_h0,
                                     "Bajo HA (diferencia = Δ)"    = col_ha), name = NULL) +
      labs(x = expression(bar(x)[tratamiento] - bar(x)[control]), y = "Densidad",
           subtitle = sprintf("EE = σ·√(2/n) = %.2f   ·   rechazamos H0 si |diferencia| > %.2f (líneas discontinuas)",
                              se, crit)) +
      coord_cartesian(xlim = eje_x()) +
      scale_y_continuous(labels = function(y) sprintf("%.2f", y)) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top", legend.box = "vertical",
            panel.grid.minor = element_blank())
  })
}

shinyApp(ui = ui, server = server)
