library(shiny)
library(ggplot2)

# ---------------------------------------------------------------
# Generador de problemas
# ---------------------------------------------------------------
fmt_z <- function(z) formatC(z, format = "f", digits = 2)

elegir_distractores <- function(p, pool, n = 3, gap = 0.01) {
  pool <- round(pool, 4)
  pool <- pool[pool > 0.0005 & pool < 0.9995]
  cand <- numeric(0)
  for (x in pool[sample.int(length(pool))]) {
    if (all(abs(c(p, cand) - x) >= gap)) cand <- c(cand, x)
    if (length(cand) == n) break
  }
  while (length(cand) < n) {
    x <- round(runif(1, 0.01, 0.99), 4)
    if (all(abs(c(p, cand) - x) >= gap)) cand <- c(cand, x)
  }
  cand
}

nuevo_problema <- function(tipos) {
  tipo <- sample(tipos, 1)
  zs <- c(-309:-1, 1:309) / 100   # z entre -3.09 y 3.09 (como en las tablas)

  if (tipo == "le") {
    z <- sample(zs, 1)
    p <- pnorm(z)
    enunciado <- sprintf("P(Z \u2264 %s)", fmt_z(z))
    codigo <- sprintf("pnorm(%s)", fmt_z(z))
    lo <- -Inf; hi <- z
    pool <- c(1 - p, abs(p - 0.5),
              pnorm(z + c(-0.2, -0.1, 0.1, 0.2)))
  } else if (tipo == "ge") {
    z <- sample(zs, 1)
    p <- 1 - pnorm(z)
    enunciado <- sprintf("P(Z \u2265 %s)", fmt_z(z))
    codigo <- sprintf("1 - pnorm(%s)", fmt_z(z))
    lo <- z; hi <- Inf
    pool <- c(1 - p, abs(pnorm(z) - 0.5),
              1 - pnorm(z + c(-0.2, -0.1, 0.1, 0.2)))
  } else {
    repeat {
      ab <- sort(sample(zs, 2))
      if (diff(ab) >= 0.4) break
    }
    a <- ab[1]; b <- ab[2]
    p <- pnorm(b) - pnorm(a)
    enunciado <- sprintf("P(%s \u2264 Z \u2264 %s)", fmt_z(a), fmt_z(b))
    codigo <- sprintf("pnorm(%s) - pnorm(%s)", fmt_z(b), fmt_z(a))
    lo <- a; hi <- b
    pool <- c(1 - p, pnorm(b), pnorm(a), 1 - pnorm(a),
              abs(pnorm(b) - 0.5), abs(pnorm(a) - 0.5),
              pnorm(b) + pnorm(a))
  }

  p <- round(p, 4)
  opciones <- sample(c(p, elegir_distractores(p, pool)))
  list(
    enunciado = enunciado, codigo = codigo, p = p,
    opciones = opciones, correcta = which(opciones == p),
    lo = lo, hi = hi
  )
}

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Práctica: probabilidades con la normal estándar"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        "tipos", "Tipos de pregunta",
        choices = c("P(Z \u2264 z)" = "le",
                    "P(Z \u2265 z)" = "ge",
                    "P(a \u2264 Z \u2264 b)" = "between"),
        selected = c("le", "ge", "between")
      ),
      checkboxInput("sombrear", "Mostrar área sombreada", TRUE),
      hr(),
      h4("Puntuación"),
      textOutput("marcador")
    ),
    mainPanel(
      width = 9,
      plotOutput("grafico", height = "320px"),
      h3(textOutput("pregunta")),
      uiOutput("opciones_ui"),
      actionButton("comprobar", "Comprobar", class = "btn-primary"),
      actionButton("nueva", "Nueva pregunta"),
      br(), br(),
      uiOutput("feedback")
    )
  )
)

# ---------------------------------------------------------------
# Server
# ---------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    q = nuevo_problema(c("le", "ge", "between")),
    respondida = FALSE, aciertos = 0, total = 0, ultima = NULL
  )

  observeEvent(input$nueva, {
    tipos <- if (length(input$tipos) == 0) c("le", "ge", "between") else input$tipos
    rv$q <- nuevo_problema(tipos)
    rv$respondida <- FALSE
    rv$ultima <- NULL
  })

  observeEvent(input$comprobar, {
    req(input$respuesta, !rv$respondida)
    rv$respondida <- TRUE
    rv$total <- rv$total + 1
    ok <- as.integer(input$respuesta) == rv$q$correcta
    if (ok) rv$aciertos <- rv$aciertos + 1
    rv$ultima <- ok
  })

  output$marcador <- renderText(sprintf("%d aciertos de %d", rv$aciertos, rv$total))

  output$pregunta <- renderText(paste("Calcula", rv$q$enunciado))

  output$opciones_ui <- renderUI({
    radioButtons(
      "respuesta", NULL,
      choiceNames = formatC(rv$q$opciones, format = "f", digits = 4),
      choiceValues = seq_along(rv$q$opciones),
      selected = character(0)
    )
  })

  output$feedback <- renderUI({
    if (is.null(rv$ultima)) return(NULL)
    q <- rv$q
    resumen <- sprintf("%s = %s  (en R: %s)",
                       q$enunciado, formatC(q$p, format = "f", digits = 4), q$codigo)
    if (rv$ultima) {
      div(class = "alert alert-success", strong("\u00a1Correcto! "), resumen)
    } else {
      div(class = "alert alert-danger", strong("Incorrecto. "), resumen)
    }
  })

  output$grafico <- renderPlot({
    q <- rv$q
    x <- seq(-4, 4, length.out = 800)
    df <- data.frame(x = x, y = dnorm(x))

    g <- ggplot(df, aes(x, y))
    if (input$sombrear) {
      g <- g + geom_area(data = subset(df, x >= q$lo & x <= q$hi),
                         fill = "#3B82F6", alpha = 0.45)
    }
    limites <- c(q$lo, q$hi)
    limites <- limites[is.finite(limites)]
    breaks <- sort(c(limites, if (all(abs(limites) > 0.3)) 0))

    g +
      geom_line(linewidth = 0.9) +
      geom_vline(xintercept = limites, linetype = "dashed", colour = "grey30") +
      scale_x_continuous(breaks = breaks, labels = fmt_z(breaks), limits = c(-4, 4)) +
      labs(x = "z", y = "Densidad") +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui, server)
