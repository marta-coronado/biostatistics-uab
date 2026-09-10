library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Distribuciones poblacionales vs muestrales"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("alpha", "Nivel α:", 0.05, min = 0.0001, max = 0.3, step = 0.01),
      numericInput("delta", "Diferencia de medias Δ:", 0.5, min = 0.01, max = 10, step = 0.1),
      numericInput("sigma", "Desviación estándar σ:", 1, min = 0.1, max = 10, step = 0.1),
      sliderInput("powerTarget", "Potencia deseada (1−β):", 
                  min = 0.5, max = 0.99, value = 0.8, step = 0.01)
    ),
    
    mainPanel(
      h3("Distribuciones poblacionales"),
      plotOutput("popPlot"),
      h3("Distribuciones muestrales"),
      plotOutput("samplePlot"),
      verbatimTextOutput("statsText")
    )
  )
)

server <- function(input, output) {
  
  # Función para calcular potencia dado n
  calc_power <- function(n, alpha, delta, sigma) {
    sd_xbar <- sigma / sqrt(n)
    z_alpha <- qnorm(1 - alpha)
    c_crit  <- z_alpha * sd_xbar
    beta <- pnorm(c_crit, mean = delta, sd = sd_xbar)
    1 - beta
  }
  
  # Encontrar n que alcanza la potencia deseada
  reactive_n <- reactive({
    alpha <- input$alpha
    delta <- input$delta
    sigma <- input$sigma
    target <- input$powerTarget
    
    f <- function(n) calc_power(n, alpha, delta, sigma) - target
    res <- tryCatch(uniroot(f, c(2, 1000000))$root, error = function(e) NA)
    round(res)
  })
  
  # Panel poblacional
  output$popPlot <- renderPlot({
    delta <- input$delta
    sigma <- input$sigma
    
    x <- seq(min(-3*sigma, delta - 3*sigma), max(3*sigma, delta + 3*sigma), length.out = 1000)
    dist_null_p <- dnorm(x, mean = 0, sd = sigma)
    dist_alt_p  <- dnorm(x, mean = delta, sd = sigma)
    
    df <- data.frame(x, dist_null_p, dist_alt_p)
    
    ggplot(df) +
      geom_line(aes(x, dist_null_p), color = "blue", size = 1) +
      geom_line(aes(x, dist_alt_p), color = "red", size = 1) +
      labs(title = "Distribuciones poblacionales (σ)",
           x = "Valor", y = "Densidad") +
      theme_minimal(base_size = 14)
  })
  
  # Panel muestral
  output$samplePlot <- renderPlot({
    alpha <- input$alpha
    delta <- input$delta
    sigma <- input$sigma
    n <- reactive_n()
    
    if (is.na(n)) return()
    
    sd_xbar <- sigma / sqrt(n)
    z_alpha <- qnorm(1 - alpha)
    c_crit  <- z_alpha * sd_xbar
    
    x <- seq(min(-3*sd_xbar, delta - 3*sd_xbar), max(c_crit + 3*sd_xbar, delta + 3*sd_xbar), length.out = 1000)
    dist_null_m <- dnorm(x, mean = 0, sd = sd_xbar)
    dist_alt_m  <- dnorm(x, mean = delta, sd = sd_xbar)
    
    df <- data.frame(x, dist_null_m, dist_alt_m)
    df_alpha <- subset(df, x >= c_crit)
    df_beta  <- subset(df, x < c_crit)
    
    ggplot(df) +
      geom_line(aes(x, dist_null_m), color = "blue", size = 1) +
      geom_line(aes(x, dist_alt_m), color = "red", size = 1) +
      geom_area(data = df_alpha, aes(x, dist_null_m), fill = "blue", alpha = 0.25) +
      geom_area(data = df_beta, aes(x, dist_alt_m), fill = "red", alpha = 0.25) +
      geom_vline(xintercept = c_crit, linetype = "dashed") +
      labs(title = paste("Distribuciones muestrales (σ/√n, n =", n, ")"),
           subtitle = "Área azul = α, Área roja = β",
           x = "Valor de la media muestral", y = "Densidad") +
      theme_minimal(base_size = 14)
  })
  
  output$statsText <- renderText({
    alpha <- input$alpha
    delta <- input$delta
    sigma <- input$sigma
    target <- input$powerTarget
    n <- reactive_n()
    
    if (is.na(n)) return("No se pudo calcular n para la potencia deseada.")
    
    d <- delta / sigma
    d_cat <- if (d < 0.2) "muy pequeño" else if (d < 0.5) "pequeño" else if (d < 0.8) "mediano" else "grande"
    
    paste0("Potencia objetivo: ", round(target, 3),
           "\nTamaño de muestra requerido (n): ", n,
           "\nΔ (unidades originales): ", round(delta, 3),
           "\nCohen's d: ", round(d, 3), " — efecto ", d_cat)
  })
  
}

shinyApp(ui = ui, server = server)
