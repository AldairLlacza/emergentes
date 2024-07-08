# Crear un conjunto de datos de ejemplo
set.seed(123)
n <- 100
data <- data.frame(
  Tienda = sample(1:5, n, replace = TRUE),
  Clima = sample(c("Soleado", "Nublado", "Lluvioso"), n, replace = TRUE),
  Temporada = sample(c("Verano", "Otoño", "Invierno"), n, replace = TRUE),
  DiaSemana = sample(c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes"), n, replace = TRUE),
  Ventas = rnorm(n, mean = 100, sd = 20)
)
# Crear un modelo de regresión
modelo <- lm(Ventas ~ Clima + Temporada + DiaSemana, data = data)

# Hacer predicciones para cada tienda
data$Predicciones <- predict(modelo, newdata = data)

# Crear agentes para cada tienda
agentes <- lapply(unique(data$Tienda), function(tienda) {
  datos_tienda <- data[data$Tienda == tienda, ]
  existencias_iniciales <- 1000  # Cantidad inicial de existencias para cada tienda
  
  agente <- list(
    Tienda = tienda,
    Existencias = existencias_iniciales,
    Datos = datos_tienda
  )
  
  return(agente)
})

# Función para que los agentes ajusten sus existencias
ajustar_existencias <- function(agente) {
  ventas_predichas <- predict(modelo, newdata = agente$Datos)
  demanda <- sum(ventas_predichas)
  
  # Ajustar las existencias en función de la demanda
  agente$Existencias <- agente$Existencias - demanda
  return(agente)
}

# Ejecutar la función para cada agente
agentes_actualizados <- lapply(agentes, ajustar_existencias)
agentes_actualizados
