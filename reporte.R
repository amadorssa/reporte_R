# Instalación de paquetes necesarios
install.packages("openxlsx")
install.packages("dplyr")
install.packages("moments")
install.packages("ggplot2")

# Carga de librerías
library("openxlsx")
library("dplyr")
library("moments")
library("ggplot2")

# Establecer el directorio de trabajo
setwd("/Users/imac/Library/CloudStorage/OneDrive-UniversidaddeSonora/University/Trabajo Delfin/Ricardo/Reporte")

# Cargar los datos
data <- read.xlsx("tabla datos.xlsx", sheet = 1)

# Cambiar los nombres de las columnas
names(data) <- c("origen", "destino", "dia", "salida", "pronostico", "real")

# Convertir 'dia' a formato Date
data$dia <- as.Date(data$dia, origin = "1899-12-30")

# Crear un data frame con los datos
data <- data.frame(data)
View(data)

# Descripción resumida de los datos
summary(data)

# Identificar y eliminar valores faltantes
data <- na.omit(data)

# Transformar variables categóricas (origen y destino) a factores
data$origen <- factor(data$origen)
data$destino <- factor(data$destino)

# Crear una nueva columna con la diferencia entre lo real y el pronóstico
data <- mutate(data, diferencia = real - pronostico)

# Crear una nueva columna con el valor absoluto de la diferencia
data <- mutate(data, diferencia_abs = abs(diferencia))

## Estadísticas descriptivas
# Función para calcular estadísticas descriptivas
descriptive_statistics <- function(dataframe, columna) {
  if(!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  if(!columna %in% colnames(dataframe)) {
    stop("La columna especificada no existe en el dataframe.")
  }

  data <- dataframe[[columna]]

  if(!is.numeric(data)) {
    stop("La columna debe ser numérica.")
  }

  statistics <- data.frame(
    media = mean(data, na.rm = TRUE),
    mediana = median(data, na.rm = TRUE),
    desviacion_estandar = sd(data, na.rm = TRUE),
    varianza = var(data, na.rm = TRUE),
    rango = max(data, na.rm = TRUE) - min(data, na.rm = TRUE),
    minimo = min(data, na.rm = TRUE),
    maximo = max(data, na.rm = TRUE),
    skewness = skewness(data, na.rm = TRUE),
    kurtosis = kurtosis(data, na.rm = TRUE),
    Q1 = quantile(data, 0.25, na.rm = TRUE),
    Q3 = quantile(data, 0.75, na.rm = TRUE)
  )

  return(statistics)
}

# Estadísticas descriptivas para las variables de interés
statistics_salida <- descriptive_statistics(data, "salida")
print(statistics_salida)

statistics_pronostico <- descriptive_statistics(data, "pronostico")
print(statistics_pronostico)

statistics_real <- descriptive_statistics(data, "real")
print(statistics_real)

statistics_diferencia <- descriptive_statistics(data, "diferencia")
print(statistics_diferencia)


## Gráficos
# Boxplot de la distribución de las diferencias por origen
# Ayuda a visualizar la variabilidad de las diferencias entre el pronóstico y lo real para cada origen
ggplot(data, aes(x = origen, y = diferencia)) +
  geom_boxplot(notch = TRUE) +
  ggtitle("Distribución de las diferencias por origen") +
  xlab("Origen") +
  ylab("Diferencia (pronóstico - real)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("boxplot_origen.png", plot = last_plot())

# Boxplot de la distribución de las diferencias por destino
# Similar al anterior, pero enfocado en los destinos para identificar si hay diferencias en función del destino
ggplot(data, aes(x = destino, y = diferencia)) +
  geom_boxplot(notch = TRUE) +
  ggtitle("Distribución de las diferencias por destino") +
  xlab("Destino") +
  ylab("Diferencia (pronóstico - real)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("boxplot_destino.png", plot = last_plot())

# Histograma doble del pronóstico y lo real
# Permite comparar las distribuciones de los valores pronosticados y los valores reales
ggplot(data = data, aes(x = pronostico, fill = "Pronóstico")) +
  geom_histogram(binwidth = 1, alpha = 0.5) +
  geom_histogram(aes(x = real, fill = "Real"), binwidth = 1, alpha = 0.5) +
  labs(title = "Histograma de pronóstico y real",
       x = "Valor",
       y = "Frecuencia") +
  scale_fill_manual(values = c("Pronóstico" = "blue", "Real" = "red"))

ggsave("histograma.png", plot = last_plot())

# Gráfico de dispersión del pronóstico y lo real
# Visualiza la relación entre los valores pronosticados y los valores reales, útil para detectar patrones o discrepancias
ggplot(data = data, aes(x = pronostico, y = real)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión de pronóstico y real",
       x = "Pronóstico",
       y = "Real")

ggsave("scatterplot.png", plot = last_plot())

# Mapa de calor de la diferencia por origen y destino
# Visualiza la magnitud de las diferencias absolutas entre pronóstico y real para cada combinación de origen y destino
ggplot(data = data, aes(x = origen, y = destino, fill = diferencia_abs)) +
    geom_tile(color = "white", linewidth = 0.25) +
    coord_fixed() +
    scale_fill_gradient(low = "grey", high = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
    labs(title = "Mapa de Calor de Diferencias por Origen y Destino",
         x = "Origen",
         y = "Destino") +
    guides(fill = guide_colourbar(title = "Diferencia absoluta"))

ggsave("heatmap.png", plot = last_plot())

# Gráfico de barras de la diferencia total por origen
# Muestra la suma de las diferencias absolutas por cada origen para identificar cuál contribuye más a la discrepancia
data_origen <- data %>%
  group_by(origen) %>%
  summarise(total_diferencia = sum(diferencia_abs))

ggplot(data = data_origen, aes(x = origen, y = total_diferencia)) +
  geom_bar(stat = "identity", fill = "#373737") +
  labs(title = "Diferencia Total por origen",
       x = "Origen",
       y = "Diferencia total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave("barplot_origen.png", plot = last_plot())

# Serie de tiempo de la diferencia total por día
# Visualiza cómo las diferencias totales varían a lo largo del tiempo
data_time <- data %>%
  group_by(dia) %>%
  summarise(total_diferencia = sum(diferencia_abs))

# Time series plot of the total difference by day
ggplot(data = data_time, aes(x = dia, y = total_diferencia)) +
  geom_line() +
  labs(title = "Serie de Tiempo de la Diferencia Total por Día",
       x = "Día",
       y = "Diferencia Total")

ggsave("timeseries.png", plot = last_plot())

## Análisis de datos
# Función para eliminar outliers usando el método del IQR
remove_outliers <- function(dataframe, columna) {
  if(!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  if(!columna %in% colnames(dataframe)) {
    stop("La columna especificada no existe en el dataframe.")
  }

  data <- dataframe[[columna]]

  if(!is.numeric(data)) {
    stop("La columna debe ser numérica.")
  }

  Q1 <- quantile(data, 0.25, na.rm = TRUE)
  Q3 <- quantile(data, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  outliers <- data < lower_bound | data > upper_bound

  dataframe <- dataframe[!outliers, ]

  return(dataframe)
}

## Pruebas estadísticas
# Prueba t para la diferencia entre el pronóstico y lo real
# Esta prueba evalúa si hay una diferencia significativa entre los pronósticos y los valores reales
t_test <- t.test(data$pronostico, data$real, paired = TRUE)
print(t_test)

# Prueba ANOVA para la diferencia entre el pronóstico y lo real por origen
# Evalúa si hay diferencias significativas en los horarios de salida entre diferentes orígenes
anova_test <- aov(salida ~ origen, data = data)
summary(anova_test)

# Modelo de regresión lineal para evaluar la relación entre la hora de salida y el destino
# Ayuda a entender si los destinos influyen significativamente en los horarios de salida
modelo <- lm(salida ~ destino, data = data)
summary(modelo)
