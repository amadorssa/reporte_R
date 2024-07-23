install.packages("openxlsx")
install.packages("dplyr")
install.packages("moments")
install.packages("ggplot2")

library("openxlsx")
library("dplyr")
library("moments")
library("ggplot2")

# Set the working directory
setwd("/Users/imac/Library/CloudStorage/OneDrive-UniversidaddeSonora/University/Trabajo Delfin/Ricardo/Reporte")

# Load the data
data <- read.xlsx("tabla datos.xlsx", sheet = 1)

# Change the header names
names(data) <- c("origen", "destino", "dia", "salida", "pronostico", "real")

# Convert dia to Date format
data$dia <- as.Date(data$dia, origin = "1899-12-30")

# Create a data frame with data
data <- data.frame(data)
View(data)

# Description of the data
summary(data)

# Identify and remove missing values
data <- na.omit(data)

# Transform categorical variables (origen and destino) to factors
data$origen <- factor(data$origen)
data$destino <- factor(data$destino)

# Create a new column with the difference between the real and the forecast
data <- mutate(data, diferencia = real - pronostico)

# Create a new column with the abs value of diferencia
data <- mutate(data, diferencia_abs = abs(diferencia))

## Estatisitcs

# Descriptive statistics (mean, median, mode, variance, standard deviation, skewness, kurtosis)
descriptive_statistics <- function(dataframe, columna) {
  if(!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }

  if(!columna %in% colnames(dataframe)) { # nolint: spaces_left_parentheses_linter, line_length_linter.
    stop("La columna especificada no existe en el dataframe.")
  }

  data <- dataframe[[columna]]

  if(!is.numeric(data)) { # nolint
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

statistics_salida <- descriptive_statistics(data, "salida")
print(statistics_salida)

statistics_pronostico <- descriptive_statistics(data, "pronostico")
print(statistics_pronostico)

statistics_real <- descriptive_statistics(data, "real")
print(statistics_real)

statistics_diferencia <- descriptive_statistics(data, "diferencia")
print(statistics_diferencia)


# Boxplot of the distribution of the differences by origen
ggplot(data, aes(x = origen, y = diferencia)) +
  geom_boxplot(notch = TRUE) +
  ggtitle("Distribución de las diferencias por origen") +
  xlab("Origen") +
  ylab("Diferencia (pronóstico - real)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("boxplot_origen.png", plot = last_plot())

# Boxplot of the distribution of the differences by origen
ggplot(data, aes(x = destino, y = diferencia)) +
  geom_boxplot(notch = TRUE) +
  ggtitle("Distribución de las diferencias por destino") +
  xlab("Destino") +
  ylab("Diferencia (pronóstico - real)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

ggsave("boxplot_destino.png", plot = last_plot())

# Double histogram of the forecast and the real
ggplot(data = data, aes(x = pronostico, fill = "Pronóstico")) +
  geom_histogram(binwidth = 1, alpha = 0.5) +
  geom_histogram(aes(x = real, fill = "Real"), binwidth = 1, alpha = 0.5) +
  labs(title = "Histograma de Pronóstico y Real",
       x = "Valor",
       y = "Frecuencia") +
  scale_fill_manual(values = c("Pronóstico" = "blue", "Real" = "red"))

ggsave("histograma.png", plot = last_plot())

# Scatter plot of the forecast and the real
ggplot(data = data, aes(x = pronostico, y = real)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión de Pronóstico y Real",
       x = "Pronóstico",
       y = "Real")

ggsave("scatterplot.png", plot = last_plot())

# Heatmap difference by origen and destino
ggplot(data = data, aes(x = origen, y = destino, fill = diferencia_abs)) +
    geom_tile(color = "white", linewidth = 0.25) +
    #geom_text(aes(label = diferencia), color = "white", size = 4) +
    coord_fixed() +
    scale_fill_gradient(low = "grey", high = "black") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
    labs(title = "Mapa de Calor de Diferencias por Origen y Destino",
         x = "Origen",
         y = "Destino") +
    guides(fill = guide_colourbar(title = "Diferencia absoluta"))

ggsave("heatmap.png", plot = last_plot())

# Create a data frame with the data by origen and the sum of the differences
data_origen <- data %>%
  group_by(origen) %>%
  summarise(total_diferencia = sum(diferencia_abs))

# Bar plot of the total difference by origen
ggplot(data = data_origen, aes(x = origen, y = total_diferencia)) +
  geom_bar(stat = "identity", fill = "#373737") +
  labs(title = "Diferencia Total por Origen",
       x = "Origen",
       y = "Diferencia Total") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

ggsave("barplot_origen.png", plot = last_plot())

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

# Remove outliers (IQR)
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

