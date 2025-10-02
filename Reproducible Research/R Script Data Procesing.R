# ============================================================================
# ANÁLISIS COMPLETO DE DATOS DE MONITOREO DE ACTIVIDAD PERSONAL
# Proyecto de Investigación Reproducible
# ============================================================================

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(lubridate)
library(lattice)
library(knitr)

# ============================================================================
# 1. DESCARGA Y CARGA DE DATOS
# ============================================================================

# Descargar datos desde la URL original
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

if (!file.exists("activity.csv")) {
  download.file(url, destfile = "activity.zip")
  unzip("activity.zip")
  cat("Datos descargados y extraídos exitosamente\n")
} else {
  cat("Los datos ya existen localmente\n")
}

# Cargar los datos
activity <- read.csv("activity.csv")

# Convertir la columna date a Date
activity$date <- as.Date(activity$date)

# Explorar la estructura de los datos
cat("\n=== EXPLORACIÓN DE DATOS ===\n")
str(activity)
summary(activity)

# Verificar valores faltantes
missing_steps <- sum(is.na(activity$steps))
missing_percentage <- round(mean(is.na(activity$steps)) * 100, 2)
cat("\nTotal de valores faltantes en 'steps':", missing_steps, "\n")
cat("Porcentaje de valores faltantes:", missing_percentage, "%\n")

# ============================================================================
# 2. ¿CUÁL ES EL NÚMERO TOTAL MEDIO DE PASOS DADOS POR DÍA?
# ============================================================================

cat("\n=== ANÁLISIS DE PASOS TOTALES POR DÍA (SIN IMPUTACIÓN) ===\n")

# Calcular el total de pasos por día (ignorando NAs)
daily_steps <- activity %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE), .groups = 'drop')

# Remover días con 0 pasos (días con todos los valores NA)
daily_steps_clean <- daily_steps[daily_steps$total_steps > 0, ]

# Estadísticas descriptivas
mean_original <- mean(daily_steps_clean$total_steps)
median_original <- median(daily_steps_clean$total_steps)

cat("Media de pasos por día:", round(mean_original, 2), "\n")
cat("Mediana de pasos por día:", round(median_original, 2), "\n")

# GRÁFICO 1: Histograma del total de pasos por día (ORIGINAL)
p1 <- ggplot(daily_steps_clean, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Histograma del Número Total de Pasos por Día (Datos Originales)",
       x = "Total de Pasos por Día",
       y = "Frecuencia") +
  theme_minimal() +
  geom_vline(aes(xintercept = mean_original), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_original), color = "blue", linetype = "dashed", size = 1)

print(p1)
ggsave("histogram_original.png", p1, width = 10, height = 6)

# ============================================================================
# 3. ¿CUÁL ES EL PATRÓN DE ACTIVIDAD DIARIA PROMEDIO?
# ============================================================================

cat("\n=== PATRÓN DE ACTIVIDAD DIARIA PROMEDIO ===\n")

# Calcular el promedio de pasos por intervalo de 5 minutos
interval_pattern <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(interval) %>%
  summarise(avg_steps = mean(steps), .groups = 'drop')

# Encontrar el intervalo con el máximo número de pasos
max_interval <- interval_pattern$interval[which.max(interval_pattern$avg_steps)]
max_steps_avg <- max(interval_pattern$avg_steps)

# Función para convertir intervalo a formato de tiempo
interval_to_time <- function(interval) {
  hours <- interval %/% 100
  minutes <- interval %% 100
  sprintf("%02d:%02d", hours, minutes)
}

cat("Intervalo con máximo número de pasos:", max_interval, "\n")
cat("Promedio de pasos en ese intervalo:", round(max_steps_avg, 2), "\n")
cat("Hora del día con máxima actividad:", interval_to_time(max_interval), "\n")

# GRÁFICO 2: Series de tiempo del patrón de actividad diaria
p2 <- ggplot(interval_pattern, aes(x = interval, y = avg_steps)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Patrón de Actividad Diaria Promedio",
       x = "Intervalo de 5 minutos",
       y = "Número Promedio de Pasos") +
  theme_minimal() +
  geom_vline(aes(xintercept = max_interval), color = "red", linetype = "dashed") +
  annotate("text", x = max_interval + 200, y = max_steps_avg - 20, 
           label = paste("Máximo:", interval_to_time(max_interval)), color = "red")

print(p2)
ggsave("time_series_pattern.png", p2, width = 12, height = 6)

# ============================================================================
# 4. IMPUTACIÓN DE VALORES FALTANTES
# ============================================================================

cat("\n=== ESTRATEGIA DE IMPUTACIÓN ===\n")

# Analizar el patrón de valores faltantes
missing_pattern <- activity %>%
  group_by(date) %>%
  summarise(missing_count = sum(is.na(steps)),
            total_intervals = n(),
            .groups = 'drop')

# Días con valores faltantes
days_with_missing <- missing_pattern[missing_pattern$missing_count > 0, ]
cat("Días con valores faltantes:\n")
print(days_with_missing)

complete_missing_days <- sum(days_with_missing$missing_count == 288)
cat("\nDías con TODOS los valores faltantes:", complete_missing_days, "\n")

cat("\nESTRATEGIA ELEGIDA: Imputación por promedio del intervalo\n")
cat("Justificación:\n")
cat("1. Los días faltantes tienen TODOS los intervalos faltantes\n")
cat("2. Usar el promedio del intervalo captura mejor los patrones diarios\n")
cat("3. Es más realista que usar un valor global o cero\n")

# Implementar la estrategia de imputación
activity_imputed <- activity

# Crear un vector de medias por intervalo para imputación eficiente
interval_means <- interval_pattern$avg_steps
names(interval_means) <- interval_pattern$interval

# Imputar valores faltantes
for (i in 1:nrow(activity_imputed)) {
  if (is.na(activity_imputed$steps[i])) {
    interval_value <- as.character(activity_imputed$interval[i])
    activity_imputed$steps[i] <- interval_means[interval_value]
  }
}

# Verificar que no hay valores faltantes
cat("\nValores faltantes después de la imputación:", sum(is.na(activity_imputed$steps)), "\n")

# ============================================================================
# 5. HISTOGRAMA CON DATOS IMPUTADOS
# ============================================================================

cat("\n=== ANÁLISIS CON DATOS IMPUTADOS ===\n")

# Calcular nuevas estadísticas con datos imputados
daily_steps_imputed <- activity_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps), .groups = 'drop')

mean_imputed <- mean(daily_steps_imputed$total_steps)
median_imputed <- median(daily_steps_imputed$total_steps)

cat("Media con datos imputados:", round(mean_imputed, 2), "\n")
cat("Mediana con datos imputados:", round(median_imputed, 2), "\n")

# Comparación antes y después
cat("\nComparación antes y después de imputación:\n")
cat("Media antes:", round(mean_original, 2), "-> Media después:", round(mean_imputed, 2), "\n")
cat("Mediana antes:", round(median_original, 2), "-> Mediana después:", round(median_imputed, 2), "\n")

# GRÁFICO 3: Histograma con datos imputados
p3 <- ggplot(daily_steps_imputed, aes(x = total_steps)) +
  geom_histogram(binwidth = 1000, fill = "lightcoral", color = "black", alpha = 0.7) +
  labs(title = "Histograma del Número Total de Pasos por Día (Datos Imputados)",
       x = "Total de Pasos por Día",
       y = "Frecuencia") +
  theme_minimal() +
  geom_vline(aes(xintercept = mean_imputed), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median_imputed), color = "blue", linetype = "dashed", size = 1)

print(p3)
ggsave("histogram_imputed.png", p3, width = 10, height = 6)

# ============================================================================
# 6. DIFERENCIAS ENTRE DÍAS DE SEMANA Y FINES DE SEMANA
# ============================================================================

cat("\n=== ANÁLISIS POR TIPO DE DÍA ===\n")

# Crear factor para tipo de día (usando datos imputados)
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
                                   "weekend", "weekday")
activity_imputed$day_type <- as.factor(activity_imputed$day_type)

# Calcular patrones de actividad por tipo de día
activity_patterns <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarise(avg_steps = mean(steps), .groups = 'drop')

# Estadísticas por tipo de día
daily_by_type <- activity_imputed %>%
  group_by(date, day_type) %>%
  summarise(total_steps = sum(steps), .groups = 'drop') %>%
  group_by(day_type) %>%
  summarise(mean_steps = round(mean(total_steps), 2),
            median_steps = round(median(total_steps), 2),
            .groups = 'drop')

cat("Estadísticas por tipo de día:\n")
print(daily_by_type)

# Análisis de picos de actividad
weekday_pattern <- activity_patterns[activity_patterns$day_type == "weekday", ]
weekend_pattern <- activity_patterns[activity_patterns$day_type == "weekend", ]

weekday_max <- weekday_pattern$interval[which.max(weekday_pattern$avg_steps)]
weekend_max <- weekend_pattern$interval[which.max(weekend_pattern$avg_steps)]

cat("\nPico de actividad en días de semana:", interval_to_time(weekday_max), 
    "con", round(max(weekday_pattern$avg_steps), 2), "pasos\n")
cat("Pico de actividad en fines de semana:", interval_to_time(weekend_max), 
    "con", round(max(weekend_pattern$avg_steps), 2), "pasos\n")

# GRÁFICO 4: Panel plot usando ggplot2
p4 <- ggplot(activity_patterns, aes(x = interval, y = avg_steps)) +
  geom_line(color = "blue", size = 1) +
  facet_wrap(~ day_type, ncol = 1, 
             labeller = labeller(day_type = c("weekday" = "Días de Semana", 
                                             "weekend" = "Fines de Semana"))) +
  labs(title = "Comparación de Patrones de Actividad: Días de Semana vs Fines de Semana",
       x = "Intervalo de 5 minutos",
       y = "Número Promedio de Pasos") +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))

print(p4)
ggsave("panel_plot_ggplot.png", p4, width = 12, height = 8)

# GRÁFICO 5: Panel plot usando lattice (versión alternativa)
png("panel_plot_lattice.png", width = 800, height = 600)
xyplot(avg_steps ~ interval | day_type, data = activity_patterns, 
       type = "l", layout = c(1, 2),
       xlab = "Intervalo de 5 minutos", 
       ylab = "Número Promedio de Pasos",
       main = "Patrones de Actividad: Días de Semana vs Fines de Semana",
       strip = strip.custom(factor.levels = c("Días de Semana", "Fines de Semana")))
dev.off()

# ============================================================================
# 7. RESUMEN FINAL
# ============================================================================

cat("\n=== RESUMEN FINAL DEL ANÁLISIS ===\n")
cat("1. Total de observaciones procesadas:", nrow(activity), "\n")
cat("2. Valores faltantes imputados:", missing_steps, "\n")
cat("3. Media final de pasos por día:", round(mean_imputed, 2), "\n")
cat("4. Intervalo de máxima actividad:", interval_to_time(max_interval), "\n")
cat("5. Diferencias por tipo de día: Se observan patrones distintos\n")
cat("   - Días de semana: pico matutino pronunciado\n")
cat("   - Fines de semana: actividad más distribuida\n")

cat("\nAnálisis completado exitosamente. Gráficos guardados:\n")
cat("- histogram_original.png\n")
cat("- time_series_pattern.png\n")
cat("- histogram_imputed.png\n")
cat("- panel_plot_ggplot.png\n")
cat("- panel_plot_lattice.png\n")

# Guardar datos procesados
write.csv(activity_imputed, "activity_imputed_final.csv", row.names = FALSE)
write.csv(daily_steps_imputed, "daily_steps_final.csv", row.names = FALSE)
write.csv(activity_patterns, "activity_patterns_final.csv", row.names = FALSE)

cat("\nDatos procesados guardados:\n")
cat("- activity_imputed_final.csv\n")
cat("- daily_steps_final.csv\n")
cat("- activity_patterns_final.csv\n")

cat("\n¡ANÁLISIS COMPLETO!\n")
