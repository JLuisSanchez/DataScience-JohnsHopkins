# plot1.R
# Objetivo: Crear un histograma de la potencia activa global.
# Nota: Este script no requiere librerías externas. Todas las funciones
# son parte de los paquetes base de R (como utils, graphics, grDevices).

# --- 1. Carga y Preparación de Datos ---

# Función para descargar, descomprimir y cargar los datos
load_data <- function() {
  # Verificar si ya tenemos los datos procesados para ahorrar tiempo
  if (file.exists("power_data.rds")) {
    message("Cargando datos pre-procesados desde 'power_data.rds'...")
    return(readRDS("power_data.rds"))
  }
  
  message("Procesando los datos por primera vez. Esto puede tardar un momento...")
  
  # URL del archivo zip
  file_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  zip_file <- "household_power_consumption.zip"
  data_file <- "household_power_consumption.txt"
  
  # Descargar el archivo si no existe
  if (!file.exists(zip_file)) {
    download.file(file_url, destfile = zip_file, method = "curl")
  }
  
  # Descomprimir el archivo
  unzip(zip_file)
  
  # Leer los datos
  full_data <- read.table(
    data_file,
    header = TRUE,
    sep = ";",
    na.strings = "?",
    stringsAsFactors = FALSE
  )
  
  # Filtrar por las fechas de interés: 2007-02-01 y 2007-02-02
  data_subset <- subset(full_data, Date == "1/2/2007" | Date == "2/2/2007")
  
  # Convertir columnas a los tipos de datos correctos
  data_subset$Global_active_power <- as.numeric(data_subset$Global_active_power)
  
  # Crear una columna de fecha y hora combinada (DateTime)
  data_subset$DateTime <- strptime(
    paste(data_subset$Date, data_subset$Time),
    format = "%d/%m/%Y %H:%M:%S"
  )
  
  # Guardar el objeto procesado para futuras ejecuciones
  saveRDS(data_subset, "power_data.rds")
  
  return(data_subset)
}

# Ejecutar la función para cargar los datos
power_data <- load_data()


# --- 2. Generación del Gráfico ---

# Abrir el dispositivo gráfico PNG
png("plot1.png", width = 480, height = 480)

# Crear el histograma
hist(
  power_data$Global_active_power,
  col = "red",
  main = "Global Active Power",
  xlab = "Global Active Power (kilowatts)",
  ylab = "Frequency"
)

# Cerrar el dispositivo gráfico

message("plot1.png ha sido creado exitosamente.")