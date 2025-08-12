# Cargar librerías necesarias
library(dplyr)

# Descargar y descomprimir datos si no existen
if(!file.exists("UCI HAR Dataset")){
  zip_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(zip_url, "dataset.zip", mode="wb")
  unzip("dataset.zip")
}

# 1. Cargar datos de entrenamiento y prueba
features <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

# 2. Unir datos en un solo set
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)

colnames(X) <- features[, 2]
merged_data <- cbind(Subject, Y, X)

# 3. Extraer solo mean() y std()
tidy_step2 <- merged_data %>% select(subject, code, contains("mean()"), contains("std()"))

# 4. Sustituir códigos de actividad por nombres
tidy_step2$code <- activity_labels[tidy_step2$code, 2]

# 5. Renombrar variables descriptivamente
names(tidy_step2)[2] <- "activity"
names(tidy_step2) <- gsub("^t", "Time", names(tidy_step2))
names(tidy_step2) <- gsub("^f", "Frequency", names(tidy_step2))
names(tidy_step2) <- gsub("Acc", "Accelerometer", names(tidy_step2))
names(tidy_step2) <- gsub("Gyro", "Gyroscope", names(tidy_step2))
names(tidy_step2) <- gsub("Mag", "Magnitude", names(tidy_step2))
names(tidy_step2) <- gsub("BodyBody", "Body", names(tidy_step2))
names(tidy_step2) <- gsub("-mean\\(\\)", "Mean", names(tidy_step2))
names(tidy_step2) <- gsub("-std\\(\\)", "StandardDeviation", names(tidy_step2))
names(tidy_step2) <- gsub("-X", ".X", names(tidy_step2))
names(tidy_step2) <- gsub("-Y", ".Y", names(tidy_step2))
names(tidy_step2) <- gsub("-Z", ".Z", names(tidy_step2))

# 6. Crear tidy data promedio por sujeto y actividad
tidy_data <- tidy_step2 %>%
  group_by(subject, activity) %>%
  summarise(across(everything(), mean), .groups = "drop")

# 7. Guardar dataset final como .txt con el formato requerido
write.table(tidy_data, "tidy_data.txt", row.name = FALSE)
