# Устанавливаем необходимый пакет для модели линейной регрессии
install.packages("tidyverse")
# Загружаем библиотеку
library(tidyverse)

# Создаем фрейм данных из предоставленных данных
file_music <- "D:\\R_lab2\\lab2\\spotify-2023.csv"
data <- read.csv(file_music)
summary(data)

data <- data %>% select(in_spotify_charts, streams, in_spotify_playlists, bpm, key, mode, danceability_., valence_., energy_., acousticness_., instrumentalness_., liveness_., speechiness_.)
head(data)

ggplot(data,aes(x = streams)) + 
  geom_histogram()

data$key <- factor(data$key)
data$mode <- factor(data$mode)


# Разбиваем данные на тренировочный и тестовый набор
set.seed(123)
train_indices <- sample(1:nrow(data), 0.8 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Строим модель линейной регрессии
model <- lm(streams ~ ., data = data)

# Выводим статистику модели
summary(model)

