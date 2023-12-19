# Загрузите необходимые библиотеки
library(openintro)

# Загрузите датасет cars93
data(cars93)

# Гистограмма с бинами шириной 3
hist(cars93$price, breaks = seq(0, max(cars93$price) + 3, by = 3), main = "Histogram (binwidth = 3)", xlab = "Price")

# Гистограмма с бинами шириной 30
hist(cars93$price, breaks = seq(0, max(cars93$price) + 30, by = 30), main = "Histogram (binwidth = 30)", xlab = "Price")

# Гистограмма с бинами шириной 60
hist(cars93$price, breaks = seq(0, max(cars93$price)+ 60, by = 60), main = "Histogram (binwidth = 60)", xlab = "Price")
# Постройте boxplot для столбца price
boxplot(cars93$price, main = "Boxplot of Price")

# Создайте новый датасет cars_no_out, исключив крупнейшие выбросы
cars_no_out <- subset(cars93, price < 40000)

# Постройте boxplot для столбца price с уменьшенным набором данных
boxplot(cars_no_out$price, main = "Boxplot of Price (No Outliers)")
# Загрузите необходимые библиотеки
library(ggplot2)

# Постройте гистограммы с разбиением на группы (faceted histograms) для столбца mpg_city
# в зависимости от типа транспортного средства
ggplot(cars93, aes(x = mpg_city)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~ type) +
  labs(x = "Fuel Efficiency (mpg_city)", y = "Count") +
  ggtitle("Distribution of Fuel Efficiency by Vehicle Type")
# Загрузите необходимые библиотеки
library(dplyr)
library(ggplot2)

# Создайте собственный конвейер данных
cars93 %>%
  filter(price < 30000) %>%
  ggplot(aes(x = drive_train)) +
  geom_bar() +
  labs(x = "Drive Train", y = "Count") +
  ggtitle("Histogram of Cars by Drive Train (Price < $30K)")
