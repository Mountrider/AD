# Load the required library
library(dplyr)
library(readxl)
library(openintro)
library(tidyverse)
library(readr)
# Read the CSV file into a dataframe
comics <- read.csv("E:\\university\\AD\\lab2\\comic_characters.csv")

# Exercise 1
# 1. Print the first rows of the dataset
head(comics)

# 2. Check the levels of align column
levels(comics$align)

# 3. Check the levels of gender column
levels(comics$sex)

# 4. Create a 2-way contingency table
contingency_table <- comics %>%
  count(sex, align) %>%
  pivot_wider(names_from = align, values_from = n)

# Assign the tab name to the resulting data frame
tab <- contingency_table
# Exercise 2
# 1. Print tab to explore which level of align has the fewest total entries
print(tab)

# 2. Use filter() to filter out all rows of comics with that level
filtered_comics <- comics %>%
  filter(align != "level_to_drop")

# 3. Use droplevels() to drop the unused levels from the dataframe
# Convert the 'align' column to a factor
filtered_comics$align <- as.factor(filtered_comics$align)

# Use droplevels() to drop unused levels
filtered_comics$align <- droplevels(filtered_comics$align)

# 4. Save the simplified dataset as comics_filtered
comics_filtered <- filtered_comics

# 5. Check if the unused level of align is dropped
levels(comics_filtered$align)
# Load the required library
library(ggplot2)

# Exercise 3
# 1. Create a side-by-side barchart with align on the x-axis and fill the bars with the gender of the character
ggplot(comics_filtered, aes(x = align, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(x = "Alignment", y = "Count", fill = "Gender") +
  ggtitle("Character Alignment by Gender")

# 2. Create another side-by-side barchart with sex on the x-axis and fill the bars with the alignment of the character.
ggplot(comics_filtered, aes(x = sex, fill = align)) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", fill = "Alignment") +
  ggtitle("Character Gender by Alignment")
# Упражнение 4.1
# 1. Создаем таблицу совместных пропорций для переменных "пол" и "выравнивание"
joint_proportions <- comics_filtered %>%
  group_by(sex, align) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(proportion = count / sum(count))

# 2. Создаем столбчатую диаграмму для визуализации совместных пропорций
ggplot(joint_proportions, aes(x = align, y = proportion, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Выравнивание", y = "Пропорция", fill = "Пол") +
  ggtitle("Совместные пропорции пола персонажей по выравниванию")
# Упражнение 4.2
# 1. Создаем таблицу условных пропорций (по столбцам) для переменных "пол" и "выравнивание"
conditional_proportions <- comics_filtered %>%
  group_by(align) %>%
  mutate(total_count = sum(n())) %>%
  ungroup() %>%  # Снять группировку, чтобы создать total_count за пределами группировки
  group_by(align, sex) %>%  # Изменено место группировки
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total_count = sum(count)) %>%  # Пересчитываем общее количество для всех строк внутри align
  mutate(proportion = count / total_count, .groups = 'drop')  # Вычисляем пропорции

# 2. Создаем столбчатую диаграмму для визуализации условных пропорций
ggplot(conditional_proportions, aes(x = align, y = proportion, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Выравнивание", y = "Пропорция", fill = "Пол") +
  ggtitle("Условные пропорции пола персонажей по выравниванию")

