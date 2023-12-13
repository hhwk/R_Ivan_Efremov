# Создание списка с данными
cars <- c("Mitsubishi Eclipse", "Toyota Supra", "Mazda RX-7", "Nissan Skyline", "Honda S2000")
prices <- c(20000, 30000, 25000, 35000, 28000)
years <- c(1999, 1998, 1997, 1999, 2000)
mileage <- c(50000, 40000, 45000, 55000, 38000)
owners <- c(2, 1, 3, 2, 1)
colors <- c("orange", "silver", "red", "blue", "yellow")
transmission <- c("manual", "manual", "manual", "automatic", "manual")

# Создание датафрейма
car_df <- data.frame(cars = cars, prices = prices, years = years, mileage = mileage, owners = owners, colors = colors, transmission = transmission)

# Сохранение датафрейма в формате csv
write.csv(car_df, file = "car_data.csv")

# Построение графиков
# Гистограмма цен на машины
hist(car_df$prices, main = "Car Prices", xlab = "Price", col = "lightblue")

# Диаграмма со столбцами количества машин разного цвета
barplot(table(car_df$colors), main = "Car Colors", xlab = "Color", ylab = "Count", col = "lightgreen")
# Пример использования lapply для вывода всех цветов машин
lapply(car_df$colors, function(x) {
  print(paste("Color:", x))
})

# Пример использования sapply для вывода среднего числа владельцев машин
average_owners <- sapply(car_df$owners, mean)
print(paste("Среднее число владельцев:", average_owners))

# Улучшенный код для подсчета количества слов в тексте
text <- "Это пример текста. Он содержит несколько слов, разделенных пробелами."
word_count <- length(strsplit(text, "\\s")[[1]])
print(paste("Количество слов:", word_count))
