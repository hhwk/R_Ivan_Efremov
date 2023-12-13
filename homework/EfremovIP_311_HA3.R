#Задание 1
# Установим порядковый номер и общее количество студентов
k <- 3
n <- 10

# Биномиальное распределение
p <- k/n
binom_dist <- rbinom(n, n, p)

# Хи-квадрат распределение
df <- (n-1) + k
chi_sq_dist <- rchisq(n, df)

# Нормальное распределение
m <- k*100
sigma <- sqrt(n + k)
norm_dist <- rnorm(n, m, sigma)

# Посчитаем математическое ожидание, дисперсию, стандартное отклонение, моду, медиану для каждого распределения
mean_binom <- mean(binom_dist)
var_binom <- var(binom_dist)
sd_binom <- sd(binom_dist)
mode_binom <- as.numeric(names(sort(table(binom_dist), decreasing=TRUE)[1]))
median_binom <- median(binom_dist)

mean_chi_sq <- mean(chi_sq_dist)
var_chi_sq <- var(chi_sq_dist)
sd_chi_sq <- sd(chi_sq_dist)
mode_chi_sq <- as.numeric(names(sort(table(chi_sq_dist), decreasing=TRUE)[1]))
median_chi_sq <- median(chi_sq_dist)

mean_norm <- mean(norm_dist)
var_norm <- var(norm_dist)
sd_norm <- sd(norm_dist)
mode_norm <- as.numeric(names(sort(table(norm_dist), decreasing=TRUE)[1]))
median_norm <- median(norm_dist)

# Нарисуем графики функций плотности распределения и функции распределения для каждого распределения
par(mfrow=c(3,2))
hist(binom_dist, prob=TRUE, main="Биномиальное распределение", xlab="Значение", ylab="Плотность")
lines(density(binom_dist), col="blue")

hist(chi_sq_dist, prob=TRUE, main="Хи-квадрат распределение", xlab="Значение", ylab="Плотность")
lines(density(chi_sq_dist), col="blue")

hist(norm_dist, prob=TRUE, main="Нормальное распределение", xlab="Значение", ylab="Плотность")
lines(density(norm_dist), col="blue")

curve(pbinom(x, n, p), from=0, to=n, col="blue", main="Функция распределения биномиального")
curve(pchisq(x, df), from=0, to=max(chi_sq_dist), col="blue", main="Функция распределения хи-квадрат")
curve(pnorm(x, m, sigma), from=min(norm_dist), to=max(norm_dist), col="blue", main="Функция распределения нормального")

#Задание 2
# Задача 1

# а) Найти вероятность того, что ровно два автомобиля из ремонтирующихся – фирменной расцветки
n_cars <- 60
n_firm_cars <- 35
n_repair <- 5

p_two_firm_cars <- choose(n_firm_cars, 2) * choose(n_cars - n_firm_cars, n_repair - 2) / choose(n_cars, n_repair)
p_two_firm_cars

# б) Найти вероятность того, что сейчас ремонтируется хотя бы один из автомобилей фирменной расцветки
p_at_least_one_firm_car <- 1 - choose(n_cars - n_firm_cars, n_repair) / choose(n_cars, n_repair)
p_at_least_one_firm_car

# Задача 2

# а) Найти вероятность того, что среди 390 человек есть хотя бы один с нужной датой ДР
n_students <- 390
p_at_least_one_birthday <- 1 - (364/365) ^ n_students
p_at_least_one_birthday

# б) Найти количество человек, чтобы вероятность была больше 0.5
n_needed <- log(1 - 0.5) / log(364/365)
ceiling(n_needed)

#Задание 3
# Вероятность получения каждой из комбинаций

# а) пара
p_pair <- (13 * choose(4, 2) * choose(12, 3) * (4^3)) / choose(52, 5)
p_pair

# б) две пары
p_two_pairs <- (choose(13, 2) * (choose(4, 2)^2) * choose(11, 1) * (4^2)) / choose(52, 5)
p_two_pairs

# в) тройка
p_three_of_a_kind <- (13 * choose(4, 3) * choose(12, 2) * (4^2)) / choose(52, 5)
p_three_of_a_kind

# г) стрит
p_straight <- (10 * (4^5)) / choose(52, 5)
p_straight

# д) флэш
p_flush <- (choose(13, 5) * (4^5)) / choose(52, 5)
p_flush

# е) три+два
p_full_house <- (13 * choose(4, 3) * 12 * choose(4, 2)) / choose(52, 5)
p_full_house

# ж) каре
p_four_of_a_kind <- (13 * choose(4, 4) * 48) / choose(52, 5)
p_four_of_a_kind

# з) стрит-флэш
p_straight_flush <- (10 * 4) / choose(52, 5)
p_straight_flush

# и) ройал-флэш
p_royal_flush <- (4/choose(52, 5))
p_royal_flush

# Вероятность того, что не выпадет ни одна из комбинаций
p_no_combination <- 1 - (p_pair + p_two_pairs + p_three_of_a_kind + p_straight + p_flush + p_full_house + p_four_of_a_kind + p_straight_flush + p_royal_flush)
p_no_combination
