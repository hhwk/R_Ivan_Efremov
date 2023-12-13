#Задание 1(а)
A <- matrix(c(1, 0, 0, 2, 0, 3, 1, 4, 2, 0, -3, 5), nrow = 3, byrow = TRUE)
A_t <- t(A)
cat("Размерность матрицы A:", dim(A), "\n")
cat("Размерность транспонированной матрицы A^T:", dim(A_t), "\n\n")
cat("Матрица A:\n")
print(A)
cat("\nМатрица A^T:\n")
print(A_t)

#Задание 1(б)
A <- matrix(c(1, 3, 5, 1), nrow = 4, byrow = TRUE)
A_t <- t(A)
cat("Размерность матрицы A:", dim(A), "\n")
cat("Размерность транспонированной матрицы A^T:", dim(A_t), "\n\n")
cat("Матрица A:\n")
print(A)
cat("\nМатрица A^T:\n")
print(A_t)

#Задание 2
A <- matrix(c(2, -3, 4, 7, 6, -5, -1, 8, 9), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 3, -4, -7, -5, 5, 1, -8, -8), nrow = 3, byrow = TRUE)
sum_matrix <- A + B
cat("Матрица A:\n")
print(A)
cat("\nМатрица B:\n")
print(B)
cat("\nСумма матриц A и B:\n")
print(sum_matrix)

#Задание 3
A <- matrix(c(1, 0, 3, -4, 2, 1), nrow = 3, byrow = TRUE)
B <- matrix(c(1, -1, 2, 3, 1, -5), nrow = 3, byrow = TRUE)
C <- matrix(c(3, 4, 1, -3, 8, 6), nrow = 3, byrow = TRUE)
result <- 3 * A + 4 * B - 2 * C
print("Результат выражения 3A + 4B - 2C:")
print(result)

#Задание 5(а)
A <- matrix(c(1, 0, 2, -3, 2, 1, 0, 0, 1), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 3, 1, 0, 1, 1, -2, 1, 3), nrow = 3, byrow = TRUE)
AB <- A %*% B
BA <- B %*% A
print("Произведение AB:")
print(AB)

print("Произведение BA:")
print(BA)

#Задание 5(в)
A <- matrix(c(1, -1, 2), nrow = 3, ncol = 1)
B <- matrix(c(3, 4, 1), nrow = 1, ncol = 3)
result_AB <- A %*% B
result_BA <- B %*% A
print("Произведение AB:")
print(result_AB)
print("Произведение BA:")
print(result_BA)

#Задание 6(а)
A <- matrix(c(1, 2, 3, -1), nrow = 2, byrow = TRUE)
result <- A %*% A
print("Квадрат матрицы A:")
print(result)

#Задание 6(б)
A <- matrix(c(2, 1, 1, 4), nrow = 2, byrow = TRUE)
result <- (A %*% A) %*% A
print("Квадрат матрицы A:")
print(result)

#Задание 7
A <- matrix(c(4, -3, 9, 1), nrow = 2, byrow = TRUE)
f_A <- A %*% A - 2 * A
print("f(A):")
print(f_A)
