# Função e derivada
f <- function(x) { x^2 - 2 }
f_prime <- function(x) { 2*x }

# Newton-Raphson com registro das iterações
newton_raphson_plot <- function(x0, tol = 1e-6, max_iter = 20) {
  x <- x0
  iter <- numeric()
  x_vals <- numeric()
  
  for (i in 1:max_iter) {
    iter <- c(iter, i)
    x_vals <- c(x_vals, x)
    
    x_new <- x - f(x)/f_prime(x)
    
    if (abs(x_new - x) < tol) {
      x <- x_new
      iter <- c(iter, i+1)
      x_vals <- c(x_vals, x)
      break
    }
    
    x <- x_new
  }
  
  # Plot da convergência
  plot(iter, x_vals, type="b", pch=19, col="blue",
       xlab="Iteração", ylab="x_n",
       main="Convergência do Método de Newton-Raphson")
  abline(h = sqrt(2), col="red", lty=2)  # valor exato da raiz
  legend("bottomright", legend=c("x_n", "Raiz exata"),
         col=c("blue", "red"), pch=c(19, NA), lty=c(1,2))
  
  return(x)
}

# Rodar o método com x0 = 1
raiz <- newton_raphson_plot(x0 = 1)
raiz





# Função e derivada
f <- function(x) { x^2 - 2 }
f_prime <- function(x) { 2*x }

# Newton-Raphson com registro das iterações
newton_raphson_steps <- function(x0, tol = 1e-6, max_iter = 20) {
  x <- x0
  steps <- numeric()
  
  for (i in 1:max_iter) {
    steps <- c(steps, x)
    x_new <- x - f(x)/f_prime(x)
    
    if (abs(x_new - x) < tol) {
      steps <- c(steps, x_new)
      break
    }
    
    x <- x_new
  }
  
  return(steps)
}

# Valor inicial
x0 <- 1
steps <- newton_raphson_steps(x0)

# Gerar gráfico da função
x_vals <- seq(0, 2, length.out = 200)
plot(x_vals, f(x_vals), type="l", lwd=2, col="black",
     ylab="f(x)", xlab="x", main="Newton-Raphson se aproximando da raiz")
abline(h=0, col="gray")  # eixo x

# Adicionar linhas mostrando cada passo do Newton-Raphson
for (i in 1:(length(steps)-1)) {
  x_curr <- steps[i]
  x_next <- steps[i+1]
  
  # Linha vertical até a curva
  segments(x0 = x_curr, y0 = 0, x1 = x_curr, y1 = f(x_curr), col="blue", lty=2)
  # Linha horizontal até o eixo x da próxima iteração
  segments(x0 = x_curr, y0 = f(x_curr), x1 = x_next, y1 = 0, col="red", lty=2)
  # Pontos nas iterações
  points(x_curr, f(x_curr), pch=19, col="blue")
}

# Último ponto
points(steps[length(steps)], 0, pch=19, col="blue")

legend("topleft", legend=c("f(x)", "Iterações NR"),
       col=c("black", "blue"), lwd=c(2, NA), pch=c(NA,19))

