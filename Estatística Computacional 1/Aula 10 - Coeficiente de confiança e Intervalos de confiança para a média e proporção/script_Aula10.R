# Carregar pacotes necessarios
library(tidyverse)

# Definir parametros
set.seed(42)
lambda <- 1    # Parametro da distribuicao exponencial media = 1)
n <- 200        # Tamanho da amostra
conf_level <- 0.95  # Nivel de confianca

# Gerar uma unica amostra de tamanho n da distribuicao exponencial
amostra <- rexp(n, rate = lambda)  # Amostra da distribuicao exponencial

# Calcular a media amostral
media_amostral <- mean(amostra)

# Calcular o desvio padrao da exponencial (sigma)
sigma <- 1 / lambda

# Calcular o intervalo de confianca
z <- qnorm(1 - (1 - conf_level) / 2)  # Quantil da normal para 95%
erro_margem <- z * (sigma / sqrt(n))  # Margem de erro
ic_lower <- media_amostral - erro_margem  # Limite inferior
ic_upper <- media_amostral + erro_margem  # Limite superior

# Exibicao dos resultados
cat("Intervalo de confianca para a media amostral: ", 
    sprintf("[%.3f, %.3f]", ic_lower, ic_upper), "\n")

