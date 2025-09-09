# Pacotes necessários
library(MASS)       
library(tidyverse)


set.seed(22)


# -------------------------------------------------------------
# Exemplo: Caso assintótico - Médias de variáveis exponenciais
# -------------------------------------------------------------
p <- 2             # quantidade de estatisticas
tam <- 60          # tamanho da amostra (grande para TCL)
n_simula <- 5000

# Gerar amostras exponenciais independentes
X1 <- matrix(rexp(n_simula * tam, rate = 1), nrow = n_simula)
X2 <- matrix(rexp(n_simula * tam, rate = 1), nrow = n_simula)

S1 <- rowMeans(X1)
S2 <- rowMeans(X2)
S <- cbind(S1, S2)

mu <- c(1, 1)
V <- diag(1 / tam, p_B)  # matriz de covariâncias teórica das médias
d <- t(t(S) - mu)
V_inv <- solve(V)
Q <- rowSums((d %*% V_inv) * d)

# Base de dados
df <- data.frame(Q = Q)


# Plot com legenda
ggplot(df, aes(x = Q)) +
  geom_histogram(aes(y = ..density.., fill = "valores simulados"), 
                 bins = 100, alpha = 0.6, color = "white") +
    stat_function(fun = dchisq, args = list(df = p), 
                aes(color = "densidade teórica"), linewidth = 1.2) +
    scale_fill_manual(name = "", values = c("valores simulados" = "#404080")) +
  scale_color_manual(name = "", values = c("densidade teórica" = "red")) +
  labs(x = "Q", y = "Densidade") +
  theme_minimal() +
  theme(legend.position = "bottom",  
        legend.text = element_text(size = 12))

