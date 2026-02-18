# Carregar pacotes
library(tidyverse)

# Parâmetros da distribuição Beta
alpha <- 2
beta <- 5
n <- 100  # Tamanho da amostra
n_simulacoes <- 1000  # Número de simulações

# Função para simular medianas amostrais
simula_mediana <- function(n, alpha, beta, n_simulacoes) {
  medianas <- replicate(n_simulacoes, {
    amostra <- rbeta(n, shape1 = alpha, shape2 = beta)  # Amostras da distribuição Beta
    median(amostra)  # Mediana amostral
  })
  data.frame(mediana = medianas)
}

# Gerar os dados para o tamanho de amostra n = 100
dados_mediana <- simula_mediana(n, alpha, beta, n_simulacoes)

# Visualização com ggplot
ggplot(dados_mediana, aes(x = mediana)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.6, fill = "lightblue") +
  geom_vline(xintercept = qbeta(0.5, alpha, beta), color = "red", linetype = "dashed", size = 1) +
  labs(x = "mediana amostral", y = "densidade") +
  theme_minimal()
