# Carregar pacotes necessários
library(ggplot2)

# Definir parâmetros
n_X <- 100     # Tamanho da amostra de X
n_Y <- 300     # Tamanho da amostra de Y
n_simulacoes <- 1000  # Número de simulações de U

# Gerar amostras de X e Y
X <- matrix(rnorm(n_simulacoes * n_X, mean = 40, sd = 3), ncol = n_simulacoes)
Y <- matrix(rnorm(n_simulacoes * n_Y, mean = 100, sd = 2), ncol = n_simulacoes)

# Calcular as variâncias amostrais S2x e S2y
S2_X <- apply(X, 2, var)
S2_Y <- apply(Y, 2, var)

# Calcular U = (4 * S2_X) / (9 * S2_Y)
U <- (4 * S2_X) / (9 * S2_Y)

# Criar histograma de U
ggplot(data = tibble(x = U), mapping = aes(x = x)) +
  geom_histogram(aes(y = ..density.., fill = "amostra de U"), alpha = 0.4) +
  stat_function(fun = stats::df, args = list(df1 = n_X, df2 = n_Y), 
                aes(color = "densidade teórica"), linewidth=1.2) +
  labs(y = "densidade", x = "x", fill = "", color = "") +
  scale_fill_manual(values = c("amostra de U" = "orange")) +
  scale_color_manual(values = c("densidade teórica" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))
