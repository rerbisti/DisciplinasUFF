
# Carregar pacotes
library(tidyverse)

# Função para calcular aproximação da série de Taylor (Maclaurin) para e^x
taylor_exp <- function(x, n) {
  termos <- sapply(0:n, function(k) x^k / factorial(k))
  sum(termos)
}

# Valores de x
x_vals <- seq(-2, 2, by = 0.1)
n_vals <- c(1, 2, 4) # graus do polinômio

# Criar um data frame com todas as aproximações
df <- expand.grid(x = x_vals, n = n_vals) %>%
  rowwise() %>%
  mutate(y = taylor_exp(x, n),
         tipo = paste0("n = ", n)) %>%
  ungroup()

# Função original
df_orig <- data.frame(x = x_vals, y = exp(x_vals), tipo = "e^x")

# Combinar dados
df_all <- bind_rows(df_orig, df)

ggplot(df_all, aes(x = x, y = y, color = tipo)) +
  geom_line(size = 1.2) +
  labs(title = expression("Aproximações da Série de Taylor para " * e^x),
       x = "x", y = "f(x)", color = "Série") +
  theme_minimal() +
  scale_color_manual(values = c("black", "red", "blue", "green")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.position = "bottom", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16))

ggsave(filename = "Figuras/STaylor.png", plot = last_plot(),
       width = 10, height = 6, dpi = 600)
