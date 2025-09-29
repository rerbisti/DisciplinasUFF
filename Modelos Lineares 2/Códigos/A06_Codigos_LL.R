library(tidyverse)

# ----------------------------
# Simulação
set.seed(123)
n <- 20
x <- runif(n, 0, 5)
beta0_true <- 0.5
beta1_true <- 0.3
mu <- exp(beta0_true + beta1_true * x)
y <- rpois(n, mu)

# Modelo de interesse
fit <- glm(y ~ x, family = poisson)
beta_hat <- coef(fit)

# Função log-verossimilhança
logLikPoisson <- function(beta0, beta1, x, y) {
  mu_hat <- exp(beta0 + beta1 * x)
  sum(dpois(y, lambda = mu_hat, log = TRUE))
}

# Sequências de parâmetros
beta0_seq <- seq(beta_hat[1] - 1.5, beta_hat[1] + 1.2, length.out = 200)
beta1_seq <- seq(beta_hat[2] - 0.35, beta_hat[2] + 0.4, length.out = 200)

# Log-verossimilhança do saturado
LL_saturado <- sum(dpois(y, lambda = y, log = TRUE))

# ----------------------------
# 1) Superfície em grade
LL_df <- expand.grid(beta0 = beta0_seq, beta1 = beta1_seq) %>%
  mutate(LL = mapply(logLikPoisson, beta0, beta1, MoreArgs = list(x = x, y = y)))

p1 <- ggplot(LL_df, aes(beta0, beta1, z = LL)) +
  geom_contour_filled(breaks = seq(max(LL_df$LL)-10, max(LL_df$LL), length.out=15)) +
  geom_point(aes(x = beta_hat[1], y = beta_hat[2]), color = "red", size = 3) +
  annotate("text", x = beta_hat[1], y = beta_hat[2], label = "EMV", vjust = -1, color = "red") +
  labs(title = "Log-verossimilhança do modelo de interesse",
       x = expression(beta[0]), y = expression(beta[1])) +
  theme_minimal()+
  theme(legend.position = "none")

# ----------------------------
# 2) Corte em beta0 (fixando beta1 = MLE)
LL_beta0 <- tibble(beta0 = beta0_seq,
                   LL = sapply(beta0_seq, function(b0) logLikPoisson(b0, beta_hat[2], x, y)))

p2 <- ggplot(LL_beta0, aes(beta0, LL)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = LL_saturado, linetype = "dashed", color = "blue") +
  geom_point(aes(x = beta_hat[1], y = max(LL)), color = "red", size = 3) +
  theme_minimal()+
  labs(
    title = expression("LL perfilada para " ~ beta[0] ~ " (fixando " ~ beta[1] ~ " = EMV)"),
    x = expression(beta[0]),
    y = expression("Log-verossimilhança")
  )

# ----------------------------
# 3) Corte em beta1 (fixando beta0 = MLE)
LL_beta1 <- tibble(beta1 = beta1_seq,
                   LL = sapply(beta1_seq, function(b1) logLikPoisson(beta_hat[1], b1, x, y)))

p3 <- ggplot(LL_beta1, aes(beta1, LL)) +
  geom_line(size = 1.2) +
  geom_hline(yintercept = LL_saturado, linetype = "dashed", color = "blue") +
  geom_point(aes(x = beta_hat[2], y = max(LL)), color = "red", size = 3) +
  theme_minimal()+
  labs(
    title = expression("LL perfilada para " ~ beta[1] ~ " (fixando " ~ beta[0] ~ " = EMV)"),
    x = expression(beta[0]),
    y = expression("Log-verossimilhança")
  )

# ----------------------------
# Exibir juntos (se quiser usar patchwork)
library(patchwork)
g1 = p1 / (p2 | p3)

ggsave("Figuras/vero.png", plot = g1, width = 8, height = 6, dpi = 600)

