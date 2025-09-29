# Instalar e carregar o pacote glmnet (se necessário)
# install.packages("glmnet")
library(glmnet)
library(tidyverse)
library(pROC)


set.seed(42)

# Simulação dos dados
n <- 200
p <- 10  # número de preditores

X <- matrix(rnorm(n * p), n, p)
# Coeficientes verdadeiros (alguns zero para mostrar sparsity)
beta_true <- c(2, -1.5, 0, 0, 1, 0, 0, 0.5, 0, 0)
eta <- X %*% beta_true
prob <- 1 / (1 + exp(-eta))  # probabilidade logística
y <- rbinom(n, 1, prob)      # variável resposta binária

# Ajuste do modelo logístico com Lasso
# alpha = 1 → Lasso
fit_lasso <- glmnet(X, y, family = "binomial", alpha = 1)

# Visualizar caminho dos coeficientes
plot(fit_lasso, xvar = "lambda", label = TRUE)

# Escolher lambda via validação cruzada
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cv_lasso)

# Coeficientes no lambda que minimiza o erro de validação
coef(cv_lasso, s = "lambda.min")

#################################################

## Comparando as probabilidades estimadas

# Probabilidades ajustadas no lambda ótimo
prob_hat_lasso <- predict(cv_lasso, newx = X, s = "lambda.min", type = "response")

# Ajuste GLM usual
df_glm <- as.data.frame(X)
colnames(df_glm) <- paste0("X", 1:p)
df_glm$y <- y
formula_glm <- as.formula(paste("y ~", paste(colnames(df_glm)[1:p], collapse = " + ")))
fit_glm <- glm(formula_glm, data = df_glm, family = binomial)
prob_hat_glm <- predict(fit_glm, type = "response")

# Criar dataframe longo para ggplot
df_plot <- data.frame(
  prob_true = prob,
  Lasso = as.numeric(prob_hat_lasso),
  GLM = as.numeric(prob_hat_glm))

df_long <- pivot_longer(df_plot, cols = c(Lasso, GLM), 
                        names_to = "Modelo", values_to = "Prob_Ajustada")

# Gráfico das probabilidades estimadas
ggplot(df_long, aes(x = prob_true, y = Prob_Ajustada, color = Modelo)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Probabilidade Verdadeira",
       y = "Probabilidade Ajustada") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Medidas de ajuste

# Probabilidades ajustadas (converter para vetor)
prob_hat_glm <- as.numeric(prob_hat_glm)
prob_hat_lasso <- as.numeric(prob_hat_lasso)

# 1. AUC
roc_glm <- roc(y, prob_hat_glm)
roc_lasso <- roc(y, prob_hat_lasso)
auc_glm <- auc(roc_glm)
auc_lasso <- auc(roc_lasso)

# 2. Log-verossimilhança
loglik_glm <- logLik(fit_glm)
beta_hat <- as.numeric(coef(cv_lasso, s = "lambda.min"))
eta_hat <- cbind(1, X) %*% beta_hat
loglik_lasso <- sum(y*eta_hat - log(1 + exp(eta_hat)))

# 3. AIC e BIC
aic_glm <- AIC(fit_glm)
bic_glm <- BIC(fit_glm)

# Para o Lasso (df = número de coeficientes não nulos)
df_lasso <- sum(beta_hat != 0)
AIC_lasso <- -2 * loglik_lasso + 2 * df_lasso
BIC_lasso <- -2 * loglik_lasso + log(n) * df_lasso

# Criar tabela comparativa
tabela <- data.frame(
  Medida = c("AUC (ROC)", "Log-Verossimilhança", "AIC", "BIC"),
  GLM_Usual = c(round(auc_glm, 3), round(loglik_glm, 3), round(aic_glm, 3), round(bic_glm, 3)),
  Lasso = c(round(auc_lasso, 3), round(loglik_lasso, 3), round(AIC_lasso, 3), round(BIC_lasso, 3)))

print(tabela)

