library(tidyverse)
library(gridExtra)

######################################################################

# Simulação dos dados

set.seed(123)

n <- 100
x1 <- rnorm(n, 2, 1)
x2 <- rnorm(n, 1, 1)

beta0 <- 1
beta1 <- 0.3
beta2 <- 0.1

mu <- exp(beta0 + beta1*x1 + beta2*x2)
y <- rpois(n, mu)

hist(y)

dados <- tibble(y,x1,x2)

######################################################################

# Ajuste de um GLM (ex: Poisson)
fit <- glm(y ~ x1 + x2, family = poisson(link = "log"), data = dados)
summary(fit)
coef(fit)

# Valores ajustados
dados$y_hat <- exp(coef(fit)[1] + coef(fit)[2]*x1 + coef(fit)[3]*x2)
dados$y_hat2 <- fit$fitted.values

# Resíduos de Pearson e de Deviance
dados$rp <- residuals(fit, type = "pearson")
dados$rd <- residuals(fit, type = "deviance")

# Resíduos padronizados aproximados
h <- hatvalues(fit)
dados$rp_std <- rp / sqrt(1 - h)
dados$rd_std <- rd / sqrt(1 - h)

# Resíduo vs Ajustado
g.1 <- ggplot(dados, aes(x = y_hat, y = rp_std)) +
  geom_point(size=2) +
  labs(x=expression(hat(y)),y='residuos de pearson')+
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank()) +
  geom_hline(aes(yintercept = 0), col="gray10") +
  geom_hline(aes(yintercept = 2), col="red", lty = 2) +
  geom_hline(aes(yintercept = -2), col="red", lty = 2) +
  geom_hline(aes(yintercept = 3), col="green", lty = 2) +
  geom_hline(aes(yintercept = -3), col="green", lty = 2)

g.2 <- ggplot(dados, aes(x = x1, y = rp_std)) +
  geom_point(size=2) +
  labs(x=expression(x[1]),y='residuos de pearson')+
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank()) +
  geom_hline(aes(yintercept = 0), col="gray10") +
  geom_hline(aes(yintercept = 2), col="red", lty = 2) +
  geom_hline(aes(yintercept = -2), col="red", lty = 2) +
  geom_hline(aes(yintercept = 3), col="green", lty = 2) +
  geom_hline(aes(yintercept = -3), col="green", lty = 2)


g.3 <- ggplot(dados, aes(x = x2, y = rp_std)) +
  geom_point(size=2) +
  labs(x=expression(x[2]),y='residuos de pearson')+
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank()) +
  geom_hline(aes(yintercept = 0), col="gray10") +
  geom_hline(aes(yintercept = 2), col="red", lty = 2) +
  geom_hline(aes(yintercept = -2), col="red", lty = 2) +
  geom_hline(aes(yintercept = 3), col="green", lty = 2) +
  geom_hline(aes(yintercept = -3), col="green", lty = 2)


grid.arrange(g.1,g.2,g.3, ncol=2)



# Criando base com informações para análise
res.base = tibble(rp_std, cook.d = cooks.distance(fit), as_tibble(dfbetas(fit)),
                   hii = hatvalues(fit))

# Grafico distancia de cook
ggplot(res.base) +
  geom_segment(aes(x = 1:nrow(res.base), xend = 1:nrow(res.base), y = 0, yend = cook.d),
               color = 'cornflowerblue', size = 1) +
  geom_point(aes(x = 1:nrow(res.base), y = cook.d), size = 3) +
  geom_hline(yintercept = 1, color = 'salmon', linewidth = 1) +
  labs(x = 'observação', y = 'Distancia de Cook') +
  theme_bw() +
  theme(panel.border = element_blank())



# Graficos DFBETAS
g1 <- ggplot(res.base) +
  geom_segment(aes(x = 1:nrow(res.base), xend = 1:nrow(res.base), y = 0, yend = x1),
               color = 'cornflowerblue', size = 1) +
  geom_point(aes(x = 1:nrow(res.base), y = x1), size = 3) +
  geom_hline(yintercept = c(2, -2) / sqrt(nrow(res.base)), color = 'salmon',
             size = 1) +
  labs(x = 'observação', y = 'DFBETAS', title = expression(beta[1])) +
  theme_bw() +
  theme(panel.border = element_blank())

g2 <- ggplot(res.base) +
  geom_segment(aes(x = 1:nrow(res.base), xend = 1:nrow(res.base), y = 0, yend = x2),
               color = 'cornflowerblue', size = 1) +
  geom_point(aes(x = 1:nrow(res.base), y = x2), size = 3) +
  geom_hline(yintercept = c(2, -2) / sqrt(nrow(res.base)), color = 'salmon',
             size = 1) +
  labs(x = 'observação', y = 'DFBETAS', title = expression(beta[2])) +
  theme_bw() +
  theme(panel.border = element_blank())

grid.arrange(g1,g2,nrow=1)

# Graficos DFFITS
ggplot(res.base) +
  geom_segment(aes(x = 1:nrow(res.base), xend = 1:nrow(res.base), y = 0, yend = hii),
               color = 'cornflowerblue', size = 1) +
  geom_point(aes(x = 1:nrow(res.base), y = hii), size = 3) +
  geom_hline(yintercept = 2*sum(res.base$hii) / nrow(res.base), color = 'salmon',
             size = 1) +
  labs(x = 'observação', y = 'hii') +
  theme_bw() +
  theme(panel.border = element_blank())

