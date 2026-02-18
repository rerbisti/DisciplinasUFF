library(tidyverse)

## Aula 04 - Parte I

# tamanho da amostra
n = 1000

### Atividade 1
x = runif(n, -1, 1)
x.t = abs(x)

ggplot(data = tibble(x = x.t), mapping = aes(x = x)) +
  geom_histogram(aes(y = ..density.., fill = "amostra gerada"), alpha = 0.4) +
  stat_function(fun = dunif, args = list(min = 0, max = 1), aes(color = "densidade teórica"),
                linewidth=1.2) +
  labs(y = "densidade", x = "x", fill = "", color = "") +
  xlim(0, 1) +
  ylim(0, 2) + 
  scale_fill_manual(values = c("amostra gerada" = "blue")) +
  scale_color_manual(values = c("densidade teórica" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggplot(data = tibble(x = x.t), aes(sample = x)) +
  stat_qq(distribution = qunif, dparams = list(min = 0, max = 1), size = 3) +  
  geom_abline(intercept = 0, col = "red", size = 1.2) +
  labs(x = "quantis teóricos",
       y = "quantis amostrais") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank())

### Atividade 2
y = runif(n)
y.t = -log(y)

ggplot(data = tibble(x = y.t), mapping = aes(x = x)) +
  geom_histogram(aes(y = ..density.., fill = "amostra gerada"), alpha = 0.4) +
  stat_function(fun = dexp, args = list(rate = 1), aes(color = "densidade teórica"),
                linewidth=1.2) +
  labs(y = "densidade", x = "x", fill = "", color = "") +
  xlim(0, 4) +
  ylim(0, 1.2) + 
  scale_fill_manual(values = c("amostra gerada" = "blue")) +
  scale_color_manual(values = c("densidade teórica" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggplot(data = tibble(x = y.t), aes(sample = x)) +
  stat_qq(distribution = qexp, dparams = list(rate = 1), size = 3) +  
  geom_abline(intercept = 0, col = "red", size = 1.2) +
  labs(x = "quantis teóricos",
       y = "quantis amostrais") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank())


## Atividade 3
z = rnorm(n)
z2 = z^2

ggplot(data = tibble(x = z2), mapping = aes(x = x)) +
  geom_histogram(aes(y = ..density.., fill = "amostra gerada"), alpha = 0.4) +
  stat_function(fun = dchisq, args = list(df = 1), aes(color = "densidade teórica"),
                linewidth=1.2) +
  labs(y = "densidade", x = "x", fill = "", color = "") +
  xlim(0, 3) +
  ylim(0, 2) + 
  scale_fill_manual(values = c("amostra gerada" = "blue")) +
  scale_color_manual(values = c("densidade teórica" = "red")) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))

ggplot(data = tibble(x = z2), aes(sample = x)) +
  stat_qq(distribution = qchisq, dparams = list(df = 1), size = 3) +  
  geom_abline(intercept = 0, col = "red", size = 1.2) +
  labs(x = "quantis teóricos",
       y = "quantis amostrais") +
  xlim(0,9) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank())


## Desafio

pop = c(1,3,5,5,7)

# calculando a média e a variância populacional
med.pop = mean(pop)
var.pop = var(pop)*(4/5)

n = 2     # tamanho da amostra
m = 1000  # numero de amostras

# definindo a matriz que armazenará os valores amostrados
x = matrix(data = NA, ncol = m, nrow = n)
xbarra = NULL

# obtendo a amostra e calculando a sua respectiva média
for(j in 1:m){
  x[,j] = sample(pop, n, replace = TRUE)
  xbarra[j] = mean(x[,j])}

ggplot(data = tibble(valores = xbarra), mapping = aes(x = valores)) +
  geom_histogram(aes(y = ..density.., fill = "amostra gerada"), alpha = 0.4) +
  stat_function(fun = dnorm, args = list(mean = med.pop, 
                                         sd = sqrt(var.pop/n)), 
                aes(color = "densidade teórica"),
                linewidth=1.2) +
  scale_fill_manual(values = c("amostra gerada" = "purple")) +
  scale_color_manual(values = c("densidade teórica" = "red")) +
  labs(x = expression(bar(x)),
       y = "densidade", fill = "", colour = "") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12))
