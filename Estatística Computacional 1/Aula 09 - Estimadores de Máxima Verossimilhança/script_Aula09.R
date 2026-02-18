# Carregar pacotes necessários
library(tidyverse)


# Dados observados
data = c(2.5,3.6,7.1,2.1,3.0,4.8,9.5,2.3,3.3,1.9)

# Função de verossimilhança da Exponencial
log_likelihood_exp = function(lambda, data) {
  ll = sum(dexp(data, rate = lambda, log = TRUE))
  return(ll)
}

# Maximização da verossimilhança
result_exp = optimise(function(lambda) log_likelihood_exp(lambda, data), 
                         interval = c(0.1, 5), maximum = TRUE)

# Gráfico da função de verossimilhança
lambda_values = seq(0.01, 5, length.out = 1000)
ll_values = sapply(lambda_values, log_likelihood_exp, data = data)
df_exp = data.frame(lambda = lambda_values, log_likelihood = ll_values)
lambda_hat = 1/mean(data)


ggplot(df_exp, aes(x = lambda, y = log_likelihood)) +
  geom_line() +
  geom_vline(xintercept = result_exp$maximum,
             col = "red", linetype = "dashed", linewidth = 1.1) +
  geom_point(aes(x = lambda_hat,
                 y = max(ll_values)), col = "blue", size = 4) +
  labs(title = "Maximo da Funcao Verossimilhanca", 
       x = expression(mu), y = "log-verossimilhanca") +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        panel.border = element_blank())
