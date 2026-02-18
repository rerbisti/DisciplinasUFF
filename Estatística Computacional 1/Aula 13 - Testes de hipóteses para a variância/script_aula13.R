
## Ex1
library(EnvStats)

#H0: sigma2 = 3^2
#H1: sigma2 < 3^2

sigma2_0 = 3^2
nivel_sig = 0.05
amostra = c(1.69, 0.22, 1.83, 3.22, 3.90, 1.57, 1.74, 0.24, 2.46, 1.36, 2.46, 2.71, 3.04, 1.39, 2.51)


teste = varTest(amostra, sigma.squared = sigma2_0, alternative = "less")
teste

cat("Quem estava correto: ", ifelse(teste$p.value < nivel_sig, "Assistente", "Instrutor "), "\n")


## Ex2

#H0: sigma2 = 7.2^2
#H1: sigma2 < 7.2^2

n = 25
S2 = 3.5^2
sigma2_0 = 7.2^2          
nivel_sig = 0.05

est_teste = (n-1)*S2/sigma2_0
p_critico = qchisq(nivel_sig, 25-1)

cat("Decisão: ", ifelse(est_teste < p_critico, "Rejeitar H0", "Não rejeitar H0"), "\n")
