library(tidyverse)


# Criando o data frame
dados <- data.frame(
  Carbo = c(33, 40, 37, 27, 30, 43, 34, 48, 30, 38, 50, 51, 30, 36, 41, 42, 46, 24, 35, 37),
  Age = c(33, 47, 49, 35, 46, 52, 62, 23, 32, 42, 31, 61, 63, 40, 50, 64, 56, 61, 48, 28),
  Peso = c(100, 92, 135, 144, 140, 101, 95, 101, 98, 105, 108, 85, 130, 127, 109, 107, 117, 100, 118, 102),
  Proteina = c(14, 15, 18, 12, 15, 15, 14, 17, 15, 14, 17, 19, 19, 20, 15, 16, 18, 13, 18, 14))

# Visualizar os dados
y = dados$Carbo
n = 20
q = 3
p = 4

X0 = as.matrix(cbind(1,dados[,3:4]))
beta0 = c(33.13032,-0.22165,1.82429)

X1 = as.matrix(cbind(1,dados[,2:4]))
beta1 = c(36.96006,-0.11368,-0.22802,1.95771)

num_F = ((beta1%*%t(X1)%*%y) - (beta0%*%t(X0)%*%y))/(p-q)
num_F

den_F = (t(y)%*%y - beta1%*%t(X1)%*%y)/(n-p)
den_F

est_F = num_F/den_F
est_F

fit0 = lm(Carbo ~ Peso + Proteina,data = dados)
summary(fit0)
fit1 = lm(Carbo ~ Age + Peso + Proteina, data = dados)
summary(fit1)

anova(fit0,fit1)






