## Aula síncrona 04/12/2025
## Modelos Lineares II

library(tidyverse)
library(gridExtra)
library(readxl)


############################################################

## Exemplo 1

# Este conjunto de dados contém informações de todos os Pokémon disponíveis 
# no Pokémon GO. Cada linha corresponde a um Pokémon na ordem Pokédex, e as 
# variável são: nome, número na Pokédex, resistência base, ataque base, defesa base,
# tipo primário, tipo secundário, pontos máximos de vida, taxa de captura e fuga, 
# peso em quilogramas, altura em metros, máximo de Pontos de Combate no nível 
# de treinador 40 e a geração à qual o Pokémon pertence.


poke <- read_csv("pogo.csv") %>%
        filter(Primary != "Flying")

poke$Primary <- as.factor(poke$Primary)
poke$Legendary <- as.factor(poke$Legendary)

unique(poke$Primary)

# Analise dos dados

hist(poke$Attack)

boxplot(poke$Attack~poke$Primary)
summary(poke$Primary)


boxplot(poke$Attack~poke$Legendary)
summary(poke$Legendary)


plot(poke$Capture_rate,poke$Attack,col = as.factor(poke$Primary),pch=16,xlab="captura",ylab="ataque")
#legend("topleft",legend=unique(dados$metodo),col=1:4,pch=16,bty="n")

plot(poke$Weight,poke$Attack,col = as.factor(poke$Primary),pch=16,xlab="peso",ylab="ataque")
#legend("topleft",legend=unique(dados$metodo),col=1:4,pch=16,bty="n")


n <- dim(poke)[1]

# Modelo: E(Yjkl)= mu + alpha_j + theta_k + gamma*cr_jkl + beta*w_jkl 

fit.1 <- glm(Attack ~ Primary + Legendary + Capture_rate + log(Weight), family="gaussian", data=poke)
summary(fit.1)




####################################################

## Exemplo 2

# Os dados referem-se a quantidade de pessoas vivas após 50 anos de sua graduação.


ano <- 1938:1947
sexo <- c("F","M")
s_arts <- c(14,11,15,15,8,13,18,18,1,13,16,13,11,12,8,11,4,4,0,13)
s_science <- c(1,4,6,3,4,8,5,16,1,10,9,9,12,12,20,16,25,32,4,25)
t_arts <- c(19,16,18,21,9,13,22,22,1,16,30,22,25,14,12,20,10,12,1,23)
t_science <- c(1,4,7,3,4,9,5,17,1,10,14,12,19,15,28,21,31,38,5,31)


df <- data.frame(S=c(s_arts,s_science),
                 Tot=c(t_arts,t_science),
                 Sexo=rep(rep(sexo,each=10),times=2),
                 Ano=rep(ano,times=4),
                 curso  = rep(c("Arts","Science"),each=20))
df


boxplot(df$S/df$Tot ~ df$Sexo)
boxplot(df$S/df$Tot ~ df$curso)

# Modelo
fit.2 <- glm(cbind(S,Tot-S) ~ Sexo + curso, family = binomial(link="logit"), data = df)
summary(fit.2)


# Calculuando RC e seu intervalo de confiança de 95%
exp(cbind(OR = coef(fit.2), confint(fit.2, level=0.95)))

# Y ajustado
prop_ajustada <- fit.2$fitted.values
df$y_ajustado <- df$Tot * prop_ajustada

plot(df$y_ajustado,df$S,pch=16,xlab="y ajustado", ylab="y verdadeiro")
abline(coef = c(0,1),col=2,lwd=2)


# analise dos residuos
res <- rstandard(fit.2, type = "pearson") 

D.1 <- tibble(y.hat = df$y_ajustado, cov = df$Sexo, r = res)

g.1 = ggplot(D.1, aes(x = y.hat, y = r)) +
  geom_point(size=2) +
  labs(x='semana',y='resíduos')+
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank()) + 
  geom_hline(aes(yintercept = 0), col="gray60") + 
  geom_hline(aes(yintercept = -2), col="gray20",lty=2) + 
  geom_hline(aes(yintercept = 2), col="gray20",lty=2)


g.2 = ggplot(D.1,aes(sample=r)) + 
  stat_qq() +
  stat_qq_line(col="gray60", lwd=1.1) +
  labs(x='quantis teóricos',y='quantis amostrais') +
  theme_bw() + 
  theme(plot.title=element_text(hjust=0.5),
        panel.border = element_blank())

grid.arrange(g.1,g.2,ncol=2)



######################################################################

## Exemplo 3

# Conjunto de dados derivado de dados anônimos produzidos como parte de um 
# estudo da London School of Hygiene & Tropical Medicine para medir associação
# entre a morte, o consumo de alcool e o uso de drogas.

# death (0 = Não, 1 = Sim)
# alcohol (1 = Baixo consumo de álcool, 2 = Alto consumo de álcool)
# drug.use (0 = Nenhum uso de drogas ilícitas, 1 = Usa drogas ilícitas)


base <- read_excel("clinical.xlsx") %>%
        group_by(drug.use, death, alcohol) %>%
        summarise(Freq=n())
base

mytable <- xtabs(Freq ~ drug.use+death+alcohol, data=base)
mytable

addmargins(mytable)

prop.table(mytable, margin = c(1,3))


# Modelo A: log(mu_jkl)= mu + d_j + m_k + a_l  (modelo aditivo supondo independencia)

fit.a <- glm(Freq ~ drug.use + death + as.factor(alcohol), family = poisson(link="log"),
             data = base)
summary(fit.a)


# Modelo B: log(mu_jk)= mu + m_j + a_k 

fit.b <- glm(Freq ~ death + alcohol, family = poisson(link="log"),
             data = base)
summary(fit.b)

# Razao
exp(coef(fit.b)[2])

# Essa é a chance de morrer, uma vez que "não" é a categoria de base. 
# Ou seja, a chance de morrer é de cerca de 0,3 para 1, independentemente 
# de consumir álcool ou não.

# Esse modelo não é adequado, pois parece existir associação entre as variaveis.

# Valores observados versus os valores ajustados.
cbind(fit.b$data, fit.b = round(fitted(fit.b),0))


# Modelo C: log(mu_jk)= mu + m_j + a_k + ma_jk

fit.c <- glm(Freq ~ alcohol*death, family = poisson(link="log"), data = base)
summary(fit.b)


cbind(fit.c$data, fit.b=round(fitted(fit.b),0), fit.c=round(fitted(fit.c),0))

# O modelo parece se ajustar bem, melhor do que o Modelo B,

# Avaliando os efeitos que as variáveis têm umas sobre as outras
round(exp(cbind(OR = coef(fit.c), confint(fit.c,level=0.95))),3)


# Interpretando: as pessoas que consumiram alcool tem chances de morrer
# 2.5 vezes mais do que as as pessoas que não fizeram o consumo de alcool. 

