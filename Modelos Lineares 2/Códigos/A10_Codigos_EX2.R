## Modelos Lineares II
## 15/06/2023

## 1. Comparação de modelos
## 2. Regressao logistica Geral
## 3. Métodos de regularização (LASSO)


library(mlbench)
library(tidyverse)
library(caret)
library(glmnet)
library(sAIC)
library(readxl)


## 1) Exercicio do teste antigo

x <- c(5,9,13,17,21,25)
m <- c(500,500,500,500,500,500)
y <- c(100,147,211,277,343,391)
P <- y/m

# Ajustando os tres modelos

fit1 <- glm(cbind(y,m-y)~x, family=binomial(link="logit"))
summary(fit1)

fit2 <- glm(cbind(y,m-y)~x, family=binomial(link="probit"))
summary(fit2)

fit3 <- glm(cbind(y,m-y)~x, family=binomial(link="cloglog"))
summary(fit3)


# probabilidade ajustada de cada modelo 

pi_logit <- fit1$fitted.values
pi_probit <- fit2$fitted.values
pi_cloglog <- fit3$fitted.values

par(mar=c(4,4,0.5,0.5))
plot(x,P, cex=1.2,lwd=2,type="p",xlab='x',ylab=expression(pi),pch=16,col=2)
points(x,fit1$fitted.values, col="blue", pch=16)
points(x,fit2$fitted.values, col="green", pch=16)
points(x,fit3$fitted.values, col="orange", pch=16)
legend("bottomright",legend=c("logito","probito","cloglog"),col=c("blue","green","orange"),pch=16,bty="n")
#lines(y_hat3 ~ x, newdat, col="blue", lwd=2)

# claramente o modelo loglog complementar tem o pior ajuste.

# Avaliando medidas de comparação de modelos:

# estimando o modelo nulo para calcular o pseudo R²
fit.nulo <- glm(cbind(y,m-y)~1, family=binomial(link="logit"))
summary(fit.nulo)


Modelo <- c("Logit","Probit","Cloglog")
deviance <- c(deviance(fit1),deviance(fit2),deviance(fit3))
AIC <- c(fit1$aic,fit2$aic,fit3$aic)
pR2 <- round(c(1-(logLik(fit1)/logLik(fit.nulo)), 1-(logLik(fit2)/logLik(fit.nulo)), 1-(logLik(fit3)/logLik(fit.nulo))),3)

R <- data.frame(Modelo,AIC,Deviance=deviance,"pseudo R²"= pR2)
R


# Como visto no grafico, o modelo clolog tem o pior ajuste. Ele apresentou o maior AIC, a maior
# deviance e o menor pseudo R².
# Os modelos Probito e Logito apresentam, graficamente, ajustes muito parecidos. Entretanto, o modelo
# logito teve leve vantagem. Seu AIC é um pouco menor do que o AIC do probito, sua deviance também e
# o pseudo R² é praticamente o mesmo.
# Logo, escolhemos o modelo logito como o que melhor se ajusta ao conjunto de dados.








##################################################################
##################################################################

# base heart disease.xlsx contem informaçoes de 4170 pacientes 
# de um hospital da Flórida. O objetivo é avaliar a probabilidade 
# de ocorrencia da doença arterial coronariana nos pacientes desse 
# hospital a partir de suas caracteristicas.

# male: indica se o paciente é do sexo masculino (1-masculino; 0-feminino);
# age: idade do paciente (em anos);
# smoker: indica se o paciente é fumante (1-fumante; 0-caso contrario);
# diabetes: indica se o paciente tem diabetes (1-diabético; 0-caso contrario);
# BMI: indice de massa corporal;
# CHD: indica se o paciente tem doença arterial coronariana.

# Lendo a base de dados
base <- read_excel("heart_disease.xlsx")

# Ajustando o modelo
fit <- glm(CHD ~ male +
                 age +
                 smoker +
                 diabetes +
                 BMI, 
                 family=binomial(link = "logit"), data = base)


# Avaliando o ajuste (significância das variáveis, deviance e AIC)
summary(fit)

# Avaliando a adequabilidade do modelo a partir da ANOVA
anova(fit,test = "Chisq")

# Calculuando RC e seu intervalo de confiança de 95%
exp(cbind(OR = coef(fit), confint(fit, level=0.95)))

############################################################################
############################################################################



## Objetivo: prever a probabilidade de ser diabético a partir de diversas 
## variáveis clínicas.

data("PimaIndiansDiabetes2", package = "mlbench")
dados <- na.omit(PimaIndiansDiabetes2)

# recorte dos dados
head(dados, 3)
dim(dados)

#pregnant - Número de vezes que engravidou
#glucose - Concentração de glicose no plasma (teste de tolerância à glicose)
#pressure - pressão arterial diastólica (mm Hg)
#triceps - Espessura da dobra cutânea do tríceps (mm)
#insulin - insulina sérica de 2 horas (mu U / ml)
#mass - Índice de massa corporal (peso em kg / (altura em m) \ ^ 2)
#pedigree - Diabetes Pedigree Function (valores que medem a tendência 
#           ao desenvolvimento de diabetes com base nas relações genéticas 
#           do indivíduo)
#age - Idade (anos)
#diabetes - Variável de classe de diabetes (teste para diabetes)



## Dividindo aleatoriamente os dados em conjunto de treinamento (80% para 
## construir o modelo) e conjunto de teste (20% para avaliar a capacidade 
## preditiva do modelo).

set.seed(100)
amostras.treino <- dados$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)

train.data <- dados[amostras.treino, ]
test.data <- dados[-amostras.treino, ]


# Organizando os dados (essa função já coloca variáveis categóricas como dummies)
x <- model.matrix(diabetes~., train.data)[,-1]

# Transformando as categorias de y em classes numéricas
y <- ifelse(train.data$diabetes == "pos", 1, 0)

# Encontrando o melhor lambda via cross-validation

set.seed(100) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cv.lasso)

cv.lasso$lambda.min          # lambda que minimiza a deviance
#log(cv.lasso$lambda.min)  

# A função cv.glmnet() também encontra o valor de lambda que fornece um modelo 
# mais simples, mas que está dentro de um erro padrão do valor ideal de lambda.

cv.lasso$lambda.1se
#log(cv.lasso$lambda.1se)


coef(cv.lasso, cv.lasso$lambda.min)
coef(cv.lasso, cv.lasso$lambda.1se)


## Comparando a acurácia dos modelos

## Modelo com lambda.min
modelo.1 <- glmnet(x,y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.min)
coef(modelo.1)

AIC.1 <- sAIC(x=x, y=y, beta=coef(modelo.1), family="binomial")
AIC.1

#modelo <- glmnet(x,y, alpha = 1, family = "binomial")
#plot(modelo, xvar = "lambda", label = TRUE)
#abline(v=log(cv.lasso$lambda.min))
#abline(v=log(cv.lasso$lambda.1se))


# Fazendo a previsão para o dado de teste
x.test <- model.matrix(diabetes ~., test.data)[,-1]
prob1 <- exp(modelo.1 %>% predict(newx = x.test))/(1+exp(modelo.1 %>% predict(newx = x.test)))
pred.classes1 <- ifelse(prob1 > 0.5, "pos", "neg")

# Acurácia do modelo
obs.classes <- test.data$diabetes
ac.1 <- mean(pred.classes1 == obs.classes)
ac.1


## Modelo com lambda.1se
modelo.2 <- glmnet(x,y, alpha = 1, family = "binomial",lambda = cv.lasso$lambda.1se)
coef(modelo.2)

AIC.2 <- sAIC(x=x, y=y, beta=coef(modelo.2), family="binomial")
AIC.2

# Fazendo a previsão para o dado de teste
prob2 <- exp(modelo.2 %>% predict(newx = x.test))/(1+exp(modelo.2 %>% predict(newx = x.test)))
pred.classes2 <- ifelse(prob2 > 0.5, "pos", "neg")

# Acurácia do modelo
ac.2 <- mean(pred.classes2 == obs.classes)
ac.2


## Modelo completo (logito)
modelo.3 <- glm(diabetes ~., data = train.data, family = binomial("logit"))
summary(modelo.3)

AIC.3 <- modelo.3$aic

# Fazendo a previsão para o dado de teste
prob3 <- modelo.3 %>% predict(test.data, type = "response")
pred.classes3 <- ifelse(prob3 > 0.5, "pos", "neg")

# Acurácia do modelo
ac.3 <- mean(pred.classes3 == obs.classes)
ac.3


## Modelo completo (probito)
modelo.4 <- glm(diabetes ~., data = train.data, family = binomial("probit"))
summary(modelo.4)

AIC.4 <- modelo.4$aic

# Fazendo a previsão para o dado de teste
prob4 <- modelo.4 %>% predict(test.data, type = "response")
pred.classes4 <- ifelse(prob4 > 0.5, "pos", "neg")

# Acurácia do modelo
ac.4 <- mean(pred.classes4 == obs.classes)
ac.4


## Modelo reduzido
modelo.5 <- glm(diabetes ~ glucose + mass + pedigree, data = train.data, family = binomial("logit"))
summary(modelo.5)

AIC.5 <- modelo.5$aic

# Fazendo a previsão para o dado de teste
prob5 <- modelo.5 %>% predict(test.data, type = "response")
pred.classes5 <- ifelse(prob5 > 0.5, "pos", "neg")

# Acurácia do modelo
ac.5 <- mean(pred.classes5 == obs.classes)
ac.5



resumo <- data.frame(prob1,prob2,prob3,prob4,prob5,pred.classes1,pred.classes2,pred.classes3,pred.classes4,pred.classes5,
                     ifelse(pred.classes1==pred.classes2 & pred.classes1==pred.classes3 &
                              pred.classes2==pred.classes3 & pred.classes1==pred.classes4 &
                              pred.classes1==pred.classes5 & pred.classes2==pred.classes4 &
                              pred.classes2==pred.classes5 & pred.classes3==pred.classes4 &
                              pred.classes3==pred.classes5 & pred.classes4==pred.classes5,0,1),obs.classes)
names(resumo) <- c("p1","p2","p3","p4","p5","class1","class2","class3","class4","class5","acuracia","obs")
head(resumo)

diff <- resumo %>% filter(acuracia==1)
diff

Modelo <- c("M1","M2","M3","M4","M5")
Acuracia <- c(ac.1,ac.2,ac.3,ac.4,ac.5)
deviance <- c(deviance(modelo.1),deviance(modelo.2),deviance(modelo.3),deviance(modelo.4),deviance(modelo.5))
AIC <- c(AIC.1$AIC,AIC.2$AIC,AIC.3,AIC.4,AIC.5)
R <- data.frame(Modelo,AIC,deviance,Acuracia)
R
