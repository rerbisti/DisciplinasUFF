## Aula 02/12/2025
## Modelos Lineares II


# Exemplo

# Modelos log lineares modelam contagens de células em tabelas de contingência. 
# Eles são um pouco diferentes de outros métodos de modelagem porque não 
# distinguem entre variáveis resposta e explicativas. Todas as variáveis 
# em um modelo log linear são essencialmente "respostas".

# Avaliaremos o conjunto de dados disponível no livro do Agresti (1996). 
# Os dados referem-se a uma pesquisa de 1992 realizada pela Wright State University
# School of Medicine e pelo United Health Services em Dayton, Ohio. A pesquisa 
# perguntou a 2.276 alunos do último ano do ensino médio em uma área não urbana 
# perto de Dayton, Ohio, se eles já haviam usado álcool, cigarros ou maconha.

# Neste caso, no interesse está em avaliar como a contagem de células na tabela 
# depende dos níveis das variáveis categóricas. Se estivéssemos interessados em 
# modelar, por exemplo, o uso de maconha como uma função do uso de álcool e cigarro, 
# poderíamos realizar uma regressão logística. Mas se estamos interessados em 
# compreender a relação entre todas as três variáveis sem que uma seja 
# necessariamente a "resposta", então podemos tentar um modelo log linear.

# Construindo a tabela com os dados
alunos <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), 
                 dim = c(2,2,2), 
                 dimnames = list("cigarro" = c("sim","nao"),
                                 "maconha" = c("sim","nao"),
                                 "alcool" = c("sim","nao")))
# Tabela disponibilizada com os dados
ftable(alunos, row.vars = c("alcool","cigarro"))

addmargins(alunos)

prop.table(alunos, margin = c(1,3))

# Base de dados no formato para o glm (empilhada)
df <- as.data.frame(as.table(alunos))
df[,-4] <- lapply(df[,-4], relevel, ref = "nao")
df


# Modelo 1: log(mu_jkl)= mu + c_j + m_k + a_l  (modelo aditivo supondo independencia)

fit.1 <- glm(Freq ~ cigarro + maconha + alcool, family = poisson(link="log"),
             data = df)
summary(fit.1)

# Os coeficientes neste modelo podem ser interpretados como chance se os 
# exponenciarmos. Por exemplo, se exponenciarmos o coeficiente para maconha, 
# obteremos:

exp(coef(fit.1)[3])

# Essa é a chance de usar maconha, uma vez que "não" é a categoria de base. 
# Ou seja, a chance de usar maconha é de cerca de 0,73 para 1, independentemente 
# do aluno ter experimentado álcool ou cigarro.

# Esse modelo não é adequado, pois parece existir associação entre as variaveis.
# Podemos verificar isso, avaliando os valores observados versus os valores ajustados.

cbind(fit.1$data, fit.1 = round(fitted(fit.1),0))


# Modelo 2: log(mu_jkl)= mu + c_j + m_k + a_l + cm_jk + ca_jl + ma_kl
fit.2 <- glm(Freq ~ cigarro + maconha + alcool + cigarro*maconha + cigarro*alcool + 
            maconha*alcool,family = poisson(link="log"),data = df)
summary(fit.2)


cbind(fit.1$data, fit1=round(fitted(fit.1),0), fit2=round(fitted(fit.2),0))

# O modelo parece se ajustar, mas como podemos descrever a associação entre as 
# variáveis? Que efeito as variáveis têm umas sobre as outras? 
# Basta analisar os coeficientes das interações. 
# Vamos exponenciar esses coeficientes, obtendo, assim, a razao de chances

exp(coef(fit.2)[7])

# Interpretando: os alunos que experimentaram maconha tem chances de terem experimentado 
# álcool 19 vezes mais do que os alunos que não experimentaram a maconha. 

# Obs: Como ajustamos um modelo de associação homogêneo, a razão de chances é 
# a mesma, independentemente de eles terem ou não experimentado cigarros.


exp(cbind(OR = coef(fit.2)[5:7], confint(fit.2,parm = 5:7, level=0.95)))

# Associações muito fortes: por exemplo, alunos que fumaram tem chance, pelo menos,
# 12 vezes maior de experimentar maconha do que alunos que não fumaram.



# E se ajustarmos o modelo com três interações (MODELO SATURADO)?
# Isso permite que as associações de pares mudem de acordo com o nível da 
# terceira variável.

# Modelo 3: log(mu_jkl)= mu + c_j + m_k + a_l + cm_jk + ca_jl + ma_kl + cma_jkl


fit.3 <- glm(Freq ~ cigarro + maconha + alcool + cigarro*maconha + cigarro*alcool + 
               maconha*alcool + cigarro*maconha*alcool,family = poisson(link="log"),data = df)
summary(fit.3)


cbind(fit.1$data, Ind=round(fitted(fit.1),0), Homogeneo=round(fitted(fit.2),0),
      Saturado=fitted(fit.3))


# A deviance é 0.
# Sempre preferimos o modelo mais simples. 
# Geralmente, não queremos terminar com um modelo saturado que se encaixa 
# perfeitamente em nossos dados.

# No entanto, é útil ajustar um modelo saturado para verificar se a interação 
# de ordem superior é estatisticamente significativa. 

# Podemos verificar que o modelo de associação homogênea se ajusta tão bem 
# quanto o modelo saturado

# H0: modelo 2 se ajusta tão bem quanto o modelo saturado
# H1: modelo saturado é melhor

anova(fit.2, fit.3, test="Chisq")







