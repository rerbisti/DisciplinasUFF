# ------------------------------------- #
#         Aula  -  16/09/2025           #
#         MODELOS LINEARES 2            #
# ------------------------------------- #


# ************************** #
#    Exercicio 3 - Lista 1   #
# ************************** #

# Analisando os dados
y = c(2,3,6,7,8,9,10,12,15)
x = c(-1,-1,0,0,0,0,1,1,1)
plot(x,y,xlab='x',ylab='y',cex=2,cex.lab=1.5,lwd=2,cex.axis=1.5,pch=16)

# Estimação por máxima veroximilhança - Método escore
y = c(2,3,6,7,8,9,10,12,15)
x = c(-1,-1,0,0,0,0,1,1,1)
maxite = 10000
X = matrix(NA,nrow=9,ncol=2)
X[,1] = rep(1,9)
X[,2] = x
tol = 10^(-4)
beta_ant  = matrix(NA,nrow=2,ncol=1)
beta_ant[1,1] = 7
beta_ant[2,1] = 5

w = 1/(beta_ant[1,1]+beta_ant[2,1]*x)
W = diag(w)

z = y

beta_novo = (solve(t(X)%*%W%*%X))%*%t(X)%*%W%*%z

for(i in 2:maxite){

beta_ant = beta_novo
        w = 1/(beta_ant[1,1]+beta_ant[2,1]*x)
        W = diag(w)
        z = y
        beta_novo = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
        print(i)

        if(max(abs(beta_novo - beta_ant))<=tol){
             print("Convergiu!")
             print(paste("Iteração:",i))
             print("----------")
             break()
             }

        if (i==maxite){
            print("Não convergiu!")
            }
}

# Estimativas pontuais dos coeficientes
beta_hat = beta_novo

# Valores ajustados
y_hat = NULL
y_hat = beta_hat[1,1]+beta_hat[2,1]*x
y_hat
plot(x,y,cex=2,lwd=2,type="p",xlab='x',ylab='y',pch=16)
points(x,y_hat,lwd=2,col=2,cex=2,cex.lab=1.5,cex.axis=1.5,pch=16)

# Matriz de Informação de Fisher e sua inversa
w = 1/(beta_hat[1,1]+beta_hat[2,1]*x)
W = diag(w)
mfisher = t(X)%*%W%*%X
inv_mfisher = solve(mfisher)

# Intervalo de 95% de confiança para os coeficientes
li_beta1<-beta_hat[1,1]-qnorm(0.975)*sqrt(inv_mfisher[1,1])
ls_beta1<-beta_hat[1,1]+qnorm(0.975)*sqrt(inv_mfisher[1,1])
c(li_beta1,ls_beta1)

li_beta2<-beta_hat[2,1]-qnorm(0.975)*sqrt(inv_mfisher[2,2])
ls_beta2<-beta_hat[2,1]+qnorm(0.975)*sqrt(inv_mfisher[2,2])
c(li_beta2,ls_beta2)


# Deviance
Deviance = 2*sum(y*log(y/y_hat))

# Teste de adequabilidade do modelo: nível de significância 0,05
Deviance
qchisq(0.95,9-2)
1-pchisq(Deviance,9-2)

# Deviance para o modelo nulo
y_barra = mean(y)
Deviance_nula = 2*sum(y*log(y/y_barra))

delta_D = Deviance_nula - Deviance
delta_D
1-pchisq(delta_D,1)


# Resíduos de Pearson
residuo_pearson = (y - y_hat)/sqrt(y_hat)
plot(x,residuo_pearson,lwd=4,xlab='x',ylab='Resíduos')
abline(h=0,col=2)


# ****************** #
#    Comandos do R   #
# ****************** #

# Estimação por máxima veroximilhança - Função glm
y = c(2,3,6,7,8,9,10,12,15)
x = c(-1,-1,0,0,0,0,1,1,1)
MLG1 = glm(y~x,family=poisson(link=identity))
MLG1$coefficients                    # estimativas pontuais dos coeficientes
confint(MLG1,level=0.95)

# Valores ajustados
MLG1$fitted.value     #ou fitted(MLG1)
plot(x,y,cex=2,lwd=2,type="p",xlab='x',ylab='y',pch=16)
points(x,MLG1$fitted.value,lwd=2,col=2,cex=2,cex.lab=1.5,cex.axis=1.5,pch=16)

# Resumo da análise: Estimativas pontuais e estatíticas de teste
summary(MLG1)

# Deviance
Dev = deviance(MLG1) 

# Análise de deviance (comparação de modelos)
anova(MLG1,test="Chisq")

#Preditor Linear
eta_hat = MLG1$linear.predictors   #predict(MLG1)

# Resíduos
residuo = residuals(MLG1,type="pearson")   #residuals(MLG1,type="deviance")

plot(x,residuo,lwd=4,xlab='x',ylab='Resíduos')
abline(h=0,col=2)

# todas as saídas da função
names(MLG1)


###################################################


# ******************************* #
#    Capitulo 5: Exercicio 5.4    #
# ******************************* #

# Os dados abaixo apresentam os tempos de morte, yi, em semanas a partir 
# do diagnóstico e o log10 da contagem inicial de leucócitos, xi, para 
# dezessete pacientes que sofrem de leucemia. 
# (Exemplo retirado de Cox e Snell, 1981.)

y <- c(65,156,100,134,16,108,121,4,39,143,56,26,22,1,1,5,65)
x <- c(3.36,2.88,3.63,3.41,3.78,4.02,4.00,4.23,3.73,3.85,3.97,4.51,
       4.54,5.00,5.00,4.72,5.00)
n <- length(y)
X = matrix(NA,nrow=n,ncol=2)
X[,1] = rep(1,n)
X[,2] = x
plot(x,y,pch=16)

fit <- glm(y ~ x, family=Gamma(link=log))
summary(fit, dispersion = 1)
#summary(fit)



# Matriz de Informação de Fisher e sua inversa
b0 <- fit$coefficients[1] 
b1 <- fit$coefficients[2] 
W = diag(1,n)
mfisher = t(X)%*%W%*%X
inv_mfisher = solve(mfisher)

# Intervalo de 95% de confiança para os coeficientes
li_b0 <- b0-qnorm(0.975)*sqrt(inv_mfisher[1,1])
ls_b0 <- b0+qnorm(0.975)*sqrt(inv_mfisher[1,1])
c(li_b0,ls_b0)

#confint(fit)

# Ajuste do modelo
y_hat <- exp(b0 + b1*x)
plot(x,y,pch=16)
points(x,y_hat,pch=16,col=2)

#fit0 <- glm(y ~ 1, family=Gamma(link=log))
#summary(fit0, dispersion = 1)

# Testando a significância de b1
delta_D = fit$null.deviance - fit$deviance
delta_D
1-pchisq(delta_D,2-1)





