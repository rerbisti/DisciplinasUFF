library(Matrix)

# Gerando os dados artificiais
n = 100

x = rnorm(n)
beta0 = 0.5  # valor verdadeiro do beta0
beta1 = 1    # valor verdadeiro do beta1


mu = beta0 + beta1*x

y = rpois(n,exp(mu))

hist(y)

################################################

matX = as.matrix(cbind(1,x))

# valores iniciais (m=0)
b0 = c(0,1)

# passo 1 da iteração (m=1)
wii = exp(matX%*%b0)
matW = diag(as.vector(wii))

z = matX%*%b0 + ((y-exp(matX%*%b0))/exp(matX%*%b0))

b1 = (solve(t(matX)%*%matW%*%matX))%*%(t(matX)%*%matW%*%z)
b1

# passo 2 da iteração (m=2)
wii = exp(matX%*%b1)
matW = diag(as.vector(wii))

z = matX%*%b1 + ((y-exp(matX%*%b1))/exp(matX%*%b1))

b2 = (solve(t(matX)%*%matW%*%matX))%*%(t(matX)%*%matW%*%z)
b2

# passo 3 da iteração (m=3)
wii = exp(matX%*%b2)
matW = diag(as.vector(wii))

z = matX%*%b2 + ((y-exp(matX%*%b2))/exp(matX%*%b2))

b3 = (solve(t(matX)%*%matW%*%matX))%*%(t(matX)%*%matW%*%z)
b3


# passo 4 da iteração (m=4)
wii = exp(matX%*%b3)
matW = diag(as.vector(wii))

z = matX%*%b3 + ((y-exp(matX%*%b3))/exp(matX%*%b3))

b4 = (solve(t(matX)%*%matW%*%matX))%*%(t(matX)%*%matW%*%z)
b4

################################################

fit = glm(y~x, family = poisson())
summary(fit)


