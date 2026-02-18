#### DESAFIO - AULA 14

# Definindo os parâmetros
n1 = 30  # Tamanho da amostra 1
n2 = 30  # Tamanho da amostra 2
media1 = 40  # Média da população 1
media2 = 44  # Média da população 2
variancia1 = 9  # Variância da população 1
variancia2 = 10  # Variância da população 2
num_pares = 500  # Número de pares de amostras

# Função para realizar o teste
teste = function(){
  # Simulando as amostras
  amostra1 <- rnorm(n1, mean = media1, sd = sqrt(variancia1))
  amostra2 <- rnorm(n2, mean = media2, sd = sqrt(variancia2))
  
  # Teste de igualdade de variâncias
  teste_variancia <- var.test(amostra1, amostra2)
  
  # Se as variâncias forem iguais, realizamos o teste t padrão, caso contrário, o teste t de Welch
  if (teste_variancia$p.value >= 0.05) {
    # Variâncias iguais, usamos o teste t padrão
    teste_media <- t.test(amostra1, amostra2, var.equal = TRUE)
  } else {
    # Variâncias diferentes, usamos o teste t de Welch
    teste_media <- t.test(amostra1, amostra2, var.equal = FALSE)
  }
  
  # Retornando os resultados: p-valor do teste de variâncias e p-valor do teste de médias
  return(c(teste_variancia$p.value, teste_media$p.value))
}

# Simulando para 500 pares de amostras
resultados = replicate(num_pares, teste())

# Contando as decisões
decisoes_variancia = sum(resultados[1, ] < 0.05)  # Teste de variância significativo
decisoes_media = sum(resultados[2, ] < 0.05)  # Teste de média significativo

# Exibindo os resultados
cat("Número de rejeições da hipótese nula para a igualdade de variâncias:", decisoes_variancia, "\n")
cat("Número de rejeições da hipótese nula para a igualdade de médias:", decisoes_media, "\n")
