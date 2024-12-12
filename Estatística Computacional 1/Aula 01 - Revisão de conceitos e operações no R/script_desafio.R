library(tidyverse)
library(readxl)

# Lendo o arquivo
base = read_excel('desafio_1.xlsx')
base

# Construindo os labels dos estados
base = base |>
        mutate(UF = ifelse(SG_UF_NOT == 31, 'Minas Gerais',
                    ifelse(SG_UF_NOT == 32, 'Espírito Santo',
                    ifelse(SG_UF_NOT == 33, 'Rio de Janeiro', 'São Paulo')))) |>
        filter(! UF %in% c('Minas Gerais','Espírito Santo'))

# Agregando o dados por UF, SEMANA e SEXO
base.new = base |>
            group_by(SEM_NOT, UF, CS_SEXO) |>
            summarise(casos = n())

# Gerando a visualização
g1 = base.new |>
      ggplot(aes(x = CS_SEXO,y = casos, fill = CS_SEXO)) +
      geom_boxplot(alpha=0.5, 
                   outlier.colour = 'gray60',
                   outlier.size = 2,
                   na.rm=TRUE) + 
      labs(x="Sexo",
           y="casos de dengue") +
      theme_bw() +
      theme(axis.text.x = element_text(hjust = 0.5, vjust = 0.5, size = 9),
            axis.text.y = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.title.y = element_text(size=12),
            panel.border = element_blank(),
            legend.position = 'none') + 
      facet_wrap(~UF)
g1  

Comentario: observe que os casos em São Paulo são mais frequentes do que no Rio de Janeiro. De fato, a população em SP é maior, 
o ideal seria refazer esse grafico plotando a quantidade de casos corrigido pelo tamanho populacional. Entretando, parece não haver 
diferenças, dentro de um mesmo estado, entre os sexos. Tanto no Rio quanto em SP o valor mediano de casos no ano de 2019 é bastante 
similar. Observe que em algumas semanas há um pico de casos que, neste gráfico, são registradas como outliers.
