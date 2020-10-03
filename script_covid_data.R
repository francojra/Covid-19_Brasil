# Dados de COVID-19 no Brasil
# Autora: Jeanne Franco

# Pacotes necessários -----------------------------------------------------

remotes::install_github("liibre/coronabr")
library(coronabr) # usado para extrair o banco de dados sobre coronavírus
library(ggplot2) # usado para montar os gráficos
library(tidyverse) # usado para manipular a tabela
library(RColorBrewer) # usado para selecionar palheta de cores
library(devtools) # usado para carregar o pacote ggthemr
devtools::install_github('cttobin/ggthemr')
library(ggthemr) # usado para carregar novos temas do ggplot2
library(grid) # usado unir os gráficos em uma figura
library(cowplot) # usado como auxiliar do pacote grid

# Leitura de dados - Fonte:Brasil.io --------------------------------------

dados_br <- get_corona_br(by_uf = TRUE)
head(dados_br)
summary(dados_br)
str(dados_br)
names(dados_br)
head(dados_br)
tail(dados_br)
View(dados_br)

# Seleção de dados por região ---------------------------------------------

# Norte
estados <- c("AC", "AP", "AM", "RO", "RR", "PA", "TO")
SI_data <- filter(dados_br, state %in% estados)
view(SI_data) # Ver tabela gerada
summary(SI_data) # Para identificar número máximo, mínimo e médio de casos covid-19

# Nordeste
estados <- c("RN", "CE", "BA", "PB", "MA", "PI", "AL", "SE", "PE")
SI_data1 <- filter(dados_br, state %in% estados)
view(SI_data1) 
summary(SI_data1) 

# Centro-Oeste
estados <- c("MS", "DF", "MT", "GO")
SI_data2 <- filter(dados_br, state %in% estados)

# Sudeste
estados <- c("MG", "SP", "RJ", "ES")
SI_data3 <- filter(dados_br, state %in% estados)

# Sul
estados <- c("SC", "PR", "RS")
SI_data4 <- filter(dados_br, state %in% estados)

# Gráficos ggplot2 --------------------------------------------------------

# Norte

ggthemr('earth', type = 'outer') # tema do gráfico

c1 <- ggplot(SI_data, aes(x = date, 
                         y = last_available_confirmed, col = state)) +
  geom_line(size = 1.4) +
  scale_color_brewer(palette = "BrBG", name = "Norte") +
  labs(y = "Casos por 100 mil habitantes",
       x = "Meses") + 
  theme(legend.position = "top",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
c1

# Nordeste
options(scipen = 999) # retira números científicos do gráfico
c2 <- ggplot(SI_data1, aes(x = date, 
                          y = last_available_confirmed, col = state)) +
  geom_line(size = 1.4) +
  scale_color_brewer(palette = "BrBG", name = "Nordeste") +
  labs(y = "Casos por 100 mil habitantes",
       x = "Meses") + 
  theme(legend.position = "top",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
c2

# centro-oeste
c3 <- ggplot(SI_data2, aes(x = date, 
                           y = last_available_confirmed, col = state)) +
  geom_line(size = 1.4) +
  scale_color_brewer(palette = "BrBG", name = "Centro-Oeste") +
  labs(y = "Casos por 100 mil habitantes",
       x = "Meses") + 
  theme(legend.position = "top",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
c3

# Sudeste
c4 <- ggplot(SI_data3, aes(x = date, 
                           y = last_available_confirmed, col = state)) +
  geom_line(size = 1.4) +
  scale_color_brewer(palette = "BrBG", name = "Sudeste") +
  labs(y = "Casos por 100 mil habitantes",
       x = "Meses") + 
  theme(legend.position = "top",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
c4

# Sul
c5 <- ggplot(SI_data4, aes(x = date, 
                           y = last_available_confirmed, col = state)) +
  geom_line(size = 1.4) +
  scale_color_brewer(palette = "BrBG", name = "Sul") +
  labs(y = "Casos por 100 mil habitantes",
       x = "Meses") + 
  theme(legend.position = "top",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))
c5

# Total de morte pela covid-19 por estado
c6 <- ggplot(dados_br, aes(x = reorder(state, -new_deaths),
                           y = new_deaths, fill = state)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, option = "magma") +
  labs(y = "Número atualizado de mortes",
       x = "Estados brasileiros") + 
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(colour = "white", size = 13),
        axis.text.y = element_text(colour = "white", size = 13))
c6

# Gráficos juntos e título
grid.arrange(c1, c2, c3, c4, c5, c6, 
             top = textGrob("Registros de infectados e mortos pela Covid-19 - Fevereiro a outubro de 2020", 
                                gp = gpar(fontface = 3, fontsize = 22)),
             bottom = textGrob("Fonte dos dados: Brail.io - Repositório de dados públicos
                                Autoria: Jeanne Franco (2020)",
                               gp = gpar(fontface = 3, fontsize = 16))) 

