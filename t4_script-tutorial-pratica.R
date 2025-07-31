# Instalacao dos pacotes necessarios
install.packages("tidyverse") #manipulacao e visualizacao de dados
install.packages("broom") #formatacao de resultados

# Carregando os pacotes
library(tidyverse)
library(broom)

# Definicao do tamanho amostral por grupo (Vacina Nova, Antiga 1 e Antiga 2)
n <- 1500


#SUPERIORIDADE#####

# Funcao para gerar valores de titulos 
gerar_gmt_seed <- function(n, meanlog, sdlog = 1, seed) {
  set.seed(seed)
  valores <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
  valores_truncados <- pmin(pmax(valores, 9), 3779)
  round(valores_truncados, 0)
}

# Simulacao de dados para três grupos de vacina
grupo1 <- data.frame(
  ID = 1:n,
  Vacina = rep("Vacina Nova", n),
  titulo_pos = gerar_gmt_seed(n, meanlog = 5.1, seed = 5570)
)

grupo2 <- data.frame(
  ID = (n + 1):(2 * n),
  Vacina = rep("Vacina Antiga 1", n),
  titulo_pos = gerar_gmt_seed(n, meanlog = 4.7, seed = 5571)
)

grupo3 <- data.frame(
  ID = (2 * n + 1):(3 * n),
  Vacina = rep("Vacina Antiga 2", n),
  titulo_pos = gerar_gmt_seed(n, meanlog = 5.07, seed = 5572)
)

#Criacao do Banco Tutorial contendo as informacoes das 3 vacinas
banco_tutorial <- bind_rows(grupo1, grupo2, grupo3)
banco_tutorial$ln_titulo_pos <- log(banco_tutorial$titulo_pos) #Aplicacao da transformacao logaritmica nos titulos

# Histograma dos titulos de anticorpos por vacina recebida
hist1 <- ggplot(banco_tutorial, aes(x = titulo_pos, fill = Vacina)) +
  geom_histogram(binwidth = 200, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ Vacina) +
  scale_y_continuous(limits = c(0, 700))+
  labs(
    title = "Distribuição dos títulos de anticorpos pós vacinação por grupo de vacina",
    x = "Titulo de anticorpo pós 30 dias de vacinação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
hist1

# Histograma dos ln dos titulos de anticorpos por vacina recebida
hist2 <- ggplot(banco_tutorial, aes(x = ln_titulo_pos, fill = Vacina)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ Vacina) +
  scale_y_continuous(limits = c(0, 150))+
  labs(
    title = "Distribuição do ln dos títulos de anticorpos pós vacinação por grupo de vacina",
    x = "Ln do titulo de anticorpo pós 30 dias de vacinação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
hist2


# Tabela com médias e IC95% dos títulos de anticorpos por grupo de vacina
tabela_descritiva1 <- banco_tutorial %>%
  group_by(Vacina) %>%
  summarise(
    resultado = list(t.test(ln_titulo_pos)),
    .groups = "drop"
  ) %>%
  mutate(tidy_resultado = map(resultado, broom::tidy)) %>%
  unnest(tidy_resultado) %>%
  transmute(
    Vacina,
    media = formatC(exp(estimate), format = "f", digits = 2),
    `IC95%` = paste0("[", formatC(exp(conf.low), format = "f", digits = 2), 
                     " – ", formatC(exp(conf.high), format = "f", digits = 2), "]")
  )

print(tabela_descritiva1)


# Razao das GMT e IC95% por grupo de vacina
rgmt_ant1 <- t.test(
  banco_tutorial$ln_titulo_pos[banco_tutorial$Vacina == "Vacina Nova"] -
    banco_tutorial$ln_titulo_pos[banco_tutorial$Vacina == "Vacina Antiga 1"]
)

rgmt_ant2 <- t.test(
  banco_tutorial$ln_titulo_pos[banco_tutorial$Vacina == "Vacina Nova"] -
    banco_tutorial$ln_titulo_pos[banco_tutorial$Vacina == "Vacina Antiga 2"]
)

tab_rgmt <- data.frame(
  Comparação = c("Nova vs Antiga 1", "Nova vs Antiga 2"),
  rGMT = round(exp(c(rgmt_ant1$estimate, rgmt_ant2$estimate)),2),
  IC_inf = round(exp(c(rgmt_ant1$conf.int[1], rgmt_ant2$conf.int[1])),2),
  IC_sup = round(exp(c(rgmt_ant1$conf.int[2], rgmt_ant2$conf.int[2])),2)
)

print(tab_rgmt)

# Gráfico da rGMT das vacinas
graf1<-ggplot(tab_rgmt, aes(y = Comparação, x = rGMT)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgreen") +
  #geom_vline(xintercept = 2/3, linetype = "dashed", color = "red") +
  #geom_vline(xintercept = 3/2, linetype = "dashed", color = "red") +
  scale_x_continuous(
    limits = c(0.4, 1.6),
    breaks = seq(0.4, 1.6, by = 0.2)
  ) +
  labs(
    title = "Análise de Superioridade - Razão das Médias Geométricas (rGMT)",
    x = "rGMT",
    y = ""
  ) +
  theme_minimal()
graf1


#EQUIVALENCIA####
# Simulacao de alocacao dos individuos da "Vacina Nova" em 3 lotes

set.seed(64)
lotes_nova <- sample(rep(c("Lote A", "Lote B", "Lote C"), each = 500))

banco_tutorial$lote <- NA_character_
banco_tutorial$lote[banco_tutorial$Vacina == "Vacina Nova"] <- lotes_nova

# Histograma dos titulos por Lote
hist3 <- ggplot(filter(banco_tutorial, !is.na(lote)), aes(x = titulo_pos, fill = lote)) +
  geom_histogram(binwidth = 200, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ lote) +
  scale_y_continuous(limits = c(0, 250))+
    labs(
    title = "Distribuição dos títulos de anticorpos pós vacinação por lote",
    x = "Titulo de aticorpo pós 30 dias de vacinação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

hist3


# Histograma dos logaritmos dos titulos por Lote
hist4 <- ggplot(filter(banco_tutorial, !is.na(lote)), aes(x = ln_titulo_pos, fill = lote)) +
  geom_histogram(binwidth = 0.2, color = "black", alpha = 0.7, position = "identity") +
  facet_wrap(~ lote) +
  scale_y_continuous(limits = c(0, 50))+
    labs(
    title = "Distribuição dos ln dos títulos de anticorpos pós vacinação por lote",
    x = "Ln dos titulos de aticorpos pós 30 dias de vacinação",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

hist4

# Tabela com médias dos titulos e IC95% por Lote
tabela_descritiva2 <- filter(banco_tutorial, !is.na(lote)) %>%
  group_by(lote) %>%
  summarise(
    resultado = list(t.test(ln_titulo_pos)),
    .groups = "drop"
  ) %>%
  mutate(tidy_resultado = map(resultado, broom::tidy)) %>%
  unnest(tidy_resultado) %>%
  transmute(
    lote,
    media = formatC(exp(estimate), format = "f", digits = 2),
    `IC95%` = paste0("[", formatC(exp(conf.low), format = "f", digits = 2), 
                     " – ", formatC(exp(conf.high), format = "f", digits = 2), "]")
  )

print(tabela_descritiva2)


# tabela do rGMT e IC95% por lote
rgmt_lote_ab <- t.test(
  banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote A"] -
    banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote B"]
)

rgmt_lote_ac <- t.test(
  banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote A"] -
    banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote C"]
)

rgmt_lote_bc <- t.test(
  banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote B"] -
    banco_tutorial$ln_titulo_pos[banco_tutorial$lote == "Lote C"]
)


tab_rgmt_lote <- data.frame(
  Comparação = c("Lote A vs Lote B", "Lote A vs Lote C", "Lote B vs Lote C"),
  rGMT = round(exp(c(rgmt_lote_ab$estimate, rgmt_lote_ac$estimate, rgmt_lote_bc$estimate)),2),
  IC_inf = round(exp(c(rgmt_lote_ab$conf.int[1], rgmt_lote_ac$conf.int[1], rgmt_lote_bc$conf.int[1])),2),
  IC_sup = round(exp(c(rgmt_lote_ab$conf.int[2], rgmt_lote_ac$conf.int[2], rgmt_lote_bc$conf.int[2])),2)
)

print(tab_rgmt_lote)


# Gráfico da rGMT por lote
graf2<-ggplot(tab_rgmt_lote, aes(y = Comparação, x = rGMT)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = 2/3, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 3/2, linetype = "dashed", color = "red") +
  scale_x_continuous(
    limits = c(0.4, 1.6),
    breaks = seq(0.4, 1.6, by = 0.2)
  ) +
  labs(
    title = "Análise de Equivalencia - Razão das Médias Geométricas (rGMT)",
    x = "rGMT",
    y = ""
  ) +
  theme_minimal()
graf2

