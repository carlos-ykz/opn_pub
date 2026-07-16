# ══════════════════════════════════════════════════════════════════
# DataMundi — Opinião Pública 2026
# Cruzamento: Interesse em assuntos internacionais (Q2) x Opinião
# sobre os BRICS (Q19)
# ══════════════════════════════════════════════════════════════════
#
# O QUE ESTE SCRIPT FAZ
# Versão simplificada do cruzamento entre Q2 e Q19. Em vez de manter
# as escalas completas de 4 pontos (Nada/Pouco/Interessado/Muito e
# Discordo totalmente/.../Concordo totalmente), colapsamos cada
# variável em DUAS categorias:
#
#   Q2  -> "Interessados" (Muito interessado + Interessado)
#          vs "Pouco/nada interessados" (Pouco interessado + Nada
#          interessado)
#   Q19 -> "Concorda" (Concordo totalmente + Concordo em parte)
#          vs "Discorda" (Discordo totalmente + Discordo em parte)
#
# Isso deixa o gráfico muito mais fácil de ler: uma única pergunta,
# "que % de cada grupo de interesse concorda com cada afirmação sobre
# os BRICS?", respondida com 4 barras (2 afirmações x 2 grupos de
# interesse) em vez de um facet com 16 fatias.
#
# Se depois vocês quiserem voltar para a escala de 4 pontos (para
# mostrar a intensidade da concordância, não só concorda/discorda),
# usem a versão anterior do script como base — a lógica de limpeza
# (Passos 1 a 4) é a mesma, só muda o agrupamento no Passo 5.
# ══════════════════════════════════════════════════════════════════


## PASSO 1 — Carregar os pacotes -------------------------------------------

library(tidyverse)
library(theme.datamundi)


## PASSO 2 — Carregar a base -------------------------------------------------

df <- read.csv("survey2023_v3.csv")


## PASSO 3 — Limpar Q2 e já agrupar em "Interessados" / "Pouco ou nada" -----
#
# Primeiro extraímos o rótulo de texto (removendo o código numérico e
# o " | "), do mesmo jeito que no sample_code.R. Depois usamos
# case_when() para colapsar as 4 categorias da escala em 2 grupos.
# Quem respondeu "NS", "NR" ou uma das categorias espontâneas
# ("Não sigo as notícias", "Indiferente") fica como NA e é descartado
# mais adiante — essas respostas não indicam nem interesse nem
# desinteresse, então não fazem sentido em nenhum dos dois grupos.

df <- df %>%
  rename(interesse = Q2) %>%
  filter(!is.na(id)) %>%
  mutate(
    interesse = ifelse(interesse %in% c("90 | NS", "99 | NR"), NA, interesse),
    interesse_char = gsub("[^[:alpha:] ] ", "", as.character(interesse)),
    interesse_grupo = case_when(
      interesse_char %in% c("Muito interessado", "Interessado") ~ "Interessados",
      interesse_char %in% c("Pouco interessado", "Nada interessado") ~ "Pouco/nada interessados",
      TRUE ~ NA_character_
    )
  )


## PASSO 4 — Limpar Q19a e Q19b, agrupar em "Concorda" / "Discorda", -------
## e JUNTAR as duas em uma variável só --------------------------------------
#
# Mesma ideia do Passo 3: limpamos o texto de cada afirmação e
# colapsamos a escala de 4 pontos em concorda/discorda. Quem ficou
# "Nem concordo nem discordo (ESPONTÂNEA)", NS ou NR vira NA e é
# descartado — não representa nem concordância nem discordância.
# No fim, pivot_longer() junta Q19a e Q19b em uma coluna só
# (afirmacao_brics), permitindo cruzar as duas com Q2 no mesmo
# gráfico.

df2 <- df %>%
  mutate(
    Q19a = ifelse(Q19a %in% c("90 | NS", "99 | NR"), NA, Q19a),
    Q19b = ifelse(Q19b %in% c("90 | NS", "99 | NR"), NA, Q19b),
    Q19a_char = gsub("[^[:alpha:] ] ", "", as.character(Q19a)),
    Q19b_char = gsub("[^[:alpha:] ] ", "", as.character(Q19b)),
    Q19a_grupo = case_when(
      Q19a_char %in% c("Concordo totalmente", "Concordo em parte") ~ "Concorda",
      Q19a_char %in% c("Discordo totalmente", "Discordo em parte") ~ "Discorda",
      TRUE ~ NA_character_
    ),
    Q19b_grupo = case_when(
      Q19b_char %in% c("Concordo totalmente", "Concordo em parte") ~ "Concorda",
      Q19b_char %in% c("Discordo totalmente", "Discordo em parte") ~ "Discorda",
      TRUE ~ NA_character_
    )
  )%>%
  pivot_longer(
    cols = c(Q19a_grupo, Q19b_grupo),
    names_to = "afirmacao_brics",
    values_to = "concordancia_grupo"
  ) %>%
  mutate(
    afirmacao_brics = case_when(
      afirmacao_brics == "Q19a_grupo" ~ "Capaz de equilibrar\no poder mundial",
      afirmacao_brics == "Q19b_grupo" ~ "Aliança\nconstrangedora"
    )
  )


df2 <- df %>%
  mutate(
    Q19a = ifelse(Q19a %in% c("90 | NS", "99 | NR"), NA, Q19a),
    Q19b = ifelse(Q19b %in% c("90 | NS", "99 | NR"), NA, Q19b),
    Q19a_char = gsub("[^[:alpha:] ] ", "", as.character(Q19a)),
    Q19b_char = gsub("[^[:alpha:] ] ", "", as.character(Q19b)),
    Q19a_grupo = case_when(
      Q19a_char %in% c("Concordo totalmente", "Concordo em parte") ~ "Concorda",
      Q19a_char %in% c("Discordo totalmente", "Discordo em parte") ~ "Discorda",
      TRUE ~ NA_character_
    ),
    Q19b_grupo = case_when(
      Q19b_char %in% c("Concordo totalmente", "Concordo em parte") ~ "Concorda",
      Q19b_char %in% c("Discordo totalmente", "Discordo em parte") ~ "Discorda",
      TRUE ~ NA_character_
    )
  ) %>% 
  mutate(agree =ifelse(Q19a_grupo == "Concorda" & Q19b_grupo == "Concorda",
                       "Concorda com ambas", NA),
         agree =ifelse(Q19a_grupo == "Concorda" & Q19b_grupo == "Discorda",
                       "Concorda com equilíbrio", agree),
         agree =ifelse(Q19a_grupo == "Discorda" & Q19b_grupo == "Concorda",
                       "Concorda com constrangimento", agree),
         agree =ifelse(Q19a_grupo == "Discorda" & Q19b_grupo == "Discorda",
                       "Discorda com ambas", agree)) %>%
  mutate(num = 1) %>%
  aggregate(num ~ interesse_grupo + agree, sum, data=.)

df2 %>%
  ggplot(aes(x = interesse_grupo, y = num, fill = agree)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_datamundi(base_size = 16) +
  labs(
    x = "Interesse em assuntos internacionais",
    y = "Concordam com a afirmação sobre os BRICS",
    fill = "Afirmação"
  )
+
  geom_text(
    aes(label = paste0(round(pct_concorda * 100), "%")),
    position = position_dodge(width = 0.9), vjust = -0.5, size = 5, color = "white"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.75))

df2$Q19a_char
## PASSO 5 — Calcular o % que concorda em cada grupo de interesse -----------
#
# Removemos as linhas com NA em qualquer uma das duas variáveis
# (interesse_grupo, concordancia_grupo). n_grupo é o total de
# respondentes válidos em cada combinação de grupo de interesse x
# afirmação — o denominador do percentual.

df_resumo <- df %>%
  filter(!is.na(interesse_grupo), !is.na(concordancia_grupo)) %>%
  group_by(afirmacao_brics, interesse_grupo) %>%
  summarise(
    n_grupo = n(),
    n_concorda = sum(concordancia_grupo == "Concorda"),
    pct_concorda = n_concorda / n_grupo,
    .groups = "drop"
  )


## PASSO 6 — Construir o gráfico ----------------------------------------------
#
# Um gráfico de barras simples: no eixo x, o grupo de interesse; a
# altura da barra é o % que CONCORDA com a afirmação; e uma cor por
# afirmação (barras lado a lado, position = "dodge"). Assim dá para
# comparar, de forma direta, se quem é mais interessado em assuntos
# internacionais concorda mais ou menos com cada visão sobre os
# BRICS.

df_resumo %>%
  ggplot(aes(x = interesse_grupo, y = pct_concorda, fill = afirmacao_brics)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_datamundi(base_size = 16) +
  labs(
    x = "Interesse em assuntos internacionais ",
    y = "Quantidade de pessoas que concordam com a afirmação sobre os BRICS",
    fill = "Afirmação "
  ) +
  geom_text(
    aes(label = paste0(round(pct_concorda * 100), "%")),
    position = position_dodge(width = 0.9), vjust = -0.5, size = 5, color = "white"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.75))
ggsave(
  filename = "Opinião sobre BRICS.png", 
  width = 8,                          
  height = 4,                          
  units = "in",                        
  dpi = 300)

