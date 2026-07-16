# ══════════════════════════════════════════════════════════════════
# DataMundi — Opinião Pública 2026
# Recorte: BRICS — a ambivalência brasileira (Q19a x Q19b)
# ══════════════════════════════════════════════════════════════════
#
# O QUE ESTE SCRIPT FAZ
# A Q19 tem duas frases sobre o BRICS:
#   Q19a: "Os BRICS são uma força capaz de equilibrar o poder mundial
#          diante das potências tradicionais como os EUA, a Alemanha
#          e o Japão."
#   Q19b: "Os BRICS são uma aliança constrangedora, com países
#          autoritários como a Rússia e a China, ou que apresentam
#          altos níveis de exclusão social como a Índia e a África
#          do Sul."
#
# Mostrar as duas separadas só revela que cada uma tem ~47% de
# concordância — não mostra o achado interessante. O achado é que,
# na maioria dos casos, é A MESMA PESSOA concordando com as duas
# frases contraditórias ao mesmo tempo, não dois grupos diferentes
# de tamanho parecido. Pra ver isso, cruzamos Q19a e Q19b por
# entrevistado e criamos 4 "perfis de opinião". É esse cruzamento
# (Passo 4) que é diferente do sample_code.R original — lá cada
# pergunta era tratada sozinha; aqui duas perguntas viram uma
# variável só.
# ══════════════════════════════════════════════════════════════════


## PASSO 1 — Carregar os pacotes -------------------------------------------

library(tidyverse)
library(theme.datamundi)


## PASSO 2 — Carregar a base -------------------------------------------------

df <- read.csv("survey2023_v3.csv")


## PASSO 3 — Limpar e recodificar as duas variáveis --------------------------
#
# Como vamos repetir a mesma limpeza duas vezes (uma pra Q19a, outra
# pra Q19b), criamos uma função auxiliar em vez de copiar e colar o
# código. Ela classifica cada resposta em:
#   "Concorda"  -> concordo totalmente OU concordo em parte
#   "Discorda"  -> discordo totalmente OU discordo em parte
#   NA          -> NS, NR ou "Nem concordo nem discordo" (ESPONTÂNEA)
#     (assim como no sample_code.R original, essas respostas saem da
#     análise, mas continuam contando no denominador do Passo 6)

concorda_discorda <- function(x) {
  case_when(
    x %in% c("1 | Concordo totalmente", "2 | Concordo em parte") ~ "Concorda",
    x %in% c("4 | Discordo em parte", "5 | Discordo totalmente")  ~ "Discorda",
    TRUE ~ NA_character_
  )
}

df <- df %>%
  filter(!is.na(id)) %>%
  mutate(
    brics_equilibra      = concorda_discorda(Q19a),  # "BRICS equilibra o poder mundial"
    brics_constrangedora = concorda_discorda(Q19b)   # "BRICS é uma aliança constrangedora"
  )


## PASSO 4 — Cruzar as duas variáveis num "perfil de opinião" ----------------
#
# Este é o passo central do recorte. Em vez de olhar Q19a e Q19b
# separadamente, olhamos a COMBINAÇÃO das duas respostas de cada
# entrevistado, o que dá 4 perfis possíveis:

df <- df %>%
  mutate(
    perfil_brics = case_when(
      brics_equilibra == "Concorda" & brics_constrangedora == "Concorda" ~ "Concorda com as duas",
      brics_equilibra == "Discorda" & brics_constrangedora == "Concorda" ~ "Só concorda: é constrangedora",
      brics_equilibra == "Concorda" & brics_constrangedora == "Discorda" ~ "Só concorda: equilibra o poder",
      brics_equilibra == "Discorda" & brics_constrangedora == "Discorda" ~ "Discorda das duas",
      TRUE ~ NA_character_  # NS/NR/espontânea em pelo menos uma das duas
    )
  )


## PASSO 5 — Definir a ordem lógica das categorias ---------------------------
#
# Ordenamos da mais ambivalente (concorda com as duas frases opostas)
# até a mais "coerente e crítica" (discorda das duas), passando
# pelas duas posições parciais no meio. É essa ordem que faz o
# gráfico contar a história sozinho: a barra da ambivalência aparece
# primeiro e, sozinha, já é maior que qualquer posição coerente.

ordem_perfil <- c(
  "Concorda com as duas",
  "Só concorda: é constrangedora",
  "Só concorda: equilibra o poder",
  "Discorda das duas"
)


## PASSO 6 — Calcular frequências e percentuais ------------------------------
#
# n_total é a base inteira válida (igual ao sample_code.R original) —
# não só quem entrou numa das 4 categorias. Por isso os percentuais
# das barras não vão somar 100%: sobra o pedaço de quem teve NS/NR/
# espontânea em pelo menos uma das duas perguntas (~20% da amostra).

n_total <- nrow(df)

df_resumo <- df %>%
  filter(!is.na(perfil_brics)) %>%
  mutate(perfil_brics = factor(perfil_brics, levels = ordem_perfil)) %>%
  group_by(perfil_brics) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = n / n_total)


## PASSO 7 — Construir o gráfico ----------------------------------------------

df_resumo %>%
  ggplot(aes(x = perfil_brics, y = n)) +
  geom_bar(stat = "identity") +
  theme_datamundi(base_size = 20) +
  labs(
    x = "Opinião sobre o BRICS: força de equilíbrio, aliança constrangedora, as duas coisas, ou nenhuma",
    y = "Número de respondentes"
  ) +
  geom_text(
    aes(label = paste0(round(pct * 100, 1), "%")),
    vjust = 1.5, size = 8, color = "white"
  )
ggsave(
  filename = "Opinião sobre BRICS_amb.png", 
  width = 8,                          
  height = 4,                          
  units = "in",                        
  dpi = 300)

# ══════════════════════════════════════════════════════════════════
# RESULTADO ESPERADO (validado em Python contra os dados atuais, N=1601):
#   Concorda com as duas             ~26,9%  <- maior barra
#   Só concorda: é constrangedora    ~18,4%
#   Só concorda: equilibra o poder   ~17,8%

#   Discorda das duas                ~16,6%
#   (~20% fica de fora do gráfico por NS/NR/espontânea em uma das
#    duas perguntas, mas continua contando no denominador do pct)
#
# LEGENDA SUGERIDA PRO POST:
# "O perfil mais comum não é quem só vê o BRICS como avanço, nem
# quem só vê como problema — é quem concorda com as duas coisas ao
# mesmo tempo."
# ══════════════════════════════════════════════════════════════════

