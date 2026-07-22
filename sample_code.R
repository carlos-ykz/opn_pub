# ══════════════════════════════════════════════════════════════════
# DataMundi — Opinião Pública 2026
# Script de exemplo: da base de dados bruta a um gráfico de barras
# ══════════════════════════════════════════════════════════════════
#
# O QUE ESTE SCRIPT FAZ
# Usa uma pergunta do survey (Q14_1: "O(a) senhor(a) concorda ou
# discorda que, para aumentar sua influência no mundo, o Brasil
# utilize o poder militar?") para mostrar o caminho completo de uma
# análise: carregar dados -> limpar -> calcular frequências -> plotar.
#
# Esse mesmo caminho (limpar -> contar -> %  -> gráfico) é o que
# vocês vão repetir para QUALQUER outra pergunta do questionário.
# Entendendo este script, vocês sabem replicar para as demais.
# ══════════════════════════════════════════════════════════════════


## PASSO 1 — Carregar os pacotes -------------------------------------------
#
# tidyverse: conjunto de pacotes para manipulação (dplyr) e
#   visualização (ggplot2) de dados. É o que usamos para os verbos
#   encadeados com %>% (ex.: filter, mutate, group_by).
# theme.datamundi: pacote interno do lab com o tema visual (cores,
#   fontes) que usamos em todos os gráficos do DataMundi, para manter
#   a identidade visual consistente entre os posts e artigos.
# Para instalar o pacote do lab, é só rodar:
# remotes::install_github("datamundi-lab/theme.datamundi")
# lembrando que precisa ter o pacote "remotes" para instalar.

library(tidyverse)
library(theme.datamundi)


## PASSO 2 — Carregar a base e entender o formato bruto ---------------------
#
# Cada pergunta de múltipla escolha do survey vem codificada como
# TEXTO no formato "<código numérico> | <rótulo>", por exemplo:
#   "1 | Concordo totalmente"
#   "90 | NS"           (NS = Não Sabe)
#   "99 | NR"           (NR = Não Respondeu)
# Isso é útil porque preserva tanto o código numérico (para ordenar
# ou tratar como escala) quanto o texto legível (para os gráficos).
# Mas precisamos separar essas duas informações antes de analisar —
# é isso que fazemos no PASSO 3.

df <- read.csv("survey2023_v3.csv")


## PASSO 3 — Limpar e recodificar a variável de interesse --------------------
#
# Vamos transformar Q14_1 (nome técnico da pergunta sobre poder
# militar) em uma variável chamada "militar", em três sub-passos:
#
#   3a) renomear a coluna para um nome que faça sentido;
#   3b) remover entrevistas sem "id" (questionários inválidos/vazios);
#   3c) tratar "NS" e "NR" como dados ausentes (NA) — eles não
#       representam uma opinião, então não devem entrar no gráfico;
#   3d) separar o texto "<código> | <rótulo>" em duas versões da
#       mesma variável: uma só com o rótulo (texto) e outra só com o
#       código (número), cada uma útil para um tipo de análise.

df <- df %>%
  rename(militar = Q14_1) %>%
  filter(!is.na(id)) %>%
  mutate(
    # 3c) "90 | NS" e "99 | NR" viram NA (ausência de resposta válida)
    militar = ifelse(militar %in% c("90 | NS", "99 | NR", "90 | NR"), NA, militar),

    # 3d-i) versão em TEXTO: remove o código e o " | ", sobra só o rótulo
    #   ex.: "1 | Concordo totalmente" -> "Concordo totalmente"
    militar_char = gsub("[^[:alpha:] ] ", "", as.character(militar)),

    # 3d-ii) versão NUMÉRICA: extrai só os dígitos do código e
    #   subtrai 1 para a escala começar em 0 (útil em alguns modelos
    #   estatísticos). Fica como factor porque, aqui, o número
    #   representa uma categoria (escala Likert), não uma quantidade.
    militar_n = as.factor(
      as.numeric(gsub("[^0-9]", "", as.character(militar))) - 1
    )
  )


## PASSO 4 — Definir a ordem lógica das categorias ---------------------------
#
# Por padrão, o R ordenaria essas categorias alfabeticamente, o que
# não faz sentido para uma escala de concordância. Aqui definimos
# manualmente a ordem "correta" (de discordância total a concordância
# total) para que o eixo do gráfico siga essa lógica.

nivel_concordancia <- c(
  "Discordo totalmente",
  "Discordo em parte",
  "Concordo em parte",
  "Concordo totalmente"
)


## PASSO 5 — Calcular frequências e percentuais ------------------------------
#
# n_total: número de entrevistas válidas na base (o denominador do
#   percentual). Calculamos a partir dos próprios dados em vez de
#   digitar o número na mão — assim, se a base mudar (outra rodada
#   do survey, por exemplo), o cálculo se ajusta sozinho.

n_total <- nrow(df)

df_resumo <- df %>%
  # 5a) remove quem respondeu "Nem concordo nem discordo (espontânea)"
  #     e quem ficou como NA — queremos só as 4 categorias da escala
  filter(!militar_char %in% c(NA, "Nem concordo nem discordo(ESPONTÂNEA)")) %>%
  # 5b) transforma o texto em fator já na ordem lógica do Passo 4 —
  #     é isso que garante que o gráfico saia na ordem certa
  mutate(militar_char = factor(militar_char, levels = nivel_concordancia)) %>%
  # 5c) conta quantas respostas caem em cada categoria
  group_by(militar_char) %>%
  summarise(n = n(), .groups = "drop") %>%
  # 5d) calcula o percentual de cada categoria sobre o total da amostra
  mutate(pct = n / n_total)


## PASSO 6 — Construir o gráfico ----------------------------------------------
#
# ggplot funciona em camadas, adicionadas com "+". Cada linha abaixo
# é uma camada:
#   ggplot(aes(...))   -> define os dados e quais variáveis vão nos eixos
#   geom_bar(...)      -> desenha as barras (stat = "identity" porque
#                          já temos os valores prontos em "n", não
#                          precisamos que o ggplot conte de novo)
#   theme_datamundi()  -> aplica a identidade visual do lab
#   labs(...)          -> define os textos dos eixos
#   geom_text(...)     -> adiciona o rótulo de percentual dentro de
#                          cada barra

df_resumo %>%
  ggplot(aes(x = militar_char, y = n)) +
  geom_bar(stat = "identity") +
  theme_datamundi(base_size = 20) + # base_size determina o tamanho das letras em geral
  labs(
    x = "Opinião sobre o uso do poder militar como ferramenta de influência internacional",
    y = "Número de respondentes"
  ) +
  geom_text(
    aes(label = paste0(round(pct * 100, 2), "%")),
    vjust = 1.5, size = 8, color = "white"
  )

# ══════════════════════════════════════════════════════════════════
# PARA REPLICAR COM OUTRA PERGUNTA:
# Troque "Q14_1" (Passo 3, rename) pelo nome da coluna da pergunta
# que vocês querem analisar, ajustem os rótulos das categorias no
# Passo 4 conforme as opções de resposta daquela pergunta (olhem o
# questionário para ver as opções exatas) e atualizem os textos do
# labs() no Passo 6. O resto do caminho é o mesmo.
# ══════════════════════════════════════════════════════════════════
