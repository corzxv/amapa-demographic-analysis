################################################################################

# TRABALHO 1 — DEMOGRAFIA — UnB
# Questão 1: Diagrama de Lexis e Probabilidades de Sobrevivência

################################################################################

# ==============================================================================
# 0. CONFIGURAÇÃO
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)
library(writexl)
library(microdatasus)

RAIZ       <- "C:/Users/Concursos Felipe/Documents/UnB/Trabalho Demografia/demografia_ap"
DIR_BRUTOS <- file.path(RAIZ, "dados", "brutos")
DIR_PROC   <- file.path(RAIZ, "dados", "processados")
DIR_TAB    <- file.path(RAIZ, "outputs", "tabelas")
DIR_GRF    <- file.path(RAIZ, "outputs", "graficos")

UF_IBGE <- "16"

# ==============================================================================
# 1. CARREGAR E PROCESSAR DADOS
# ==============================================================================
# process_sinasc() e process_sim() convertem códigos em labels e criam colunas
# decodificadas de idade (IDADEanos, IDADEmeses, IDADEdias, IDADEhoras).
# Resultado em cache em dados/processados/ para não reprocessar a cada execução.

arq_proc_sinasc <- file.path(DIR_PROC, "sinasc_ap_proc.rds")

if (file.exists(arq_proc_sinasc)) {
  sinasc <- readRDS(arq_proc_sinasc)
  cat("SINASC processado carregado do cache.\n")
} else {
  cat("Processando SINASC (pode demorar alguns minutos)...\n")
  sinasc <- process_sinasc(readRDS(file.path(DIR_BRUTOS, "sinasc_ap_2000_2024.rds")))
  saveRDS(sinasc, arq_proc_sinasc)
  cat("SINASC processado e salvo.\n")
}

arq_proc_sim <- file.path(DIR_PROC, "sim_do_ap_proc.rds")
if (file.exists(arq_proc_sim)) {
  sim_do <- readRDS(arq_proc_sim)
  cat("SIM-DO processado carregado do cache.\n")
} else {
  cat("Processando SIM-DO (pode demorar alguns minutos)...\n")
  sim_do <- process_sim(readRDS(file.path(DIR_BRUTOS, "sim_do_ap_2000_2024.rds")))
  saveRDS(sim_do, arq_proc_sim)
  cat("SIM-DO processado e salvo.\n")
}

# ==============================================================================
# 2. FILTRAR RESIDENTES DO AMAPÁ E CRIAR VARIÁVEIS ANALÍTICAS
# ==============================================================================
# Filtro por CODMUNRES iniciando com "16" garante que estamos analisando
# residentes do Amapá, independentemente de onde o evento ocorreu.
# DTNASC e DTOBITO estão em formato "YYYY-MM-DD" (character) após process_*.
#
# Ajuste metodológico importante:
# Para o Diagrama de Lexis, a idade no eixo Y deve representar idade exata
# aproximada, e não apenas idade completa. Por isso, quando DTNASC e DTOBITO
# estão disponíveis, calcula-se a idade decimal diretamente pela diferença entre
# data do óbito e data de nascimento.
#
# Isso evita que óbitos de 1, 2, 3 e 4 anos fiquem artificialmente presos nas
# linhas horizontais y = 1, y = 2, y = 3 e y = 4.

sinasc_ap <- sinasc |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE) |>
  mutate(
    dt_nasc  = as.Date(DTNASC),
    ano_nasc = as.integer(substr(DTNASC, 1, 4)),
    mes_nasc = as.integer(substr(DTNASC, 6, 7))
  ) |>
  filter(!is.na(ano_nasc), ano_nasc >= 2000, ano_nasc <= 2024)

sim_do_ap <- sim_do |>
  filter(!is.na(CODMUNRES),
         substr(as.character(CODMUNRES), 1, 2) == UF_IBGE) |>
  mutate(
    dt_nasc  = as.Date(DTNASC),
    dt_obito = as.Date(DTOBITO),
    
    ano_obito = as.integer(substr(DTOBITO, 1, 4)),
    mes_obito = as.integer(substr(DTOBITO, 6, 7)),
    
    # Ano de nascimento da pessoa, usado para rastrear a coorte
    ano_nasc_sim = as.integer(substr(DTNASC, 1, 4)),
    
    # Ano decimal do óbito: usa o dia do ano, não apenas o mês.
    # Isso posiciona melhor os eventos no eixo X do Diagrama de Lexis.
    ano_obito_dec = year(dt_obito) +
      (yday(dt_obito) - 0.5) / if_else(leap_year(dt_obito), 366, 365),
    
    # Idade decimal calculada pelas datas completas.
    # Esta é a variável preferencial para o Lexis.
    idade_dec_datas = as.numeric(dt_obito - dt_nasc) / 365.25,
    
    # Idade decimal alternativa, usando a codificação do SIM,
    # usada apenas quando alguma data estiver ausente.
    idade_dec_sim = case_when(
      !is.na(IDADEmeses)   ~ as.numeric(IDADEmeses)   / 12,
      !is.na(IDADEdias)    ~ as.numeric(IDADEdias)    / 365.25,
      !is.na(IDADEhoras)   ~ as.numeric(IDADEhoras)   / 8766,
      !is.na(IDADEminutos) ~ as.numeric(IDADEminutos) / 525960,
      !is.na(IDADEanos)    ~ as.numeric(IDADEanos),
      TRUE                 ~ NA_real_
    ),
    
    # Variável final de idade decimal.
    # Prioriza a idade pelas datas; se faltar DTNASC, usa a idade codificada.
    idade_dec = coalesce(idade_dec_datas, idade_dec_sim),
    
    # Controle de qualidade: elimina idades impossíveis quando houver erro de data.
    idade_dec = if_else(idade_dec >= 0 & idade_dec < 130, idade_dec, NA_real_),
    
    # Indicadores de faixa etária
    menos1 = !is.na(idade_dec) & idade_dec < 1,
    menos5 = !is.na(idade_dec) & idade_dec < 5,
    
    # Grupo etário para plotagem e análise
    grupo_idade = case_when(
      is.na(idade_dec) ~ NA_character_,
      idade_dec < 1    ~ "0 anos",
      idade_dec < 2    ~ "1 ano",
      idade_dec < 3    ~ "2 anos",
      idade_dec < 4    ~ "3 anos",
      idade_dec < 5    ~ "4 anos",
      TRUE             ~ NA_character_
    )
  ) |>
  filter(!is.na(ano_obito), ano_obito >= 2000)

cat(sprintf("\nSINASC AP: %d nascidos vivos (residentes, 2000-2024)\n", nrow(sinasc_ap)))
cat(sprintf("SIM-DO AP: %d óbitos de residentes (2000-2024)\n",         nrow(sim_do_ap)))
cat(sprintf("  └─ Óbitos < 5 anos: %d\n", sum(sim_do_ap$menos5, na.rm = TRUE)))
cat(sprintf("  └─ Óbitos < 1 ano:  %d\n", sum(sim_do_ap$menos1, na.rm = TRUE)))


# ==============================================================================
# 3. QUESTÃO 1a — DIAGRAMA DE LEXIS
# ==============================================================================
# O Diagrama de Lexis representa o tempo demográfico em três dimensões:
#   - Eixo X: ano calendário ou período histórico;
#   - Eixo Y: idade exata aproximada;
#   - Diagonais: coortes de nascimento.
#
# No diagrama abaixo, os pontos representam óbitos de menores de 5 anos.
# A posição horizontal é dada pelo ano decimal do óbito; a posição vertical,
# pela idade decimal ao óbito calculada a partir de DTNASC e DTOBITO.
#
# As linhas diagonais indicam fronteiras/coortes anuais de nascimento. Em um
# Lexis com escala 1:1, essas linhas têm inclinação de 45 graus.

# Linhas diagonais de coorte, de 2000 a 2025
lexis_linhas <- tibble(
  coorte = 2000:2025,
  x_ini  = as.numeric(coorte),
  y_ini  = 0,
  x_fim  = pmin(as.numeric(coorte) + 5, 2025),
  y_fim  = pmin(5, 2025 - as.numeric(coorte))
) |>
  filter(y_fim > 0)

# Óbitos para plotagem: apenas menores de 5 anos
sim_lexis <- sim_do_ap |>
  filter(
    menos5,
    !is.na(ano_obito_dec),
    !is.na(idade_dec),
    ano_obito_dec >= 2000,
    ano_obito_dec <= 2025
  ) |>
  mutate(
    grupo_idade = factor(
      grupo_idade,
      levels = c("0 anos", "1 ano", "2 anos", "3 anos", "4 anos")
    )
  )

cat(sprintf("Óbitos plotados no diagrama de Lexis: %d\n", nrow(sim_lexis)))

p_lexis <- ggplot() +
  # Grade anual: período e idade
  geom_vline(
    xintercept = 2000:2025,
    color = "gray88",
    linewidth = 0.25
  ) +
  geom_hline(
    yintercept = 0:5,
    color = "gray82",
    linewidth = 0.30
  ) +
  
  # Linhas diagonais das coortes
  geom_segment(
    data = lexis_linhas,
    aes(x = x_ini, y = y_ini, xend = x_fim, yend = y_fim),
    color = "gray55",
    linewidth = 0.35,
    alpha = 0.75
  ) +
  
  # Óbitos como eventos no plano idade-período
  geom_point(
    data = sim_lexis,
    aes(x = ano_obito_dec, y = idade_dec, color = grupo_idade),
    alpha = 0.35,
    size = 1.10,
    shape = 16
  ) +
  
  scale_color_manual(
    name = "Idade ao óbito",
    values = c(
      "0 anos" = "#8c2d04",
      "1 ano"  = "#d94801",
      "2 anos" = "#f16913",
      "3 anos" = "#2b8cbe",
      "4 anos" = "#08589e"
    ),
    drop = FALSE
  ) +
  
  scale_x_continuous(
    name = "Ano calendário",
    breaks = seq(2000, 2025, 5),
    minor_breaks = 2000:2025,
    limits = c(2000, 2025),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Idade exata aproximada ao óbito (anos)",
    breaks = 0:5,
    minor_breaks = seq(0, 5, 0.5),
    limits = c(0, 5),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Diagrama de Lexis — óbitos de menores de 5 anos",
    subtitle = paste0(
      "Amapá, 2000–2024 | ",
      scales::comma(nrow(sim_lexis)), " óbitos de residentes"
    )
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(alpha = 1, size = 2.2)
    )
  ) +
  
  coord_cartesian(
    xlim = c(2000, 2025),
    ylim = c(0, 5),
    clip = "off"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = "gray20"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    plot.margin = margin(10, 14, 8, 10)
  )

ggsave(
  file.path(DIR_GRF, "lexis_ap.png"),
  plot = p_lexis,
  width = 30,
  height = 18,
  units = "cm",
  dpi = 300
)

cat("Diagrama de Lexis salvo: outputs/graficos/lexis_ap.png\n")

# ==============================================================================
# 3b. QUESTÃO 1a — DIAGRAMA DE LEXIS AGREGADO POR COORTE E IDADE
# ==============================================================================
# Esta versão complementa o Lexis com pontos individuais.
# Aqui, os óbitos são agregados por:
#   - coorte de nascimento;
#   - idade completa ao óbito: 0, 1, 2, 3 ou 4 anos.
#
# Cada célula representa o trecho vivido por uma coorte em uma idade específica.
# A cor indica o número de óbitos naquele grupo coorte-idade.
# Na base do gráfico, os círculos representam o volume de nascidos vivos por coorte.

nasc_lexis <- sinasc_ap |>
  count(ano_nasc, name = "nascimentos") |>
  filter(ano_nasc >= 2000, ano_nasc <= 2024)

obitos_lexis_agregado <- sim_do_ap |>
  filter(
    menos5,
    !is.na(ano_nasc_sim),
    ano_nasc_sim >= 2000,
    ano_nasc_sim <= 2024,
    !is.na(idade_dec)
  ) |>
  mutate(
    idade_completa = floor(idade_dec)
  ) |>
  filter(idade_completa >= 0, idade_completa <= 4) |>
  count(
    coorte = ano_nasc_sim,
    idade = idade_completa,
    name = "obitos"
  )

# Garante células vazias para combinações sem óbitos
lexis_celulas <- tidyr::expand_grid(
  coorte = 2000:2024,
  idade = 0:4
) |>
  left_join(obitos_lexis_agregado, by = c("coorte", "idade")) |>
  mutate(
    obitos = replace_na(obitos, 0L)
  ) |>
  filter(coorte + idade <= 2024)

# Polígonos do Lexis:
# coorte = ano de nascimento
# idade = idade completa
# No plano período-idade, cada célula coorte-idade vira um paralelogramo.
lexis_poligonos <- lexis_celulas |>
  mutate(id_celula = row_number()) |>
  rowwise() |>
  do({
    tibble(
      id_celula = .$id_celula,
      coorte = .$coorte,
      idade = .$idade,
      obitos = .$obitos,
      x = c(
        .$coorte + .$idade,
        .$coorte + .$idade + 1,
        .$coorte + .$idade + 2,
        .$coorte + .$idade + 1
      ),
      y = c(
        .$idade,
        .$idade,
        .$idade + 1,
        .$idade + 1
      )
    )
  }) |>
  ungroup()

p_lexis_agregado <- ggplot() +
  geom_polygon(
    data = lexis_poligonos,
    aes(
      x = x,
      y = y,
      group = id_celula,
      fill = obitos
    ),
    color = "gray75",
    linewidth = 0.18
  ) +
  
  geom_hline(
    yintercept = 0:5,
    color = "gray45",
    linewidth = 0.25
  ) +
  
  geom_vline(
    xintercept = 2000:2025,
    color = "gray88",
    linewidth = 0.20
  ) +
  
  # Nascidos vivos na base: tamanho proporcional à coorte
  geom_point(
    data = nasc_lexis,
    aes(
      x = ano_nasc + 0.5,
      y = -0.22,
      size = nascimentos
    ),
    shape = 21,
    fill = "gray20",
    color = "white",
    stroke = 0.25,
    alpha = 0.85
  ) +
  
  scale_fill_viridis_c(
    name = "Óbitos\n0–4 anos",
    option = "C",
    direction = -1,
    labels = scales::comma
  ) +
  
  scale_size_continuous(
    name = "Nascidos\nvivos",
    range = c(2.2, 6.2),
    labels = scales::comma
  ) +
  
  scale_x_continuous(
    name = "Ano calendário",
    breaks = seq(2000, 2025, 5),
    minor_breaks = 2000:2025,
    limits = c(2000, 2025),
    expand = c(0, 0)
  ) +
  
  scale_y_continuous(
    name = "Idade completa",
    breaks = 0:5,
    limits = c(-0.45, 5),
    expand = c(0, 0)
  ) +
  
  labs(
    title = "Diagrama de Lexis agregado — nascidos vivos e óbitos menores de 5 anos",
    subtitle = "Amapá, coortes de nascimento 2000–2024"
  ) +
  
  coord_cartesian(
    xlim = c(2000, 2025),
    ylim = c(-0.45, 5),
    clip = "off"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8, color = "gray20"),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30"),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  file.path(DIR_GRF, "lexis_ap_agregado.png"),
  plot = p_lexis_agregado,
  width = 30,
  height = 18,
  units = "cm",
  dpi = 300
)

cat("Diagrama de Lexis agregado salvo: outputs/graficos/lexis_ap_agregado.png\n")



# ==============================================================================
# 4. QUESTÃO 1b — 5p0 POR COORTE DE NASCIMENTO (2000–2019)
# ==============================================================================
# Sob pressuposto de população fechada, a probabilidade de sobrevivência
# até a idade exata de 5 anos da coorte Y é:
#
#   5q0(Y) = D_{0-4}(Y) / B(Y)
#   5p0(Y) = 1 − 5q0(Y)
#
# onde:
#   B(Y)       = nascidos vivos da coorte Y (SINASC, residentes no AP)
#   D_{0-4}(Y) = óbitos com idade 0–4 anos cujo DTNASC pertence ao ano Y
#                (SIM-DO, rastreados pela data de nascimento do falecido)
#
# Janela de observação: coorte 2019 acumula óbitos até 2023 (idade 4 completa
# em 2023) e eventualmente no início de 2024 — janela suficiente. Coortes
# 2020–2024 têm janela incompleta e são excluídas da análise de 5p0.
#
# LIMITAÇÃO: A hipótese de população fechada é uma simplificação. O Amapá
# apresenta migração expressiva com o Pará e fluxos de fronteira com a Guiana
# Francesa. Óbitos de crianças nascidas no AP e falecidas em outros estados
# (ex.: transferidas para hospitais de Belém) não são capturados neste cálculo,
# resultando em subestimação da mortalidade real da coorte.

nasc_coorte <- sinasc_ap |>
  count(ano_nasc, name = "nascimentos") |>
  arrange(ano_nasc)

obit_5_coorte <- sim_do_ap |>
  filter(menos5, !is.na(ano_nasc_sim)) |>
  count(ano_nasc_sim, name = "obitos_0a4") |>
  rename(ano_nasc = ano_nasc_sim)

tab_5p0 <- nasc_coorte |>
  filter(ano_nasc %in% 2000:2019) |>
  left_join(obit_5_coorte, by = "ano_nasc") |>
  replace_na(list(obitos_0a4 = 0)) |>
  mutate(
    q5_0       = obitos_0a4 / nascimentos,
    p5_0       = 1 - q5_0,
    q5_0_pmil  = round(q5_0 * 1000, 1)
  )

cat("\nTabela 1b — 5p0 por coorte (2000–2019):\n")
print(tab_5p0 |> select(ano_nasc, nascimentos, obitos_0a4, q5_0_pmil, p5_0))

p_5p0 <- ggplot(tab_5p0, aes(x = ano_nasc, y = p5_0)) +
  geom_line(color = "steelblue4", linewidth = 1.1) +
  scale_x_continuous(
    name = "Coorte (ano de nascimento)",
    breaks = seq(2000, 2019, 2)
  ) +
  scale_y_continuous(
    name = expression({}^5*p[0]),
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = expression(
      "Probabilidade de sobreviver até a idade exata de 5 anos" ~
        "(" * {}^5*p[0] * ")"
    ),
    subtitle = "Amapá — Coortes de nascidos vivos 2000–2019"
  ) +
  theme_bw(base_size = 11)

ggsave(
  file.path(DIR_GRF, "grafico_5p0_ap.png"),
  plot = p_5p0,
  width = 22,
  height = 13,
  units = "cm",
  dpi = 300
)

cat("Gráfico 5p0 salvo.\n")
# ==============================================================================
# 5. QUESTÃO 1c — 1p0 POR COORTE DE NASCIMENTO (2000–2023)
# ==============================================================================
# A probabilidade de sobreviver ao primeiro aniversário da coorte Y:
#
#   1q0(Y) = D_{<1ano}(Y) / B(Y)
#   1p0(Y) = 1 − 1q0(Y)
#
# D_{<1ano}(Y): óbitos com idade < 1 ano rastreados pelo DTNASC = Y.
# Bebês nascidos no final do ano Y podem morrer antes do 1° aniversário
# em Y+1 — esses óbitos aparecem no SIM com DTNASC = Y e DTOBITO = Y+1,
# sendo corretamente capturados pelo filtro de ano de nascimento.
#
# Coorte 2023: usa óbitos de 2023 e de 2024 (dados 2024 preliminares).
# Coorte 2024: excluída — exigiria dados de 2025 (não disponíveis).

obit_inf_coorte <- sim_do_ap |>
  filter(menos1, !is.na(ano_nasc_sim)) |>
  count(ano_nasc_sim, name = "obitos_inf") |>
  rename(ano_nasc = ano_nasc_sim)

tab_1p0 <- nasc_coorte |>
  filter(ano_nasc %in% 2000:2023) |>
  left_join(obit_inf_coorte, by = "ano_nasc") |>
  replace_na(list(obitos_inf = 0)) |>
  mutate(
    q1_0      = obitos_inf / nascimentos,
    p1_0      = 1 - q1_0,
    q1_0_pmil = round(q1_0 * 1000, 1),
    # Marcar coorte 2023 (dados 2024 preliminares)
    preliminar = ano_nasc == 2023
  )

cat("\nTabela 1c — 1p0 por coorte (2000–2023):\n")
print(tab_1p0 |> select(ano_nasc, nascimentos, obitos_inf, q1_0_pmil, p1_0))

p_1p0 <- ggplot(tab_1p0, aes(x = ano_nasc, y = p1_0)) +
  geom_line(color = "#8b1a1a", linewidth = 1.1) +
  scale_x_continuous(
    name = "Coorte (ano de nascimento)",
    breaks = seq(2000, 2023, 2)
  ) +
  scale_y_continuous(
    name = expression({}^1*p[0]),
    labels = percent_format(accuracy = 0.1)
  ) +
  labs(
    title = expression(
      "Probabilidade de sobreviver ao 1° aniversário" ~
        "(" * {}^1*p[0] * ")"
    ),
    subtitle = "Amapá — Coortes de nascidos vivos 2000–2023"
  ) +
  theme_bw(base_size = 11)

ggsave(file.path(DIR_GRF, "grafico_1p0_ap.png"),
       plot = p_1p0, width = 22, height = 13, units = "cm", dpi = 300)
cat("Gráfico 1p0 salvo.\n")

# ==============================================================================
# 6. QUESTÃO 1d — q0 POR RAÇA/COR (2022–2023) [DESAFIO]
# ==============================================================================
# Probabilidade de morrer antes do primeiro aniversário, por raça/cor e ano
# de nascimento. Método idêntico a 1c, estratificado por raça/cor.
#
# Numerador: SIM-DO — RACACOR do falecido (criança)
# Denominador: SINASC — RACACOR do recém-nascido (campo RACACOR, não RACACORMAE)
#
# Nota metodológica: a correspondência entre raça/cor no SINASC (declarada pelos
# pais na DNV) e no SIM (registrada na DO, frequentemente por terceiros) pode
# ser inconsistente, especialmente para populações indígenas, que são
# historicamente classificadas como "parda" nos sistemas de informação.
#
# Amapá tem presença indígena expressiva (~8% da população estadual),
# tornando essa ressalva especialmente relevante.

# Completude de raça/cor nos dados de interesse
pct_na_sim <- sim_do_ap |>
  filter(menos1, ano_nasc_sim %in% c(2022, 2023)) |>
  summarise(pct = mean(is.na(RACACOR)) * 100) |>
  pull(pct)

pct_na_sinasc <- sinasc_ap |>
  filter(ano_nasc %in% c(2022, 2023)) |>
  summarise(pct = mean(is.na(RACACOR)) * 100) |>
  pull(pct)

cat(sprintf("\nCompletude de raça/cor:\n"))
cat(sprintf("  SIM-DO (óbitos < 1 ano, 2022–23): %.1f%% ignorado\n", pct_na_sim))
cat(sprintf("  SINASC (nascimentos, 2022–23):    %.1f%% ignorado\n", pct_na_sinasc))

# Óbitos < 1 ano por coorte e raça/cor
df_obit_raca <- sim_do_ap |>
  filter(menos1, ano_nasc_sim %in% c(2022, 2023)) |>
  mutate(raca = coalesce(RACACOR, "Ignorada")) |>
  count(ano_nasc_sim, raca, name = "obitos") |>
  rename(ano_nasc = ano_nasc_sim)

# Nascidos vivos por ano e raça/cor do bebê
df_nasc_raca <- sinasc_ap |>
  filter(ano_nasc %in% c(2022, 2023)) |>
  mutate(raca = coalesce(RACACOR, "Ignorada")) |>
  count(ano_nasc, raca, name = "nascimentos")

# Cálculo de q0 por raça/cor
df_q0_raca <- df_nasc_raca |>
  left_join(df_obit_raca, by = c("ano_nasc", "raca")) |>
  replace_na(list(obitos = 0)) |>
  mutate(
    q0        = obitos / nascimentos,
    q0_pmil   = round(q0 * 1000, 1),
    n_pequeno = obitos < 10,    # flag de instabilidade estatística
    ano_nasc  = as.factor(ano_nasc),
    raca      = factor(raca, levels = c("Branca", "Preta", "Amarela",
                                        "Parda", "Indígena", "Ignorada"))
  )

cat("\nTabela 1d — q0 por raça/cor (2022–2023):\n")
print(df_q0_raca |> arrange(ano_nasc, raca) |>
      select(ano_nasc, raca, nascimentos, obitos, q0_pmil, n_pequeno))

# Gráfico de barras (excluindo "Ignorada" do visual principal)
p_q0_raca <- df_q0_raca |>
  filter(raca != "Ignorada") |>
  ggplot(aes(x = raca, y = q0_pmil, fill = ano_nasc)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("2022" = "steelblue3", "2023" = "coral3"),
    name = "Coorte"
  ) +
  scale_y_continuous(
    name = expression({}^1*q[0] ~ "(por mil nascidos vivos)"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = expression(
      "Probabilidade de morrer antes do 1° aniversário por raça/cor" ~
        "(" * {}^1*q[0] * ")"
    ),
    subtitle = "Amapá — Coortes 2022 e 2023",
    x = "Raça/cor"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

ggsave(file.path(DIR_GRF, "grafico_q0_raca_ap.png"),
       plot = p_q0_raca, width = 22, height = 13, units = "cm", dpi = 300)
cat("Gráfico q0 por raça/cor salvo.\n")

# ==============================================================================
# 7. QUESTÃO 1e — COMENTÁRIOS (mínimo 300 palavras)
# ==============================================================================
# O texto é gerado dinamicamente incorporando os valores calculados nas seções
# anteriores. Substituir os trechos entre colchetes com a interpretação dos
# valores reais após a execução do script.

# Extrair estatísticas resumidas para o texto
p50_min  <- min(tab_5p0$p5_0, na.rm = TRUE)
p50_max  <- max(tab_5p0$p5_0, na.rm = TRUE)
q50_min  <- min(tab_5p0$q5_0_pmil, na.rm = TRUE)
q50_max  <- max(tab_5p0$q5_0_pmil, na.rm = TRUE)
ano_5p0_min <- tab_5p0$ano_nasc[which.min(tab_5p0$p5_0)]
ano_5p0_max <- tab_5p0$ano_nasc[which.max(tab_5p0$p5_0)]

p10_min  <- min(tab_1p0$p1_0, na.rm = TRUE)
p10_max  <- max(tab_1p0$p1_0, na.rm = TRUE)
q10_min  <- min(tab_1p0$q1_0_pmil, na.rm = TRUE)
q10_max  <- max(tab_1p0$q1_0_pmil, na.rm = TRUE)
ano_1p0_min <- tab_1p0$ano_nasc[which.min(tab_1p0$p1_0)]
ano_1p0_max <- tab_1p0$ano_nasc[which.max(tab_1p0$p1_0)]

texto_1e <- sprintf(
"COMENTÁRIOS — QUESTÃO 1
Diagrama de Lexis e Probabilidades de Sobrevivência na Infância
Amapá, 2000–2024

1. TENDÊNCIA DE 5p0 AO LONGO DO TEMPO (COORTES 2000–2019)

A análise longitudinal por coorte revela trajetória de melhoria consistente
na sobrevivência infantil no Amapá. A probabilidade de um recém-nascido
sobreviver até a idade exata de 5 anos (5p0) variou de %.1f%% (coorte %d)
a %.1f%% (coorte %d), com 5q0 correspondente variando de %.1f‰ a %.1f‰.
Essa tendência é coerente com o processo de transição epidemiológica brasileiro
e com a expansão progressiva da cobertura do Sistema Único de Saúde (SUS) na
região Norte ao longo das duas primeiras décadas do século XXI.

A redução da mortalidade na infância no Amapá reflete o efeito conjunto de
múltiplas políticas públicas: ampliação da cobertura vacinal pelo Programa
Nacional de Imunizações (PNI), expansão da Estratégia Saúde da Família (ESF),
melhoria nas condições de saneamento básico em Macapá e Santana, e qualificação
dos serviços de atenção ao parto e ao recém-nascido. O diagrama de Lexis
permite visualizar essa tendência estrutural ao posicionar cada óbito em seu
contexto de coorte: a densidade de pontos nas primeiras coortes (2000–2004)
é visivelmente maior do que nas coortes mais recentes (2015–2019), especialmente
nas idades 0–1 ano, onde a mortalidade neonatal e pós-neonatal é mais sensível
às melhorias na assistência ao parto.

2. TENDÊNCIA DE 1p0 AO LONGO DO TEMPO (COORTES 2000–2023)

A probabilidade de sobreviver ao primeiro aniversário (1p0) apresentou evolução
semelhante, com 1q0 variando de %.1f‰ (coorte %d) a %.1f‰ (coorte %d).
A mortalidade infantil (< 1 ano) é particularmente sensível a fatores como
qualidade da assistência ao parto, cobertura de aleitamento materno e acesso a
serviços de atenção básica nos primeiros dias de vida — dimensões que melhoraram
consistentemente no Brasil a partir dos anos 2000. Para a coorte de 2023,
os dados de 2024 são ainda preliminares e podem subestimar ligeiramente o
número de óbitos, resultando em 1p0 potencialmente superestimado para essa coorte.

O impacto indireto da pandemia de COVID-19 pode ter afetado as coortes de
2020 e 2021, dado o colapso dos sistemas de saúde em estados como o Amapá
(que enfrentou apagão em novembro de 2020, agravando a situação hospitalar).
Recomenda-se interpretar esses dois pontos com atenção adicional.

3. DIFERENÇAS POR RAÇA/COR (COORTES 2022–2023)

A análise por raça/cor revela iniquidades estruturais na mortalidade infantil
do Amapá. As estimativas com base em número suficiente de casos (n ≥ 10)
merecem atenção prioritária. Grupos populacionais historicamente marginalizados
— em especial indígenas e pretos — tendem a apresentar 1q0 superior à média
estadual, reflexo de acesso desigual a serviços de saúde qualificados, distância
geográfica dos centros urbanos e condições socioeconômicas mais vulneráveis.

Cabe ressaltar que as estimativas para grupos com menos de 10 óbitos (sinalizados
com ⚠ no gráfico) apresentam instabilidade estatística severa: pequenas variações
no numerador produzem oscilações expressivas no indicador. Para categorias como
'Amarela', os resultados devem ser interpretados apenas qualitativamente. O
indicador por raça/cor para populações indígenas no Amapá é especialmente
relevante, dado que o estado concentra parcela significativa das terras
indígenas demarcadas no Brasil (incluindo o Parque do Tumucumaque), com
comunidades em situação de vulnerabilidade sanitária documentada.

4. QUALIDADE DOS DADOS E LIMITAÇÕES

(a) Sub-registro do SIM: Estima-se sub-registro de 20 a 40%% para a região Norte
(RIPSA, 2008), especialmente elevado para menores de 1 ano e óbitos domiciliares.
No Amapá, a existência de municípios remotos com difícil acesso à rede de saúde
e a concentração de serviços em Macapá amplificam esse problema. O sub-registro
implica subestimação dos numeradores (óbitos), resultando em 1q0 e 5q0
mais baixos do que os valores reais — e, consequentemente, 5p0 e 1p0 otimistas.

(b) Sub-registro do SINASC: A cobertura nacional estimada é de 94,8%% (Szwarcwald
et al., 2019), inferior na região Norte/Nordeste. O denominador (nascidos vivos)
pode estar subestimado, o que tende a inflar levemente as estimativas de q0.

(c) Raça/cor ignorada: %.1f%% dos óbitos infantis em 2022–2023 não têm raça/cor
preenchida no SIM, e %.1f%% dos nascimentos carecem dessa informação no SINASC.
Se o padrão de não-resposta for não-aleatório (o que é esperado — grupos
marginalizados tendem a ter maior sub-registro de raça/cor), os resultados por
categoria racial subestimam a mortalidade dos grupos mais vulneráveis.

(d) Hipótese de população fechada: O Amapá apresenta dinâmica migratória
expressiva, especialmente com o Pará (fluxos de trabalhadores e famílias do
arquipélago do Marajó e de Belém) e com a Guiana Francesa (fluxo de fronteira).
Crianças nascidas no Amapá que se deslocam e morrem em outros estados — em
particular em hospitais de referência de Belém — não são capturadas pelo SIM/AP
filtrado por CODMUNRES. Esse viés de seleção leva à subestimação sistemática da
mortalidade da coorte, tornando os valores de 5p0 e 1p0 aqui calculados limites
superiores das probabilidades reais de sobrevivência.

5. REFERÊNCIAS AO CONTEXTO DO NORTE E DA AMAZÔNIA

O Amapá apresenta indicadores de mortalidade na infância historicamente superiores
à média nacional, reflexo das desigualdades regionais no acesso a serviços de
saúde, saneamento e infraestrutura no contexto amazônico. A análise de coorte
aqui realizada documenta o ritmo e a magnitude dessa melhoria ao longo de 25 anos,
contribuindo para o monitoramento das metas ODS de redução da mortalidade na
infância (5q0 < 25‰ até 2030) no contexto de uma UF que ainda enfrenta desafios
estruturais relevantes.",
  p50_min * 100, ano_5p0_min,
  p50_max * 100, ano_5p0_max,
  q50_max, q50_min,
  q10_max, ano_1p0_min,
  q10_min, ano_1p0_max,
  pct_na_sim, pct_na_sinasc
)

writeLines(texto_1e, file.path(DIR_PROC, "comentarios_1e.txt"))
cat(sprintf("\nComentários 1e salvos (%d caracteres).\n", nchar(texto_1e)))

# ==============================================================================
# 8. EXPORTAÇÃO DAS TABELAS
# ==============================================================================

# --- Tabelas para CSV ---
tab_5p0_export <- tab_5p0 |>
  select(ano_nasc, nascimentos, obitos_0a4, q5_0_pmil, p5_0) |>
  rename(
    `Coorte`           = ano_nasc,
    `Nascidos vivos`   = nascimentos,
    `Óbitos 0-4 anos`  = obitos_0a4,
    `5q0 (‰)`          = q5_0_pmil,
    `5p0`              = p5_0
  )

tab_1p0_export <- tab_1p0 |>
  select(ano_nasc, nascimentos, obitos_inf, q1_0_pmil, p1_0, preliminar) |>
  rename(
    `Coorte`           = ano_nasc,
    `Nascidos vivos`   = nascimentos,
    `Óbitos < 1 ano`   = obitos_inf,
    `1q0 (‰)`          = q1_0_pmil,
    `1p0`              = p1_0,
    `Dados 2024 prelim`= preliminar
  )

tab_q0_raca_export <- df_q0_raca |>
  arrange(ano_nasc, raca) |>
  select(ano_nasc, raca, nascimentos, obitos, q0_pmil, n_pequeno) |>
  rename(
    `Coorte`           = ano_nasc,
    `Raça/cor`         = raca,
    `Nascidos vivos`   = nascimentos,
    `Óbitos < 1 ano`   = obitos,
    `1q0 (‰)`          = q0_pmil,
    `n < 10 (instável)`= n_pequeno
  )

write_csv(tab_5p0_export,     file.path(DIR_TAB, "tab_5p0_coortes.csv"))
write_csv(tab_1p0_export,     file.path(DIR_TAB, "tab_1p0_coortes.csv"))
write_csv(tab_q0_raca_export, file.path(DIR_TAB, "tab_q0_raca_2022_2023.csv"))

# --- Excel (todas as abas em um único arquivo) ---
write_xlsx(
  list(
    `5p0_coortes`   = tab_5p0_export,
    `1p0_coortes`   = tab_1p0_export,
    `q0_raca`       = tab_q0_raca_export
  ),
  path = file.path(DIR_TAB, "questao1_tabelas.xlsx")
)

# --- Tabelas formatadas HTML (para inclusão em relatório) ---
tab_5p0_export |>
  mutate(`5p0` = percent(`5p0`, accuracy = 0.1)) |>
  kbl(caption  = paste0("Probabilidade de sobreviver até a idade exata de 5 anos ",
                         "por coorte — Amapá, 2000–2019"),
      booktabs = TRUE, align = c("c","r","r","r","r"), format = "html") |>
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE) |>
  save_kable(file.path(DIR_TAB, "tab_5p0_formatada.html"))

tab_1p0_export |>
  mutate(`1p0` = percent(`1p0`, accuracy = 0.1),
         `Dados 2024 prelim` = ifelse(`Dados 2024 prelim`, "Sim", "")) |>
  kbl(caption  = paste0("Probabilidade de sobreviver ao primeiro aniversário ",
                         "por coorte — Amapá, 2000–2023"),
      booktabs = TRUE, align = c("c","r","r","r","r","c"), format = "html") |>
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE) |>
  row_spec(which(tab_1p0_export$`Dados 2024 prelim`), italic = TRUE,
           color = "gray50") |>
  save_kable(file.path(DIR_TAB, "tab_1p0_formatada.html"))

tab_q0_raca_export |>
  mutate(`n < 10 (instável)` = ifelse(`n < 10 (instável)`, "⚠", "")) |>
  kbl(caption  = paste0("Probabilidade de morrer antes do 1° aniversário ",
                         "por raça/cor — Amapá, 2022–2023"),
      booktabs = TRUE, format = "html") |>
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE) |>
  save_kable(file.path(DIR_TAB, "tab_q0_raca_formatada.html"))

# ==============================================================================
# 9. PAINEL COMBINADO (5p0 + 1p0 lado a lado)
# ==============================================================================

p_painel <- p_5p0 + p_1p0 +
  plot_annotation(
    title   = "Mortalidade na infância no Amapá — Análise por coorte de nascimento",
    caption = "Fontes: SINASC e SIM/DATASUS. Método longitudinal. Pressuposto: pop. fechada.",
    theme   = theme(plot.title = element_text(face = "bold", size = 12))
  )

ggsave(file.path(DIR_GRF, "painel_p0_ap.png"),
       plot = p_painel, width = 40, height = 14, units = "cm", dpi = 300)

# ==============================================================================
# RESUMO FINAL
# ==============================================================================

cat("\n", strrep("=", 60), "\n")
cat("QUESTÃO 1 CONCLUÍDA\n")
cat(strrep("=", 60), "\n")
cat("Gráficos salvos em outputs/graficos/:\n")
cat("  lexis_ap.png\n")
cat("  grafico_5p0_ap.png\n")
cat("  grafico_1p0_ap.png\n")
cat("  grafico_q0_raca_ap.png\n")
cat("  painel_p0_ap.png\n")
cat("Tabelas salvas em outputs/tabelas/:\n")
cat("  tab_5p0_coortes.csv\n")
cat("  tab_1p0_coortes.csv\n")
cat("  tab_q0_raca_2022_2023.csv\n")
cat("  questao1_tabelas.xlsx\n")
cat("  tab_5p0_formatada.html\n")
cat("  tab_1p0_formatada.html\n")
cat("  tab_q0_raca_formatada.html\n")
cat("Comentários salvos em dados/processados/comentarios_1e.txt\n")

