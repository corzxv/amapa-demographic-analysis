# ==============================================================================
# 3B. QUESTÃO 1a — DIAGRAMA DE LEXIS NUMÉRICO
# ==============================================================================
# Nesta versão, os óbitos não aparecem como pontos individuais.
# Eles são agregados dentro de cada quadrado idade-ano do Diagrama de Lexis.
#
# Cada quadrado representa:
#   - eixo X: ano calendário do óbito
#   - eixo Y: idade completa ao óbito
#
# A diagonal divide o quadrado em dois triângulos, correspondentes às duas
# coortes de nascimento que atravessam aquele intervalo idade-período.
#
# Número no triângulo superior: óbitos da coorte mais antiga.
# Número no triângulo inferior: óbitos da coorte mais recente.

ANOS_LEXIS_NUM   <- 2000:2024
IDADES_LEXIS_NUM <- 0:4

# Base de óbitos para o Lexis numérico
sim_lexis_num <- sim_do_ap |>
  filter(
    menos5,
    !is.na(ano_obito_dec),
    !is.na(idade_dec)
  ) |>
  mutate(
    ano_cal   = floor(ano_obito_dec),
    idade_int = floor(idade_dec),
    
    frac_ano   = ano_obito_dec - ano_cal,
    frac_idade = idade_dec - idade_int,
    
    # Identificação do triângulo do quadrado de Lexis.
    # Quando o ano de nascimento está disponível, ele é usado para classificar
    # a coorte de forma mais direta. Quando não está, usa-se a posição decimal.
    triangulo = case_when(
      !is.na(ano_nasc_sim) & ano_nasc_sim == ano_cal - idade_int     ~ "inferior",
      !is.na(ano_nasc_sim) & ano_nasc_sim == ano_cal - idade_int - 1 ~ "superior",
      frac_idade >= frac_ano                                         ~ "superior",
      TRUE                                                           ~ "inferior"
    )
  ) |>
  filter(
    ano_cal %in% ANOS_LEXIS_NUM,
    idade_int %in% IDADES_LEXIS_NUM
  )

# Grade de quadrados idade-ano
lexis_grade_num <- tidyr::expand_grid(
  ano_cal   = ANOS_LEXIS_NUM,
  idade_int = IDADES_LEXIS_NUM
) |>
  mutate(
    x_ini = ano_cal,
    x_fim = ano_cal + 1,
    y_ini = idade_int,
    y_fim = idade_int + 1
  )

# Contagem dos óbitos em cada triângulo
lexis_counts_num <- sim_lexis_num |>
  count(ano_cal, idade_int, triangulo, name = "obitos") |>
  tidyr::complete(
    ano_cal   = ANOS_LEXIS_NUM,
    idade_int = IDADES_LEXIS_NUM,
    triangulo = c("superior", "inferior"),
    fill = list(obitos = 0)
  ) |>
  tidyr::pivot_wider(
    names_from  = triangulo,
    values_from = obitos,
    names_prefix = "n_"
  )

# Junta grade e contagens
lexis_plot_num <- lexis_grade_num |>
  left_join(lexis_counts_num, by = c("ano_cal", "idade_int")) |>
  mutate(
    n_superior = coalesce(n_superior, 0L),
    n_inferior = coalesce(n_inferior, 0L),
    n_total    = n_superior + n_inferior,
    
    # Para não poluir demais o gráfico, zeros ficam em branco.
    # Se quiser mostrar zeros, troque por: as.character(n_superior)
    label_superior = if_else(n_superior > 0, as.character(n_superior), ""),
    label_inferior = if_else(n_inferior > 0, as.character(n_inferior), "")
  )

cat(sprintf(
  "Óbitos agregados no Lexis numérico: %d\n",
  sum(lexis_plot_num$n_total, na.rm = TRUE)
))

p_lexis_num <- ggplot(lexis_plot_num) +
  # Quadrados do Lexis
  geom_rect(
    aes(xmin = x_ini, xmax = x_fim, ymin = y_ini, ymax = y_fim),
    fill = "white",
    color = "gray75",
    linewidth = 0.35
  ) +
  # Diagonal de cada quadrado
  geom_segment(
    aes(x = x_ini, y = y_ini, xend = x_fim, yend = y_fim),
    color = "gray65",
    linewidth = 0.45
  ) +
  # Óbitos no triângulo superior
  geom_text(
    aes(x = ano_cal + 0.28, y = idade_int + 0.72, label = label_superior),
    size = 2.6,
    color = "#b2182b"
  ) +
  # Óbitos no triângulo inferior
  geom_text(
    aes(x = ano_cal + 0.72, y = idade_int + 0.28, label = label_inferior),
    size = 2.6,
    color = "#b2182b"
  ) +
  scale_x_continuous(
    name = "Ano",
    breaks = seq(2000, 2025, 5),
    minor_breaks = 2000:2025,
    limits = c(2000, 2025),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    name = "Idade completa ao óbito",
    breaks = 0:5,
    limits = c(0, 5),
    expand = c(0, 0)
  ) +
  coord_fixed() +
  labs(
    title = "Diagrama de Lexis",
    subtitle = paste0(
      "Amapá, 2000–2024 "
    )
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray30")
  )

p_lexis_num

ggsave(
  file.path(DIR_GRF, "lexis_ap_numerico.png"),
  plot = p_lexis_num,
  width = 32,
  height = 10,
  units = "cm",
  dpi = 300
)

cat("Diagrama de Lexis numérico salvo: outputs/graficos/lexis_ap_numerico.png\n")
