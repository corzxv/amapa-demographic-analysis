# Trabalho 1 de Demografia: Amapá (2000-2024)

Repositório do Trabalho 1 da disciplina de Demografia, com foco na dinâmica demográfica do Amapá (UF AP, código IBGE 16) entre 2000 e 2024.

O projeto reúne scripts em R, bases tratadas, tabelas, gráficos e o relatório final em LaTeX/Overleaf. As análises foram construídas a partir de dados do SINASC, SIM, SIM-DOFET, projeções populacionais do IBGE - Revisão 2024 e Censo Demográfico 2022.

## Conteúdo analisado

- Diagrama de Lexis para óbitos de menores de 5 anos.
- Probabilidade de sobrevivência até o primeiro aniversário e até a idade exata de 5 anos.
- Mortalidade infantil por raça/cor em 2022 e 2023.
- Indicadores de natalidade, fecundidade e reprodução.
- Comparação dos indicadores de 2022 usando projeção populacional e Censo 2022.
- Tabelas de contingência para idade, escolaridade da mãe e tipo de parto.
- Taxa Bruta de Mortalidade e taxas específicas por sexo e idade.
- Mortalidade infantil, neonatal, pós-neonatal e perinatal.
- Estrutura de causas de morte, com destaque para COVID-19.
- Tábuas de vida abreviadas por sexo para 2010 e 2024.

## Estrutura

```text
demografia_ap/
  dados/
    brutos/          # bases baixadas ou importadas das fontes
    processados/     # bases tratadas e logs de qualidade
  scripts/           # scripts R por etapa da análise
  outputs/
    tabelas/         # tabelas exportadas
    graficos/        # gráficos gerados
  relatorio/         # materiais auxiliares do relatório

relatorio_overleaf/  # projeto LaTeX final para Overleaf
```

O arquivo `Trabalho_1___Demog_revisado_corrigido_bib.zip` contém a versão final do projeto Overleaf, já revisada.

## Scripts principais

- `00_setup_e_dados.R`: configuração do projeto, download/importação e checagens iniciais das bases.
- `01_lexis_sobrevivencia.R`: Diagrama de Lexis e probabilidades de sobrevivência na infância.
- `02_fecundidade.R`: indicadores de natalidade, fecundidade, reprodução e tabelas de contingência do SINASC 2024.
- `03_mortalidade_geral_infantil.R`: mortalidade geral, taxas específicas e indicadores infantis/perinatais.
- `ofc_q3_c_d.R`: estrutura de causas de morte e tábuas de vida.

## Fontes de dados

- DATASUS/MS: SINASC, SIM e SIM-DOFET.
- IBGE: Projeções da População - Revisão 2024.
- IBGE: Censo Demográfico 2022.

## Observações

Os materiais de aula, provas antigas e bibliografia disponibilizada pela disciplina não foram publicados neste repositório. O objetivo aqui é manter apenas os códigos, bases de trabalho, saídas produzidas e relatório final do trabalho.

Os dados de 2024 devem ser interpretados com cautela, pois podem estar sujeitos a atualização nas bases do DATASUS.
