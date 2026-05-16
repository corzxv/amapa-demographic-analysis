# Log de Qualidade dos Dados — Trabalho 1 — Demografia UnB
**UF:** Amapá (AP, código IBGE 16)  
**Período:** 2000–2024  
**Última atualização:** 2026-05-09  
**Script gerador:** `scripts/00_setup_e_dados.R`

---

## 1. SINASC — Nascidos Vivos do Amapá (2000–2024)

### Fonte e método de acesso
- **Sistema:** SINASC (Sistema de Informações sobre Nascidos Vivos) — MS/DATASUS
- **Pacote R:** `microdatasus::fetch_datasus(uf = "AP", information_system = "SINASC")`
- **Arquivo bruto:** `dados/brutos/sinasc_ap_2000_2024.rds`

### Cobertura temporal
| Situação | Detalhe |
|---|---|
| 2000–2022 | Dados consolidados (defasagem < 18 meses confirmada em anos anteriores) |
| 2023 | Potencialmente ainda em consolidação (verificar percentual de completude vs. anos anteriores) |
| 2024 | **PRELIMINARES/PARCIAIS** — o SINASC tem defasagem histórica de 12–18 meses; os dados de 2024 podem estar incompletos na data de download. Comparar total anual com estimativa preliminar do MS antes de calcular indicadores. |

### Filtro de residência
- O parâmetro `uf = "AP"` no microdatasus recupera registros cujo **local de ocorrência** é o Amapá.
- Para garantir que as mães são **residentes no Amapá**, aplicar `filter(substr(CODMUNRES, 1, 2) == "16")` na etapa de limpeza (`01_limpeza_sinasc.R`).
- Efeito "invasão": parturientes de municípios do Pará que dão à luz em Macapá podem aparecer nos dados brutos → removidas pelo filtro de residência.

### Completude estimada das variáveis-chave
| Variável | Conteúdo | Completude esperada | Observação |
|---|---|---|---|
| `DTNASC` | Data de nascimento | > 99% | Base do denominador temporal |
| `CODMUNRES` | Município de residência da mãe | > 95% | Essencial para filtro de UF |
| `IDADEMAE` | Idade da mãe | > 95% | Necessária para TEF por grupo etário |
| `SEXO` | Sexo do RN | ~98% | Pequena proporção ignorada (código 0 ou 9) |
| `RACACORMAE` | Raça/cor da mãe | ~85–90% | Sub-registro maior em anos anteriores |
| `ESCMAE` | Escolaridade da mãe | ~80–85% | Melhora após 2010 |
| `GESTACAO` | Semanas de gestação | ~85% | Relevante para óbitos fetais |
| `CONSULTAS` | Nº de consultas pré-natal | ~90% | Qualidade assistencial |
| `PESO` | Peso ao nascer (g) | ~97% | Importante para prematuridade |

### Problemas conhecidos
- **Sub-registro SINASC no Norte:** estimado em ~85–90% (nacional ~95%); MS/IBGE realiza correções periódicas.
- **Evolução temporal:** anos anteriores a 2005 podem ter maior incompletude.
- **Código MUNRESID ausente** em algumas versões: usar `CODMUNRES` como referência primária.

### Verificação de totais com TabNet
Conferir em: http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sinasc/cnv/nvuf.def  
(Seleção: Amapá, todos os anos, nascimentos por local de residência)

---

## 2. SIM-DO — Óbitos Gerais do Amapá (2000–2024)

### Fonte e método de acesso
- **Sistema:** SIM (Sistema de Informações sobre Mortalidade) — MS/DATASUS
- **Pacote R:** `microdatasus::fetch_datasus(uf = "AP", information_system = "SIM-DO")`
- **Arquivo bruto:** `dados/brutos/sim_do_ap_2000_2024.rds`

### Cobertura temporal
| Situação | Detalhe |
|---|---|
| 2000–2022 | Dados consolidados |
| 2023 | Em consolidação final |
| 2024 | **PRELIMINARES** — mesmo cuidado do SINASC; possível subestimação dos óbitos totais |

### Decisão metodológica sobre residência vs. ocorrência
> **Problema:** `fetch_datasus(uf = "AP")` retorna óbitos ocorridos no Amapá, não necessariamente de residentes do AP.
>
> **Limitação documentada:** não é viável baixar todas as 27 UFs e filtrar por residência (custo computacional elevado). A abordagem adotada é:
> 1. Baixar óbitos ocorridos no AP.
> 2. Filtrar por `substr(CODMUNRES, 1, 2) == "16"` para excluir óbitos de não-residentes.
> 3. Documentar que óbitos de residentes do AP ocorridos em outros estados (tipicamente transferências para Belém/PA ou Manaus/AM) **não são capturados**.
>
> **Impacto provável:** subestimação leve dos óbitos, mais pronunciada em grupos que recorrem a hospitais de referência fora do estado (casos graves, alguns óbitos infantis). Considerar este viés na interpretação das taxas.

### Completude estimada das variáveis-chave
| Variável | Conteúdo | Completude esperada | Observação |
|---|---|---|---|
| `DTOBITO` | Data do óbito | > 99% | Base temporal |
| `CODMUNRES` | Município de residência | > 90% | Filtro de UF |
| `CAUSABAS` | Causa básica (CID-10) | ~85% | ~15% nacionais mal definidas; Norte pode ter mais |
| `SEXO` | Sexo | > 98% | |
| `IDADE` | Idade codificada | > 95% | Decodificar: 4NN = NN anos; 5NN = 100+NN anos |
| `RACACOR` | Raça/cor | ~80% | Melhora após 2010 |
| `ESC` | Escolaridade | ~70–80% | Campo com maior sub-registro |
| `LOCOCOR` | Local de ocorrência | > 90% | |

### Causas mal definidas
- **Nacional:** ~15% dos óbitos sem causa básica definida (CID cap. XVIII, R00–R99).
- **Norte/Amapá:** esperado acima da média nacional — monitorar percentual antes de análises por causa.
- **Ação sugerida:** calcular e reportar o percentual de causas mal definidas por ano; avaliar tendência.

### Verificação de totais com TabNet
Conferir em: http://tabnet.datasus.gov.br/cgi/tabcgi.exe?sim/cnv/obt10uf.def  
(Seleção: Amapá, local de residência, todos os anos)

---

## 3. SIM-DOFET — Óbitos Fetais do Amapá (2000–2024)

### Fonte e método de acesso
- **Sistema:** SIM — Declarações de Óbito Fetal — MS/DATASUS
- **Pacote R:** `microdatasus::fetch_datasus(uf = "AP", information_system = "SIM-DOFET")`
- **Arquivo bruto:** `dados/brutos/sim_dofet_ap_2000_2024.rds`

### Definição legal de óbito fetal (Brasil)
Feto sem vida ao nascer com **≥ 20 semanas de gestação** OU **≥ 500 g** OU **≥ 25 cm**.

### Uso previsto
- Cálculo da **Taxa de Mortalidade Perinatal**:
  ```
  TMPerinatal = (Óbitos fetais ≥ 28 sem + Óbitos 0–6 dias) /
                (Nascimentos + Óbitos fetais) × 1000
  ```
- Cálculo da **Taxa de Mortalidade Fetal Tardia**:
  ```
  TMFetal_tardia = Óbitos fetais ≥ 28 sem /
                  (Nascimentos + Óbitos fetais) × 1000
  ```

### Problemas conhecidos
- **Sub-registro elevado:** óbitos fetais precoces (< 22 semanas) frequentemente não registrados.
- **Variável GESTACAO** com alta incompletude — usar todos os óbitos fetais registrados como proxy conservadora.
- Volume pequeno de casos → instabilidade nas taxas para anos individuais; considerar média trienal.

---

## 4. Projeções Populacionais IBGE — Revisão 2024

### Fonte e método de acesso
- **Publicação:** IBGE, Projeção da População do Brasil e das Unidades da Federação, Revisão 2024
- **Pacote R:** `sidrar::get_sidra()`
- **Tabela SIDRA tentada (1ª prioridade):** **9697** — Revisão 2024, por sexo e idade simples
- **Tabela SIDRA alternativa (fallback):** **7358** — Revisão 2018 (base Censo 2010)
- **Arquivo bruto:** `dados/brutos/projecoes_ibge_ap.rds`

### Distinção entre revisões
| Revisão | Base censitária | Período de projeção | Relevância |
|---|---|---|---|
| **2024** | Censo 2022 | 2022–2070 | **Preferencial** — metodologia atualizada |
| 2018 | Censo 2010 | 2010–2060 | Aceita como alternativa documentada |

> **Decisão metodológica:** se apenas a tabela 7358 (revisão 2018) estiver disponível via `sidrar`, documentar a limitação e verificar se o IBGE disponibilizou a revisão 2024 por outros meios (FTP, portal IBGE). A revisão 2024 é a metodologicamente correta para análises com referência ao Censo 2022.

### Verificação manual
- Portal IBGE: https://www.ibge.gov.br/estatisticas/sociais/populacao/9109-projecao-da-populacao.html
- SIDRA (busca por tabelas): https://sidra.ibge.gov.br/pesquisa/projecao-da-populacao/tabelas

### Variáveis necessárias
- Ano (2000–2024)
- Sexo (masculino / feminino / total)
- Idade simples (0, 1, 2, ..., 99+)
- UF = Amapá (código 16)

---

## 5. Censo 2022 — População do Amapá por Sexo e Idade

### Fonte e método de acesso
- **Publicação:** IBGE, Censo Demográfico 2022
- **Pacote R:** `sidrar::get_sidra(9514)` (fonte primária adotada)
- **censobr:** descartado — pacote confirmou suporte apenas até 2010 (`Data currently available only for the years 1960 1970 1980 1991 2000 2010`)
- **Arquivo bruto:** `dados/brutos/censo2022_pop_ap.rds`

### Tabelas SIDRA disponíveis
| Tabela | Descrição | Classificações |
|---|---|---|
| **9514** | Pop. residente por sexo e grupos de idade | Sexo (C2), Grupos de idade (C287) |
| **9515** | Pop. residente por sexo e idade simples | Sexo (C2), Idade (C58) — **preferencial** |

> **Decisão metodológica:** SIDRA tabela 9515 (se disponível) é preferencial por fornecer **idades simples**, necessárias para calcular TEF e TEMx diretamente sem interpolação. Tabela 9514 (grupos quinquenais) requer interpolação adicional.

### Uso previsto
- Denominador para taxas brutas de natalidade e mortalidade no ano 2022.
- Base para construção da pirâmide etária do Amapá em 2022.
- Ponto de ancoragem para avaliar consistência das projeções (valor real vs. projetado para 2022).

### Variáveis necessárias (censobr)
| Variável | Descrição |
|---|---|
| `code_state` | Código UF (filtrar == 16) |
| `v0601` | Sexo (1 = Masc, 2 = Fem) |
| `v0602` | Idade (anos completos) |
| `v0010` | Peso amostral (para expansão) |

---

## 6. Resumo Geral de Status das Bases

| Base | Arquivo .rds | Download | Período | Dados 2024 | Filtro Residência |
|---|---|---|---|---|---|
| SINASC | `sinasc_ap_2000_2024.rds` | microdatasus | 2000–2024 | PRELIMINARES | Aplicar `CODMUNRES == "16..."` |
| SIM-DO | `sim_do_ap_2000_2024.rds` | microdatasus | 2000–2024 | PRELIMINARES | Aplicar `CODMUNRES == "16..."` |
| SIM-DOFET | `sim_dofet_ap_2000_2024.rds` | microdatasus | 2000–2024 | PRELIMINARES | Aplicar `CODMUNRES == "16..."` |
| Projeções IBGE | `projecoes_ibge_ap.rds` | sidrar | 2022–2070 | Projeção | Já filtrado por UF=16 |
| Censo 2022 | `censo2022_pop_ap.rds` | censobr/sidrar | 2022 | N/A | Já filtrado por UF=16 |

---

## 7. Campos Problemáticos Transversais

### Raça/cor (`RACACOR` / `RACACORMAE`)
- Sub-registro mais elevado em anos anteriores a 2010.
- No Amapá, importante para análises de equidade (população indígena expressiva).
- Categorias: 1=Branca, 2=Preta, 3=Amarela, 4=Parda, 5=Indígena, 9=Ignorada.

### Escolaridade (`ESC` / `ESCMAE`)
- Campo com maior incompletude (60–85% de completude, variável por sistema e ano).
- Recodificação necessária: os códigos mudaram ao longo dos anos (pré e pós 2010).

### Municípios de residência
- Verificar se todos os `CODMUNRES` com prefixo "16" correspondem a municípios válidos do Amapá (16 municípios: Macapá, Santana, Laranjal do Jari, etc.).
- Códigos inválidos ou zerados → tratar como missing.

---

## 8. Referências Metodológicas

- Materiais da disciplina de Demografia
- RIPSA, *Indicadores básicos para a saúde no Brasil*, Cap. 4 — SIM e SINASC
- Szwarcwald et al. (2019) — cobertura do SINASC (~94,8% nacional, heterogênea)
- `_contexto_demografia.md` §3.4 — cuidados metodológicos consolidados
