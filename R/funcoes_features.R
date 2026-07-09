# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : funcoes_features.R
# Objetivo: Criar variáveis derivadas utilizadas nas análises
# ==========================================================

# -----------------------------------------------------------------------------
# Este arquivo reúne as funções responsáveis pela engenharia de atributos do
# projeto.
#
# As funções implementam a criação de variáveis categóricas e indicadores
# compostos (KPI's), preservando a lógica estatística definida no estudo e
# preparando a base para as análises exploratórias e para a modelagem
# preditiva.
#
# Funções disponíveis:
#   • categorizar_variaveis() : cria as variáveis categóricas.
#   • criar_kpis()            : cria os indicadores compostos e suas categorias.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Função: categorizar_variaveis()
# -----------------------------------------------------------------------------
# Cria variáveis categóricas a partir das variáveis originais da base.
#
# Objetivo:
#   Transformar variáveis numéricas em categorias mais interpretáveis para
#   análises descritivas, testes estatísticos e modelagem preditiva.
#
# Parâmetro:
#   dados : tibble contendo a base tratada.
#
# Retorno:
#   Tibble acrescido das variáveis categóricas.
# -----------------------------------------------------------------------------

categorizar_variaveis <- function(dados) {
  
  dados <-
    dados |>
    mutate(
      
      # -----------------------------------------------------------------------
      # Variável resposta
      # -----------------------------------------------------------------------
      
      ABAIXOPESO = factor(
        if_else(
          ABAIXOPESO == 1,
          "abaixo_peso",
          "peso_normal"
        ),
        levels = c(
          "abaixo_peso",
          "peso_normal"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Escolaridade materna
      # -----------------------------------------------------------------------
      
      MEDUC_cat = factor(
        case_when(
          MEDUC <= 9  ~ "baixa",
          MEDUC <= 15 ~ "media",
          TRUE         ~ "alta"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Número de gestações
      # -----------------------------------------------------------------------
      
      GRAVIDEZ_cat = factor(
        case_when(
          NUMGRAVTOTAL == 1 ~ "primigesta",
          NUMGRAVTOTAL <= 3 ~ "2a3",
          TRUE              ~ "4+"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Início do pré-natal
      # -----------------------------------------------------------------------
      
      PRENATAL_cat = factor(
        case_when(
          PRENATAL <= 3 ~ "inicio",
          PRENATAL <= 5 ~ "medio",
          TRUE          ~ "tardio"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Histórico de abortos
      # -----------------------------------------------------------------------
      
      ABORTOS_cat = factor(
        case_when(
          ABORTOS == 0 ~ "nenhum",
          ABORTOS <= 2 ~ "1a2",
          TRUE         ~ "3+"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Resultado do último nascimento
      # -----------------------------------------------------------------------
      
      ULTNASC_cat = factor(
        case_when(
          ULTNASC == 1 ~ "com_vida",
          ULTNASC == 2 ~ "morte_fetal",
          TRUE         ~ "nao_se_aplica"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Estado civil
      # -----------------------------------------------------------------------
      
      ESTCIVIL_cat = factor(
        case_when(
          ESTCIVIL == 1 ~ "casado",
          ESTCIVIL == 2 ~ "solteiro",
          TRUE          ~ "ignorado"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Número de filhos vivos
      # -----------------------------------------------------------------------
      
      FILHOSVIVOS_cat = factor(
        case_when(
          FILHOSVIVOS == 0 ~ "0",
          FILHOSVIVOS <= 2 ~ "1a2",
          TRUE             ~ "3+"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Consumo de cigarro
      # -----------------------------------------------------------------------
      
      FUMO_cat = factor(
        case_when(
          CIGARROSDIA == 0  ~ "nao_fumante",
          CIGARROSDIA <= 20 ~ "fumante_leve",
          TRUE              ~ "fumante_intenso"
        )
      ),
      
      # -----------------------------------------------------------------------
      # Consumo de álcool
      # -----------------------------------------------------------------------
      
      ALCOOL_cat = factor(
        case_when(
          ALCOOLDIA == 0 ~ "nao_consome",
          ALCOOLDIA <= 2 ~ "consumo_leve",
          TRUE           ~ "consumo_elevado"
        )
      )
      
    )
  
  dados
  
}

# -----------------------------------------------------------------------------
# Função: criar_kpis()
# -----------------------------------------------------------------------------
# Cria indicadores compostos (KPI's) utilizados nas análises e nos modelos
# preditivos.
#
# Os KPI's sintetizam diferentes dimensões do risco gestacional:
#   • KPI1_ORGANICAS       : comorbidades clínicas.
#   • KPI2_GESTACIONAL     : complicações obstétricas.
#   • KPI3_COMPORTAMENTAL  : hábitos de risco.
#   • KPI4_PRENATAL        : acesso e qualidade do pré-natal.
#
# Também cria versões categorizadas em:
#   - baixo_risco
#   - risco_moderado
#   - alto_risco
#
# Parâmetro:
#   dados : tibble contendo as variáveis categorizadas.
#
# Retorno:
#   Tibble acrescido dos KPI's.
# -----------------------------------------------------------------------------

criar_kpis <- function(dados) {
  
  dados <-
    dados |>
    mutate(
      
      # -----------------------------------------------------------------------
      # KPI 1 - Comorbidades Orgânicas
      # -----------------------------------------------------------------------
      
      KPI1_ORGANICAS =
        ANEMIA +
        DOENCACARDIACA * 2 +
        DOENCAPULMONAR * 2 +
        DIABETES * 2 +
        HERPES +
        HEMOGLOB +
        DOENCARENAL * 2 +
        RHSENSIVEL,
      
      # -----------------------------------------------------------------------
      # KPI 2 - Complicações Gestacionais
      # -----------------------------------------------------------------------
      
      KPI2_GESTACIONAL =
        HYDRAMNIOS +
        HIPERCRO * 2 +
        HIPER +
        ECLAMPSIA * 3 +
        COLOUTINCO +
        PREMATURO * 2 +
        SANGRAUTERINO +
        AMNIO,
      
      # -----------------------------------------------------------------------
      # KPI 3 - Comportamentos de Risco
      # -----------------------------------------------------------------------
      
      KPI3_COMPORTAMENTAL =
        case_when(
          FUMA == 0 & BEBE == 0 ~ 0,
          FUMA == 1 & CIGARROSDIA <= 20 & BEBE == 0 ~ 1,
          FUMA == 1 & CIGARROSDIA > 20 & BEBE == 0 ~ 2,
          FUMA == 0 & BEBE == 1 & ALCOOLDIA <= 2 ~ 1,
          FUMA == 0 & BEBE == 1 & ALCOOLDIA > 2 ~ 2,
          FUMA == 1 & CIGARROSDIA <= 20 &
            BEBE == 1 & ALCOOLDIA <= 2 ~ 2,
          FUMA == 1 & CIGARROSDIA > 20 &
            BEBE == 1 & ALCOOLDIA <= 2 ~ 3,
          FUMA == 1 & CIGARROSDIA > 20 &
            BEBE == 1 & ALCOOLDIA > 2 ~ 4,
          TRUE ~ 1
        ),
      
      # -----------------------------------------------------------------------
      # KPI 4 - Acesso ao Pré-natal
      # -----------------------------------------------------------------------
      
      KPI4_PRENATAL =
        case_when(
          MEDUC_cat == "baixa" ~ 2,
          MEDUC_cat == "media" ~ 1,
          TRUE ~ 0
        ) +
        case_when(
          ESTCIVIL_cat == "solteiro" ~ 1,
          TRUE ~ 0
        ) +
        case_when(
          PRENATAL_cat == "tardio" ~ 2,
          PRENATAL_cat == "medio" ~ 1,
          TRUE ~ 0
        ) +
        case_when(
          ULTRA == 0 ~ 1,
          TRUE ~ 0
        )
      
    ) |>
    mutate(
      
      # -----------------------------------------------------------------------
      # Categorização dos KPI's
      # -----------------------------------------------------------------------
      
      KPI1_Organico_cat = case_when(
        KPI1_ORGANICAS == 0 ~ "baixo_risco",
        KPI1_ORGANICAS <= 2 ~ "risco_moderado",
        TRUE ~ "alto_risco"
      ),
      
      KPI2_Gestacional_cat = case_when(
        KPI2_GESTACIONAL == 0 ~ "baixo_risco",
        KPI2_GESTACIONAL == 1 ~ "risco_moderado",
        TRUE ~ "alto_risco"
      ),
      
      KPI3_Comportamental_cat = case_when(
        KPI3_COMPORTAMENTAL == 0 ~ "baixo_risco",
        KPI3_COMPORTAMENTAL <= 2 ~ "risco_moderado",
        TRUE ~ "alto_risco"
      ),
      
      KPI4_Prenatal_cat = case_when(
        KPI4_PRENATAL == 0 ~ "baixo_risco",
        KPI4_PRENATAL <= 2 ~ "risco_moderado",
        TRUE ~ "alto_risco"
      )
      
    ) |>
    mutate(
      
      across(
        ends_with("_cat"),
        as.factor
      )
      
    )
  
  dados
  
}