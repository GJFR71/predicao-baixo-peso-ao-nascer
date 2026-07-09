# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 04_bivariada.R
# Objetivo: Análise bivariada entre o desfecho e as variáveis
#           explicativas
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Importação e preparação dos dados -----------------------------------

source("R/01_importacao.R")

source("R/02_tratamento.R")

# Funções auxiliares ---------------------------------------------------

source("R/funcoes_eda.R")

# ---------------------------------------------------------------------
# Variáveis para análise bivariada
# ---------------------------------------------------------------------

variaveis_bivariadas <-
  c(
    "MEDUC_cat",
    "MIDADE_cat",
    "NUMGRAVTOTAL_cat",
    "PRENATAL_cat",
    "NASCMORTO_cat",
    "ABORTOS_cat",
    "ESTCIVIL_cat",
    "FILHOSVIVOS_cat",
    "CIGARROSDIA_cat",
    "ALCOOLDIA_cat",
    "BEBE",
    "FUMA",
    "ANEMIA",
    "DOENCACARDIACA",
    "DOENCAPULMONAR",
    "DIABETES",
    "HERPES",
    "HYDRAMNIOS",
    "HEMOGLOB",
    "HIPERCRO",
    "HIPER",
    "ECLAMPSIA",
    "COLOUTINCO",
    "REMEDIOINFANTIL",
    "PREMATURO",
    "DOENCARENAL",
    "RHSENSIVEL",
    "SANGRAUTERINO",
    "AMNIO",
    "ULTRA"
  )

# ---------------------------------------------------------------------
# Análise bivariada
# ---------------------------------------------------------------------

purrr::walk(
  variaveis_bivariadas,
  \(variavel) {
    
    analisar_bivariada(
      dados = dados,
      variavel = variavel
    )
    
  }
)