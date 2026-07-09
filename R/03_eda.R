# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 03_eda.R
# Objetivo: Realizar a análise exploratória dos dados
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Importação e preparação dos dados -----------------------------------

source("R/01_importacao.R")

source("R/02_tratamento.R")

# Funções auxiliares ---------------------------------------------------

source("R/funcoes_eda.R")

# ---------------------------------------------------------------------
# Inspeção geral da base
# ---------------------------------------------------------------------

glimpse(dados)

summary(dados)

round(colMeans(is.na(dados)) * 100, 2)

table(dados$ABAIXOPESO)

# ---------------------------------------------------------------------
# Análise univariada das variáveis numéricas
# ---------------------------------------------------------------------

variaveis_numericas <-
  tibble::tribble(
    ~variavel,        ~titulo,
    "MIDADE",         "Idade Materna",
    "MEDUC",          "Escolaridade Materna",
    "NUMGRAVTOTAL",   "Número Total de Gestações",
    "PRENATAL",       "Início do Pré-natal",
    "NASCMORTO",      "Nascidos Mortos",
    "ABORTOS",        "Abortos",
    "FILHOSVIVOS",    "Filhos Vivos",
    "CIGARROSDIA",    "Cigarros por Dia",
    "ALCOOLDIA",      "Consumo de Álcool"
  )

purrr::pwalk(
  variaveis_numericas,
  \(variavel, titulo) {
    
    analisar_variavel_numerica(
      dados = dados,
      variavel = variavel,
      titulo = titulo
    )
    
  }
)

# ---------------------------------------------------------------------
# Análise univariada das variáveis categóricas
# ---------------------------------------------------------------------

variaveis_categoricas <-
  tibble::tribble(
    ~variavel,          ~titulo,
    "BEBE",             "Consumo de Bebida Alcoólica",
    "FUMA",             "Tabagismo",
    "ANEMIA",           "Anemia",
    "DOENCACARDIACA",   "Doença Cardíaca",
    "DOENCAPULMONAR",   "Doença Pulmonar",
    "DIABETES",         "Diabetes",
    "HERPES",           "Herpes",
    "HYDRAMNIOS",       "Hidrâmnio",
    "HEMOGLOB",         "Hemoglobinopatia",
    "HIPERCRO",         "Hipertensão Crônica",
    "HIPER",            "Hipertensão Gestacional",
    "ECLAMPSIA",        "Eclâmpsia",
    "COLOUTINCO",       "Colo Uterino Incompetente",
    "REMEDIOINFANTIL",  "Uso de Medicamento",
    "PREMATURO",        "Histórico de Prematuridade",
    "DOENCARENAL",      "Doença Renal",
    "RHSENSIVEL",       "Sensibilização Rh",
    "SANGRAUTERINO",    "Sangramento Uterino",
    "AMNIO",            "Amniocentese",
    "ULTRA",            "Ultrassonografia"
  )

purrr::pwalk(
  variaveis_categoricas,
  \(variavel, titulo) {
    
    analisar_variavel_categorica(
      dados = dados,
      variavel = variavel,
      titulo = titulo
    )
    
  }
)