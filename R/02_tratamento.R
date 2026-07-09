# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 02_tratamento.R
# Objetivo: Executar a preparação dos dados para análise
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Importação dos dados -------------------------------------------------

source("R/01_importacao.R")

# Funções auxiliares ---------------------------------------------------

source("R/funcoes_tratamento.R")

source("R/funcoes_features.R")

# ---------------------------------------------------------------------
# Tratamento dos dados
# ---------------------------------------------------------------------

dados <-
  tratar_dados(dados)

# ---------------------------------------------------------------------
# Engenharia de atributos
# ---------------------------------------------------------------------

dados <-
  categorizar_variaveis(dados)

# ---------------------------------------------------------------------
# Criação dos indicadores compostos (KPIs)
# ---------------------------------------------------------------------

dados <-
  criar_kpis(dados)