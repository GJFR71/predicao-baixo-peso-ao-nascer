# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 01_importacao.R
# Objetivo: Importar a base de dados e realizar uma inspeção inicial
# ==========================================================

# Carrega o ambiente do projeto (pacotes e configurações globais).
source("R/00_setup.R")

# Importação ------------------------------------------------------------

# Lê a base de dados original disponibilizada em formato SAS.
dados <- read_sas(
  here("data", "raw", "bebes.sas7bdat")
)

# Inspeção inicial ------------------------------------------------------

# Exibe a estrutura das variáveis, seus tipos e alguns valores.
glimpse(dados)

# Apresenta um resumo estatístico inicial da base.
summary(dados)