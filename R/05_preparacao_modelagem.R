# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 05_preparacao_modelagem.R
# Objetivo: Preparar a base de dados para os modelos preditivos
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Preparação dos dados -------------------------------------------------

# O script 02_tratamento.R realiza a importação, limpeza,
# tratamento e engenharia de atributos.

source("R/02_tratamento.R")

# ---------------------------------------------------------------------
# Seleção das variáveis para modelagem
# ---------------------------------------------------------------------

base_modelagem <-
  dados |>
  select(
    
    # Variável resposta
    ABAIXOPESO,
    
    # Variáveis clínicas
    MIDADE,
    ABORTOS,
    FILHOSVIVOS_cat,
    
    # Indicadores compostos
    KPI1_Organico_cat,
    KPI2_Gestacional_cat,
    KPI3_Comportamental_cat,
    KPI4_Prenatal_cat
    
  )

# ---------------------------------------------------------------------
# Receita de pré-processamento
# ---------------------------------------------------------------------

receita <-
  recipe(
    ABAIXOPESO ~ .,
    data = base_modelagem
  ) |>
  step_normalize(
    all_numeric_predictors()
  ) |>
  step_dummy(
    all_nominal_predictors()
  ) |>
  step_zv(
    all_predictors()
  )

# ---------------------------------------------------------------------
# Divisão da base em treino e teste
# ---------------------------------------------------------------------

set.seed(1234)

divisao <-
  initial_split(
    base_modelagem,
    prop = 0.80,
    strata = ABAIXOPESO
  )

dados_treino <-
  training(divisao)

dados_teste <-
  testing(divisao)

# ---------------------------------------------------------------------
# Validação cruzada
# ---------------------------------------------------------------------

set.seed(1234)

cv_folds <-
  vfold_cv(
    dados_treino,
    v = 5,
    strata = ABAIXOPESO
  )

# ---------------------------------------------------------------------
# Inspeção das bases
# ---------------------------------------------------------------------

cat("\n")
cat("=====================================================\n")
cat("BASE DE TREINAMENTO\n")
cat("=====================================================\n\n")

print(dim(dados_treino))

glimpse(dados_treino)

cat("\n")
cat("=====================================================\n")
cat("BASE DE TESTE\n")
cat("=====================================================\n\n")

print(dim(dados_teste))

glimpse(dados_teste)

cat("\n")
cat("=====================================================\n")
cat("VALIDAÇÃO CRUZADA\n")
cat("=====================================================\n\n")

print(cv_folds)