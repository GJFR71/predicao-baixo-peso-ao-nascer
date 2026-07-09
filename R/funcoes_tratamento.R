# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : funcoes_tratamento.R
# Objetivo: Limpar e preparar a base de dados para as análises
# ==========================================================

# -----------------------------------------------------------------------------
# Função: tratar_dados()
# -----------------------------------------------------------------------------
# Realiza a preparação inicial da base de dados por meio da:
#   - remoção de variáveis não utilizadas;
#   - definição dos parâmetros de imputação;
#   - imputação de valores ausentes.
#
# Parâmetro:
#   dados : base de dados importada.
#
# Retorno:
#   Tibble contendo os dados tratados.
# -----------------------------------------------------------------------------

tratar_dados <- function(dados) {
  
  # Remoção de variáveis -----------------------------------------------
  
  dados <-
    dados |>
    select(
      -PIDADE,
      -PEDUC,
      -ANOSMORTEFETAL,
      -ANOSNASCVIDA
    )
  
  # Parâmetros para imputação ------------------------------------------
  
  media_idade <-
    mean(dados$MIDADE, na.rm = TRUE)
  
  media_escolaridade <-
    mean(dados$MEDUC, na.rm = TRUE)
  
  mediana_gravidez <-
    median(dados$NUMGRAVTOTAL, na.rm = TRUE)
  
  # Imputação de valores ausentes --------------------------------------
  
  dados <-
    dados |>
    mutate(
      MIDADE = coalesce(MIDADE, media_idade),
      
      MEDUC = coalesce(MEDUC, media_escolaridade),
      
      NUMGRAVTOTAL = coalesce(
        NUMGRAVTOTAL,
        mediana_gravidez
      ),
      
      PRENATAL = coalesce(PRENATAL, 0),
      
      NASCMORTO = coalesce(NASCMORTO, 0),
      
      ABORTOS = coalesce(ABORTOS, 0)
    )
  
  dados
}