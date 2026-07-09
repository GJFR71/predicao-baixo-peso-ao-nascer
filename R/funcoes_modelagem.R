# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : funcoes_modelagem.R
# Objetivo: Funções auxiliares para ajuste dos modelos preditivos
# ==========================================================

# -----------------------------------------------------------------------------
# Este arquivo reúne as funções responsáveis pelo treinamento dos modelos de
# aprendizado de máquina utilizados no projeto.
#
# Cada função possui uma única responsabilidade, permitindo reutilização,
# manutenção e comparação entre diferentes algoritmos.
#
# Funções disponíveis:
#   • ajustar_regressao_logistica()
#   • ajustar_random_forest()       (em construção)
#   • ajustar_xgboost()             (em construção)
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Função: ajustar_regressao_logistica()
# -----------------------------------------------------------------------------
# Ajusta um modelo de regressão logística para predição de baixo peso ao nascer.
#
# Parâmetro:
#   dados : tibble contendo a base destinada à modelagem.
#
# Retorno:
#   Objeto do tipo glm.
# -----------------------------------------------------------------------------

ajustar_regressao_logistica <- function(dados) {
  
  modelo <-
    glm(
      ABAIXOPESO ~ .,
      data = dados,
      family = binomial(link = "logit")
    )
  
  modelo
  
}