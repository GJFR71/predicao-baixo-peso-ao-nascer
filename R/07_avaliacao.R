# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 07_avaliacao.R
# Objetivo: Avaliar, comparar e apresentar o desempenho dos modelos
#           preditivos utilizados no estudo
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Ajuste dos modelos ---------------------------------------------------

source("R/06_modelagem.R")


# ---------------------------------------------------------------------
# Ranking dos modelos
# ---------------------------------------------------------------------

cat("\n")
cat("=====================================================\n")
cat("RANKING DOS MODELOS\n")
cat("=====================================================\n\n")

print(
  comparacao_modelos,
  n = Inf,
  width = Inf
)

# ---------------------------------------------------------------------
# Melhor modelo
# ---------------------------------------------------------------------

melhor_modelo <-
  comparacao_modelos |>
  slice(1)

cat("\n")
cat("=====================================================\n")
cat("MELHOR MODELO\n")
cat("=====================================================\n\n")

print(
  melhor_modelo,
  width = Inf
)

# ---------------------------------------------------------------------
# Comparação gráfica
# ---------------------------------------------------------------------

grafico_comparacao_modelos <-
  comparacao_modelos |>
  pivot_longer(
    cols = c(
      Acuracia,
      Recall,
      Precisao,
      F1_Score,
      AUC
    ),
    names_to = "Metrica",
    values_to = "Valor"
  ) |>
  ggplot(
    aes(
      x = Modelo,
      y = Valor,
      fill = Modelo
    )
  ) +
  geom_col() +
  facet_wrap(
    ~ Metrica,
    scales = "free_y"
  ) +
  labs(
    title = "Comparação do desempenho dos modelos",
    x = NULL,
    y = "Valor da métrica"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  )

print(grafico_comparacao_modelos)


# ---------------------------------------------------------------------
# Conclusão da avaliação
# ---------------------------------------------------------------------

cat("\n")
cat("=====================================================\n")
cat("CONCLUSÃO DA AVALIAÇÃO\n")
cat("=====================================================\n\n")

conclusao_modelo <-
  paste0(
    "O modelo selecionado foi ",
    melhor_modelo$Modelo,
    ", por apresentar o maior Recall (",
    round(melhor_modelo$Recall, 3),
    "), acompanhado de AUC = ",
    round(melhor_modelo$AUC, 3),
    ", F1-Score = ",
    round(melhor_modelo$F1_Score, 3),
    ", Accuracy = ",
    round(melhor_modelo$Acuracia, 3),
    " e Precisão = ",
    round(melhor_modelo$Precisao, 3),
    "."
  )

cat(conclusao_modelo)

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_finais <- list(
  comparacao_modelos = comparacao_modelos,
  melhor_modelo = melhor_modelo,
  grafico_comparacao_modelos = grafico_comparacao_modelos,
  conclusao_modelo = conclusao_modelo,
  tabela_resultado_rf = tabela_resultado_rf,
  tabela_resultado_xgb = tabela_resultado_xgb,
  tabela_resultado_lasso = tabela_resultado_lasso,
  tabela_resultado_ridge = tabela_resultado_ridge,
  tabela_resultado_elasticnet = tabela_resultado_elasticnet
)


# ---------------------------------------------------------------------
# Salvamento dos resultados
# ---------------------------------------------------------------------

if (!dir.exists(here("objetos"))) {
  
  dir.create(
    here("objetos"),
    recursive = TRUE
  )
  
}

saveRDS(
  resultados_finais,
  file = here("objetos", "resultados_finais.rds")
)

cat("\n")
cat("Resultados consolidados salvos em:\n")
cat(here("objetos", "resultados_finais.rds"), "\n")