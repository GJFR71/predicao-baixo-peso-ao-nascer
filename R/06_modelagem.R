# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 06_modelagem.R
# Objetivo: Ajustar os modelos preditivos de aprendizado de máquina
# ==========================================================

# Configuração do ambiente --------------------------------------------

source("R/00_setup.R")

# Preparação da base para modelagem -----------------------------------

source("R/05_preparacao_modelagem.R")

# ---------------------------------------------------------------------
# RANDOM FOREST
# ---------------------------------------------------------------------
# Modelo de classificação baseado em árvores de decisão utilizando
# agregação por bootstrap (bagging).
#
# O objetivo é maximizar a capacidade de identificação de recém-nascidos
# com baixo peso, priorizando a sensibilidade (Recall).
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Especificação do modelo
# ---------------------------------------------------------------------

modelo_rf <-
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

# ---------------------------------------------------------------------
# Workflow
# ---------------------------------------------------------------------

workflow_rf <-
  workflow() |>
  add_recipe(receita) |>
  add_model(modelo_rf)

# ---------------------------------------------------------------------
# Espaço de busca dos hiperparâmetros
# ---------------------------------------------------------------------

parametros_rf <-
  workflow_rf |>
  extract_parameter_set_dials() |>
  finalize(dados_treino)

set.seed(2708)

grade_rf <-
  grid_random(
    parametros_rf,
    size = 20
  )

# ---------------------------------------------------------------------
# Ajuste por validação cruzada
# ---------------------------------------------------------------------

set.seed(2708)

resultados_tune_rf <-
  tune_grid(
    workflow_rf,
    resamples = cv_folds,
    grid = grade_rf,
    metrics = metric_set(
      accuracy,
      recall,
      precision,
      f_meas,
      roc_auc
    ),
    control =
      control_grid(
        verbose = TRUE,
        save_pred = TRUE
      )
  )

# ---------------------------------------------------------------------
# Melhor combinação de hiperparâmetros
# ---------------------------------------------------------------------

melhores_parametros_rf <-
  select_best(
    resultados_tune_rf,
    metric = "recall"
  )

melhores_parametros_rf

# ---------------------------------------------------------------------
# Modelo final
# ---------------------------------------------------------------------

workflow_rf_final <-
  finalize_workflow(
    workflow_rf,
    melhores_parametros_rf
  )

modelo_rf_final <-
  fit(
    workflow_rf_final,
    data = dados_treino
  )

# ---------------------------------------------------------------------
# Predições no conjunto de teste
# ---------------------------------------------------------------------

predicoes_rf_classe <-
  predict(
    modelo_rf_final,
    new_data = dados_teste,
    type = "class"
  )

predicoes_rf_prob <-
  predict(
    modelo_rf_final,
    new_data = dados_teste,
    type = "prob"
  )

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_rf <-
  dados_teste |>
  select(ABAIXOPESO) |>
  bind_cols(
    predicoes_rf_classe,
    predicoes_rf_prob
  ) |>
  mutate(
    ABAIXOPESO = as.factor(ABAIXOPESO)
  )

# ---------------------------------------------------------------------
# Matriz de confusão
# ---------------------------------------------------------------------

conf_mat_rf <-
  conf_mat(
    resultados_rf,
    truth = ABAIXOPESO,
    estimate = .pred_class
  )

print(conf_mat_rf)

# ---------------------------------------------------------------------
# Métricas de desempenho
# ---------------------------------------------------------------------

metricas_rf <-
  bind_rows(
    accuracy(
      resultados_rf,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    precision(
      resultados_rf,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    recall(
      resultados_rf,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    f_meas(
      resultados_rf,
      truth = ABAIXOPESO,
      estimate = .pred_class
    )
  )

print(metricas_rf)

# ---------------------------------------------------------------------
# Curva ROC e AUC
# ---------------------------------------------------------------------

roc_auc(
  resultados_rf,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
)

roc_curve(
  resultados_rf,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
) |>
  autoplot()

# ---------------------------------------------------------------------
# Tabela consolidada
# ---------------------------------------------------------------------

tabela_resultado_rf <-
  tibble(
    Modelo = "Random Forest",
    Acuracia =
      accuracy(
        resultados_rf,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Recall =
      recall(
        resultados_rf,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Precisao =
      precision(
        resultados_rf,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    F1_Score =
      f_meas(
        resultados_rf,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    AUC =
      roc_auc(
        resultados_rf,
        truth = ABAIXOPESO,
        .pred_abaixo_peso
      )$.estimate
  )

tabela_resultado_rf |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

# ---------------------------------------------------------------------
# XGBOOST
# ---------------------------------------------------------------------
# Modelo baseado em Gradient Boosting.
#
# O objetivo permanece a maximização do Recall, priorizando a
# identificação dos recém-nascidos com baixo peso.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Especificação do modelo
# ---------------------------------------------------------------------

modelo_xgb <-
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune()
  ) |>
  set_engine("xgboost") |>
  set_mode("classification")

# ---------------------------------------------------------------------
# Workflow
# ---------------------------------------------------------------------

workflow_xgb <-
  workflow() |>
  add_recipe(receita) |>
  add_model(modelo_xgb)

# ---------------------------------------------------------------------
# Grade de hiperparâmetros
# ---------------------------------------------------------------------

grid_xgb <-
  grid_latin_hypercube(
    trees(),
    tree_depth(),
    learn_rate(range = c(-2, -0.5)),
    loss_reduction(),
    sample_prop(),
    finalize(
      mtry(),
      dados_treino
    ),
    size = 20
  )

# ---------------------------------------------------------------------
# Ajuste por validação cruzada
# ---------------------------------------------------------------------

set.seed(2708)

tuning_xgb <-
  tune_grid(
    workflow_xgb,
    resamples = cv_folds,
    grid = grid_xgb,
    metrics = metric_set(
      accuracy,
      recall,
      roc_auc
    ),
    control =
      control_grid(
        verbose = TRUE,
        save_pred = TRUE
      )
  )

# ---------------------------------------------------------------------
# Melhores resultados
# ---------------------------------------------------------------------

show_best(
  tuning_xgb,
  metric = "accuracy"
)

show_best(
  tuning_xgb,
  metric = "recall"
)

show_best(
  tuning_xgb,
  metric = "roc_auc"
)

# ---------------------------------------------------------------------
# Seleção do melhor modelo
# ---------------------------------------------------------------------

melhores_parametros_xgb <-
  select_best(
    tuning_xgb,
    metric = "recall"
  )

melhores_parametros_xgb

# ---------------------------------------------------------------------
# Workflow final
# ---------------------------------------------------------------------

workflow_xgb_final <-
  finalize_workflow(
    workflow_xgb,
    melhores_parametros_xgb
  )

# ---------------------------------------------------------------------
# Ajuste final
# ---------------------------------------------------------------------

modelo_xgb_final <-
  fit(
    workflow_xgb_final,
    data = dados_treino
  )

# ---------------------------------------------------------------------
# Predições no conjunto de teste
# ---------------------------------------------------------------------

predicoes_xgb_classe <-
  predict(
    modelo_xgb_final,
    new_data = dados_teste,
    type = "class"
  )

predicoes_xgb_prob <-
  predict(
    modelo_xgb_final,
    new_data = dados_teste,
    type = "prob"
  )

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_xgb <-
  dados_teste |>
  select(ABAIXOPESO) |>
  bind_cols(
    predicoes_xgb_classe,
    predicoes_xgb_prob
  ) |>
  mutate(
    ABAIXOPESO = as.factor(ABAIXOPESO)
  )

# ---------------------------------------------------------------------
# Matriz de confusão
# ---------------------------------------------------------------------

conf_mat_xgb <-
  conf_mat(
    resultados_xgb,
    truth = ABAIXOPESO,
    estimate = .pred_class
  )

print(conf_mat_xgb)

# ---------------------------------------------------------------------
# Métricas de desempenho
# ---------------------------------------------------------------------

metricas_xgb <-
  bind_rows(
    accuracy(
      resultados_xgb,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    precision(
      resultados_xgb,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    recall(
      resultados_xgb,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    f_meas(
      resultados_xgb,
      truth = ABAIXOPESO,
      estimate = .pred_class
    )
  )

print(metricas_xgb)

# ---------------------------------------------------------------------
# Curva ROC
# ---------------------------------------------------------------------

roc_auc(
  resultados_xgb,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
)

roc_curve(
  resultados_xgb,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
) |>
  autoplot()

# ---------------------------------------------------------------------
# Tabela consolidada
# ---------------------------------------------------------------------

tabela_resultado_xgb <-
  tibble(
    Modelo = "XGBoost",
    Acuracia =
      accuracy(
        resultados_xgb,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Recall =
      recall(
        resultados_xgb,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Precisao =
      precision(
        resultados_xgb,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    F1_Score =
      f_meas(
        resultados_xgb,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    AUC =
      roc_auc(
        resultados_xgb,
        truth = ABAIXOPESO,
        .pred_abaixo_peso
      )$.estimate
  ) |>
  mutate(
    across(
      where(is.numeric),
      round,
      3
    )
  )

print(tabela_resultado_xgb)

# ---------------------------------------------------------------------
# ELASTIC NET
# ---------------------------------------------------------------------
# Regressão logística penalizada utilizando combinação das penalizações
# L1 (Lasso) e L2 (Ridge).
#
# O foco permanece na maximização do Recall.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Especificação do modelo
# ---------------------------------------------------------------------

modelo_elasticnet <-
  logistic_reg(
    penalty = 0.0178,
    mixture = 0.75
  ) |>
  set_engine("glmnet") |>
  set_mode("classification")

# ---------------------------------------------------------------------
# Workflow
# ---------------------------------------------------------------------

workflow_elasticnet <-
  workflow() |>
  add_recipe(receita) |>
  add_model(modelo_elasticnet)

# ---------------------------------------------------------------------
# Ajuste do modelo
# ---------------------------------------------------------------------

modelo_elasticnet_final <-
  fit(
    workflow_elasticnet,
    data = dados_treino
  )

# ---------------------------------------------------------------------
# Predições
# ---------------------------------------------------------------------

predicoes_elasticnet_classe <-
  predict(
    modelo_elasticnet_final,
    new_data = dados_teste,
    type = "class"
  )

predicoes_elasticnet_prob <-
  predict(
    modelo_elasticnet_final,
    new_data = dados_teste,
    type = "prob"
  )

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_elasticnet <-
  dados_teste |>
  select(ABAIXOPESO) |>
  bind_cols(
    predicoes_elasticnet_classe,
    predicoes_elasticnet_prob
  ) |>
  mutate(
    ABAIXOPESO = as.factor(ABAIXOPESO)
  )

# ---------------------------------------------------------------------
# Matriz de confusão
# ---------------------------------------------------------------------

conf_mat_elasticnet <-
  conf_mat(
    resultados_elasticnet,
    truth = ABAIXOPESO,
    estimate = .pred_class
  )

print(conf_mat_elasticnet)

# ---------------------------------------------------------------------
# Métricas
# ---------------------------------------------------------------------

metricas_elasticnet <-
  bind_rows(
    accuracy(
      resultados_elasticnet,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    precision(
      resultados_elasticnet,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    recall(
      resultados_elasticnet,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    f_meas(
      resultados_elasticnet,
      truth = ABAIXOPESO,
      estimate = .pred_class
    )
  )

print(metricas_elasticnet)

# ---------------------------------------------------------------------
# Curva ROC
# ---------------------------------------------------------------------

roc_auc(
  resultados_elasticnet,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
)

roc_curve(
  resultados_elasticnet,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
) |>
  autoplot()

# ---------------------------------------------------------------------
# Tabela consolidada
# ---------------------------------------------------------------------

tabela_resultado_elasticnet <-
  tibble(
    Modelo = "Elastic Net",
    Acuracia =
      accuracy(
        resultados_elasticnet,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Recall =
      recall(
        resultados_elasticnet,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Precisao =
      precision(
        resultados_elasticnet,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    F1_Score =
      f_meas(
        resultados_elasticnet,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    AUC =
      roc_auc(
        resultados_elasticnet,
        truth = ABAIXOPESO,
        .pred_abaixo_peso
      )$.estimate
  ) |>
  mutate(
    across(
      where(is.numeric),
      round,
      3
    )
  )

print(tabela_resultado_elasticnet)

# ---------------------------------------------------------------------
# RIDGE
# ---------------------------------------------------------------------
# Regressão logística penalizada utilizando penalização L2 (Ridge).
#
# O Ridge mantém todas as variáveis no modelo, reduzindo a variância
# dos coeficientes e aumentando a estabilidade em cenários com
# colinearidade.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Especificação do modelo
# ---------------------------------------------------------------------

modelo_ridge <-
  logistic_reg(
    penalty = tune(),
    mixture = 0
  ) |>
  set_engine("glmnet") |>
  set_mode("classification")

# ---------------------------------------------------------------------
# Workflow
# ---------------------------------------------------------------------

workflow_ridge <-
  workflow() |>
  add_recipe(receita) |>
  add_model(modelo_ridge)

# ---------------------------------------------------------------------
# Ajuste por validação cruzada
# ---------------------------------------------------------------------

set.seed(2708)

tuning_ridge <-
  tune_grid(
    workflow_ridge,
    resamples = cv_folds,
    grid = 20,
    metrics = metric_set(
      accuracy,
      recall,
      precision,
      f_meas,
      roc_auc
    ),
    control =
      control_grid(
        verbose = TRUE
      )
  )

# ---------------------------------------------------------------------
# Melhores resultados
# ---------------------------------------------------------------------

show_best(
  tuning_ridge,
  metric = "recall"
)

show_best(
  tuning_ridge,
  metric = "accuracy"
)

show_best(
  tuning_ridge,
  metric = "roc_auc"
)

# ---------------------------------------------------------------------
# Melhor modelo
# ---------------------------------------------------------------------

melhores_parametros_ridge <-
  select_best(
    tuning_ridge,
    metric = "recall"
  )

workflow_ridge_final <-
  finalize_workflow(
    workflow_ridge,
    melhores_parametros_ridge
  )

modelo_ridge_final <-
  fit(
    workflow_ridge_final,
    data = dados_treino
  )

# ---------------------------------------------------------------------
# Predições
# ---------------------------------------------------------------------

predicoes_ridge_classe <-
  predict(
    modelo_ridge_final,
    new_data = dados_teste,
    type = "class"
  )

predicoes_ridge_prob <-
  predict(
    modelo_ridge_final,
    new_data = dados_teste,
    type = "prob"
  )

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_ridge <-
  dados_teste |>
  select(ABAIXOPESO) |>
  bind_cols(
    predicoes_ridge_classe,
    predicoes_ridge_prob
  ) |>
  mutate(
    ABAIXOPESO = as.factor(ABAIXOPESO)
  )

# ---------------------------------------------------------------------
# Matriz de confusão
# ---------------------------------------------------------------------

conf_mat_ridge <-
  conf_mat(
    resultados_ridge,
    truth = ABAIXOPESO,
    estimate = .pred_class
  )

print(conf_mat_ridge)

# ---------------------------------------------------------------------
# Métricas
# ---------------------------------------------------------------------

metricas_ridge <-
  bind_rows(
    accuracy(
      resultados_ridge,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    precision(
      resultados_ridge,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    recall(
      resultados_ridge,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    f_meas(
      resultados_ridge,
      truth = ABAIXOPESO,
      estimate = .pred_class
    )
  )

print(metricas_ridge)

# ---------------------------------------------------------------------
# Curva ROC
# ---------------------------------------------------------------------

roc_auc(
  resultados_ridge,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
)

roc_curve(
  resultados_ridge,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
) |>
  autoplot()

# ---------------------------------------------------------------------
# Tabela consolidada
# ---------------------------------------------------------------------

tabela_resultado_ridge <-
  tibble(
    Modelo = "Ridge",
    Acuracia =
      accuracy(
        resultados_ridge,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Recall =
      recall(
        resultados_ridge,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Precisao =
      precision(
        resultados_ridge,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    F1_Score =
      f_meas(
        resultados_ridge,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    AUC =
      roc_auc(
        resultados_ridge,
        truth = ABAIXOPESO,
        .pred_abaixo_peso
      )$.estimate
  ) |>
  mutate(
    across(
      where(is.numeric),
      round,
      3
    )
  )

print(tabela_resultado_ridge)

# ---------------------------------------------------------------------
# LASSO
# ---------------------------------------------------------------------
# Regressão logística penalizada utilizando penalização L1 (Lasso).
#
# O Lasso realiza seleção automática de variáveis ao reduzir alguns
# coeficientes exatamente para zero, produzindo modelos mais parcimoniosos.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Especificação do modelo
# ---------------------------------------------------------------------

modelo_lasso <-
  logistic_reg(
    penalty = tune(),
    mixture = 1
  ) |>
  set_engine("glmnet") |>
  set_mode("classification")

# ---------------------------------------------------------------------
# Workflow
# ---------------------------------------------------------------------

workflow_lasso <-
  workflow() |>
  add_recipe(receita) |>
  add_model(modelo_lasso)

# ---------------------------------------------------------------------
# Grade de penalização
# ---------------------------------------------------------------------

grade_lasso <-
  grid_regular(
    penalty(range = c(-4, 0)),
    levels = 20
  )

# ---------------------------------------------------------------------
# Ajuste por validação cruzada
# ---------------------------------------------------------------------

set.seed(2708)

tuning_lasso <-
  tune_grid(
    workflow_lasso,
    resamples = cv_folds,
    grid = grade_lasso,
    metrics = metric_set(
      accuracy,
      recall,
      precision,
      f_meas,
      roc_auc
    ),
    control =
      control_grid(
        verbose = TRUE
      )
  )

# ---------------------------------------------------------------------
# Melhores resultados
# ---------------------------------------------------------------------

show_best(
  tuning_lasso,
  metric = "recall"
)

show_best(
  tuning_lasso,
  metric = "accuracy"
)

show_best(
  tuning_lasso,
  metric = "roc_auc"
)

# ---------------------------------------------------------------------
# Melhor modelo
# ---------------------------------------------------------------------

melhores_parametros_lasso <-
  select_best(
    tuning_lasso,
    metric = "recall"
  )

workflow_lasso_final <-
  finalize_workflow(
    workflow_lasso,
    melhores_parametros_lasso
  )

modelo_lasso_final <-
  fit(
    workflow_lasso_final,
    data = dados_treino
  )

# ---------------------------------------------------------------------
# Predições
# ---------------------------------------------------------------------

predicoes_lasso_classe <-
  predict(
    modelo_lasso_final,
    new_data = dados_teste,
    type = "class"
  )

predicoes_lasso_prob <-
  predict(
    modelo_lasso_final,
    new_data = dados_teste,
    type = "prob"
  )

# ---------------------------------------------------------------------
# Consolidação dos resultados
# ---------------------------------------------------------------------

resultados_lasso <-
  dados_teste |>
  select(ABAIXOPESO) |>
  bind_cols(
    predicoes_lasso_classe,
    predicoes_lasso_prob
  ) |>
  mutate(
    ABAIXOPESO = as.factor(ABAIXOPESO)
  )

# ---------------------------------------------------------------------
# Matriz de confusão
# ---------------------------------------------------------------------

conf_mat_lasso <-
  conf_mat(
    resultados_lasso,
    truth = ABAIXOPESO,
    estimate = .pred_class
  )

print(conf_mat_lasso)

# ---------------------------------------------------------------------
# Métricas
# ---------------------------------------------------------------------

metricas_lasso <-
  bind_rows(
    accuracy(
      resultados_lasso,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    precision(
      resultados_lasso,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    recall(
      resultados_lasso,
      truth = ABAIXOPESO,
      estimate = .pred_class
    ),
    f_meas(
      resultados_lasso,
      truth = ABAIXOPESO,
      estimate = .pred_class
    )
  )

print(metricas_lasso)

# ---------------------------------------------------------------------
# Curva ROC
# ---------------------------------------------------------------------

roc_auc(
  resultados_lasso,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
)

roc_curve(
  resultados_lasso,
  truth = ABAIXOPESO,
  .pred_abaixo_peso
) |>
  autoplot()

# ---------------------------------------------------------------------
# Tabela consolidada
# ---------------------------------------------------------------------

tabela_resultado_lasso <-
  tibble(
    Modelo = "Lasso",
    Acuracia =
      accuracy(
        resultados_lasso,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Recall =
      recall(
        resultados_lasso,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    Precisao =
      precision(
        resultados_lasso,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    F1_Score =
      f_meas(
        resultados_lasso,
        truth = ABAIXOPESO,
        estimate = .pred_class
      )$.estimate,
    AUC =
      roc_auc(
        resultados_lasso,
        truth = ABAIXOPESO,
        .pred_abaixo_peso
      )$.estimate
  ) |>
  mutate(
    across(
      where(is.numeric),
      round,
      3
    )
  )

print(tabela_resultado_lasso)

# ---------------------------------------------------------------------
# Comparação dos modelos
# ---------------------------------------------------------------------

comparacao_modelos <-
  bind_rows(
    tabela_resultado_rf,
    tabela_resultado_xgb,
    tabela_resultado_elasticnet,
    tabela_resultado_ridge,
    tabela_resultado_lasso
  ) |>
  arrange(
    desc(Recall),
    desc(AUC),
    desc(F1_Score)
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ round(.x, 3)
    )
  )

cat("\n")
cat("=====================================================\n")
cat("COMPARAÇÃO DOS MODELOS\n")
cat("=====================================================\n\n")

print(comparacao_modelos)

# ---------------------------------------------------------------------
# Melhor modelo
# ---------------------------------------------------------------------

cat("\n")
cat("=====================================================\n")
cat("MELHOR MODELO\n")
cat("=====================================================\n\n")

print(
  comparacao_modelos |>
    slice(1)
)

# ---------------------------------------------------------------------
# Comparação gráfica
# ---------------------------------------------------------------------

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
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none"
  )