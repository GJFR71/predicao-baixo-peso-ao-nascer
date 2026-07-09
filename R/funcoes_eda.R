# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : funcoes_eda.R
# Objetivo: Funções auxiliares para Análise Exploratória dos Dados
# ==========================================================

# -----------------------------------------------------------------------------
# Função: analisar_variavel_numerica()
# -----------------------------------------------------------------------------

analisar_variavel_numerica <- function(
    dados,
    variavel,
    titulo
) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(titulo, "\n")
  cat(strrep("=", 70), "\n\n")
  
  print(summary(dados[[variavel]]))
  
  p_hist <-
    ggplot(
      dados,
      aes(x = .data[[variavel]])
    ) +
    geom_histogram(
      bins = 30,
      fill = "steelblue",
      color = "black"
    ) +
    labs(
      title = paste("Distribuição de", titulo),
      x = titulo,
      y = "Frequência"
    )
  
  print(p_hist)
  
  p_box <-
    ggplot(
      dados,
      aes(y = .data[[variavel]])
    ) +
    geom_boxplot(
      fill = "steelblue",
      color = "black"
    ) +
    labs(
      title = paste("Boxplot de", titulo),
      y = titulo
    )
  
  print(p_box)
  
  invisible(NULL)
  
}

# -----------------------------------------------------------------------------
# Função: analisar_variavel_categorica()
# -----------------------------------------------------------------------------
# Realiza a análise exploratória de uma variável categórica por meio de:
#   - tabela de frequências;
#   - gráfico de barras.
#
# Parâmetros:
#   dados    : tibble contendo os dados.
#   variavel : nome da variável.
#   titulo   : título utilizado no gráfico.
#   labels   : rótulos das categorias (opcional).
#
# Retorno:
#   Exibe a tabela de frequências e o gráfico de barras.
# -----------------------------------------------------------------------------

analisar_variavel_categorica <- function(
    dados,
    variavel,
    titulo,
    labels = c("Não", "Sim")
) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat(titulo, "\n")
  cat(strrep("=", 70), "\n\n")
  
  print(table(dados[[variavel]]))
  
  grafico <-
    ggplot(
      dados,
      aes(
        x = factor(
          .data[[variavel]],
          labels = labels
        )
      )
    ) +
    geom_bar(
      fill = "steelblue",
      color = "black"
    ) +
    labs(
      title = titulo,
      x = NULL,
      y = "Frequência"
    ) +
    theme_minimal()
  
  print(grafico)
  
  invisible(NULL)
  
}

# -----------------------------------------------------------------------------
# Função: analisar_bivariada()
# -----------------------------------------------------------------------------
# Realiza a análise bivariada entre a variável resposta (ABAIXOPESO)
# e uma variável categórica, exibindo:
#   - tabela de contingência;
#   - teste Qui-quadrado;
#   - gráfico de barras proporcionais.
#
# Parâmetros:
#   dados     : tibble contendo os dados.
#   variavel  : nome da variável explicativa.
#
# Retorno:
#   Exibe resultados da análise bivariada.
# -----------------------------------------------------------------------------

analisar_bivariada <- function(
    dados,
    variavel
) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("Variável:", variavel, "\n")
  cat(strrep("=", 70), "\n\n")
  
  # ---------------------------------------------------------------------------
  # Tabela de contingência
  # ---------------------------------------------------------------------------
  
  tabela <-
    table(
      dados[[variavel]],
      dados$ABAIXOPESO
    )
  
  print(tabela)
  
  # ---------------------------------------------------------------------------
  # Teste Qui-quadrado
  # ---------------------------------------------------------------------------
  
  teste <-
    chisq.test(tabela)
  
  print(teste)
  
  # ---------------------------------------------------------------------------
  # Gráfico
  # ---------------------------------------------------------------------------
  
  grafico <-
    ggplot(
      dados,
      aes(
        x = .data[[variavel]],
        fill = ABAIXOPESO
      )
    ) +
    geom_bar(
      position = "fill"
    ) +
    labs(
      title = paste(
        "Baixo peso ao nascer por",
        variavel
      ),
      x = NULL,
      y = "Proporção",
      fill = "Desfecho"
    ) +
    scale_y_continuous(
      labels = scales::percent
    ) +
    theme_minimal()
  
  print(grafico)
  
  invisible(NULL)
  
}