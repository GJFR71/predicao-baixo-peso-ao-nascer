# ==========================================================
# Projeto : Baixo Peso ao Nascer
# Arquivo : 00_setup.R
# Objetivo: Configurar o ambiente do projeto
# ==========================================================

# Pacotes --------------------------------------------------------------
# Carrega todos os pacotes utilizados ao longo do projeto.
# Este script deve ser executado antes dos demais.

library(tidyverse)
library(tidymodels)
library(haven)
library(here)
library(ranger)
library(glmnet)
library(doParallel)
library(gt)

# Opções globais -------------------------------------------------------
# Define configurações que serão utilizadas durante toda a análise.

options(
  
  # Evita que números sejam exibidos em notação científica.
  scipen = 999,
  
  # Oculta mensagens informativas do dplyr ao utilizar summarise().
  dplyr.summarise.inform = FALSE
  
)

# Tema padrão para todos os gráficos ----------------------------------
# Define um tema único para manter a identidade visual das figuras.

theme_set(
  theme_minimal(base_size = 12)
)