# Predição de Baixo Peso ao Nascer

## 📚 Descrição
Projeto completo de Análise Preditiva para identificar gestantes com risco aumentado de ter bebês com baixo peso ao nascer, utilizando dados clínicos, sociais e comportamentais.

Realizado desde o tratamento de dados, criação de KPIs compostos, análise exploratória detalhada, modelagem preditiva com diversos algoritmos (Random Forest, XGBoost, Elastic Net, Ridge e Lasso) até a escolha da melhor solução com base em métricas robustas de performance.

O foco esteve não apenas na precisão estatística, mas também em maximizar o valor estratégico do modelo para a tomada de decisão em saúde pública.

## 🧰 Tecnologias e Pacotes
- R
- tidyverse
- caret
- tidymodels
- xgboost
- pROC
- dplyr
- ggplot2
- haven

## 📈 Principais Etapas
- Tratamento e imputação de dados ausentes
- Criação de Indicadores Compostos (KPIs) de Risco Gestacional
- Análise exploratória univariada e bivariada
- Treinamento e comparação de Random Forest, XGBoost, Elastic Net, Ridge e Lasso
- Ajuste do ponto de corte via Curva ROC para otimizar Recall
- Análise de Lift para quantificar o ganho prático da solução
- Tradução dos resultados técnicos para linguagem acessível
- Seleção final do Elastic Net como modelo mais equilibrado

## 🚀 Resultados
- Elastic Net alcançou Recall otimizado de 70,8%, com ganho de 26% em relação à seleção aleatória.
- A aplicação do modelo pode orientar decisões estratégicas em programas de intervenção materno-infantil.

## 📋 Arquivos
- `Relatorio_Final_ElasticNet_Completo.docx`: Relatório completo do projeto (descrição analítica e estratégica).
- `codigo_modelagem.R`: Script completo com tratamento de dados, análises e modelagem.

## 📢 Observação
- Os dados reais utilizados no projeto são confidenciais.
- As visualizações (Curva ROC, boxplots, histogramas) podem ser reproduzidas ao rodar o script `codigo_modelagem.R`.

---

# 🌈 Storytelling do Projeto

O baixo peso ao nascer é um problema crítico de saúde pública, associado a complicações neonatais e impactos de longo prazo no desenvolvimento.
Este projeto nasceu da necessidade de identificar precocemente gestantes com risco aumentado, utilizando análise preditiva aplicada a bases clínicas reais.

Através da criação de indicadores compostos, modelagem estatística avançada e otimização de métricas de sensibilidade, o modelo desenvolvido proporciona suporte efetivo às políticas de prevenção em saúde materno-infantil.

Houve especial atenção à comunicação dos resultados: o ajuste do ponto de corte foi realizado não apenas para fins técnicos, mas para maximizar o impacto prático. A análise de Lift foi traduzida em linguagem simples, facilitando a compreensão por profissionais de saúde não especialistas em dados.

# 💡 Reflexão Profissional

Este projeto evidenciou a importância de combinar técnica estatística com sensibilidade ao problema de negócio.

Não se tratou apenas de "ajustar modelos", mas de entender o contexto clínico, construir representações significativas dos dados (KPIs) e otimizar o modelo para o impacto prático.

Além disso, mostrou-se essencial a capacidade de traduzir análises estatísticas complexas em insights claros e acessíveis, promovendo a integração entre a Ciência de Dados e a tomada de decisão em saúde pública.

Esta experiência reforçou minha convicção de que dados devem servir à tomada de decisão consciente, à geração de valor real e à melhoria de vidas.

---
Projeto desenvolvido por **Glaucio Jorge Ferreira Rosa**.
