# Predição de Baixo Peso ao Nascer

<div align="center">

**Projeto de Machine Learning em R para apoio à identificação de recém-nascidos com maior risco de baixo peso ao nascer**

<br>

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Quarto](https://img.shields.io/badge/Quarto-39729E?style=for-the-badge&logo=quarto&logoColor=white)
![Machine Learning](https://img.shields.io/badge/Machine%20Learning-Saúde%20Pública-orange?style=for-the-badge)
![Status](https://img.shields.io/badge/status-concluído-brightgreen?style=for-the-badge)

</div>

---

## Sobre o Projeto

Este projeto desenvolve e avalia modelos preditivos para identificar recém-nascidos com maior probabilidade de apresentar **baixo peso ao nascer**, utilizando informações maternas, obstétricas e clínicas.

O fluxo analítico contempla desde a preparação da base de dados até a seleção do modelo final, com foco em **apoio à tomada de decisão em saúde materno-infantil**.

Além da modelagem estatística, o projeto inclui tratamento dos dados, análise exploratória, engenharia de atributos, comparação entre algoritmos de aprendizado de máquina e elaboração de um relatório técnico em linguagem acessível.

---

## Contexto

O baixo peso ao nascer é um importante indicador de saúde pública, pois pode estar associado a complicações neonatais e a impactos no desenvolvimento infantil.

Nesse contexto, a análise preditiva pode apoiar a identificação de padrões de risco e auxiliar na priorização de casos que demandam maior atenção em programas de acompanhamento materno-infantil.

A pergunta central do projeto foi:

> **É possível utilizar dados maternos, obstétricos e clínicos para identificar, de forma antecipada, recém-nascidos com maior risco de baixo peso ao nascer?**

---

## Objetivos

- Preparar e tratar a base de dados para modelagem preditiva.
- Realizar análise exploratória univariada e bivariada.
- Construir variáveis derivadas e indicadores relacionados ao risco gestacional.
- Comparar diferentes algoritmos de aprendizado de máquina.
- Selecionar o modelo com melhor desempenho para identificação dos casos de interesse.
- Produzir um relatório técnico com interpretação prática dos resultados.

---

## Fluxo Analítico

```text
Importação dos Dados
        │
        ▼
Tratamento e Limpeza
        │
        ▼
Análise Exploratória
        │
        ▼
Engenharia de Atributos
        │
        ▼
Preparação para Modelagem
        │
        ▼
Treinamento dos Modelos
        │
        ▼
Avaliação Comparativa
        │
        ▼
Seleção do Modelo Final
        │
        ▼
Relatório Técnico
```

---

## Estrutura do Repositório

```text
baixo-peso-ao-nascer/
│
├── R/
│   ├── 00_setup.R
│   ├── 01_importacao.R
│   ├── 02_tratamento.R
│   ├── 03_eda.R
│   ├── 04_features.R
│   ├── 05_preparacao_modelagem.R
│   ├── 06_modelagem.R
│   ├── 07_avaliacao.R
│   └── 08_relatorio.qmd
│
├── dados/
│   └── Base de dados não disponibilizada publicamente
│
├── objetos/
│   └── Objetos intermediários gerados pelo projeto
│
├── relatorios/
│   └── Relatórios técnicos gerados em Quarto
│
├── figuras/
│   └── Gráficos e visualizações do projeto
│
├── renv/
│   └── Ambiente reprodutível do projeto
│
├── README.md
├── renv.lock
└── baixo-peso-ao-nascer.Rproj
```

---

## Tecnologias Utilizadas

| Etapa | Ferramentas |
|---|---|
| Linguagem principal | R |
| Organização do projeto | RStudio Project, here |
| Reprodutibilidade | renv |
| Manipulação de dados | tidyverse, dplyr, janitor |
| Importação de dados | haven |
| Análise exploratória | skimr, ggplot2 |
| Modelagem preditiva | tidymodels, glmnet, ranger, xgboost |
| Avaliação de modelos | yardstick, pROC |
| Visualização e tabelas | ggplot2, gt |
| Relatório técnico | Quarto |

---

## Principais Etapas

| Etapa | Descrição |
|---|---|
| Tratamento dos dados | Limpeza, padronização e tratamento de valores ausentes |
| Engenharia de atributos | Construção de variáveis derivadas e indicadores de risco |
| Análise exploratória | Avaliação de padrões, distribuições e relações entre variáveis |
| Modelagem | Treinamento de diferentes algoritmos de classificação |
| Avaliação | Comparação dos modelos por métricas de desempenho |
| Comunicação | Elaboração de relatório técnico com linguagem acessível |

---

## Modelos Avaliados

Foram avaliados cinco modelos de aprendizado de máquina:

| Modelo | Finalidade |
|---|---|
| Lasso | Regressão penalizada com regularização L1 |
| Ridge | Regressão penalizada com regularização L2 |
| Random Forest | Ensemble baseado em árvores de decisão |
| XGBoost | Modelo de gradient boosting |
| Elastic Net | Regressão penalizada com combinação L1 e L2 |

---

## Critério de Seleção

Os modelos foram comparados a partir de múltiplas métricas de desempenho:

- Acurácia;
- Recall;
- Precisão;
- F1-Score;
- Área sob a Curva ROC.

Como o projeto está inserido em um contexto de saúde, o **Recall** foi adotado como principal critério de seleção.

Essa escolha prioriza a identificação correta dos recém-nascidos com baixo peso ao nascer. Em termos práticos, o objetivo é reduzir a chance de deixar de sinalizar casos que poderiam demandar maior atenção em ações preventivas ou programas de acompanhamento materno-infantil.

Embora outras métricas, como AUC e acurácia, também sejam importantes, elas foram utilizadas como apoio à interpretação geral do desempenho dos modelos.

---

## Resultados dos Modelos

A tabela a seguir resume o desempenho dos modelos avaliados.

| Modelo | Acurácia (%) | Recall (%) | Precisão (%) | F1-Score (%) | AUC (%) |
|---|---:|---:|---:|---:|---:|
| Lasso | 63,9 | 56,5 | 66,3 | 61,0 | 68,1 |
| Ridge | 63,9 | 56,3 | 66,3 | 60,9 | 68,2 |
| Random Forest | 63,4 | 55,6 | 65,9 | 60,3 | 68,2 |
| XGBoost | 63,0 | 55,3 | 65,3 | 59,9 | 68,0 |
| Elastic Net | 62,7 | 47,5 | 68,1 | 56,0 | 67,9 |

De forma geral, os modelos apresentaram desempenhos próximos, especialmente em acurácia e AUC.

Embora Ridge e Random Forest tenham apresentado AUC ligeiramente superior, o modelo Lasso obteve o maior Recall entre os modelos avaliados, métrica priorizada neste projeto em razão do contexto de saúde materno-infantil.

---

## Modelo Final

O modelo selecionado foi o **Lasso**.

A escolha foi baseada principalmente no **Recall**, métrica priorizada no projeto por representar a capacidade do modelo de identificar corretamente os casos de baixo peso ao nascer.

| Modelo Selecionado | Indicador | Resultado (%) |
|---|---|---:|
| Lasso | Acurácia | 63,9 |
| Lasso | Recall | 56,5 |
| Lasso | Precisão | 66,3 |
| Lasso | F1-Score | 61,0 |
| Lasso | AUC | 68,1 |

A seleção do Lasso não se deu apenas pelo desempenho estatístico isolado, mas pela coerência entre o critério definido para o problema e a utilidade prática esperada do modelo.

Em aplicações relacionadas à saúde, a identificação dos casos de maior risco tende a ser mais relevante do que a simples maximização da acurácia geral. Por esse motivo, o Recall foi adotado como critério principal de decisão.

---

## Resultados Principais

Os resultados indicam que os modelos avaliados tiveram desempenho relativamente próximo, sem diferenças expressivas entre as principais abordagens.

O modelo **Lasso** foi selecionado por apresentar o melhor Recall, alcançando **56,5%**, além de manter desempenho competitivo nas demais métricas.

Entre os principais achados, destacam-se:

| Aspecto | Interpretação |
|---|---|
| Melhor Recall | O Lasso apresentou a maior capacidade de identificação dos casos de baixo peso ao nascer |
| AUC competitiva | O modelo selecionado manteve desempenho próximo aos melhores modelos em AUC |
| Precisão de 66,3% | Entre os casos sinalizados pelo modelo, houve proporção relevante de classificações corretas |
| F1-Score de 61,0% | O modelo apresentou equilíbrio entre Recall e Precisão |
| Desempenho semelhante entre modelos | Os resultados sugerem que o ganho principal está na escolha da métrica alinhada ao problema |

O resultado reforça a importância de selecionar o modelo não apenas pela melhor métrica global, mas pelo critério mais adequado ao contexto de aplicação.

Neste projeto, como o objetivo era apoiar a identificação de recém-nascidos com maior risco de baixo peso ao nascer, a priorização do Recall foi considerada mais adequada do que a escolha baseada exclusivamente em AUC ou acurácia.

---

## Entregas do Projeto

- Pipeline analítico modular em R.
- Base tratada e preparada para modelagem.
- Engenharia de atributos e construção de indicadores relacionados ao risco gestacional.
- Análise exploratória dos dados.
- Comparação entre diferentes algoritmos de aprendizado de máquina.
- Seleção do modelo final com base em critério alinhado ao contexto clínico.
- Relatório técnico desenvolvido em Quarto.
- Estrutura organizada para reutilização em projetos futuros.

---

## Relatório Técnico

O projeto inclui um relatório técnico elaborado em Quarto, com foco na comunicação dos resultados para públicos técnicos e não técnicos.

O relatório contempla:

- Resumo executivo;
- Introdução;
- Objetivos;
- Metodologia;
- Resultados;
- Aplicação prática dos resultados;
- Limitações da aplicação do modelo;
- Considerações finais.

O documento foi desenvolvido em linguagem acessível, priorizando a interpretação prática dos achados e sua utilidade para apoio à decisão em saúde materno-infantil.

---

## Como Reproduzir o Projeto

Para executar o projeto localmente, recomenda-se utilizar o RStudio.

### 1. Clonar o repositório

```bash
git clone https://github.com/SEU-USUARIO/baixo-peso-ao-nascer.git
```

### 2. Abrir o projeto no RStudio

Abra o arquivo:

```text
baixo-peso-ao-nascer.Rproj
```

### 3. Restaurar o ambiente do projeto

No console do R, execute:

```r
renv::restore()
```

### 4. Executar os scripts na ordem

```text
00_setup.R
01_importacao.R
02_tratamento.R
03_eda.R
04_features.R
05_preparacao_modelagem.R
06_modelagem.R
07_avaliacao.R
08_relatorio.qmd
```

### 5. Renderizar o relatório

```r
quarto::quarto_render("R/08_relatorio.qmd")
```

---

## Dados

Os dados utilizados neste projeto possuem caráter confidencial e não podem ser disponibilizados publicamente.

Por esse motivo, a base original não está incluída no repositório.

Ainda assim, o fluxo analítico permanece reproduzível mediante utilização de uma base com estrutura equivalente.

---

## Limitações

- A base utilizada não pode ser disponibilizada publicamente.
- O desempenho do modelo depende da qualidade, completude e representatividade dos dados disponíveis.
- O projeto tem finalidade analítica e educacional.
- A aplicação prática exigiria validação adicional antes de uso operacional.
- O modelo não substitui avaliação clínica nem protocolos institucionais de saúde.

---

## Aprendizados

Este projeto reforçou a importância de combinar técnica estatística, entendimento do problema e comunicação clara dos resultados.

A modelagem não foi tratada apenas como comparação entre algoritmos, mas como parte de um processo maior de apoio à decisão. Por isso, a escolha das métricas e a interpretação dos resultados foram orientadas pelo impacto prático da solução.

Outro ponto relevante foi a necessidade de traduzir análises estatísticas em linguagem acessível, aproximando a Ciência de Dados dos profissionais que podem utilizar esse tipo de informação no processo decisório.

---

## Possíveis Melhorias Futuras

- Validação do modelo em bases externas.
- Inclusão de novas variáveis clínicas, sociais e comportamentais.
- Avaliação de estratégias adicionais para dados desbalanceados.
- Desenvolvimento de dashboard para apoio à interpretação dos resultados.
- Automatização do pipeline de predição em ambiente controlado.
- Comparação com novas abordagens de modelagem e calibração de probabilidades.

---

## Autor

**Glaucio Jorge Ferreira Rosa**

Estatístico, com atuação em análise de dados, Business Intelligence e Ciência de Dados, interessado na aplicação de métodos estatísticos e inteligência artificial para apoio à tomada de decisão.

Projeto desenvolvido com R, Quarto e técnicas de aprendizado de máquina aplicadas à saúde materno-infantil.
