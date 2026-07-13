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

Além da modelagem estatística, o projeto inclui tratamento dos dados, análise exploratória, análise bivariada, engenharia de atributos, comparação entre algoritmos de aprendizado de máquina e elaboração de relatório técnico em linguagem acessível.

---

## Relatório Técnico

O relatório técnico completo está disponível em duas versões:

[🌐 Acessar Relatório Técnico em HTML](https://gjfr71.github.io/predicao-baixo-peso-ao-nascer/resultados/08_relatorio.html)

[📄 Acessar Relatório Técnico em PDF](resultados/08_relatorio.pdf)

A versão HTML é recomendada para leitura online, pois preserva melhor a navegação e o visual do relatório. A versão PDF também está disponível para leitura direta no GitHub ou compartilhamento externo.

O relatório apresenta o fluxo metodológico, os resultados dos modelos, a seleção do modelo final, a comparação com a triagem clínica estimada e a interpretação prática dos achados.

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
- Selecionar o modelo com maior Sensibilidade, considerando também seu desempenho nas demais métricas.
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
Análise Bivariada
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
predicao-baixo-peso-ao-nascer/
│
├── R/
│   ├── 00_setup.R
│   ├── 01_importacao.R
│   ├── 02_tratamento.R
│   ├── 03_eda.R
│   ├── 04_bivariada.R
│   ├── 05_preparacao_modelagem.R
│   ├── 06_modelagem.R
│   ├── 07_avaliacao.R
│   ├── 08_relatorio.qmd
│   ├── funcoes_eda.R
│   ├── funcoes_features.R
│   ├── funcoes_modelagem.R
│   ├── funcoes_tratamento.R
│   └── relatorio.css
│
├── resultados/
│   ├── 08_relatorio.pdf
│   ├── 08_relatorio.html
│   └── relatorio.css
│
├── renv/
│   ├── .gitignore
│   ├── activate.R
│   └── settings.json
│
├── .gitignore
├── .Rprofile
├── README.md
├── baixo-peso-ao-nascer.Rproj
└── renv.lock
```

> A base de dados original não está incluída no repositório por possuir caráter confidencial.

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
| Setup do projeto | Carregamento de pacotes, configuração de caminhos e ambiente de trabalho |
| Importação dos dados | Leitura da base original utilizada no projeto |
| Tratamento dos dados | Limpeza, padronização e tratamento de valores ausentes |
| Análise exploratória | Avaliação inicial das variáveis e identificação de padrões gerais |
| Análise bivariada | Investigação da relação entre variáveis explicativas e o desfecho |
| Engenharia de atributos | Construção de variáveis derivadas e indicadores de risco |
| Preparação para modelagem | Separação da base, pré-processamento e organização dos dados para treino |
| Modelagem | Treinamento de diferentes algoritmos de classificação |
| Avaliação | Comparação dos modelos por métricas clínicas e preditivas |
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

Os modelos foram comparados por métricas usuais em estudos diagnósticos e preditivos na área da saúde:

- **Acurácia:** proporção de classificações corretas entre todos os casos avaliados;
- **Sensibilidade (Recall):** proporção dos casos reais de baixo peso alcançados pelo modelo;
- **Valor Preditivo Positivo — VPP (Precisão):** proporção de casos que realmente apresentaram baixo peso entre aqueles sinalizados pelo modelo;
- **Medida F1:** média harmônica entre Sensibilidade e VPP;
- **AUC-ROC:** capacidade de discriminação do modelo entre casos com e sem baixo peso.

A **Sensibilidade** foi adotada como principal critério de seleção. Essa escolha decorre do objetivo clínico do projeto: alcançar o maior número possível de recém-nascidos que efetivamente apresentem baixo peso ao nascer.

Assim, uma Sensibilidade de 56,5% significa que, entre todos os casos reais de baixo peso, o modelo sinalizou 56,5%. Esse valor não representa o percentual geral de acertos do modelo, medida expressa pela Acurácia, nem a proporção de acertos entre os casos sinalizados, medida pelo VPP.

Em termos práticos, a priorização da Sensibilidade busca reduzir a ocorrência de falso-negativos, isto é, casos reais de baixo peso que não seriam sinalizados para possível acompanhamento prioritário. As demais métricas foram utilizadas como apoio à interpretação global do desempenho dos modelos.

---

## Resultados dos Modelos

A tabela a seguir resume o desempenho dos modelos avaliados utilizando a terminologia mais comum na área da saúde.

| Modelo | Acurácia (%) | Sensibilidade (%) | VPP (%) | Medida F1 (%) | AUC-ROC (%) |
|---|---:|---:|---:|---:|---:|
| Lasso | 63,9 | 56,5 | 66,3 | 61,0 | 68,1 |
| Ridge | 63,9 | 56,3 | 66,3 | 60,9 | 68,2 |
| Random Forest | 63,4 | 55,6 | 65,9 | 60,3 | 68,2 |
| XGBoost | 63,0 | 55,3 | 65,3 | 59,9 | 68,0 |
| Elastic Net | 62,7 | 47,5 | 68,1 | 56,0 | 67,9 |

De forma geral, os modelos apresentaram resultados próximos, especialmente em Acurácia e AUC-ROC.

O Ridge e o Random Forest apresentaram AUC-ROC ligeiramente superior, enquanto o Elastic Net apresentou o maior VPP. O Lasso foi selecionado por apresentar a maior Sensibilidade, de 56,5%, conforme o critério prioritário definido para o projeto. Essa escolha não significa que o Lasso tenha apresentado o melhor desempenho em todas as métricas.

---

## Modelo Final

O modelo selecionado foi o **Lasso**.

A escolha foi baseada principalmente na **Sensibilidade**, métrica priorizada por representar a proporção de casos reais de baixo peso ao nascer alcançados pelo modelo.

| Modelo selecionado | Indicador clínico-preditivo | Resultado (%) |
|---|---|---:|
| Lasso | Acurácia | 63,9 |
| Lasso | Sensibilidade | 56,5 |
| Lasso | Valor Preditivo Positivo (VPP) | 66,3 |
| Lasso | Medida F1 | 61,0 |
| Lasso | AUC-ROC | 68,1 |

A Sensibilidade de 56,5% indica que, entre os recém-nascidos que efetivamente apresentaram baixo peso, o modelo sinalizou 56,5%. Consequentemente, 43,5% dos casos reais não foram alcançados pelo modelo, correspondendo aos falsos negativos.

Esse resultado não significa que o modelo tenha acertado 56,5% de todas as classificações. O percentual geral de classificações corretas foi de 63,9%, expresso pela Acurácia. Já o VPP de 66,3% indica que, entre os casos sinalizados pelo Lasso, 66,3% realmente apresentaram baixo peso ao nascer.

A seleção do Lasso decorreu da coerência entre a métrica prioritária e o objetivo do projeto. O modelo não apresentou superioridade em todas as métricas: Ridge e Random Forest obtiveram AUC-ROC ligeiramente maior, e Elastic Net apresentou VPP superior. Ainda assim, o Lasso alcançou a maior proporção de casos reais de baixo peso entre os modelos avaliados.

Na comparação com a triagem clínica estimada a partir dos registros disponíveis, a Sensibilidade passou de aproximadamente 47% para 56,5%. Isso representa um ganho absoluto de 9,5 pontos percentuais e uma melhora relativa de cerca de 20,2% na capacidade de alcançar casos que poderiam demandar acompanhamento prioritário.

O modelo deve ser interpretado como ferramenta complementar de apoio à decisão. Seu desempenho ainda apresenta margem para aprimoramento e não substitui avaliação clínica, protocolos institucionais ou validação externa.

---

## Resultados Principais

Os modelos avaliados apresentaram desempenho relativamente próximo, sem superioridade absoluta de uma abordagem em todas as métricas.

O **Lasso** foi selecionado por apresentar a maior **Sensibilidade**, alcançando **56,5% dos casos reais de baixo peso ao nascer**, além de manter resultados próximos aos demais modelos nos outros indicadores.

Entre os principais achados, destacam-se:

| Aspecto | Interpretação |
|---|---|
| Sensibilidade de 56,5% | Entre os casos que efetivamente apresentaram baixo peso, o Lasso sinalizou 56,5% |
| Falso-negativos de 43,5% | Entre os casos reais de baixo peso, 43,5% não foram sinalizados pelo modelo |
| Acurácia de 63,9% | Considerando todos os casos, 63,9% das classificações foram corretas |
| VPP de 66,3% | Entre os casos sinalizados pelo modelo, 66,3% realmente apresentaram baixo peso |
| Medida F1 de 61,0% | O modelo apresentou equilíbrio entre Sensibilidade e VPP |
| AUC-ROC de 68,1% | O modelo apresentou capacidade discriminatória próxima à dos demais modelos |
| Ganho sobre a triagem clínica estimada | A Sensibilidade aumentou de aproximadamente 47% para 56,5% |
| Resultados semelhantes entre modelos | A seleção foi orientada pela métrica mais coerente com o objetivo clínico, e não por superioridade global |

Os resultados reforçam a importância de selecionar modelos a partir da métrica mais adequada ao contexto de aplicação.

Neste projeto, a Sensibilidade foi priorizada porque o objetivo era alcançar o maior número possível de casos reais de baixo peso ao nascer. A Acurácia, o VPP, a Medida F1 e a AUC-ROC permaneceram importantes para avaliar as limitações e o equilíbrio geral do modelo.

---

## Relatório Técnico Completo

O projeto inclui um relatório técnico elaborado em Quarto, com foco na comunicação dos resultados para públicos técnicos e não técnicos.

Arquivos disponíveis:

| Arquivo | Descrição |
|---|---|
| [Relatório HTML publicado](https://gjfr71.github.io/predicao-baixo-peso-ao-nascer/resultados/08_relatorio.html) | Versão principal para leitura online |
| [`08_relatorio.pdf`](resultados/08_relatorio.pdf) | Versão em PDF para leitura direta no GitHub |
| [`08_relatorio.html`](resultados/08_relatorio.html) | Arquivo HTML renderizado no repositório |
| [`relatorio.css`](resultados/relatorio.css) | Arquivo de estilo utilizado pela versão HTML |

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
git clone https://github.com/GJFR71/predicao-baixo-peso-ao-nascer.git
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

Esse comando utiliza o arquivo `renv.lock` para restaurar as versões dos pacotes utilizadas no projeto.

### 4. Disponibilizar a base de dados

A base original não está incluída no repositório por possuir caráter confidencial.

Para reprodução integral, é necessário utilizar uma base com estrutura equivalente e ajustar o caminho de importação conforme previsto no script:

```text
R/01_importacao.R
```

### 5. Executar os scripts na ordem

```text
R/00_setup.R
R/01_importacao.R
R/02_tratamento.R
R/03_eda.R
R/04_bivariada.R
R/05_preparacao_modelagem.R
R/06_modelagem.R
R/07_avaliacao.R
R/08_relatorio.qmd
```

### 6. Renderizar o relatório

```r
quarto::quarto_render("R/08_relatorio.qmd")
```

O relatório renderizado será disponibilizado na pasta `resultados/`.

---

## Dados

Os dados utilizados neste projeto possuem caráter confidencial e não podem ser disponibilizados publicamente.

Por esse motivo, a base original não está incluída no repositório.

Ainda assim, o fluxo analítico permanece reproduzível mediante utilização de uma base com estrutura equivalente.

---

## Entregas do Projeto

- Pipeline analítico modular em R.
- Base tratada e preparada para modelagem.
- Engenharia de atributos e construção de indicadores relacionados ao risco gestacional.
- Análise exploratória e bivariada dos dados.
- Comparação entre diferentes algoritmos de aprendizado de máquina.
- Seleção do modelo final com base em critério alinhado ao contexto clínico.
- Comparação com triagem clínica estimada a partir dos registros disponíveis.
- Relatório técnico publicado em HTML via GitHub Pages e disponibilizado também em PDF.
- Estrutura organizada para reutilização em projetos futuros.

---

## Limitações

- A base utilizada não pode ser disponibilizada publicamente.
- O desempenho do modelo depende da qualidade, completude e representatividade dos dados disponíveis.
- A comparação com a triagem clínica foi realizada por meio de um indicador indireto construído a partir dos registros disponíveis, e não por uma variável direta de decisão profissional.
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
- Comparação com registros diretos de decisão profissional, caso estejam disponíveis em bases futuras.

---

## Autor

**Glaucio Jorge Ferreira Rosa**

Estatístico, com atuação em análise de dados, Business Intelligence e Ciência de Dados, interessado na aplicação de métodos estatísticos e inteligência artificial para apoio à tomada de decisão.

Projeto desenvolvido com R, Quarto e técnicas de aprendizado de máquina aplicadas à saúde materno-infantil.
