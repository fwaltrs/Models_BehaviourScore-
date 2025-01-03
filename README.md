# Métodos para prever um Behaviour Score
Construção de modelos de aprendizado de máquina na predição da probabilidade do cliente ser bom pagador usando dados de transações, ou seja, queremos prever um Behaviour Score.
Estudaremos modelos supervisionados de classificação, para essa predição serão estudados: Regressão Logística, Random Forest, XGBoost e LightGBM. 

O primeiro passo foi realizar um tratamento e limpeza do banco de dados. A base contém informações de 5 covariáveis categóricas e 208 contínuas. 

Como resultado final, ao comparar os modelos na amostra de teste, concluímos que no geral todos discriminam bem as classes da variável resposta. No entanto, o método que obteve os melhores resultados preditivos foi o XGBoost, com área sob a curva ROC de $82,03%, coeficiente de Gini de 64,06%, KS de 47,97%, entre outras métricas de desempenho. Por outro lado, o método que apresentou os menores resultados preditivos foi a Regressão Logística com Penalização, que obteve área sob a curva ROC de 78,46%, coeficiente de Gini de 56,92%, KS de 42,73%, entre outras medidas de desempenho, além de um maior tempo de treinamento.


