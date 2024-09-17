# Models_BehaviourScore
Construção de modelos de aprendizado de máquina na predição da probabilidade do cliente ser bom pagador usando dados de transações, ou seja, queremos prever um Behaviour Score.
Estudaremos modelos supervisionados de classificação, para essa predição serão estudados: Regressão Logística, Random Forest, XGBoost e LightGBM. 

O primeiro passo foi realizar um tratamento e limpeza do banco de dados. A base contém informações de 5 covariáveis categóricas e 208 contínuas. 

O segundo passo foi realizar uma análise descritiva (EDA) do banco de dados. Calculamos a taxa de inadimplência por período e total, e constatamos que a taxa de inadimplência na carteira é cerca de 5,37%.  Como o banco de dados possui muitas variáveis que são numéricas, fizemos um teste para testar quais as variáveis mais significativas usadas individualmente para predizer a variável resposta usando como decisão o Critério de Informação de Akaike e realizamos uma análise das variáveis com relação a taxa de inadimplência das 8 variáveis mais significativas do banco de dados, entre elas se encontram a renda, idade, vencimentos quitados com atraso e antecipados. 

O primeiro modelo foi o de Regressão Logística com Penalização (Lasso) com as medidas de desempenho utilizando o ponto de corte 0,941 e a área sobre a Curva ROC foi de 78,52 % e o índice de Gini de 57,04 % e estatística KS de 42,45%. Esses resultados indicam que o modelo possui um bom desempenho na discriminação das classes da variável resposta.

O segundo modelo foi o de Random Forest com as medidas de desempenho utilizando o ponto de corte 0,930 e a área sobre a Curva ROC foi de 79,38 % e o índice de Gini de 58,76 % e estatística KS de 44,23%. Esses resultados indicam que o modelo possui um bom desempenho na discriminação das classes da variável resposta.


