# Models_BehaviourScore
Construção de modelos de aprendizado de máquina na predição de um Behaviour Score.
Estudaremos modelos supervisionados de classificação, para essa predição serão estudados: Regressão Logística, Florestas Aleatórias, XGBoost e LightGBM. 

O primeiro passo foi realizar um tratamento e limpeza do banco de dados. Após finalizar esse tratamento, categorizamos a base de dados para usar no modelo de Regressão Logística e para categorizar utilizamos quartis. 

O segundo passo foi realizar uma análise descritiva (EDA) do banco de dados. Calculamos a taxa de inadimplência por período e total, e constatamos que a taxa de inadimplência na carteira é cerca de 5,37%.  Como o banco de dados possui muitas variáveis que são numéricas e categoricas, fizemos um teste para testar quais as variáveis mais significativas usadas individualmente para predizer a variável resposta usando como decisão o Critério de Informação de Akaike e realizamos uma análise das variáveis com relação a taxa de inadimplência das 8 variáveis mais significativas do banco de dados, entre elas se encontram a renda, idade, vencimentos quitados com atraso e antecipados.   


