###############################################################################################################
############################### bibliotecas ###################################################################

{
library(arrow)
library(dplyr)
library(glmnet)
library(vcd)
library(ggpubr)
library(ggplot2)
require(caret)
library(biotools)
library(pROC)
}

###############################################################################################################
############################### Regressão Logística com Penalização  ###########################################

###############################################################################################################
############################### Importando a base com dummies #############################################################

base <- arrow::read_parquet("H:/Meu Drive/9º SEMESTRE/TG/bases/base_dummies_V7.parquet")

###############################################################################################################
############################### Transformações e conjuntos de treinamento e validacao #########################
set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size= nrow(base),replace=TRUE)
table(split)
base$split = split

##  split
##  Teste Treinamento 
##  82470      329001

X_treino  = base[base$split =="Treinamento",-c(1,2,3,4,5,323)]
X_teste = base[base$split =="Teste",-c(1,2,3,4,5,323)]
Y_treino = base[base$split=="Treinamento",c(5)] ### classe positiva sendo 0 
Y_treino = as.factor(Y_treino$var_resposta1)

library(dplyr)
X_treino.matrix = X_treino %>% as.matrix()

##################################################################################################################################
############################### Modelo de Regressão Logística com Penalização ########################################################################

set.seed(100) 
vc_lasso = glmnet::cv.glmnet(X_treino.matrix,Y_treino,alpha=1,
                             family="binomial", type.measure =  "auc")



plot(vc_lasso)

#salvando o modelo 
caminho="H:/Meu Drive/9º SEMESTRE/TG/Regressao_logistica/Modelos_1-VarResposta/vc_lasso_1-varresposta_seed100V7.rds"
saveRDS(vc_lasso, file = caminho) 

vc_lasso = readRDS(caminho)

###############################################################################################################
######### Analisar os coeficientes estimados usando o lambda.1se ##############################################

table(coef(vc_lasso,s=vc_lasso$lambda.1se)[,1]!=0)

coefs_estimates <- coef(vc_lasso,s=vc_lasso$lambda.1se)
coefs <- coefs_estimates %>%  as.matrix %>% as_tibble
names(coefs)="Estimativa"
x=coefs %>%  mutate(variavel=rownames(coefs_estimates)) %>%  arrange(desc(Estimativa))

lista = x[which(x[,1] != 0),2]
lista <- as.data.frame(lista)
lista$variavel <- ifelse(substr(lista$variavel, 1, 3) == "CAT", 
                         substr(lista$variavel, 1, nchar(lista$variavel) - 2), 
                         lista$variavel)
lista = unique(lista$variavel) 
lista

###############################################################################################################
################### Gráfico de importancia  ###################################################################

theme_set(theme_gray(base_size = 20))
coefs_estimates <- data.frame(Variavel=
                                rownames(coefs_estimates), Coeficientes=
                                coefs_estimates[,1])
coef_pos <- coefs_estimates %>% arrange(desc(Coeficientes))

coef_neg <- coefs_estimates %>% arrange(Coeficientes)


graf_pos <- ggplot(data=coef_pos[2:21,],aes(x=reorder(Variavel,
                                                      Coeficientes), y = Coeficientes)) +
  geom_bar(stat="identity",col="white",fill="blue4")+coord_flip()+
  labs(title = "Coeficientes Positivos",
       x = "Feature",
       y = "Estimativa") + theme_minimal()  +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15,color="black"),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15,color="black"),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black")
  ) 


graf_neg <- ggplot(data=coef_neg[2:20,],aes(x=reorder(Variavel,
                                                      -Coeficientes), y = Coeficientes)) + 
  geom_bar(stat="identity",col="white",fill="red4")+coord_flip()+
  labs(title = "Coeficientes Negativos",
       x = "Feature",
       y = "Estimativa") + theme_minimal()  +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15,color="black"),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15,color="black"),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black")
  ) 
ggarrange(graf_neg,graf_pos,ncol=2,nrow=1)

##################################################################################################################################
################### Medidas de Desempenho na amostra de TESTE ####################################################################

## Usando o conjunto de Teste  #############################################################################
conjunto_teste = base[base$split=="Teste",]
X_teste <- base[base$split=="Teste",-c(1,2,3,4,5,323)]
X_teste.matrix <- X_teste %>% as.matrix()

valores_preditos_1 <- vc_lasso %>% predict(newx = X_teste.matrix,type="class",
                                           lambda=vc_lasso$lambda.1se) #valores em categoria: 0 ou 1


valores_preditos_2 <- vc_lasso %>% predict(newx = X_teste.matrix,type="response",
                                           lambda=vc_lasso$lambda.1se) #retorna a probabilidade de sucesso: ser "Bom cliente" 

valores_preditos = 1 - as.numeric(valores_preditos_1)
table(valores_preditos)

##   0     1 
##82269   201

############ calcular o risco estimado usando o corte defalut padrão 1/2

1*(as.numeric(valores_preditos_1) != as.numeric(conjunto_teste$var_resposta1))  %>% mean()
## 0.05473506

############ acurácia do modelo usando o default 1/2
observed.classes <- conjunto_teste$var_resposta1
mean(valores_preditos_1 == observed.classes) # acurácia : 0.9674221


############ matriz de confusão 
conjunto_teste$previsoes <- as.factor(
  ifelse(
    valores_preditos
    ==1,"1","0"))
conjunto_teste$var_resposta <- as.factor(conjunto_teste$var_resposta)
matriz <- confusionMatrix(as.factor(conjunto_teste$previsoes),
                          as.factor(conjunto_teste$var_resposta),positive="1")
matriz

## Confusion Matrix and Statistics
## 
## Reference
## Prediction     0     1
## 0 77875  4405
## 1   109    81
## 
## Accuracy : 0.9453          
## 95% CI : (0.9437, 0.9468)
## No Information Rate : 0.9456          
## P-Value [Acc > NIR] : 0.6698          
## 
## Kappa : 0.0304          
## 
## Mcnemar's Test P-Value : <2e-16          
##                                           
##             Sensitivity : 0.0180562       
##             Specificity : 0.9986023       
##          Pos Pred Value : 0.4263158       
##          Neg Pred Value : 0.9464633       
##              Prevalence : 0.0543955       
##          Detection Rate : 0.0009822       
##    Detection Prevalence : 0.0023039       
##       Balanced Accuracy : 0.5083292       
##                                           
##        'Positive' Class : 1  


# calculando a área sob a curva roc e gini 
auc(conjunto_teste$var_resposta,valores_preditos_2)
## Area under the curve: 0.7943
roc_obj <- roc(conjunto_teste$var_resposta1,valores_preditos_2)


auc_roc <- auc(roc_obj)

# Calcular o índice de Gini
gini <- 2 * auc_roc - 1

### Desenhando a Curva ROC e calculando medidas de sensibilidade e especificidade dependendo do corte
curvaroc <- roc(conjunto_teste$var_resposta, valores_preditos_2) #prob de ser bom

tab <- cbind(curvaroc$sensitivities,curvaroc$specificities,curvaroc$thresholds)
tab <- as.data.frame(tab)
ggroc(curvaroc) +
  geom_abline(slope = 1, intercept = 1,linetype = "dashed", color = "red4",size=1.5) +
  geom_text(aes(x = 0.8, y = 0.4, label = paste("AUC:", round(auc(curvaroc), 2))),
            color = "red4", size = 5) +  # Adiciona o texto ao gráfico
  labs(title = "Curva ROC para o /n Modelo de Regressão Logística",
       x = "Especificidade",
       y = "Sensibilidade") + 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black")
  )

### Plotar gráfico com a linha vertical no corte 
ggroc(curvaroc) +
  geom_abline(slope = 1, intercept = 1,linetype = "dashed", color = "red4",size=1.5) +
  labs(title = "Curva ROC para o Modelo de Regressão Logística",
       x = "Especificidade",
       y = "Sensibilidade") + 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color="black"),
    axis.text.y = element_text(size=12,color="black")
  ) +
  geom_segment(aes(x = especificidade_valor, y = 0, xend = especificidade_valor, yend = intersect_point$sensitivities),
               color = "purple4", linetype = "solid", size = 1.5)

# Medidas específicas de sensibilidade e especificidade
especificidade_valor <- 0.958  # exemplo de valor de especificidade
sensibilidade_valor <- 0.288  # exemplo de valor de sensibilidade
roc_data <- data.frame(specificities = rev(curvaroc$specificities), sensitivities = rev(curvaroc$sensitivities))
intersect_point <- roc_data[which.min(abs(roc_data$specificities - especificidade_valor)), ]


###############################################################################################################
################### Definição de Corte ########################################################################

# P(Y = 1): 0.053

## medidas de desempenho 

probabilidades <- predict(vc_lasso, X_teste.matrix, type = "response")
threshold1 <- 0.904
threshold2 <- 0.838
threshold3 <- 0.945


# Aplicar o ponto de corte e testar medidas de desempenho dependendo dele 
previsoes_threshold1 <- ifelse(probabilidades < threshold3, "1", "0")
table(previsoes_threshold1)
###   previsoes_threshold1
###     0     1 
###   79157  3752 

# acurácia
observed.classes <- conjunto_teste$var_resposta
mean(previsoes_threshold1 == observed.classes) 

# matriz de confusão 
require(caret)
library(biotools)
conjunto_teste$previsoes <- as.factor(
  ifelse(
    previsoes_threshold1
    ==1,"1","0"))
conjunto_teste$var_resposta <- as.factor(conjunto_teste$var_resposta) 
conjunto_teste$var_resposta <- as.factor(conjunto_teste$var_resposta)
matriz <- confusionMatrix(as.factor(conjunto_teste$previsoes), 
                          as.factor(conjunto_teste$var_resposta),positive="1")

matriz

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
#         0 75465  3371
#         1  2519  1115
# 
# Accuracy : 0.9286          
# 95% CI : (0.9268, 0.9303)
# No Information Rate : 0.9456          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.2375          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.24855         
#             Specificity : 0.96770         
#          Pos Pred Value : 0.30682         
#          Neg Pred Value : 0.95724         
#              Prevalence : 0.05440         
#          Detection Rate : 0.01352         
#    Detection Prevalence : 0.04406         
#       Balanced Accuracy : 0.60812         
#                                           
#        'Positive' Class : 1   


## Avaliando KS da base toda 
## essa analise fiz com a base toda (411471)
dados = base[,-c(1,2,3,4,5,323)] 

dados.matrix = dados %>% as.matrix()
valores_preditos = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=dados.matrix,type = "response")
valores_preditos1 = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=dados.matrix,type = "class")
valores_preditos2 = 1 - as.numeric(valores_preditos1)
table(valores_preditos2)

score=valores_preditos*1000
summary(trunc(score))

# Min.   :215.0  
# 1st Qu.:934.0  
# Median :966.0  
# Mean   :945.9  
# 3rd Qu.:983.0  
# Max.   :999.0  
hist(score)
base$score = score

ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic   
# 0.4409


# Avaliando o KS no treino (objetivo de analisar a estabilidade)
treino = base[base$split=="Treinamento",-c(1,2,3,4,5,383,384)] 
treino.matrix = treino %>% as.matrix()
valores_preditos_treino = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=treino.matrix,type = "response")
score_treino=valores_preditos_treino*1000
summary(trunc(score_treino))
 
# Min.   :226.0  
# 1st Qu.:934.0  
# Median :966.0  
# Mean   :945.9  
# 3rd Qu.:983.0  
# Max.   :999.0   
hist(score_treino)
treino  = base[base$split =="Treinamento",-c(1,2,3,5,383)]
treino$score_treino = score_treino

ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic   
# 0.4412



teste = base[base$split=="Teste",-c(1,2,3,4,5,323)] 
teste.matrix = teste %>% as.matrix()
valores_preditos_teste = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=teste.matrix,type = "response")
score_teste=valores_preditos_teste*1000
summary(trunc(score_teste))

## Min.   :215.0  
## 1st Qu.:934.0  
## Median :966.0  
## Mean   :945.7  
## 3rd Qu.:983.0  
## Max.   :999.0    
hist(score_teste)
teste  = base[base$split =="Teste",]
teste$score_teste = score_teste

ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic   
# 0.441878

#####################################################################################################################
############################### densidade do score de clientes bons e maus ##########################################


df <- data.frame(score = score, Var_Resposta = base$var_resposta)

cores = c("green2", "firebrick2")

ggplot(df, aes(x = s1, group = factor(Var_Resposta))) + 
  geom_density(aes(fill = Var_Resposta, color = Var_Resposta), alpha = 0.6) +
  labs(title = "Densidade de score por cliente/n usando modelo de Regressão Logística",
       x = "Score",
       y = "Densidade") + 
  scale_fill_manual(values = cores) +  # Definir cores de preenchimento
  scale_color_manual(values = cores) + # Definir cores das bordas
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = pretty(df$s1))+
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
  )


















