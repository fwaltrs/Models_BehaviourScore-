############################################################################################################################################################################################################
##########################################     FLORESTAS ALEATÓRIAS     ################################################################################################################################

## https://bradleyboehmke.github.io/HOML/random-forest.html
## https://bradleyboehmke.github.io/HOML/random-forest.html

###############################################################################################################
############################### bibliotecas ###################################################################

{
library(ltm)
library(arrow)
library(dplyr)
library(glmnet)
library(vcd)
library(ggpubr)
library(ggplot2)
require(caret)
library(biotools)
library(pROC)
library(randomForest)
library(ranger)
}

###############################################################################################################
############################### Importando a base total com variáveis já categorizadas #############################################################
lista = as.list(colnames(base))
# Lista de todas as colunas
todas_colunas <- colnames(base)

# Lista de colunas que você deseja remover

colunas_sem_CAT <- gsub("^CAT_", "", todas_colunas)
class(colunas_sem_CAT)


base_v2 <- df[,colunas_sem_CAT]

arrow::write_parquet(base_v2, "G:/Meu Drive/8ºSEMESTRE/TG/Base-TG/base_219var.parquet")

##########################################################################################
########################## BASE CATEGORIZADA #############################################
caminho="H:/Meu Drive/9º SEMESTRE/TG/bases/base_V7.parquet"
base <- arrow::read_parquet(caminho)

names(base) <- make.names(names(base))

set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size=nrow(base),replace=TRUE)
base$split <- split
table(base$split)

treino  = base[base$split =="Treinamento",-c(1,2,3,220,221)]
teste  = base[base$split =="Teste",-c(1,2,3,220,221)]

set.seed(290)
split <- sample(c("Treinamento","Validacao"),prob=c(0.8,0.2),size=nrow(treino),replace=TRUE)
table(split)
treino$split <- split
table(treino$split)

base1 = base[,-c(1,2,3,220,221)]
validacao  = treino[treino$split=="Validacao",-c(217)]
Y_validacao = treino[treino$split=="Validacao",c(1)]
treino  = treino[treino$split=="Treinamento",-c(217)]



treino$var_resposta <- as.factor(treino$var_resposta)

###############################################################################################################
############################### Modelo 2: Florestas Aleatórias #############################################################

names(treino) <- make.names(names(treino))

floresta <- ranger(
  formula         = var_resposta ~ ., 
  data            = treino, 
  mtry            = 5,
  importance = "impurity",
  replace=T,
  splitrule = "gini",
  probability = TRUE,
  verbose  = TRUE,
  seed   = 100,
  classification=T
)


caminho="H:/Meu Drive/9º SEMESTRE/TG/RandomForest/Modelo.1-varresposta/RandomForestseed100_1-varrespostaV7.rds"
saveRDS(floresta, file = caminho)

###############################################################################################################
############################### Previsões #############################################################

### conjunto de teste
previsoes <- predict(floresta,data=teste)  

class_previsao <- ifelse(previsoes$predictions[,2] > 0.5, "1", "0")
table(class_previsao)



###############################################################################################################
############################### Medida de Importancia #############################################################

floresta$variable.importance

importances <- tibble(variable=names(floresta$variable.importance), importance = floresta$variable.importance) %>% 
  arrange(desc(importance))

ggplot(importances %>% top_n(n=20), aes(x=reorder(variable, importance), 
                                        y=importance)) + geom_bar(stat = "identity", position="dodge",color="red4",fill="red4") +
  labs(title = "Features mais importantes\n usando Florestas Aleatórias",
       x = "Feature",
       y = "Importância") + 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color='black'),
    axis.text.y = element_text(size=12,color='black')
  )



###############################################################################################################
############################### Medidas de Desempenho #############################################################


1*(as.numeric(class_previsao)!= as.numeric(as.matrix(teste$var_resposta)))  %>% mean()

      
roc_obj <- roc(teste$var_resposta, previsoes$predictions[,1])

auc_roc <- auc(roc_obj) #0.7882

# Calcular o índice de Gini
gini <- 2 * auc_roc - 1 #0.5797392




#gráfico mais robusto
curvaroc <- roc(as.numeric(as.matrix(teste$var_resposta)), as.numeric(previsoes$predictions[,2]))

tab <- cbind(curvaroc$sensitivities,curvaroc$specificities,curvaroc$thresholds)
tab <- as.data.frame(tab)
ggroc(curvaroc) +
  geom_abline(slope = 1, intercept = 1,linetype = "dashed", color = "red4",size=1.5) +
  geom_text(aes(x = 0.8, y = 0.4, label = paste("AUC:", round(auc(curvaroc), 3))),
            color = "red4", size = 5) +  # Adiciona o texto ao gráfico
  labs(title = "Curva ROC para o\n  Modelo de Florestas Aleatórias",
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


probabilidades <- predict(floresta, data = teste)
threshold1 <- 1-0.945
threshold2 <- 1-0.874
threshold3 <- 1-0.811

# Aplicar o ponto de corte 
previsoes_threshold1 <- ifelse(probabilidades$predictions[,2] > threshold3, "1", "0")
table(previsoes_threshold1)
###   previsoes_threshold1
###     0     1 
###   79157  3752 
1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% mean()

# acurácia
observed.classes <- teste$var_resposta
mean(previsoes_threshold1 == observed.classes) # 0.6287255

# matriz de confusão 
require(caret)
library(biotools)
teste$previsoes <- as.factor(
  ifelse(
    previsoes_threshold1
    ==1,"1","0"))
teste$var_resposta <- as.factor(teste$var_resposta) 
teste$var_resposta <- as.factor(teste$var_resposta)
matriz <- confusionMatrix(as.factor(teste$previsoes), 
                          as.factor(teste$var_resposta),positive="1")

matriz


## KS
dados = base[,-c(1,2,3,220,221)] 
#dados.matrix = dados %>% as.matrix()
previsoes <- predict(floresta, data = dados)

score=previsoes$predictions[,1]*1000 ##prob de ser 0 
summary(trunc(score))
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 77.0   973.0   992.0   941.5   997.0  1000.0 
base$score = score
hist(score)
ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic  
# 0.85367


previsoes_treino <- predict(floresta, data = treino)
score_treino =previsoes_treino$predictions[,1]*1000 ##prob de ser 0 
summary(trunc(score_treino))
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 137.0   952.0   979.0   942.1   991.0  1000.0 
treino$score_treino = score_treino
hist(score_treino)
ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic  # 0.9612961
# 0.999

previsoes_teste <- predict(floresta, data = teste)
score_teste =previsoes_teste$predictions[,1]*1000 ##prob de ser 0 
summary(trunc(score_teste))
## Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 259.0   919.0   963.0   937.2   984.0  1000.0 
teste$score_teste = score_teste
hist(score_teste)
ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic  # 
# 0.4302


df <- data.frame(score = score, Var_Resposta = base$var_resposta)
df[,2] <- as.factor(df[,2])
cores = c("green2", "firebrick2")
ggplot(df, aes(x = score, group = factor(Var_Resposta))) + 
  geom_density(aes(fill = Var_Resposta, color = Var_Resposta), alpha = 0.6) +
  labs(title = "Densidade de score por cliente\n usando modelo de Florestas Aleatórias",
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
###############################################################################################################
############################### Florestas Aleatórias: ajustando hiperparametro #############################################################

caminho="G:/Meu Drive/8ºSEMESTRE/TG/Base-TG/base_tratada_versaofinalv1_OOT.parquet"
base <- arrow::read_parquet(caminho)
base$var_resposta <- as.factor(base$var_resposta)

teste <- base[base$foto=='set22',]
base = base[base$foto != 'set22',]

set.seed(100)
split <- sample(c("Treinamento","Validacao"),prob=c(0.7,0.3),size=nrow(base),replace=TRUE)

base$split <- split
table(base$split)



treino  = base[base$split=="Treinamento",-c(1,2,3,220)]
validacao  = base[base$split=="Validacao",-c(1,2,3,220)]
teste = teste[,-c(1,2,3,4)]
Y_teste = teste[,c(4)]

###############################################################################################################
############################### Hiper parametros #############################################################
n_features <- length(setdiff(names(validacao), "var_resposta"))
hyper_grid <- expand.grid(
  mtry = floor((n_features) * c(0.04,0.05,0.06,0.07,0.08,0.09,0.1,0.15,0.2)), +
  min.node.size = c(1, 3, 5, 10), 
  sample.fraction = c(.5, .63, .8), 
  respect.unordered.factors = c("ignore","order",NULL),
  num.trees = 500, +
  prediction.error = NA,
  funcao_risco = NA,
  gini = NA
  
)

for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = var_resposta ~ ., 
    data            = treino, 
    num.trees       = hyper_grid$num.trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$min.node.size[i],
    replace         = TRUE,
    sample.fraction = hyper_grid$sample.fraction[i],
    respect.unordered.factors = hyper_grid$respect.unordered.factors[i],
    importance = "impurity",
    splitrule = "gini",
    probability = TRUE,
    verbose  = TRUE,
    seed   = 100
  )
  
  hyper_grid$prediction.error[i] <- fit$prediction.error
  
  ## com a amostra de validação
  previsoes <- predict(fit,data=validacao) 
  class_previsao <- ifelse(previsoes$predictions[,2] > prevalencia, "1", "0")
  hyper_grid$funcao_risco[i] <- 1*(as.numeric(class_previsao)!= as.numeric(as.matrix(Y_validacao)))  %>% mean()
  
  roc_obj <- roc(as.numeric(as.matrix(Y_validacao)), as.numeric(previsoes$predictions[,2]))
  auc_roc <- auc(roc_obj)
  
  hyper_grid$gini[i] <- 2*auc_roc - 1
  
}
data = as.data.frame(hyper_grid)
write.table(data, file = "hyper_grid.txt", sep = "/t", quote = FALSE, row.names = FALSE)



###############################################################################################################
############################### Escolher hiper parametros #############################################################
names(treino) <- make.names(names(treino))
library(data.table)


caminho="C:/Users/ferna/Dropbox/PC/Downloads/hyper_grid2.txt"
grid <- fread(caminho)

x = grid %>% filter(is.na(prediction.error))
dim(x)
table(x$mtry)
table(x$min.node.size)
table(x$sample.fraction)
table(x$respect.unordered.factors)
table(x$num.trees)


#32  3  0.80  order  645

fit1 <- ranger(
  formula         = var_resposta ~ ., 
  data            = treino, 
  num.trees       = 645,
  mtry            = 32,
  min.node.size   = 3,
  replace         = TRUE,
  sample.fraction = 0.8,
  respect.unordered.factors = "order",
  importance = "impurity",
  splitrule = "gini",
  probability = TRUE,
  verbose  = TRUE,
  seed   = 100
)


caminho="H:/Meu Drive/9º SEMESTRE/TG/bases/base_tratada_versaofinalV2_OOT.parquet"
base <- arrow::read_parquet(caminho)
names(base) <- make.names(names(base))


teste <- base[base$foto=='set22',]
base = base[base$foto != 'set22',]

X_treino  = base[1:1000,-c(1,2,3,4)]
Y_treino = 1-base$var_resposta
Y_treino = Y_treino[1:1000]
Y_treino = as.factor(Y_treino)


library(randomForest)
set.seed(100)
t <- tuneRF(X_treino,Y_treino,mtryStart=10, ntreeTry=500,stepFactor=1.5,doBest=T)
t
# mtry = 10  OOB error = 3% 
# Searching left ...
# mtry = 7 	OOB error = 3% 
# 0 0.05 
# Searching right ...
# mtry = 15 	OOB error = 3% 
# 0 0.05 

X_treino  = base[300000:328562,-c(1,2,3,4)]
Y_treino = 1-base$var_resposta
Y_treino = Y_treino[300000:328562]
Y_treino = as.factor(Y_treino)


library(randomForest)
set.seed(100)
t2 <- tuneRF(X_treino,Y_treino,mtryStart=10, ntreeTry=500,stepFactor=1.5,doBest=T)
t2
##  mtry = 10  OOB error = 5.6% 
##  Searching left ...
##  mtry = 7 	OOB error = 5.75% 
##  -0.0275172 0.05 
##  Searching right ...
##  mtry = 15 	OOB error = 5.28% 
##  0.05628518 0.05 
##  mtry = 22 	OOB error = 4.8% 
##  0.09145129 0.05 
##  mtry = 33 	OOB error = 4.4% 
##  0.08388038 0.05 
##  mtry = 49 	OOB error = 4.12% 
##  0.06289809 0.05 
##  mtry = 73 	OOB error = 4.02% 
##  0.02463891 0.05 

caminho="H:/Meu Drive/9º SEMESTRE/TG/RandomForest/Modelo.1-varresposta/floresta_tuneRF_1-varrresposta.rds"
saveRDS(t2,caminho)

# Outro Modo --------------------------------------------------------------
# + pesado
base$var_resposta = as.factor(base$var_resposta)
treino = base[1:1000,-c(1,2,3)]
library(caret)
control <- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=c(5:25))
set.seed(100)
custom <- train(var_resposta~., data=treino, method="rf", 
                tuneGrid=tunegrid, ntree = 500, trControl=control)
custom
##  Random Forest 
##  
##  1000 samples
##  215 predictor
##  2 classes: '0', '1' 
##  
##  No pre-processing
##  Resampling: Cross-Validated (10 fold) 
##  Summary of sample sizes: 900, 900, 900, 900, 900, 900, ... 
##  Resampling results across tuning parameters:
##    
##    mtry  Accuracy  Kappa
##  5    0.97      0    
##  6    0.97      0    
##  7    0.97      0    
##  8    0.97      0    
##  9    0.97      0    
##  10    0.97      0    
##  11    0.97      0    
##  12    0.97      0    
##  13    0.97      0    
##  14    0.97      0    
##  15    0.97      0    
##  16    0.97      0    
##  17    0.97      0    
##  18    0.97      0    
##  19    0.97      0    
##  20    0.97      0    
##  21    0.97      0    
##  22    0.97      0    
##  23    0.97      0    
##  24    0.97      0    
##  25    0.97      0    
##  
##  Accuracy was used to select the optimal model using the largest value.
##  The final value used for the model was mtry = 5.