############################################################################################################################################################################################################
##########################################     FLORESTAS ALEATÓRIAS     ################################################################################################################################

## https://bradleyboehmke.github.io/HOML/random-forest.html
## https://bradleyboehmke.github.io/HOML/random-forest.html

###############################################################################################################
############################### Bibliotecas ###################################################################

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

##########################################################################################
########################## BASE CATEGORIZADA #############################################

caminho="H:/Meu Drive/9º SEMESTRE/TG/bases/base_V7.parquet"
base <- arrow::read_parquet(caminho)

set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size=nrow(base),replace=TRUE)
base$split <- split

treino  = base[base$split =="Treinamento",-c(1,2,3,220,221)]
teste  = base[base$split =="Teste",-c(1,2,3,220,221)]

set.seed(290)
split <- sample(c("Treinamento","Validacao"),prob=c(0.8,0.2),size=nrow(treino),replace=TRUE)
treino$split <- split


base1 = base[,-c(1,2,3,220,221)]
validacao  = treino[treino$split=="Validacao",-c(217)]
Y_validacao = treino[treino$split=="Validacao",c(1)]
treino  = treino[treino$split=="Treinamento",-c(217)]

treino$var_resposta <- as.factor(treino$var_resposta)

###############################################################################################################
############################### Modelo 2: Florestas Aleatórias #############################################################

names(treino) <- make.names(names(treino))
start.time <- Sys.time()
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
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

caminho="H:/Meu Drive/9º SEMESTRE/TG/RandomForest/Modelo.1-varresposta/RandomForestseed100_1-varrespostaV7.rds"
floresta = readRDS(caminho)


###############################################################################################################
############################### Previsões #############################################################

### conjunto de teste
start.time <- Sys.time()
previsoes <- predict(floresta,data=teste)  
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken   


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
      
roc_obj <- roc(teste$var_resposta, previsoes$predictions[,1])
auc_roc <- auc(roc_obj)

# Calcular o índice de Gini
gini <- 2 * auc_roc - 1 #0.5797392


#gráfico de Curva ROC
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


###############################################################################################################
############################### Ponto de corte  ###########################################

# estatística de Youden (J) 
j_estat <- curvaroc$sensitivities+curvaroc$specificities-1
max_j_estat<- max(j_estat)
id <- which(j_estat==max_j_estat)
j <- curvaroc$thresholds[id] #0.943767
1-j
# Média - G
g_mean <- sqrt(curvaroc$sensitivities*curvaroc$specificities)
max_g_mean<- max(g_mean)
id <- which(g_mean==max_g_mean)
g <- curvaroc$thresholds[id] #0.943767
1-g

#Curva precisão-Recall
library(ggplot2)
library(dplyr)
library(yardstick)
data_teste <- data.frame(conjunto_teste$var_resposta,valores_preditos_2)
names(data_teste)[1] = "verdadeiro"
names(data_teste)[2] = "probabilidade"

curve <- pr_curve(data_teste, truth= verdadeiro, probabilidade)
Recall<-curve$recall
Precision<- curve$precision
F_Measure = (2 * Precision * Recall) / (Precision + Recall)
max_F_Measure<- max(F_Measure)
id <- which(F_Measure==max_F_Measure)
curve$.threshold[id] # 0.3643022



probabilidades <- predict(floresta, data = teste)
threshold1 <- 1-0.930
threshold2 <- 1-0.874
threshold3 <- 0.811

# Aplicar o ponto de corte 
previsoes_threshold1 <- ifelse(probabilidades$predictions[,1] < threshold3, "1", "0")
table(previsoes_threshold1)

1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% mean()

# acurácia
observed.classes <- teste$var_resposta
mean(previsoes_threshold1 == observed.classes) 

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


## KS para a base toda
dados = base[,-c(1,2,3,220,221)] 
previsoes <- predict(floresta, data = dados)

score=previsoes$predictions[,1]*1000 ##prob de ser 0 

base$score = score
ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic  


# KS para a amostra de treino
previsoes_treino <- predict(floresta, data = treino)
score_treino =previsoes_treino$predictions[,1]*1000 ##prob de ser 0 

treino$score_treino = score_treino
ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic  # 0.9612961

# KS para a amostra de teste
previsoes_teste <- predict(floresta, data = teste)
score_teste =previsoes_teste$predictions[,1]*1000 
teste$score_teste = score_teste
ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic  # 


####################################################################################
############################### densidade do score ##########################################

df <- data.frame(score = score_teste, Var_Resposta = teste$var_resposta)
df[,2] <- as.factor(df[,2])
cores = c("green2", "firebrick2")
densi_florestas = ggplot(df, aes(x = score, group = factor(Var_Resposta))) + 
  geom_density(aes(fill = Var_Resposta, color = Var_Resposta), alpha = 0.6) +
  labs(title = "Densidade de score por cliente\n usando modelo de Florestas Aleatórias",
       x = "Score",
       y = "Densidade") + 
  scale_fill_manual(values = cores) +  # Definir cores de preenchimento
  scale_color_manual(values = cores) + # Definir cores das bordas
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = pretty(df$score))+
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 14),
    axis.title.x = element_text(size = 12),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 12),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
  )



####################################################################################
############################### descritiva do score ##########################################

cores = c("deepskyblue2","firebrick2")

teste = teste %>% mutate(faixas_score = ifelse(score_teste <= 100, "0-100",
                              ifelse(score_teste <= 200, "101-200",
                                     ifelse(score_teste <= 300, "201-300",
                                            ifelse(score_teste <= 400, "301-400",
                                                   ifelse(score_teste <= 500, "401-500",
                                                          ifelse(score_teste <= 600, "501-600",
                                                                 ifelse(score_teste <= 700, "601-700",
                                                                        ifelse(score_teste <= 800, "701-800", 
                                                                               ifelse(score_teste <=900, "801-900","901-1000"))))))))))
gerar_df <- function(variavel){
  tab <- table(variavel, teste$var_resposta)
  taxa_inadimplencia = prop.table(tab, margin = 1)[,2]
  representatividade = prop.table(table(variavel))
  df <- data.frame(Categoria = names(taxa_inadimplencia),
                   Taxa_Inadimplencia = as.numeric(taxa_inadimplencia),
                   Representatividade = representatividade)
  df$Categoria <- factor(df$Categoria, levels = unique(df$Categoria))
  
  return(df)
}
gerar_df(teste$faixas_score)

ggplot(gerar_df(teste$faixas_score), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",alpha=2,size=2) +
  labs(x = "Score", y = "Representatividade (%)", 
       title = "Representatividade e Taxa de Inadimplência \n Floresta Aleatória") +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Taxa de Inadimplência (%)")) + theme_minimal()+
  scale_fill_manual(values = cores) +  
  theme(
    plot.title = element_text(hjust = 0.5,size=14),
    axis.title.x = element_text(size = 12),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 12), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color = "black"),
    axis.text.y = element_text(size=12,color = "black")
  )
