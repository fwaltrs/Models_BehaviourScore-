############################################################################################################################################################################################################
##########################################     FLORESTAS ALEATÓRIAS     ################################################################################################################################

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
########################## IMPORTAR A BASE  #############################################

load("H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/base.RData")
base = base %>% filter(var_2 > 5)
base = base[,-c(1,2,3)]

#write_parquet(base, sink = "H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/base.parquet")
set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size=nrow(base),replace=TRUE)
base$split <- split


treino  = base[base$split =="Treinamento",-c(215)]
teste  = base[base$split =="Teste",-c(215)]

set.seed(290)
split <- sample(c("Treinamento","Validacao"),prob=c(0.8,0.2),size=nrow(treino),replace=TRUE)
treino$split <- split


validacao  = treino[treino$split=="Validacao",-c(215)]
Y_validacao = treino[treino$split=="Validacao",c(1)]
treino  = treino[treino$split=="Treinamento",-c(215)]

treino$var_resposta <- as.factor(treino$var_resposta)

###############################################################################################################
############################### Modelo 2: Florestas Aleatórias #############################################################

names(treino) <- make.names(names(treino))
start.time <- Sys.time()
floresta <- ranger(
  formula         = var_resposta ~ ., 
  data            = treino, 
  mtry            = 15,
  max.depth     = 10,
  num.trees = 250, 
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
# mtry  = 15,
# max.depth = 10,
# num.trees = 250, com 1.788734 mins

# mtry  = 20,
# max.depth = 10,
# num.trees = 250, com 1.998688 mins

# mtry  = 10,
# max.depth = 10,
# num.trees = 250, com 1.536168 mins

saveRDS(floresta, file = "H:/Meu Drive/9º SEMESTRE/TG/RandomForest/FINAL/florestas_mtry15.rds")
caminho= "H:/Meu Drive/9º SEMESTRE/TG/RandomForest/FINAL/florestas_mtry15.rds"
floresta = readRDS(caminho)


###############################################################################################################
############################### Previsões #############################################################

start.time <- Sys.time()
previsoes <- predict(floresta,
                     data=teste)  
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken   

###############################################################################################################
############################### Medida de Importancia #############################################################

importances <- tibble(variable=names(floresta$variable.importance), importance = floresta$variable.importance) %>% 
  arrange(desc(importance))

importancias <- data.frame(variavel = names(floresta$variable.importance),
                           importancia = floresta$variable.importance)


importancias <- importancias[order(-importancias$importancia), ]
importancias$importancia_normalizada <- importancias$importancia / sum(importancias$importancia)
importancias$importancia_normalizada_100 = importancias$importancia_normalizada *100

ggplot(importancias %>% top_n(n=25), aes(x=reorder(variavel, importancia_normalizada_100), 
                                        y=importancia_normalizada_100)) +
  geom_bar(stat="identity",col="white",fill="red4")+
  coord_flip() +
  labs(title = "Features mais importantes\n usando Random Forest",
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

write.table(previsoes,"H:/Meu Drive/9º SEMESTRE/TG/PREDITOS/pred_reg_randomforest.txt",sep=';')

###############################################################################################################
############################### Medidas de Desempenho #############################################################

# Área sob a curva ROC      
roc_obj_rf <- roc(teste$var_resposta, previsoes$predictions[,1])
auc_roc <- auc(roc_obj_rf)

# Calcular o índice de Gini
gini <- 2 * auc_roc - 1

#Gráfico de Curva ROC
curvaroc <- roc(teste$var_resposta, previsoes$predictions[,1])

tab <- cbind(curvaroc$sensitivities,1-curvaroc$specificities,curvaroc$thresholds)
tab <- as.data.frame(tab)
ggroc(curvaroc) +
  geom_abline(slope = 1, intercept = 1,linetype = "dashed", color = "red4",size=1.5) +
  geom_text(aes(x = 0.8, y = 0.4, label = paste("AUC:", round(auc(curvaroc), 2))),
            color = "red4", size = 5) +  # Adiciona o texto ao gráfico
  labs(title = "Curva ROC para o\n  Modelo de Random Forest",
       x = "1-Especificidade",
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
j <- curvaroc$thresholds[id] #0.9421705


# MÉDIA DAS PROBABILIDADES
m = mean(previsoes$predictions[,1]) #0.946


#Curva precisão-Recall
recall <- curvaroc$sensitivities  # TPR ou recall
fpr <- 1 - curvaroc$specificities  # FPR

# Obter o número de exemplos positivos (Pos) e negativos (Neg)
Pos <- sum(teste$var_resposta == 1)  # Total de positivos reais
Neg <- sum(teste$var_resposta == 0)  # Total de negativos reais


precisao <- (recall * Pos) / ((recall * Pos) + (fpr * Neg))

precisao_recall = data.frame(Precisao = precisao, Recall = recall, corte = curvaroc$thresholds)
F_Measure = (2 * precisao * recall) / (precisao + recall)

max_F_Measure = summary(F_Measure)[6]
id <- which(F_Measure==max_F_Measure)
precisao_recall[id,]$corte #0.8550067




# Verificando qual ponto de corte escolher
probabilidades <- predict(floresta, data = teste)
threshold1 <- 0.942
threshold2 <- 0.946
threshold3 <- 0.855

# Aplicar o ponto de corte 
previsoes_threshold1 <- ifelse(probabilidades$predictions[,1] < threshold2, 1, 0)
table(previsoes_threshold1)

risco = 1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% mean()
var_risco = 1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% var()

## IC para o risco estimado 

ic_inferior <- risco-1.96*sqrt(var_risco/nrow(teste)) #0.2612
ic_superior <- risco+1.96*sqrt(var_risco/nrow(teste)) #0.2672


# matriz de confusão 

teste$previsoes <- as.factor(previsoes_threshold1)
teste$var_resposta <- as.factor(teste$var_resposta) 

matriz <- confusionMatrix(as.factor(teste$previsoes), 
                          as.factor(teste$var_resposta),positive="1")

matriz$byClass["F1"]


# KS para a base toda
dados = base[,-c(1,2,3,220,221)] 
previsoes <- predict(floresta, data = dados)
score=previsoes$predictions[,1]*1000 ##prob de ser 0 
base$score = score
ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic  

# KS para a amostra de treino
previsoes_treino <- predict(floresta, data = treino)
score_treino =previsoes_treino$predictions[,1]*1000 ##prob de ser 0 
treino$score_treino = score_treino
ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic

# KS para a amostra de teste #0.4365247
previsoes_teste <- predict(floresta, data = teste)
score_teste =previsoes_teste$predictions[,1]*1000 
teste$score_teste = score_teste
ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic  # 

#############################################################################################
############################### Densidade do score ##########################################

df <- data.frame(score = score_teste, Cliente = teste$var_resposta)
df[,2] <- as.factor(df[,2])
cores = c("green2", "firebrick2")
densi_florestas = ggplot(df, aes(x = score, group = factor(Cliente))) + 
  geom_density(aes(fill = Cliente, color = Cliente), alpha = 0.6) +
  labs(title = "Densidade do Score \n usando Random Forest",
       x = "Score",
       y = "Densidade") +  ylim(0,0.020)+
  scale_fill_manual(values = cores) +  # Definir cores de preenchimento
  scale_color_manual(values = cores) + # Definir cores das bordas
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = pretty(df$score))+
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
  )

boxplot_rf = ggplot(df, aes(x = Cliente, y = score, fill = Cliente, color = Cliente)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Score", title = "Score Random Forest") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"  # Remove a legenda
  )

##############################################################################################
############################### Descritiva do score ##########################################

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

library(scales)
g2 <- ggplot(gerar_df(teste$faixas_score), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",alpha=2,size=2) +
  labs(x = "Score", y = "Frequência", 
       title = "Frequência e Taxa de Inadimplência \n Random Forest") +
  scale_y_continuous(
    limits = c(0, 100),  # Define o limite do eixo Y principal
    labels = percent_format(scale = 1),  # Formata o eixo Y principal como porcentagem
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Eixo Y secundário transformado
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  # Aumenta o tamanho do título do eixo x
    axis.title.y = element_text(size = 15),  # Aumenta o tamanho do título do eixo 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
  )
summary(teste$score_teste)
