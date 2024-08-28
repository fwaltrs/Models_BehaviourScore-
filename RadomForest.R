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

set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size=nrow(base),replace=TRUE)
base$split <- split

treino  = base[base$split =="Treinamento",-c(1,2,3,218)]
teste  = base[base$split =="Teste",-c(1,2,3,218)]

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
  mtry            = 5,
  num.trees = 500, 
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


saveRDS(floresta, file = "H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/reg_rf_2.rds")
caminho="H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/reg_rf_2.rds"
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

ggplot(importances %>% top_n(n=20), aes(x=reorder(variable, importance), 
                                        y=importance)) +
  geom_bar(stat="identity",col="white",fill="red4")+
  coord_flip() +
  labs(title = "Features mais importantes\n usando Floresta Aleatória",
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

# Área sob a curva ROC      
roc_obj <- roc(teste$var_resposta, previsoes$predictions[,1])
auc_roc <- auc(roc_obj)

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
  labs(title = "Curva ROC para o\n  Modelo de Floresta Aleatória",
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
j <- curvaroc$thresholds[id] 

# Média - G
g_mean <- sqrt(curvaroc$sensitivities*curvaroc$specificities)
max_g_mean<- max(g_mean)
id <- which(g_mean==max_g_mean)
g <- curvaroc$thresholds[id] 

#Curva precisão-Recall
library(ggplot2)
library(dplyr)
library(yardstick)
data_teste <- data.frame(as.factor(teste$var_resposta),previsoes$predictions[,1])
names(data_teste)[1] = "verdadeiro"
names(data_teste)[2] = "probabilidade"

curve <- pr_curve(data_teste, truth= verdadeiro, probabilidade)
Recall<-curve$recall
Precision<- curve$precision
F_Measure = (2 * Precision * Recall) / (Precision + Recall)
max_F_Measure<- max(F_Measure)
id <- which(F_Measure==max_F_Measure)
curve$.threshold[id] 


# Verificando qual ponto de corte escolher
probabilidades <- predict(floresta, data = teste)
threshold1 <- 0.930
threshold2 <- 0.580
threshold3 <- 0.811

# Aplicar o ponto de corte 
previsoes_threshold1 <- ifelse(probabilidades$predictions[,1] < threshold1, "1", "0")
table(previsoes_threshold1)

risco = 1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% mean()
var_risco = 1*(as.numeric(previsoes_threshold1)!= as.numeric(as.matrix(teste$var_resposta)))  %>% var()

## IC para o risco estimado 

ic_inferior <- risco-1.96*sqrt(var_risco/length(teste)) #0.2055516
ic_superior <- risco+1.96*sqrt(var_risco/length(teste)) #0.322883


# matriz de confusão 

teste$previsoes <- as.factor(
  ifelse(
    previsoes_threshold1
    ==1,"1","0"))
teste$var_resposta <- as.factor(teste$var_resposta) 
teste$var_resposta <- as.factor(teste$var_resposta)
matriz <- confusionMatrix(as.factor(teste$previsoes), 
                          as.factor(teste$var_resposta),positive="1")

matriz$byClass["F1"]


# KS para a base toda
dados = base[,-c(1,2,3,220,221)] 
previsoes <- predict(floresta, data = dados)
score=previsoes$predictions[,1]*1000 
base$score = score
ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic  

# KS para a amostra de treino
previsoes_treino <- predict(floresta, data = treino)
score_treino =previsoes_treino$predictions[,1]*1000 
treino$score_treino = score_treino
ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic  

# KS para a amostra de teste
previsoes_teste <- predict(floresta, data = teste)
score_teste =previsoes_teste$predictions[,1]*1000 
teste$score_teste = score_teste
ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic   

#############################################################################################
############################### Densidade do score ##########################################

df <- data.frame(score = score_teste, Cliente = teste$var_resposta)
df[,2] <- as.factor(df[,2])
cores = c("green2", "firebrick2")
densi_florestas = ggplot(df, aes(x = score, group = factor(Cliente))) + 
  geom_density(aes(fill = Cliente, color = Cliente), alpha = 0.6) +
  labs(title = "Densidade do Score \n usando Floresta Aleatória",
       x = "Score",
       y = "Densidade") +  ylim(0,0.020)+
  scale_fill_manual(values = cores) +  
  scale_color_manual(values = cores) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = pretty(df$score))+
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 14),
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
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


g2 <- ggplot(gerar_df(teste$faixas_score), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",alpha=2,size=2) +
  labs(x = "Score", y = "Representatividade (%)", 
       title = "Representatividade e Taxa de Inadimplência \n Floresta Aleatória") +
  scale_y_continuous(limits = c(0,100), sec.axis = sec_axis(~./100, name = "Taxa de Inadimplência (%)")) + 
  theme_minimal()+
  scale_fill_manual(values = cores) +  
  theme(
    plot.title = element_text(hjust = 0.5,size=14),
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color = "black"),
    axis.text.y = element_text(size=12,color = "black")
  )

