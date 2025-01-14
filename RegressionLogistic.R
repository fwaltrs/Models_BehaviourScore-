###############################################################################################################
############################### Bibliotecas ###################################################################

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
library(cenROC)
library(PRROC)
}

###############################################################################################################
############################### Regressão Logística com Penalização  ###########################################

###############################################################################################################
############################### Importando a base com dummies #############################################################
load("H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/base.RData")
dmy = dummyVars(" ~ .", data = base[], fullRank=T)
base = data.frame(predict(dmy, newdata = base))


###############################################################################################################
############################### Transformações e conjuntos de treinamento e validacao #########################
set.seed(290)
split <- sample(c("Treinamento","Teste"),prob=c(0.8,0.2),size= nrow(base),replace=TRUE)
base$split = split
table(split)

X_treino  = base[base$split =="Treinamento",-c(1,2,3,4,5,6,7,8,226)]
Y_treino = base[base$split=="Treinamento",c(8)] 
Y_treino = 1-Y_treino
Y_treino = as.factor(Y_treino)

library(dplyr)
X_treino.matrix = X_treino %>% as.matrix()

###############################################################################################################
############################### Modelo ########################################################################

start.time <- Sys.time()
set.seed(29) 
vc_lasso = glmnet::cv.glmnet(X_treino.matrix,
                             Y_treino,
                             alpha=1,
                             family="binomial",
                             type.measure =  "deviance")


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken 
plot(vc_lasso)

saveRDS(vc_lasso, file = "H:/Meu Drive/9º SEMESTRE/TG/Regressao_logistica/base_sem_cat/reg_logis_rsme.rds")
caminho="H:/Meu Drive/9º SEMESTRE/TG/Regressao_logistica/base_sem_cat/reg_logis_2.rds"
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

###############################################################################################################
################### Gráfico de importancia  ###################################################################

theme_set(theme_gray(base_size = 20))
coefs_estimates <- data.frame(Variavel=
                                rownames(coefs_estimates), Coeficientes=
                                coefs_estimates[,1])
coef_pos <- coefs_estimates %>% arrange(desc(Coeficientes))

coef_neg <- coefs_estimates %>% arrange(Coeficientes)


graf_pos <- ggplot(data=coef_pos[2:16,],aes(x=reorder(Variavel,
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


graf_neg <- ggplot(data=coef_neg[2:16,],aes(x=reorder(Variavel,
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

###############################################################################################################
################### Medidas de Desempenho  ####################################################################

####### Usando o conjunto de Teste ########################################################################
conjunto_teste = base[base$split =="Teste",-c(1,2,3,4,5,6,7,226)]
X_teste <- base[base$split =="Teste",-c(1,2,3,4,5,6,7,8,226)]
X_teste.matrix <- X_teste %>% as.matrix()

valores_preditos_1 <- vc_lasso %>% predict(newx = X_teste.matrix,type="class",
                                           s="lambda.1se") # corte padrão 0.5


start.time <- Sys.time()
valores_preditos_2 <- vc_lasso %>% predict(newx = X_teste.matrix,type="response",
                                           s="lambda.1se") 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

valores_preditos = 1 - as.numeric(valores_preditos_1)
table(valores_preditos) # corte padrão 0.5

# Medidas de Desempenho

# Área da curva ROC
roc_obj <- roc(conjunto_teste$var_resposta,valores_preditos_2)
auc_roc <- auc(roc_obj)

# Índice de Gini
gini <- 2 * auc_roc - 1

# Curva ROC
curvaroc <- roc(conjunto_teste$var_resposta, valores_preditos_2) #prob de ser bom
tab <- cbind(curvaroc$sensitivities,curvaroc$specificities,curvaroc$thresholds,1-curvaroc$thresholds)
tab <- as.data.frame(tab)

ggroc(curvaroc) +
  geom_abline(slope = 1, intercept = 1,linetype = "dashed", color = "red4",size=1.5) +
  geom_text(aes(x = 0.8, y = 0.4, label = paste("AUC:", round(auc(curvaroc), 2))),
            color = "red4", size = 5) +  # Adiciona o texto ao gráfico
  labs(title = "Curva ROC para o \n Modelo de Regressão Logística",
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
################### Escolha do Corte  ####################################################################

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
data_teste <- data.frame(as.factor(conjunto_teste$var_resposta),valores_preditos_2)
names(data_teste)[1] = "verdadeiro"
names(data_teste)[2] = "probabilidade"

curve <- pr_curve(data_teste, truth= verdadeiro, probabilidade)
Recall<-curve$recall
Precision<- curve$precision
F_Measure = (2 * Precision * Recall) / (Precision + Recall)
max_F_Measure<- max(F_Measure)
id <- which(F_Measure==max_F_Measure)
curve$.threshold[id] 


###############################################################################################################
################### Definição de Corte ########################################################################

probabilidades <- predict(vc_lasso, X_teste.matrix,
                          s=vc_lasso$lambda.1se,  type = "response")

threshold1 <- 0.838
threshold2 <- 0.941
threshold3 <- 0.359


# Aplicar o ponto de corte 
previsoes_threshold1 <- ifelse(probabilidades < threshold2, 1, 0) 
table(previsoes_threshold1)

risco<-1*(as.numeric(previsoes_threshold1) != conjunto_teste$var_resposta)  %>% mean()
var_risco<-1*(as.numeric(previsoes_threshold1) != conjunto_teste$var_resposta)  %>% var()

## IC para o risco estimado 

ic_inferior <- risco-1.96*sqrt(var_risco/nrow(conjunto_teste)) #0.2575837
ic_superior <- risco+1.96*sqrt(var_risco/nrow(conjunto_teste)) #0.2635755

# matriz de confusão 

conjunto_teste$previsoes <- as.factor(previsoes_threshold1)
conjunto_teste$var_resposta <- as.factor(conjunto_teste$var_resposta) 

matriz <- confusionMatrix(as.factor(conjunto_teste$previsoes), 
                          as.factor(conjunto_teste$var_resposta),positive="1")

matriz$byClass["F1"]


# Avaliando KS da base toda 
dados = base[,-c(1,2,3,4,5,323)] 
dados.matrix = dados %>% as.matrix()
valores_preditos = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=dados.matrix,type = "response")
valores_preditos1 = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=dados.matrix,type = "class")
valores_preditos2 = 1 - as.numeric(valores_preditos1)

score=valores_preditos*1000
base$score = score
ks.test(base[base$var_resposta==0,]$score, base[base$var_resposta==1,]$score)$statistic   


# Avaliando KS para treino
treino = base[base$split=="Treinamento",-c(1,2,3,4,5,383,384)] 
treino.matrix = treino %>% as.matrix()
valores_preditos_treino = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=treino.matrix,type = "response")
score_treino=valores_preditos_treino*1000
treino  = base[base$split =="Treinamento",-c(1,2,3,5,383)]
treino$score_treino = score_treino

ks.test(treino[treino$var_resposta==0,]$score_treino, treino[treino$var_resposta==1,]$score_treino)$statistic   


# Avaliando KS para teste
teste = base[base$split =="Teste",-c(1,2,3,4,5,6,7,8,226)]
teste.matrix = teste %>% as.matrix()
valores_preditos_teste = predict(vc_lasso,s=vc_lasso$lambda.1se,newx=teste.matrix,type = "response")

score_teste=valores_preditos_teste*1000
teste  = base[base$split =="Teste",]
teste$score_teste = score_teste

ks.test(teste[teste$var_resposta==0,]$score_teste, teste[teste$var_resposta==1,]$score_teste)$statistic   


#############################################################################################
############################### Densidade do score ##########################################


df <- data.frame(score = score_teste, Cliente = as.factor(teste$var_resposta))

cores = c("green2", "firebrick2")

densi_rl = ggplot(df, aes(x = s1, group = Cliente)) + 
  geom_density(aes(fill = Cliente, color = Cliente), alpha = 0.6) +
  labs(title = "Densidade do Score\n usando Regressão Logística",
       x = "Score",
       y = "Densidade") + 
  ylim(0,0.020)+
  scale_fill_manual(values = cores) +  
  scale_color_manual(values = cores) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = pretty(df$s1))+
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 14),
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12),
    axis.text.y = element_text(size=12)
  )



####################################################################################
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

g1 <-ggplot(gerar_df(teste$faixas_score), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2", width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth = 1) +  # Ajuste na escala
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2", size = 2) +
  labs(x = "Score", y = "Frequência ", 
       title = "Frequência e Taxa de Inadimplência\n Regressão Logística") +
  scale_y_continuous(
    limits = c(0, 100),  # Define o limite do eixo Y principal
    labels = percent_format(scale = 1),  # Formata o eixo Y principal como porcentagem
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Eixo Y secundário transformado
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 12),  
    axis.title.y = element_text(size = 12), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )

library(gridExtra)

grid.arrange(densi_rl, densi_florestas, ncol = 2)

grid.arrange(g1, g2, ncol = 2)










