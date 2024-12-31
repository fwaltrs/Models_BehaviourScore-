{
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(scales)
}


load("H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/base.RData")
base = base %>% filter(var_2 > 5)
base = base[,-c(1,2,3)]
base$var_resposta = factor(base$var_resposta)
gerar_df <- function(variavel){
  tab <- table(variavel, base$var_resposta)
  taxa_inadimplencia = prop.table(tab, margin = 1)[,2]
  representatividade = prop.table(table(variavel))
  df <- data.frame(Categoria = names(taxa_inadimplencia),
                   Total = as.numeric(table(variavel)),
                   Representatividade = representatividade,
                   Maus =  table(variavel, base$var_resposta)[,2],
                   Taxa_Inadimplencia = as.numeric(taxa_inadimplencia))
  
  df$Categoria <- factor(df$Categoria, levels = unique(df$Categoria))
  
  return(df)
}

gerar_df(base$foto)

gerar_df(base$CAT_var_275) #Auxílio Emergencial 
gerar_df(base$var_339) #EDS
gerar_df(base$var_1) #Genero
gerar_df(base$var_365) #Classe Econômica
gerar_df(base$var_325) #UPA


summary(base$var_2)

cores = c("green2", "firebrick2")

boxplot_idade = ggplot(base, aes(x = var_resposta, y = var_2, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Idade", title = "Idade") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )


summary(base$var_251)
boxplot_renda = ggplot(base, aes(x = var_resposta, y = var_251, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Renda", title = "Renda") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_403)
boxplot_ensino_medio = ggplot(base, aes(x = var_resposta, y = var_403, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "%", title = "% de pessoas que cursam ensino médio ") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )
summary(base$var_410)
boxplot_beneficios = ggplot(base, aes(x = var_resposta, y = var_410, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "%", title = "% de pessoas com benefícios sociais ") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_394)
boxplot_alg_min = ggplot(base, aes(x = var_resposta, y = var_394, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "salários mínimos", title = "Valor do aluguel no CEP") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_393)
boxplot_unica_pessoa = ggplot(base, aes(x = var_resposta, y = var_393, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "%", title = "% de moradias com única pessoa") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

grid.arrange(boxplot_idade,
             boxplot_renda, 
             boxplot_alg_min,
             boxplot_ensino_medio,
             boxplot_beneficios,
             boxplot_unica_pessoa, ncol = 3)


### Análise de Variáveis Comportamentais
summary(base$var_199)
boxplot_var_199 <-  ggplot(base, aes(x = var_resposta, y = var_199, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 199", title = "Qtd de vencimentos quitados\n no último mês") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_204)
boxplot_var_204 <-  ggplot(base, aes(x = var_resposta, y = var_204, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 204", title = " Total monetário quitado\n no último mês") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_68)
boxplot_var_68 <-  ggplot(base, aes(x = var_resposta, y = var_68, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 68", title = "Média de parcelas dos contratos \n nos últimos 2 anos") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

# var_42: Cancela
summary(base$var_19)
boxplot_var_19 <-  ggplot(base, aes(x = var_resposta, y = var_19, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 19", title = "Dias de atraso dos vencimentos mensais\n nos últimos 3 meses") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_22)
boxplot_var_22 <-  ggplot(base, aes(x = var_resposta, y = var_22, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 22", title = "Dias de atraso dos vencimentos mensais\n nos últimos 2 anos") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

summary(base$var_70)
boxplot_var_70 <-  ggplot(base, aes(x = var_resposta, y = var_70, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 70", title = "Qtd de vencimentos quitados antecipados\n nos últimos 3 meses") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )


summary(base$var_55)
boxplot_var_55 <-  ggplot(base, aes(x = var_resposta, y = var_55, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 55", title = "Quantidade de contratos \n nos últimos 6 meses") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )



summary(base$var_110)
boxplot_var_110<-  ggplot(base, aes(x = var_resposta, y = var_90, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 110", title = "Valor médio de vencimentos pontuais \n nos últimos 3 meses") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )



summary(base$var_3)
boxplot_var_3 <-  ggplot(base, aes(x = var_resposta, y = var_3, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 3", title = "Tempo até a primeira compra") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )


base$var_resposta = as.factor(base$var_resposta)
summary(base$var_228)
boxplot_var_228  <-  ggplot(base, aes(x = var_resposta, y = var_228, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 228", title = " % vencimentos atrasados ult_06m_vs_ult_24m") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

boxplot_var_239 <-  ggplot(base, aes(x = var_resposta, y = var_239, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 239", title = "Soma de vencimentos quitados nos últimos\n 6 meses por renda") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )


grid.arrange(boxplot_var_199,
             boxplot_var_204,
             boxplot_var_110,
             boxplot_var_19,
             boxplot_var_22,
             boxplot_var_70,
             ncol = 3)

grid.arrange(boxplot_var_55,
             boxplot_var_68,
             boxplot_var_239,
             boxplot_var_3,
             boxplot_var_4,
             boxplot_var_228,
             ncol = 3)




base1 = base %>% filter(var_4 < 3000)
boxplot_var_4 <-  ggplot(base1, aes(x = var_resposta, y = var_4, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 4", title = "Tempo até o primeiro cartão") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )

boxplot_var_3 <- ggplot(base, aes(x = var_resposta, y = var_3, fill = var_resposta, color = var_resposta)) +
  geom_boxplot(color = "black") +  # Define a borda preta dos boxplots
  scale_fill_manual(values = cores) +  
  labs(x = "Cliente", y = "Var 3", title = "Tempo até a primeira compra") +
  scale_x_discrete() + theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 23,face = "bold"),
    axis.title.x = element_text(size = 15,face = "bold"),  
    axis.title.y = element_text(size = 15,face = "bold"), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 15, color = "black",face = "bold"),
    axis.text.y = element_text(size = 15, color = "black",face = "bold"),
    legend.position = "none"  
  )
