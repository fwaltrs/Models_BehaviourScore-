
{
  library(dplyr)
  library(ggplot2)
  library(gridExtra)
  library(scales)
}


# Importando a base -------------------------------------------------------------------
load("H:/Meu Drive/9º SEMESTRE/TG/RandomForest/base_sem_cat/base.RData")

# Verificamos as variáveis mais significativas isoladamente com a Y
matriz <- matrix(data=NA, ncol=2,nrow=length(5:219))
for (i in 5:ncol(base)-1){
  modelo <- glm(var_resposta ~ ., data=base[,c(4,i)],family="binomial")
  matriz[i-4,1] <- colnames(base)[i]
  matriz[i-4,2] <- round(summary(modelo)$aic,4)
}
data11 = as.data.frame(matriz)
data11$V2 <- as.numeric(data11$V2)
data1 = data11 %>% arrange(V2)


# Análise da taxa de inadimplencia por período

order <- c('dez21', 'fev22', 'mai22', 'jul22', 'set22')
base$foto <- factor(base$foto, levels=order, ordered=TRUE)

base <- base[order(match(base$foto, order)), ]

#representatividade <- table(base$foto) / nrow(base)
base$var_resposta <- as.numeric(as.character(base$var_resposta))
taxa_inadimplencia <- aggregate(var_resposta ~ foto, base, mean)
#tapply(base$var_resposta,base$foto,mean)
df <- data.frame(Bad = taxa_inadimplencia)

ggplot(df, aes(x = Bad.foto, y = Bad.var_resposta, group = 1)) +
  geom_line(color = "red", linewidth = 1) + # Desenha a linha vermelha
  geom_point(color = "red", size = 2) + # Adiciona os pontos vermelhos
  labs(title = "Taxa de Inadimplência por período",
       x = "Período",
       y = "Taxa de Inadimplência") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = order) +
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color='black'),
    axis.text.y = element_text(size=12,color='black')
  )

# Análise descritiva de auxílios com a variavel resposta ------------------

table(base$var_resposta,base$CAT_var_275)

base$CAT_var_275 <- ifelse(base$CAT_var_275 == "C0", "NA",
                           ifelse(base$CAT_var_275 == "C1", "Sim", "Não"))
cores = c("deepskyblue2","firebrick2")

#gerar função para ajudar nos gráficos
gerar_df <- function(variavel){
  tab <- table(variavel, base$var_resposta)
  taxa_inadimplencia = prop.table(tab, margin = 1)[,2]
  representatividade = prop.table(table(variavel))
  df <- data.frame(Categoria = names(taxa_inadimplencia),
                   Taxa_Inadimplencia = as.numeric(taxa_inadimplencia),
                   Representatividade = representatividade)
  df$Categoria <- factor(df$Categoria, levels = unique(df$Categoria))
  
  return(df)
}

g1 <- ggplot(gerar_df(base$CAT_var_275), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2", width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth = 1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2", alpha = 1, size = 2) +
  labs(x = "Auxílio emergencial", y = "Frequência", 
       title = "Frequência de auxílio\n e Taxa de Inadimplência") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  # Formata o eixo y principal como porcentagem
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores) +  
  theme(
    plot.title = element_text(hjust = 0.5, size = 18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15), 
    panel.border = element_rect(color = "black", fill = NA),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black")
  )

# Análise descritiva de EDS com a variavel resposta ------------------
base$var_339 = ifelse(base$var_339==TRUE,"Sim","Não")

g2 <- ggplot(gerar_df(base$var_339), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.35) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Endereço desfavorecido socialmente", y = "Frequência", 
       title = "Frequência de EDS \n e Taxa de Inadimplência") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) +
  theme_minimal() + scale_fill_manual(values = cores) +  
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color = "black"),
    axis.text.y = element_text(size=12,color = "black")
  )

grid.arrange(g1,g2,ncol=2)


# Análise descritiva de idade com a variavel resposta ------------------

base = base %>% mutate(CAT_var_2 = ifelse(var_2 <= 20, "<= 20",
                                          ifelse(var_2 <= 25, "21-25",
                                                 ifelse(var_2 <= 30,"26-30",
                                                        ifelse(var_2 <= 35,"31-35 ",
                                                               ifelse(var_2 <= 45,"36-45","46 + " ))))))



g3 <- ggplot(gerar_df(base$CAT_var_2), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2", width = 0.8) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Idade", y = "Frequência", 
       title = "Frequência de idade\n e Taxa de Inadimplência") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) +
  theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size = 18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color='black'),
    axis.text.y = element_text(size=12,color = "black")
  )


# Descritiva em relação a renda -------------------------------------------

base$faixas_renda <- cut(base$var_251, 
                         breaks = c(seq(0, 10000, by = 500), Inf), 
                         right = FALSE, 
                         labels = c(paste0(seq(0, 9500, by = 500)+ c(0, rep(1,19)), "-", seq(500, 10000, by = 500)), "10001+"))


g4 <- ggplot(gerar_df(base$faixas_renda), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.8) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Renda", y = "Frequência", 
       title = "Frequência de renda\n e Taxa de Inadimplência") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(angle = 45, hjust = 1,size=12,color='black'),
    axis.text.y = element_text(size=12,color = "black")
  )


grid.arrange(g3,genero,ncol=2)

# Descritiva de Genero 
genero <- ggplot(gerar_df(base$var_1), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.8) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Gênero", y = "Frequência", 
       title = "Frequência de gênero\n e Taxa de Inadimplência") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels =  percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text( hjust = 0.5,size=12,color='black'),
    axis.text.y = element_text(size=12,color = "black")
  )


# Descritiva com relação a variáveis de comportamento ---------------------

base$CAT_var_103 <- as.factor(ifelse(base$var_103  <= quantile(base$var_103, 0.25,na.rm=T), "0-6",
                                     ifelse(base$var_103 <= quantile(base$var_103, 0.50,na.rm=T)  , "7-18", 
                                            ifelse(base$var_103 <= quantile(base$var_103, 0.75,na.rm=T)  ,"19-44", "45+"))))

order <- c("0-6","7-18", "19-44", "45+" )
base$CAT_var_103 <- factor(base$CAT_var_103, levels=order, ordered=TRUE)

base$CAT_var_78 <-  as.factor(ifelse(is.na(base$var_78), "C0",
                                     ifelse(base$var_78  <= quantile(base$var_78, 0.25,na.rm=T), "0-231",
                                            ifelse(base$var_78 <= quantile(base$var_78, 0.50,na.rm=T)  , "232-786", 
                                                   ifelse(base$var_78 <= quantile(base$var_78, 0.75,na.rm=T)  ,"787-2063", "2064+")))))


order <- c("0-231","232-786", "787-2063","2064+"  )
base$CAT_var_78 <- factor(base$CAT_var_78, levels=order, ordered=TRUE)

base$CAT_var_22 <- as.factor(ifelse(is.na(base$var_22), "C0", 
                                    ifelse(base$var_22 == median(base$var_22,na.rm = T),"Não", 'Sim')))


base$CAT_var_57 <- as.factor(ifelse(base$var_57  <= quantile(base$var_57, 0.25,na.rm=T), "0-3",
                                    ifelse(base$var_57 <= quantile(base$var_57, 0.50,na.rm=T)  , "4-7", 
                                           ifelse(base$var_57 <= quantile(base$var_57, 0.75,na.rm=T)  ,"8-13", "14+"))))

order <- c("0-3","4-7", "8-13","14+"  )
base$CAT_var_57 <- factor(base$CAT_var_57, levels=order, ordered=TRUE)


g1 <- ggplot(gerar_df(base$CAT_var_103), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Categoria", y = "Frequência", 
       title = "Quantidade de vencimentos quitados\n pontual nos últimos 2 anos") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14), 
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text( size=12,color='black'),
    axis.text.y = element_text(size=12,color = "black")
  )



g2 <- ggplot(gerar_df(base$CAT_var_78), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Categoria", y = "Frequência", 
       title = "Valor de vencimentos quitados\n antecipados nos últimos 2 anos") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color='black'),
    axis.text.y = element_text(size=12,color='black')
  )


g3 <- ggplot(gerar_df(base$CAT_var_22), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.3) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Categoria", y = "Frequência", 
       title = "Atraso no pagamento \n nos últimos 6 meses") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text( size=12,color='black'),
    axis.text.y = element_text(size=12,color='black')
  )



g4 <- ggplot(gerar_df(base$CAT_var_57), aes(x = Categoria)) +
  geom_bar(aes(y = Representatividade.Freq * 100), stat = "identity", fill = "deepskyblue2",width = 0.5) +
  geom_line(aes(y = Taxa_Inadimplencia * 100, group = 1), color = "firebrick2", linewidth=1) +
  geom_point(aes(y = Taxa_Inadimplencia * 100), color = "firebrick2",size=2) +
  labs(x = "Categoria", y = "Frequência", 
       title = "Total de contratos\n nos últimos 2 anos") +
  scale_y_continuous(
    labels = percent_format(scale = 1),  
    sec.axis = sec_axis(~., name = "Taxa de Inadimplência", labels = percent_format(scale = 1))  # Formata o eixo y secundário como porcentagem
  ) + theme_minimal() + scale_fill_manual(values = cores) +   
  theme(
    plot.title = element_text(hjust = 0.5,size=18),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    panel.border = element_rect(color = "black",fill=NA),
    axis.text.x = element_text(size=12,color='black'),
    axis.text.y = element_text(size=12,color='black')
  )

grid.arrange(g1, g2, g3,g4, ncol = 2)
