n_features <- length(setdiff(names(validacao), "var_resposta"))
hyper_grid <- expand.grid(
  mtry = c(3,5,10,15,20,25,30,40,50), 
  prediction.error = NA,
  KS_treino = NA,
  KS_validacao = NA,
  KS_teste = NA,
  KS_total = NA,
  auc_roc_treino = NA,
  auc_roc_validacao = NA,
  auc_roc_teste = NA, 
  auc_roc_total = NA, 
  gini_treino = NA,
  gini_validacao = NA,
  gini_teste = NA,
  gini_total = NA
  
)


for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = var_resposta ~ ., 
    data            = treino, 
    mtry            = hyper_grid$mtry[i],
    importance = "impurity",
    replace=T,
    splitrule = "gini",
    probability = TRUE,
    verbose  = TRUE,
    seed   = 100,
    classification=T
  )
  
  hyper_grid$prediction.error[i] <- fit$prediction.error
  
  ## com a amostra de validação
  previsoes <- predict(fit,data=validacao) 
  score = previsoes$predictions[,1]*1000
  validacao$score = score
  ks_validacao = ks.test(validacao[validacao$var_resposta==0,]$score, validacao[validacao$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(validacao$var_resposta, previsoes$predictions[,1])
  auc_roc_validacao <- auc(roc_obj)
  gini_validacao <- 2 * auc_roc_validacao - 1 
  
  
  previsoes <- predict(fit,data=treino) 
  score = previsoes$predictions[,1]*1000
  treino$score = score
  ks_treino = ks.test(treino[treino$var_resposta==0,]$score, treino[treino$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(treino$var_resposta, previsoes$predictions[,1])
  auc_roc_treino <- auc(roc_obj)
  gini_treino <- 2 * auc_roc_treino - 1 
  
  
  previsoes <- predict(fit,data=teste) 
  score = previsoes$predictions[,1]*1000
  teste$score = score
  ks_teste = ks.test(teste[teste$var_resposta==0,]$score, teste[teste$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(teste$var_resposta, previsoes$predictions[,1])
  auc_roc_teste <- auc(roc_obj)
  gini_teste <- 2 * auc_roc_teste - 1 
  
  
  
  previsoes <- predict(fit,data=base1) 
  score = previsoes$predictions[,1]*1000
  base1$score = score
  ks_total = ks.test(base1[base1$var_resposta==0,]$score, base1[base1$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(base1$var_resposta, previsoes$predictions[,1])
  auc_roc_total <- auc(roc_obj)
  gini_total <- 2 * auc_roc_total - 1 
  
  
  hyper_grid$KS_validacao[i] <- ks_validacao
  hyper_grid$KS_treino[i] <- ks_treino
  hyper_grid$KS_teste[i] <- ks_teste
  hyper_grid$KS_total[i] <- ks_total
  hyper_grid$auc_roc_treino[i] <- auc_roc_treino
  hyper_grid$auc_roc_validacao[i] <- auc_roc_validacao
  hyper_grid$auc_roc_teste[i] <- auc_roc_teste
  hyper_grid$auc_roc_total[i] <- auc_roc_total
  hyper_grid$gini_validacao[i] <- gini_validacao
  hyper_grid$gini_treino[i] <- gini_treino
  hyper_grid$gini_teste[i] <- gini_teste
  hyper_grid$gini_total[i] <- gini_total
  
}