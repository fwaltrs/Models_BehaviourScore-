n_features <- length(setdiff(names(validacao), "var_resposta"))

hyper_grid <- expand.grid(
  mtry = c(5,10,15,20),  # Sugestão com base no número de features
  B = c(250, 500),      # Número de árvores
  max.depth = c(3, 6, 10),
  KS_validacao = NA,
  KS_teste = NA,
  auc_roc_validacao = NA,
  auc_roc_teste = NA,
  gini_validacao = NA
)

for(i in seq_len(nrow(hyper_grid))) {
  fit <- ranger(
    formula         = var_resposta ~ ., 
    data            = treino, 
    mtry            = hyper_grid$mtry[i],
    num.trees       = hyper_grid$B[i],
    max.depth       = hyper_grid$max.depth[i],
    importance = "impurity",
    replace=T,
    splitrule = "gini",
    probability = TRUE,
    verbose  = TRUE,
    seed   = 100,
    classification=T
  )
   
  ## com a amostra de validação
  previsoes <- predict(fit,data=validacao) 
  score = previsoes$predictions[,1]*1000
  validacao$score = score
  ks_validacao = ks.test(validacao[validacao$var_resposta==0,]$score, validacao[validacao$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(validacao$var_resposta, previsoes$predictions[,1])
  auc_roc_validacao <- auc(roc_obj)
  gini_validacao <- 2 * auc_roc_validacao - 1 
  
  previsoes <- predict(fit,data=teste) 
  score = previsoes$predictions[,1]*1000
  teste$score = score
  ks_teste = ks.test(teste[teste$var_resposta==0,]$score, teste[teste$var_resposta==1,]$score)$statistic   
  roc_obj <- roc(teste$var_resposta, previsoes$predictions[,1])
  auc_roc_teste <- auc(roc_obj)
    
  hyper_grid$KS_validacao[i] <- ks_validacao
  hyper_grid$KS_teste[i] <- ks_teste
  
  hyper_grid$auc_roc_validacao[i] <- auc_roc_validacao
  hyper_grid$auc_roc_teste[i] <- auc_roc_teste
  
  hyper_grid$gini_validacao[i] <- gini_validacao
}
