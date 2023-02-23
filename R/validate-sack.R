
# validate function
validate_Sack = function(formula, engine, data, response,
                             scaled = FALSE,
                             knn_k = 7, rf_ntree = 500) {
  # change 'formula' from string to formula
  formula = as.formula(formula)
  # create empty vector for the accuracy of each fold
  accuracy = rep(NA_real_, 10)
  # empty vectors for confusion table
  pred = NULL
  act = NULL
  prob = NULL
  # specify usage of scaled variables, if applicable
  if(scaled == TRUE) {
    data = data_scaled
  }
  # k-fold cross-validation
  # create random folds
  data$r = sample(nrow(data))
  data = dplyr::arrange(data, r)
  x_row = nrow(data) %% 10
  data$fold = c(rep(1:10, (nrow(data)-x_row)/10), 1:x_row)

  for(i in 1:10) {
    # assign trn and tst for specific fold
    trn = data %>% filter(fold != i) %>% select(-fold, -r)
    tst = data %>% filter(fold == i) %>% select(-fold, r)

    # fit model to proper engine
    # generalized linear model
    if(engine == "glm") {
      m = stats::glm(formula, trn, family = "binomial")
      tst$pred = round(predict(m, tst, type = "response"),0)
      tst$prob = predict(m, tst, type = "response")
    } # linear discriminant analysis
    else if(engine == "lda") {
      m = MASS::lda(formula, trn)
      tst$pred = predict(m, tst)$class
      tst$prob = predict(m, tst)$posterior[, 2]
    } # quadratic discriminant analysis
    else if(engine == "qda") {
      m = MASS::qda(formula, trn)
      tst$pred = predict(m, tst)$class
      tst$prob = predict(m, tst)$posterior[, 2]
    } # naive bayes
    else if(engine == "naiveBayes") {
      m = e1071::naiveBayes(formula, trn)
      tst$pred = predict(m, tst, type = "class")
      tst$prob = predict(m, tst, type = "raw")
    } # k-nearest neighbors
    else if(engine == "knn") {
      m = caret::knn3(formula, trn, k = knn_k)
      mat = predict(m, tst)
      tst$pred = as.numeric(mat[,1] < mat[,2])
      tst$prob = predict(m, tst)[, 2]
    } # random forest
    else if(engine == "randomForest") {
      m = randomForest::randomForest(formula, trn, ntree = rf_ntree)
      tst$pred = round(predict(m, tst),0)
      tst$prob = predict(m, tst)
    }
    # add fold accuracy to the vector
    accuracy[i] = mean(tst$pred == tst$Sack)

    pred = c(pred, tst$pred)
    act = c(act, tst$Sack)
    prob = c(prob, tst$prob)

    auc = pROC::auc(act, pred)

    brier = DescTools::BrierScore(act, prob)
  }

  # confusion table
  ct = table(pred, act)

  prob_data = data.frame(prob, act)

  tester = {
    prob_data %>%
      mutate(prob = round(prob/0.02)*0.02) %>%
      group_by(prob) %>%
      summarise(n = n(), act = mean(act)) %>%
      mutate(dif = (act - prob)^2 * n) %>%
      arrange(prob)
  }
  rmse = sqrt(sum(tester$dif) / sum(tester$n))

  # create list to return
  ret_list = list(engine = engine,
                  acc = mean(accuracy),
                  sd = sd(accuracy),
                  auc = auc,
                  brier = brier,
                  rmse = rmse,
                  post_prob = prob,
                  ct = ct)

  return(ret_list)
}
