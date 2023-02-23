
validate_Rush = function(formula, engine, data, knn_k,
                         rf_ntree) {
  library(dplyr)
  # fix formula
  formula = as.formula(formula)

  # calc_rmse function
  source("R/calc-rmse.R")

  # empty vectors
  rmse = rep(NA_real_, 10)
  act = NULL
  pred = NULL

  # create random folds
  data$r = sample(nrow(data))
  data = dplyr::arrange(data, r)
  x_row = nrow(data) %% 10
  data$fold = c(rep(1:10, (nrow(data)-x_row)/10), 1:x_row)

  # for loop
  for(i in 1:10) {
    # assign trn and tst for specific fold
    trn = data %>% filter(fold != i) %>% select(-fold, -r)
    tst = data %>% filter(fold == i) %>% select(-fold, -r)

    if(engine == "lm") {
      m = lm(formula, trn)

    } else if(engine == "randomForest") {
      m = randomForest::randomForest(formula, trn)

    } else if(engine == "knn") {
      m = caret::knnreg(formula, trn)

    } else if(engine == "bagging") {
      m = ipred::bagging(formula, trn, nbags = 10000,
                         control = rpart::rpart.control(minsplit = 2, cp = 0))

    } else if(engine == "gbm") {
      m = gbm::gbm(formula, trn,
                   distribution = "gaussian",
                   n.trees = 10000)
    }

    tst$pred = predict(m, tst)

    act = c(act, tst$EPA)
    pred = c(pred, tst$pred)

    rmse[i] = calc_rmse(tst$EPA, tst$pred)

  }

  # output list
  list(rmse = mean(rmse),
       rmse.sd = sd(rmse),
       rmse.vec = rmse,
       engine = engine)

}





