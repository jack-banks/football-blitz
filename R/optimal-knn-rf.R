
# optimal knn and rf

knn_acc = rep(NA_real_, 40)
knn_acc_scaled = rep(NA_real_, 40)
knn_brier = rep(NA_real_, 40)
knn_brier_scaled = rep(NA_real_, 40)

for(i in 1:40)  {
  m  = validate_Pressure(fm, "knn", data = data, knn_k = i, scaled = FALSE)
  knn_acc[i] = m$acc
  knn_brier[i] = m$brier
  m  = validate_Pressure(fm, "knn", data = data, knn_k = i, scaled = TRUE)
  knn_acc_scaled[i] = m$acc
  knn_brier_scaled[i] = m$brier

  print(paste0("finished k = ",as.character(i)))
}
names(knn_brier) = as.character(1:40)
names(knn_brier_scaled) = as.character(1:40)
names(knn_acc) = as.character(1:40)
names(knn_acc_scaled) = as.character(1:40)
knn_brier[which.min(knn_brier)]
knn_brier_scaled[which.min(knn_brier_scaled)]
knn_acc[which.max(knn_acc)]
knn_acc_scaled[which.max(knn_acc_scaled)]

# optimal trees in random forest
set.seed(2023)
rf_brier = rep(NA_real_, 10)
rf_brier_scaled = rep(NA_real_, 10)
rf_acc = rep(NA_real_, 10)
rf_acc_scaled = rep(NA_real_, 10)

for(i in 1:10)  {
  m  = validate_Pressure(fm, "randomForest", data = data, rf_ntree = i*50, scaled = FALSE)
  rf_brier[i] = m$brier
  rf_acc[i] = m$acc
  m  = validate_Pressure(fm, "randomForest", data = data, rf_ntree = i*50, scaled = TRUE)
  rf_brier_scaled[i] = m$brier
  rf_acc_scaled[i] = m$acc

  print(paste0("finished ntree = ",as.character(i)))
}
names(rf_brier) = as.character(1:10)
names(rf_brier_scaled) = as.character(1:10)
names(rf_acc) = as.character(1:10)
names(rf_acc_scaled) = as.character(1:10)
rf_brier[which.min(rf_brier)]
rf_brier_scaled[which.min(rf_brier_scaled)]
rf_acc[which.max(rf_acc)]
rf_acc_scaled[which.max(rf_acc_scaled)]
