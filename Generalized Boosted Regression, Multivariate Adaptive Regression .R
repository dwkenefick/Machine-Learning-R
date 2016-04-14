Monday, april 18th


require(gbm)

spam.gbm = gbm(spam~.,data = spam,cv.folds = 10, n.trees = 2000)

best.iter = gbm.perf(spam.gbm,method = "OOB")

spam.gbm$fit
sign(spam.gbm$fit)
spam.te = read.table(file.choose(),header = TRUE)
pred.spam = sign(predict(spam.gbm,spam.te,n.trees = 2000))



mars = read.table(file.choose(),header = TRUE)
mars.tr = mars[1:700,1:11]
mars.te = mars[701:1000,1:11]

mars2 = earth(y~.,degree = 2,data = mars.tr)
mars.te$pred = predict(mars2,mars.te)
plot(mars.te$pred~mars.te$y)

plotmo(mars2)
