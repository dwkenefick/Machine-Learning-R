#############
### SETUP ###
#############

# Maximium number of Nodes
	MAXNODES = 50

# Total number of iterations
	ITERATIONS = 1

# Sample Fraction
	SAMPLE_FRACTION = .9

# Required Packages
require(nnet)
require(randomForest)

# import the data
	#data path
	data_path <- ''


	# pull in the data
	data = read.csv(data_path,header=TRUE)

################
### ANALYSIS ###
################



# save original data, subset the remainder
	data_orig = data
	# data = data[data$month !=6,]
	data = data[!(data$month ==6 & data$spending_tm1==0 & data$spending_tm2==0 & data$spending_tm3==0 & data$spending_tm4==0 & data$spending_tm5==0) ,]

# convert to factor
#data$present_tm0 <- as.factor(data$present_tm0)

# preliminary looks
	# find the length of the total data
	len = nrow(data)

	# training sample size
	sample.size = len*SAMPLE_FRACTION

	# column number of the present outcome
	present_column = which( colnames(data)== "present_tm0" )

	#column number of the spendig outcome
	spending_column = which( colnames(data)=="spending_tm0" )

###########################
### presence Prediction ###
###########################

treeseq = seq(from = 200, to = 400, by = 50)
cormatrix2_1 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))
cormatrix2_2 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))
cormatrix2_3 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))

for(i in 1:ITERATIONS ){
      cat(" iterations: ", i,",\n")
      row = 1
     
      for(t in treeseq ){                   
      cat("Trees:  ",t,"\n")

       for( v in 1:4){
			cat("sampling:  ",v+4,"\n")

                 #select the training subsample
                 train = array(0,dim=c(len,1))
                 train[sample(len,size=sample.size)] = 1

                 #get the testing subsample observed values
                 test = data[train == 0,1]
		     subset_all = data[train == 1,]
		     subset_train = subset_all[subset_all$month <= 5,]
		     subset_test = subset_all[subset_all$month == 6,]

                 forest = randomForest( y = subset_train[subset_train$bus_num_is1 == 1,c(present_column)], x = subset_train[subset_train$bus_num_is1 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is1 == 1,c(-present_column,-spending_column)] )
                 cormatrix2_1[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is1 == 1 ,c(present_column)] )

                 forest = randomForest( y = subset_train[subset_train$bus_num_is2 == 1,c(present_column)], x = subset_train[subset_train$bus_num_is2 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is2 == 1,c(-present_column,-spending_column)] )
                 cormatrix2_2[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is2 == 1 ,c(present_column)] )

                 forest = randomForest( y = subset_train[subset_train$bus_num_is3 == 1,c(present_column)], x = subset_train[subset_train$bus_num_is3 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is3 == 1,c(-present_column,-spending_column)] )
                 cormatrix2_3[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is3 == 1 ,c(present_column)] )
           }
           row = row +1                     
      }
}
write.table(cormatrix2_1, file = '')
write.table(cormatrix2_2, file = '')
write.table(cormatrix2_3, file = '')


### Amount Prediction
treeseq = seq(from = 200, to = 400, by = 50)
cormatrix1_1 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))
cormatrix1_2 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))
cormatrix1_3 = array(0, dim = c(ITERATIONS ,length(treeseq),ncol =4))

for(i in 1:ITERATIONS ){
      cat(" iterations: ", i,",\n")
      row = 1
     
      for(t in treeseq ){                   
      cat("Trees:  ",t,"\n")

       for( v in 1:4){
			cat("sampling:  ",v+4,"\n")

                 #select the training subsample
                 train = array(0,dim=c(len,1))
                 train[sample(len,size=sample.size)] = 1

                 #get the testing subsample observed values
                 test = data[train == 0,1]
		     subset_all = data[train == 1,]
		     subset_train = subset_all[subset_all$month <= 5,]
		     subset_test = subset_all[subset_all$month == 6,]

                 forest = randomForest( y = subset_train[subset_train$bus_num_is1 == 1,c(spending_column)], x = subset_train[subset_train$bus_num_is1 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is1 == 1,c(-present_column,-spending_column)] )
                 cormatrix1_1[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is1 == 1 ,c(spending_column)] )

                 forest = randomForest( y = subset_train[subset_train$bus_num_is2 == 1,c(spending_column)], x = subset_train[subset_train$bus_num_is2 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is2 == 1,c(-present_column,-spending_column)] )
                 cormatrix1_2[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is2 == 1 ,c(spending_column)] )

                 forest = randomForest( y = subset_train[subset_train$bus_num_is3 == 1,c(spending_column)], x = subset_train[subset_train$bus_num_is3 == 1,c(-present_column,-spending_column)] , ntree = t , mtry = v+5)
                 pred = predict(forest, new = subset_test[subset_test$bus_num_is3 == 1,c(-present_column,-spending_column)] )
                 cormatrix1_3[i,row,v] = cor(pred ,subset_test[subset_test$bus_num_is3 == 1 ,c(spending_column)] )

           }
           row = row +1                     
      }
}

write.table(cormatrix1_1, file = '')
write.table(cormatrix1_2, file = '')
write.table(cormatrix1_3, file = '')

# final prob predictions
final_trees = 400
final_sample = 7

forest_prob1 = randomForest( y = data[data$month < 6 & data$bus_num_is1 == 1 ,c(present_column)] , x = data[data$month < 6 & data$bus_num_is1 == 1,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_prob1, new = data[data$month==6 & data$bus_num_is1 == 1,c(-present_column,-spending_column)])
prob1 = cor(pred_prob_final ,data[data$month==6 & data$bus_num_is1 == 1,c(present_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/prob1.jpg')
varImpPlot(forest_prob1, main = "Variable Importance:  Sprout\n Presence")
dev.off()


forest_prob2 = randomForest( y = data[data$month < 6 & data$bus_num_is2 == 1 ,c(present_column)], x = data[data$month < 6 & data$bus_num_is2 == 1,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_prob2, new = data[data$month==6 & data$bus_num_is2 == 1,c(-present_column,-spending_column)])
prob2=cor(pred_prob_final ,data[data$month==6 & data$bus_num_is2 == 1,c(present_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/prob2.jpg')
varImpPlot(forest_prob2, main = "Variable Importance:  Trader Joe's\n Presence")
dev.off()


forest_prob3 = randomForest( y = data[data$month < 6 & data$bus_num_is3 == 1 ,c(present_column)], x = data[data$month < 6 & data$bus_num_is3 == 1,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_prob3, new = data[data$month==6 & data$bus_num_is3 == 1,c(-present_column,-spending_column)])
prob3=cor(pred_prob_final ,data[data$month==6 & data$bus_num_is3 == 1,c(present_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/prob3.jpg')
varImpPlot(forest_prob3, main = "Variable Importance:  Whole Foods\n Presence")
dev.off()


# final amt predictions
final_trees = 400
final_sample = 7
forest_amt1 = randomForest( y = data[data$month < 6 & data$bus_num_is1 == 1 ,c(spending_column)], x = data[data$month < 6 & data$bus_num_is1 == 1 ,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_amt1, new = data[data$month==6 & data$bus_num_is1 == 1,c(-present_column,-spending_column)])
amt1=cor(pred_prob_final ,data[data$month==6 & data$bus_num_is1 == 1,c(spending_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/amt1.jpg')
varImpPlot(forest_amt1, main = "Variable Importance:  Sprout\n Spending")
dev.off()


forest_amt2 = randomForest( y = data[data$month < 6 & data$bus_num_is2 == 1 ,c(spending_column)], x = data[data$month < 6 & data$bus_num_is2 == 1,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_amt2, new = data[data$month==6 & data$bus_num_is2 == 1,c(-present_column,-spending_column)])
amt2=cor(pred_prob_final ,data[data$month==6 & data$bus_num_is2 == 1,c(spending_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/amt2.jpg')
varImpPlot(forest_amt2, main = "Variable Importance:  Trader Joe's\n Spending")
dev.off()


forest_amt3 = randomForest( y = data[data$month < 6 & data$bus_num_is3 == 1 ,c(spending_column)], x = data[data$month < 6 & data$bus_num_is3 == 1,c(-present_column,-spending_column)] , ntree = final_trees, mtry = final_sample)
pred_prob_final = predict(forest_amt3, new = data[data$month==6 & data$bus_num_is3 == 1,c(-present_column,-spending_column)])
amt3=cor(pred_prob_final ,data[data$month==6 & data$bus_num_is3 == 1,c(spending_column)] )

jpeg('C:/Users/Dkenefick/Desktop/Personal/Case Study/R/Output/Node Purity Figures/amt3.jpg')
varImpPlot(forest_amt2, main = "Variable Importance:  Whole Foods\n Spending")
dev.off()

# print results
results_corr = array(0, dim = c(3,2))
results_corr[1,1]=prob1
results_corr[2,1]=prob2
results_corr[3,1]=prob3
results_corr[1,2]=amt1
results_corr[2,2]=amt2
results_corr[3,2]=amt3
colnames(results_corr) <- c("Retention","Spend")
rownames(results_corr) <- c("Sprouts","Trader Joes","Whole Foods")

write.table(results_corr, file = '')


