iterations = 33
treeseq = seq(from = 50, to = 1000, by = 50)
cormatrix = matrix(nrow = length(treeseq),ncol =57)
row = 1
for(t in treeseq){
      cat(" out: ", t)
     
      for( v in 1:57){
           cat(" in: ",v)
           corr = array(dim = c(iterations,1))         
          
       for( i in 1:iterations){
                 #seleect the training subsample
                 train = array(0,dim=c(len,1))
                 train[sample(len,size=sample.size)] = 1
          
                 #get the testing subsample observed values
                 test = data[train == 0,1]
 
                 forest = randomForest( y = data[train==1,1], x = data[train==1,-1] , ntree = t , mtry = v+2)
          
                 pred = predict(forest, new = data[train==0,-1] )
                 corr[i] = cor(pred>.5 ,data[train==0,1] )^2
           }
          
           cormatrix[row,v] = mean (corr)
           cat ("corr: ", cormatrix[row,v])
          
 
          
      }
      row = row +1
}
 

Code to determine the number of trees in a bootstrap forest:
(iterations is on the outside, so I can run it for as much time as I have. This got up to 105 iterations, but the data was still noisy)
 
treeseq = seq(from = 50, to = 1000, by = 50)
cormatrix = array(0, dim = c(iterations,length(treeseq),ncol =5))
for(i in 1:iterations){
      cat(" iterations: ", i,",")
      row = 1
     
      for(t in treeseq ){                   
          
       for( v in 1:5){
                 #seleect the training subsample
                 train = array(0,dim=c(len,1))
                 train[sample(len,size=sample.size)] = 1
                 #get the testing subsample observed values
                 test = data[train == 0,1]
                 forest = randomForest( y = data[train==1,1], x = data[train==1,-1] , ntree = t , mtry = v+2)
                 pred = predict(forest, new = data[train==0,-1] )
                 cormatrix[i,row,v] = cor(pred>.5 ,data[train==0,1] )^2
           }
           row = row +1                     
      }
}
 
 
 
Code to determine the number of nodes in the second layer for a neural net:
 
iterations = 2
layercor = array(dim = c(maxnodes,iterations))
for(i in 1:iterations){
      cat(" iteration: ",i)
      for( j in 1:maxnodes){
           cat(" n",j)     
           while(invalid(layercor[j,i])){
                 #seleect the training subsample
                 train = array(0,dim=c(len,1))
                 train[sample(len,size=sample.size)] = 1
          
                 #get the testing subsample observed values
                 test = data[train == 0,1]
                 capture.output(net = nnet(y = data[train==1,1], x = data[train==1,-1],  size = j, MaxNWts = 10000 ))
                 pred =predict( net, new=data[train==0,-1])
                 layercor[j,i] = cor(pred>.5,test)^2
           }    
      }    
}
