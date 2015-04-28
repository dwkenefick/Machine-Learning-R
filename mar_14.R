dep = read.delim(file.choose(),header = TRUE)
tr = dep[dep$Set == "Train",]
te = dep[dep$Set == "Test",]

#get rid of first and last collumns
tr = tr[,c(2:15)]
te = te[,c(2:15)]
tree = rpart(DRP~.,data = dep, cp = 0)
plot (tree, uniform = TRUE)
text(tree,cex=.7,digits = 2)
printcp(tree)

#so we should do 

tree1 = prune(tree,cp = .0123) 

plot (tree1 , uniform = TRUE)
text(tree1,cex=.7,digits = 2)

#best way to get conf matrix,
xtabs(~dep$DRP+ round(predict(tree1)) )



tree1 = prune(tree,cp = .0133) 

plot (tree1 , uniform = TRUE)
text(tree1,cex=.7,digits = 2)

#best way to get conf matrix,
xtabs(~dep$DRP+ round(predict(tree1)) )


####etc. for different values of CP. now, missing values.

head(dep)
complete.cases(dep[,c(5:8)])
dep$cc = complete.cases(dep[,c(5:8)])

tree2 = rpart(DRP~.,data = dep, cp = 0)

plot (tree2 , uniform = TRUE)
text(tree2,cex=.7,digits = 2)

