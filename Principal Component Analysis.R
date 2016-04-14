#notes for mach 16th
#singular values and eigenvales, singular values are sqr roots
#use svd do look at principle components, which give you orthoganal vectors. 

#good at getting a "feel" for a bunch of the most important variables.

#the hope is that principal componet regression will help predict bodyfat, since it explains so much of the variation in the x's

#these coefficients will be very stable, since the z's, or principle components, will be orthoginal

bf = read.table(file.choose(),header = TRUE)

#some stuff i missed....

identify(Z[,2]~Z[,1])

#factor analysis also works, but now we go non-linear. 
#what are addative models? y = b0 + b1x1+b2x2...
#instead of a line, some smooth function
#y = f(x1,...xp) <- goal of data mining.

#ols assumes linear in each x, but we can build up like a taylor series (but bad at the edges, and needs a big data set)

with(bodyfat,scatter.smooth(weight~age))
require(gam) #find gam
g = gam(Weight~s(Age)+s(Height),data = bf)

#weight is a smooth function of age, and a smoth function of height

# Y = s(z1) + s(z2)   , z1, z2 are some linear combonations of xs
# create a function, .1(.1(x1-x3)^3 + .2(x1+x2)^2 + sin(x2/x2))
#from -3 to 3

pprt = read.table(file.choose(),header = TRUE)
plot(pprt)
ppr1 = ppr(y~.,nterms=3,data = pprt)
summary(ppr1)
plot (ppr1, ask = TRUE)


pprbf = ppr(Pct.BF~., data = bf,nterms = 3)

plot(pprbf, ask = T)
ypred = predict(pprbf)
plot(ypred,bf[,1])

#might not predict better with more terms. only way to tell is to try a whole bunch.

#smooth functions in ppr. CS people decided to reign them in a little with sigmoids , which look like CDFs , like logit and tanh^-1 . "sigmoidal activation functions" 

require(nnet)
nnet.bf = nnet(Pct.BF~.,data = bf,size =3,linout = TRUE)
