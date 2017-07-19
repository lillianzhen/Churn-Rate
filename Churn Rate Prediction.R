
##Cleaning the Data
churn = read.csv(file.choose(),header=T, stringsAsFactors=T, encoding="UTF-8")
str(churn)
churn$college <- factor(churn$college,levels(churn$college)[c(2,1)])
churn$rep_sat <- factor(churn$rep_sat,levels(churn$rep_sat)[c(5,3,1,2,4)])
churn$rep_usage <- factor(churn$rep_usage,levels(churn$rep_usage)[c(5,3,1,2,4)])
churn$rep_change <- factor(churn$rep_change,levels(churn$rep_change)[c(3,4,2,5,1)])
save(churn,file=file.choose())

##set seeds to compare the result
set.seed(3478)
train = sample(1:nrow(churn),nrow(churn)*0.667)
churn.train = churn[train,]  
churn.test = churn[-train,]   
library(rpart)
fit = rpart(leave ~ ., 
            data=churn.train, method="class",
            control=rpart.control(xval=0, minsplit=100))
fit
plot(fit, uniform=TRUE, branch=.5, 
     main="Classification Tree for Marketing", margin=0.1)
text(fit,  use.n=TRUE, pretty=T,cex=.5)
churn.pred = predict(fit, churn.test, type="class")
churn.actual = churn.test[,"leave"]
# now make a contingency table of actual versus predicted
# this is called the confusion matrix
# for RESUBSTITUTION
cm = table(churn.actual, churn.pred)
cm


library(rpart)
# ======Q2======
library(rpart)
fit = rpart(leave ~ ., 
            data=churn.train, method="class",
            control=rpart.control(xval=10, minsplit=2,cp=0))
fit
plot(fit, # the tree to be plotted
     uniform=T, # uniform spacing of nodes
     branch=0.5, # bent branches
     compress=T, # take less space
     main="A Complex Tree", #title
     margin=0.1) #no extra space
text(fit,  # tree to be embellished
     splits=F, # do not detail split criteria
     all=F, # label only terminal nodes labeled
     use.n=T, # label leaves with observations
     pretty=F, # keep it simple
     cex=0.6) # compress fonts to 60%
churn.pred = predict(fit, churn.test, type="class")
churn.actual = churn.test[,"leave"]

cm = table(churn.actual, churn.pred)
FPR=cm[3]/(cm[1]+cm[3])
FNR=cm[2]/(cm[2]+cm[4])
accuracy = function(cm){  # input confusion matrix
    return(sum(diag(cm))/sum(cm))  # accuracy
}
acm=accuracy(cm)
# ======Q3======
plotcp(fit, 
       upper="size")  
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
fit.small = rpart(leave ~ ., 
                   data=churn.train, method="class",
                   control=rpart.control(xval=10,cp=0.002298851))
plot(fit.small, # the tree to be plotted
     uniform=T, # uniform spacing of nodes
     branch=0.5, # bent branches
     compress=T, # take less space
     main="Pruned Tree", #title
     margin=0.1) #no extra space
text(fit.small,  # tree to be embellished
     splits=T, # do not detail split criteria
     all=F, # label only terminal nodes labeled
     use.n=T, # label leaves with observations
     pretty=F, # keep it simple
     cex=0.7) # compress fonts to 70%
churn.pred = predict(fit.small, churn.test, type="class")
churn.actual = churn.test[,"leave"]
cm.small = table(churn.actual, churn.pred)
FPR.small=cm.small[3]/(cm.small[1]+cm.small[3])
FNR.small=cm.small[2]/(cm.small[2]+cm.small[4])
acm.small=accuracy(cm.small)
# ======Q4======
fit.post = prune.rpart(fit, cp=0.002298851)
library(ROCR)
leave.pred.train = predict(fit.post, churn.train, type="prob")

leave.pred.trainall = cbind(churn.train, leave.pred.train)

leave.pred.score = # first of two steps - compute the score
    prediction(leave.pred.train[,2],  # the predicted P[Yes]
               churn.train$leave) # the actual class
# next step is to compute the performance object
leave.pred.perf = performance(leave.pred.score, "tpr", "fpr")

plot(leave.pred.perf, 
     colorize=T, # colorize to show cutoff values
     lwd=4) # make the line 4 times thicker than default
abline(0,1)  # draw a line with intercept=0, slope = 1
abline(h=1) #draw a horizontal line at y = 1
abline(v=0) #draw a vertical line at x = 0

leave.cost = performance(leave.pred.score, measure="cost", cost.fn=196000000, cost.fp=51000000)
plot(leave.cost)

cutoffs = data.frame(cut=leave.cost@"x.values"[[1]], cost=leave.cost@"y.values"[[1]])
best.index = which.min(cutoffs$cost)
cutoffs[best.index,]

leave.pred.test = predict(fit.post, churn.test, type="prob")
head(leave.pred.train)

leave.pred.test.cutoff = 
    ifelse(leave.pred.test[,2] < cutoffs[best.index,]$cut,"NO","YES")
cm.post = table(churn.test$leave,leave.pred.test.cutoff)
FPR.post=cm.post[3]/(cm.post[1]+cm.post[3])
FNR.post=cm.post[2]/(cm.post[2]+cm.post[4])
acm.post=accuracy(cm.post)
# ======Q5======
Profit=196000000*(1-FNR)+51000000*(1-FPR)+294000000
Profit.small=196000000*(1-FNR.small)+51000000*(1-FPR.small)+294000000
Profit.post=196000000*(1-FNR.post)+51000000*(1-FPR.post)+294000000
result=data.frame()
result[1,1]=FPR
result[1,2]=FPR.small
result[1,3]=FPR.post
result[2,1]=FNR
result[2,2]=FNR.small
result[2,3]=FNR.post
result[3,1]=acm
result[3,2]=acm.small
result[3,3]=acm.post
result[4,1]=Profit
result[4,2]=Profit.small
result[4,3]=Profit.post
colnames(result)=c('Big Tree','Pruned Tree','Best Threshold Pruned Tree')
rownames(result)=c('FPR','FNR','Accuracy','Expected Value')
