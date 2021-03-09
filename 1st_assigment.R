library(dplyr)
library(ggplot2)
library(caTools)
library(corrgram) 
library(ISLR)
library(MASS) #αυτη η βιβλιοθηκη εμπεριεχεται στο πακετο της dplyr  
library(psych)
library(DMwR)
library(car)
library(DAAG)
library(leaps)
library(glmnet)

#ανοιγω τον φακελο που θελω για δεδομενα
data1 = read.delim(file.choose(),header = T)
names(data1)
attach(data1)

#ελεγχω αν εχω καποια εγγρραφη NA(not available )
any(is.na(data1))
#το αποτελεσμα ειναι false , αρα δεν εχω καποια τετοια εγγραφη μπορω να προχωρησω

#ελεγχω την συσχετιση των δεδομενων μου ως προς την εξαρτημενη μεταβλητη
cor(data1,data1$effort)

corrgram(data1, lower.panel=panel.shade, upper.panel=panel.cor)
#2 διαφορετικα ειδη πινακων για την συσχετιση

#βλεπω το ιστογραμμα ως προς την εξαρτημενη μεταβλητη και παρατηρω αν εχει κανονικη κατανομη
hist(data1$effort)

plot(effort ~ AdjustedFunctionPoints  , data=data1)
plot(effort ~ Inputcount  , data=data1)
plot(effort ~ Outputcount  , data=data1)
plot(effort ~ Enquirycount  , data=data1)
plot(effort ~ Filecount  , data=data1)
plot(effort ~ Interfacecount  , data=data1)
plot(effort ~ Addedcount  , data=data1)
plot(effort ~ Changedcount  , data=data1)
plot(effort ~ Deletedcount  , data=data1)

#1η μεθοδος multiple linear regression

#εντολη για πολλαπλη γραμμικη παλινδρομηση και αναθεση στην μεταβλητη effort.lm
effort.lm = lm(effort ~ .,data1 )

#εντολη για εμφανιση των αποτελεσματων της πολλαπλης γραμμικης παλινδρομησης
summary(effort.lm)

#διαστηματα εμπιστοσυνης των μεταβλητων 
confint(effort.lm)

#δειχνει τους συντελεστες
coef(effort.lm)

#κοιταω να δω αν θα πρεπει να βγαλω καποια στήλη, απο το αποτέλεσμα βλεπω οτι μου προτείνει να βγει η deletedcount,
regfit.full= regsubsets(effort~.,data1)
summary(regfit.full)

#τσεκαρω την πολυσυγγραμικοτητα με το alias and vif, και παρατηρω πολυσυγγραμικοτητα στο deletedcount ,για αυτό τον λογο την αφαιρω
vif(effort.lm)
alias(effort.lm)

#διαγραφη μιας στηλης απο την ΒΔ μας, στο παραδειγμα μας λεγεται η ΒΔ = data1 Και η στηλη = Deletedcount, την διαγραφω γιατί είναι θορυβος στην ΒΔ και δεν χρειαζεται
data1$Deletedcount = NULL
# γιατι πλεον εχω αφαιρεσει την deletedcount
effort.lm = update(effort.lm,  ~.-Deletedcount )

#ελεγχος για Ομοσκεδαστικότητα (Homoscedasticity)
par(mfrow=c(2,2)) #εντολη για να δειξει 2 * 2 =4 γραφηματα, ,στην οθονη γραφηματων
plot(effort.lm)
par(mfrow=c(1,1)) # εντολη για να δειξει 1 γραφημα ,στην οθονη γραφηματων

#θετω για χωρισμα το seed =420
set.seed(420)

#χωριζω τα δεδομενα μου σε 75% train data & 25% test data
smp_size <- floor(0.75 * nrow(data1))
train_ind <- sample(seq_len(nrow(data1)), size = smp_size)
train <- data1[train_ind, ]
test <- data1[-train_ind, ]


#κανει προβλεψη και αναθεση σε μεταβλητη p
p = predict(effort.lm,test)
summary(p)

#χτίζω το μοντελο μου ,περνωντας τα test data σε γραμμικη παλινδρομηση 
lm.train = lm(effort ~ .,train)

#mse
summary.data1 = summary(effort.lm)
mse.data1 = mean(summary.data1$residuals^2)
summary.train = summary(lm.train) 
mse.train = mean(summary.train$residuals^2)
summary.test = summary(p)
mse.test = mean(summary.test^2)


#συγκρινω τα mse απο τα δεδομενα μου, βλεπω οτι ειναι κοντα τα mse.data1 με το mse.train αλλα το mse.test των προβλέψεων έχουν μικρότερο αριθμό 
mse.data1
mse.train
mse.test

#βρίσκω το mse στις προβλεψεις μου με ενα διαφορετικο τροπο, χρησιμοποιώντας την DMwR
actual_pred = data.frame(cbind(actuals=test$effort,predicteds=p))
correlation_accuracy = cor(actual_pred)
regr.eval(actual_pred$actuals, actual_pred$predicteds)

#k- Fold Cross validation
cvResults <- suppressWarnings(CVlm( data1, form.lm=effort ~ ., m=5, dots=FALSE, seed=420, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))
attr(cvResults, 'ms') 


#2η μεθοδος Ridge regression

x=model.matrix(effort~.,data1)[,-1]
y=data1$effort

grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50] 
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60] 
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:9,]
set.seed(420)
train=sample(1:nrow(x), (nrow(x)*3)/4)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
#βλεπω οτι η Ridge παλινδρομηση ειναι αρκετα πιο αποδοτικη
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients",x=x[train,],y=y[train])[1:9,]
set.seed(420)
#cross validation
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:9,]
#εχουν διαφοροποιησεις σε σχεση με τα προηγουμενα μοντελα

#3η μεθοδος Lasso regression
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(420)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) 
#ειναι αρκετα κοντα στην MSE που ειχαμε πριν στην Ridge , ειναι λιγο μεγαλυτερη
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:9,]
lasso.coef
lasso.coef[lasso.coef!=0]



