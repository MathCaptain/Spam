library(car)
spambase.data <- read.csv("spambase.data", header=FALSE)
names(spambase.data) <- c("word_freq_make","word_freq_address","word_freq_all","word_freq_3d","word_freq_our","word_freq_over",
                          "word_freq_remove","word_freq_internet","word_freq_order","word_freq_mail","word_freq_receive",
                          "word_freq_will","word_freq_people","word_freq_report","word_freq_addresses","word_freq_free",
                          "word_freq_business","word_freq_email","word_freq_you","word_freq_credit","word_freq_your",
                          "word_freq_font","word_freq_000","word_freq_money","word_freq_hp","word_freq_hpl","word_freq_george",
                          "word_freq_650","word_freq_lab","word_freq_labs","word_freq_telnet","word_freq_857","word_freq_data",
                          "word_freq_415","word_freq_85","word_freq_technology","word_freq_1999","word_freq_parts","word_freq_pm",
                          "word_freq_direct","word_freq_cs","word_freq_meeting","word_freq_original","word_freq_project","word_freq_re",
                          "word_freq_edu","word_freq_table","word_freq_conference","char_freq_;","char_freq_(","char_freq_[",
                          "char_freq_!","char_freq_$","char_freq_#","capital_run_length_average","capital_run_length_longest",
                          "capital_run_length_total","spam")
seed <- 99
set.seed(seed)

####################
### Training set ###
####################

#80% training set / 20 % test set
spambase.data$spam <- as.factor(spambase.data$spam)
y <- spambase.data$spam
X <- spambase.data[-58]

#First step is to shuffle data 
N_train <- floor(0.8*nrow(X))
train_ind <- sample(seq_len(nrow(X)), size = N_train, replace = FALSE)
X_train <- X[train_ind, ]
y_train <- spambase.data$spam[train_ind]

#Change index to be easier to acces in loop
rownames(X_train) <- 1:nrow(X_train)

####################
##### Test set #####
####################

#Test set
X_test <- X[-train_ind, ]
y_test <- spambase.data$spam[-train_ind]

#Prescreening
#Extract p-value from each glm for Y vs each X variable
n <- 57
my.pvalue <- rep(0,n)
for( i in 1:n){
  data1 <- X_train[,i]
  spam <- y_train
  data.model <- data.frame(spam,data1)
  my.pvalue[i] <- summary(glm(spam ~ data1, data=data.model, family = "binomial"))$coef[8]
}

my.pvalue <- data.frame(names(spambase.data)[-58],my.pvalue)
names(my.pvalue) <- c("Attribute","P-value of beta1")

#Which variables are not significant
my.pvalue[which(my.pvalue[,2]>0.05),]

#Remove variable  which are not significant
pvalue.reduced <- my.pvalue[my.pvalue[,2]<0.05,]
pvalue.reduced.sorted <- pvalue.reduced[order(pvalue.reduced$`P-value of beta1`,decreasing = TRUE),]

stripchart(sort(round(pvalue.reduced.sorted$`P-value of beta1`,digits=1000)), method = "stack", offset = 1, at = .05, pch = 20)

####################
# Cross validation #
####################

#Remove variable from which are not significant
X_train <- X_train[,-which(names(X_train)=="word_freq_will")]
X_train <- X_train[,-which(names(X_train)=="word_freq_parts")]
X_train <- X_train[,-which(names(X_train)=="word_freq_address")]

dim(X_train)

# VIF check
data.vif <- cbind(y_train,X_train)
fit.vif <- glm(y_train ~ ., data=data.vif, family = "binomial")
vif(fit.vif)

#Create matrix to hold each pvalues from first 1/10 regression pvalues to last 1/10 regresson pvalues.
#M = How many times we resample
M <- 20
N <- ncol(X_train)
p1 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p2 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p3 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p4 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p5 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p6 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p7 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p8 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p9 <- as.data.frame(matrix(NA, nrow = M, ncol=N))
p10 <- as.data.frame(matrix(NA, nrow = M, ncol=N))

#Create loop to make 20 different combination of data set

#Create data frame and change colnames. Add 1 to column because we want to store beta0 as well. 
#Each beta has been shifted one when removing a column like before
pvalue <- as.data.frame(matrix(nrow = 200,ncol = (N+1)))

for(i in 1:(N+1)){
  colnames(pvalue)[i] <- paste("beta",i-1,collapse = "",sep = "")
}

for(k in 1:M){
  set.seed(seed)
  train_ind <- sample(seq_len(nrow(X_train)), size = N_train, replace = FALSE)
  X_train <- X_train[train_ind, ]
  y_train <- y_train[train_ind]
  
  #Change index to be easier to acces in loop
  rownames(X_train) <- 1:nrow(X_train)
  
  #Create data frame for each 1/10 of data set
  data.train1 <- cbind(y_train[1:368],X_train[1:368,])
  data.train2 <- cbind(y_train[369:736],X_train[369:736,])
  data.train3 <- cbind(y_train[737:1104],X_train[737:1104,])
  data.train4 <- cbind(y_train[1105:1472],X_train[1105:1472,])
  data.train5 <- cbind(y_train[1473:1840],X_train[1473:1840,])
  data.train6 <- cbind(y_train[1841:2208],X_train[1841:2208,])
  data.train7 <- cbind(y_train[2209:2576],X_train[2209:2576,])
  data.train8 <- cbind(y_train[2577:2944],X_train[2577:2944,])
  data.train9 <- cbind(y_train[2945:3312],X_train[2945:3312,])
  data.train10 <- cbind(y_train[3313:3680],X_train[3313:3680,])
  
  #Change name because cbind is measy
  names(data.train1)[names(data.train1)=="y_train[1:368]"] <- "y_train"
  names(data.train2)[names(data.train2)=="y_train[369:736]"] <- "y_train"
  names(data.train3)[names(data.train3)=="y_train[737:1104]"] <- "y_train"
  names(data.train4)[names(data.train4)=="y_train[1105:1472]"] <- "y_train"
  names(data.train5)[names(data.train5)=="y_train[1473:1840]"] <- "y_train"
  names(data.train6)[names(data.train6)=="y_train[1841:2208]"] <- "y_train"
  names(data.train7)[names(data.train7)=="y_train[2209:2576]"] <- "y_train"
  names(data.train8)[names(data.train8)=="y_train[2577:2944]"] <- "y_train"
  names(data.train9)[names(data.train9)=="y_train[2945:3312]"] <- "y_train"
  names(data.train10)[names(data.train10)=="y_train[3313:3680]"] <- "y_train"
  
  #Fit models for each subset of data. Had to set max iteration to 10 to make the model converge.
  FI <- 10
  mod1 <- glm(y_train~. , data=data.train1, family = binomial(link = "logit"),control = list(maxit = FI))
  mod2 <- glm(y_train~. , data=data.train2, family = binomial(link = "logit"),control = list(maxit = FI))
  mod3 <- glm(y_train~. , data=data.train3, family = binomial(link = "logit"),control = list(maxit = FI))
  mod4 <- glm(y_train~. , data=data.train4, family = binomial(link = "logit"),control = list(maxit = FI))
  mod5 <- glm(y_train~. , data=data.train5, family = binomial(link = "logit"),control = list(maxit = FI))
  mod6 <- glm(y_train~. , data=data.train6, family = binomial(link = "logit"),control = list(maxit = FI))
  mod7 <- glm(y_train~. , data=data.train7, family = binomial(link = "logit"),control = list(maxit = FI))
  mod8 <- glm(y_train~. , data=data.train8, family = binomial(link = "logit"),control = list(maxit = FI))
  mod9 <- glm(y_train~. , data=data.train9, family = binomial(link = "logit"),control = list(maxit = FI))
  mod10 <- glm(y_train~. , data=data.train10, family = binomial(link = "logit"),control = list(maxit = FI))
  #Extract pvalues into vector
  for (i in 1:(N+1)){
    p1[k,i] <- summary(mod1)$coefficient[,4][i]
    p2[k,i] <- summary(mod2)$coefficient[,4][1]
    p3[k,i] <- summary(mod3)$coefficient[,4][i]
    p4[k,i] <- summary(mod4)$coefficient[,4][i]
    p5[k,i] <- summary(mod5)$coefficient[,4][i]
    p6[k,i] <- summary(mod6)$coefficient[,4][i]
    p7[k,i] <- summary(mod7)$coefficient[,4][i]
    p8[k,i] <- summary(mod8)$coefficient[,4][i]
    p9[k,i] <- summary(mod9)$coefficient[,4][i]
    p10[k,i] <- summary(mod10)$coefficient[,4][i]
  }
  
  #Transfer pvalues from vector to matrix
  for(i in 1:(N+1)){
    pvalue[1+10*(k-1),i] <- p1[k,i]
    pvalue[2+10*(k-1),i] <- p2[k,i]
    pvalue[3+10*(k-1),i] <- p3[k,i]
    pvalue[4+10*(k-1),i] <- p4[k,i]
    pvalue[5+10*(k-1),i] <- p5[k,i]
    pvalue[6+10*(k-1),i] <- p6[k,i]
    pvalue[7+10*(k-1),i] <- p7[k,i]
    pvalue[8+10*(k-1),i] <- p8[k,i]
    pvalue[9+10*(k-1),i] <- p9[k,i]
    pvalue[10+10*(k-1),i] <- p10[k,i]
  }
}

pvalue[,5]
#Setting all pvalue below 0.05 as 1 and below as 0
pvalue <- ifelse(pvalue<=0.05,1,0)
#Setting all NA to 0.
pvalue[is.na(pvalue)] <- 0

#Format as dataframe
pvalue <- as.data.frame(pvalue)

#Total count of each variable.
colSums(pvalue)

colSums(pvalue)[order(colSums(pvalue),decreasing = TRUE)]

###############################################################
## New model only including variables with count greater 100 ##
###############################################################

#First resample the data again
train_ind <- sample(seq_len(nrow(X_train)), size = N_train, replace = FALSE)
X_train <- X_train[train_ind, ]
y_train <- y_train[train_ind]
data <- cbind(y_train,X_train)

#Model
mod <- glm(y_train ~ word_freq_our + word_freq_remove + word_freq_free + word_freq_business + 
             word_freq_font + word_freq_hp + word_freq_george + word_freq_650 +  word_freq_re + 
             word_freq_edu + `char_freq_[` + `char_freq_!` + `char_freq_$` + capital_run_length_average + 
             capital_run_length_longest, data = data, family = "binomial")

#Use step function to do forward and backward selection on model
step(mod, direction = "both")

#Remove variable with pvalues above 0.05. (largest above first)
mod.final <- glm(formula = y_train ~ word_freq_our + word_freq_remove + word_freq_free + 
                   word_freq_business + word_freq_font + word_freq_hp + word_freq_george + word_freq_re + 
                   word_freq_edu + `char_freq_!` + `char_freq_$` + capital_run_length_longest, 
                 family = "binomial", data = data)
summary(mod.final)

#Check vif
max(vif(mod.final))
#None above 10

#####################
## Prediction part ##
#####################

#Accuracy
test.set <- cbind(y_test,X_test)
summary(mod.final)
fitted.mod <- predict(mod.final, newdata = subset(test.set,select = c(6,8,17,18,23,26,28,46,47,53,54,57)), type = "response")
fitted.y <- ifelse(fitted.mod > .5, 1, 0)
error <- mean(fitted.y != test.set$y_test)
print(paste('Accuracy = ', 1-error))

#Area under the curve
library(ROCR)
pr=prediction(fitted.mod, test.set$y_test)
prf=performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

auc <- performance(pr,measure="auc")
auc@y.values[[1]]

#Confusion matrix
library(caret)
#Confusion matrix
tr <- as.factor(test.set$y_test)
pr <- as.factor(fitted.y)

confusionMatrix(pr,tr)

summary(mod.final)

#Confidence intervals
ci <- round(confint(mod.final),5)
ci

#Odds ratio
odds.ratio <- rep(0,12)
for(i in 1:12){
  odds.ratio[i] <- round(exp(mod.final$coefficients[i+1]),10)
}

names(odds.ratio) <-c("word_freq_our","word_freq_remove","word_freq_free","word_freq_business",
                      "word_freq_font","word_freq_hp","word_freq_george","word_freq_re","word_freq_edu",
                      "`char_freq_!`","`char_freq_$` ","capital_run_length_longest ")
odds.ratio

#Odds ratio confidence interval
ci.odds.ratio <- round(exp(confint(mod.final)),10); 
ci.odds.ratio