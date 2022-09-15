library(dplyr)
library(glmnet)
library(gam)
library(tree)
library(gbm)
library(randomForest)
library(lme4)

dat <- dat %>% rename(y = ER) 

# elimination of variables that are present more than once and those that are almost only 0
dat <- dat %>% select(- c(Followers, Likes, Comments, Emotion, 
                          ImageCategory, Disgust, Fear, BrandName))

levels(dat$BrandCategory) <- make.names(levels(dat$BrandCategory)) # the spaces can be a problem for some models

# train and test sets, model matrix and error function --------------------

set.seed(394)
set <- sample(c("train", "test"), nrow(dat), replace = T, prob = c(0.7, 0.3))
train <- dat[set == "train", ]
test <- dat[set == "test", ]

expl <- dat %>% dplyr::select(- y)
tipo <- sapply(expl, class)
tipo
var.quant <- which(tipo == "integer" | tipo == "numeric")
var.quali <- which(tipo == "factor")
names(var.quant)
names(var.quali)

for(col in var.quali){
  if(!(all(unique(test[, col]) %in%
           unique(train[, col])))){
    cat(col,"-> in the test set there new modes\n")
  }
}

train[ , var.quant] <- scale(train[, var.quant])
test[, var.quant] <- scale(test[, var.quant])


y.position <- which(colnames(train) == "y") 
xtr <- model.matrix(~. , data = train[, - y.position])[, - 1]
xte <- model.matrix(~. , data = test[, - y.position])[, - 1]

error <- function(ypred, yreal = test$y){
  mse <- mean((ypred - yreal)^2)
  mae <- mean(abs(ypred - yreal))
  rmse <- sqrt(mse)
  return(t(data.frame(mse = mse, mae = mae, rmse = rmse)))
}

# linear regression -------------------------------------------------------
m.lin <- lm(y ~ ., data = train)
# m.lin <- lm(y ~ .^2, data = train) if I want to consider the interactions
summary(m.lin)
p.lin <- predict(m.lin, newdata = test)

tab <- data.frame(Linear = error(p.lin))
t(tab)

# linear regression stepwise ----------------------------------------------

set.seed(394)
set <- sample(c("train", "test"), replace = T, 
              prob = c(0.7, 0.3), size = nrow(train))
train2 <- train[set == "train", ]

m0 <- lm(y ~ 1, data = train2)
# if m.lin is not available you can use as.formula(formula) where
# formula <- paste0(colnames(dat %>% select(- y)), collapse = " + ")
# formula <- paste0(cbind("y", formula), collapse = " ~ ")
m.step <- step(m0, scope = formula(m.lin), direction = "both")

formula(m.step)
setdiff(names(m.lin$model),names(m.step$model)) # variables not used in the stepwise
summary(m.step)

p.step <- predict(m.step, newdata = test)
tab$Linear_Stepwise <- error(p.step)

t(tab)

cat("Number of variables:", length(names(m.step$model)) - 1,"\n") ; 
cat("Number of regression parameters:", length(m.step$coefficients) - 1,"\n")


# principal components ----------------------------------------------------

pc <- prcomp(xtr)
# screeplot(pc)

plot((summary(pc)$importance)[1,]^2, type = "l",
     ylab = "Variance", xlab = "Number of components")
axis(3, seq(5, ncol(pc$x), by = 5),
     round((summary(pc)$importance)[3,seq(5, ncol(pc$x), by = 5)], 3))

(summary(pc)$importance)[, 1:19] # after the 12th variable the variance remains quite stable 
abline(v = 12, lty = 2, col = 2)

x.pc <- pc$x[, 1:12]
m.pc <- lm(train$y ~ ., data = as.data.frame(x.pc))

pc.vvv <- predict(pc, newdata = xte)
p.pc <- predict(m.pc, newdata = as.data.frame(pc.vvv))

tab$PC <- error(p.pc)

t(tab)

# ridge regression --------------------------------------------------------

# it doesn't modify the intercept
library(glmnet)
set.seed(394)

m.ridge <- cv.glmnet(xtr, train$y, alpha = 0)
plot(m.ridge)

# if lambda.min or lambda.1se are on the edge 
griglia <- exp(seq(- 2, 10, length = 300)) 
set.seed(394)
m.ridge <- cv.glmnet(xtr, train$y, alpha = 0, lambda = griglia) # nfolds = 10, length(y) if I want the leave one out
plot(m.ridge)

m.ridge$lambda.min
coef(m.ridge, s = "lambda.min") # model trained with the selected lambda

plot(m.ridge$glmnet.fit, xvar = "lambda") # coefficients shrinkage
abline(v = log(m.ridge$lambda.1se), col = 1, lwd = 2, lty = 2) 
abline(v = log(m.ridge$lambda.min), col = 1, lwd = 2, lty = 2)

p.ridge.cv <- predict(m.ridge, newx = xte, s = "lambda.min")
summary(p.ridge.cv)
summary(test$y)
tab$Ridge <- error(p.ridge.cv)

t(tab)

# lasso regression --------------------------------------------------------

library(glmnet)
set.seed(394)

m.lasso <- cv.glmnet(xtr, train$y, alpha = 1)
plot(m.lasso)

# if lambda.min or lambda.1se are on the edge 
griglia <- exp(seq(- 10, 2, length = 300))
set.seed(394)
m.lasso <- cv.glmnet(xtr, train$y, alpha = 1, lambda = griglia)
plot(m.lasso)

plot(m.lasso$glmnet.fit, xvar = "lambda") # coefficients shrinkage
abline(v = log(m.lasso$lambda.1se), col = 1, lwd = 2, lty = 2) 
abline(v = log(m.lasso$lambda.min), col = 1, lwd = 2, lty = 2)

m.lasso$lambda.min
coef(m.lasso, s = "lambda.min") # model with a fixed lambda 

p.lasso.cv <- predict(m.lasso, newx = xte, s = "lambda.min")

summary(p.lasso.cv) # check if the range of the values makes sense
summary(test$y)

tab$Lasso <- error(p.lasso.cv)

t(tab)

# coefficient considered to be equal to 0
b <- coef(m.lasso, s = "lambda.min")
colnames(xtr)[which(b == 0) - 1] # the first coefficient is the intercept and is not to be considered
varlasso <- colnames(xtr)[which(b != 0) - 1] 
varlasso

# adaptive lasso ----------------------------------------------------------

# avoid the problems caused by highly correlated variables
library(glmnet)
set.seed(394)
m.lasso.ada <- cv.glmnet(xtr, train$y, alpha = 1,
                         penalty.factor = 1/abs(coef(m.lin)[- 1]))
plot(m.lasso.ada)

lambda.grid <- exp(seq(- 6, 5, length = 150))
set.seed(394)
m.lasso.ada <- cv.glmnet(xtr, train$y, lambda = lambda.grid,
                         alpha = 1,
                         penalty.factor = 1/abs(coef(m.lin)[- 1]))
plot(m.lasso.ada)

m.lasso.ada$lambda.min
coef(m.lasso.ada, s = "lambda.min") # model with a fixed lambda 

plot(m.lasso.ada$glmnet.fit, xvar = "lambda") # coefficients shrinkage
abline(v = log(m.lasso.ada$lambda.1se), col = 1, lwd = 2, lty = 2) 
abline(v = log(m.lasso.ada$lambda.min), col = 1, lwd = 2, lty = 2)

p.lasso.ada <- predict(m.lasso.ada, newx = xte, s = "lambda.min")

summary(p.lasso.ada)
summary(test$y)
tab$Lasso_adattivo <- error(p.lasso.ada)

t(tab)

# coefficient considered to be equal to 0
b <- coef(m.lasso.ada, s = "lambda.min")
colnames(xtr)[which(b == 0) - 1] # the first coefficient is the intercept 
colnames(xtr)[which(b != 0) - 1] 

# best subset regression --------------------------------------------------

library(bestglm)

m.best <- leaps(xtr, train$y, nbest = 10)
# method = "r2" to compare the models based on the squared-R
plot(m.best$size, m.best$Cp) # best models based on the Cp
abline(v = m.best$size[m.best$Cp == min(m.best$Cp)], lty = 2, col = 2)

selected <- colnames(xtr)[m.best$which[m.best$Cp == min(m.best$Cp), ]]
selected

m.allsub <- lm(paste("train$y ~", paste(selected, collapse = " + ")),
               data = data.frame(xtr))
summary(m.allsub)
p.allsub <- predict(m.allsub, newdata = as.data.frame(xte))

tab$Best_subset <- error(p.allsub)

t(tab)


