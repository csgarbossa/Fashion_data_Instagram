
# gam ---------------------------------------------------------------------

str(apply(dat, 2, unique))
# df must be <= lower number of uniques values of the variable

form.gam1 <- character(0)
for (i in 1:length(var.quant)) {
  # to select only the lasso variables:
  # if (var.quant[i] %in% varlasso)
  form.gam1 <- c(form.gam1, paste0("s(",names(var.quant)[i],", df=4)", collapse = NULL))
}

gam.quant <- paste0(form.gam1, collapse = "+")
gam.qual <- paste0(names(var.quali), collapse = "+")
formula.gam1 <- as.formula(paste("y ~ ", paste0(c(gam.qual,gam.quant), collapse = "+"),
                                 collapse = NULL))

library(gam)

m.gam <- gam(formula.gam1, data = train)
p.gam <- predict(m.gam, newdata = test)

tab$GAM <- error(p.gam)

t(tab)

summary(m.gam)
plot(m.gam, se = T, ask = T)


# gam stepwise ------------------------------------------------------------

library(gam)

set.seed(394)
set <- sample(c("train", "test"), replace = T, 
              prob = c(0.7, 0.3), size = nrow(train))
train2 <- train[set == "train", ]

sc <- gam.scope(train2, response = y.position,  arg = c("df=2", "df=3", "df=4")) 
m.gam0 <- gam(y ~ 1, data = train, direction = "both") 
m.gam <- step.Gam(m.gam0, sc) # steps = 25
formula(m.gam)

plot(m.gam, se = T, ask = T)

p.gam <- predict(m.gam, newdata = test)

tab$GAM_stepwise <- error(p.gam)

t(tab)


# albero ------------------------------------------------------------------

library(tree)

# prune with cross-validation
m.tree <- tree(y ~. , data = train,
               control = tree.control(nobs = nrow(train), 
                                      minsize = 2, mindev = 0.0001))
plot(m.tree)

set.seed(125)
tree.prune.cv <- cv.tree(m.tree, FUN = prune.tree, K = 10, method = "deviance")
J <- min(tree.prune.cv$size[tree.prune.cv$dev == min(tree.prune.cv$dev)])
J
plot(tree.prune.cv, xlim = c(1, 25)); abline(v = J, lty = 2, col = 2)

m.tree.prune.cv <- prune.tree(m.tree, best = J)
plot(m.tree.prune.cv); text(m.tree.prune.cv, cex = .5, pretty = 1)

p.tree.cv <- predict(m.tree.prune.cv, newdata = test)

tab$Tree <- error(p.tree.cv)

t(tab)

# tree built with another library

# library(rpart)
# 
# set.seed(39)
# tree2 <- rpart(formula(m.lin), data = train,
#                control = rpart.control(xval = 10, minbucket = 2, cp = 0.0001))
# cp_ottim <- tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"]
# rsq.rpart(tree2)
# j <- tree2$cptable[which.min(tree2$cptable[,"xerror"]), "nsplit"]
# j
# abline(v = j, lty = 2, col = 2)
# fit.tree <- prune(tree2, cp = cp_ottim)
# 
# summary(fit.tree)
# print(fit.tree)
# 
# fit.tree$method
# fit.tree$variable.importance[1:10]
# 
# p.tree2 <- predict(fit.tree, newdata = test)
# 
# tab$rpart <- error(p.tree2)
# 
# t(tab)


# mars --------------------------------------------------------------------

# prune with generalized cross-validation
library(earth)

m.mars <- earth(y ~. , data = train, degree = 2) # if degree = 1 it is an additive model
summary(m.mars)

plot(m.mars$gcv.per.subset)
best <- which.min(m.mars$gcv.per.subset)
abline(v = best, col = 2, lty = 2)
m.mars$prune.terms[best, ]

rownames(m.mars$dirs)[m.mars$prune.terms[best, ]]

colnames(m.mars$bx)
plotmo(m.mars)
p.mars <- predict(m.mars, newdata = test, type = "response")

tab$Mars <- error(p.mars)

t(tab)

# mars with an other library

# the dataset needs to be standardized

library(polspline)

m.mars <- polymars(train$y, train[, - y.position])

plot(m.mars$fitting$size[m.mars$fitting$"0/1"== 1],
     m.mars$fitting$GCV[m.mars$fitting$"0/1"== 1],
     xlab = "Number of base function", ylab = "GCV")
points(m.mars$fitting$size[m.mars$fitting$"0/1"== 0],
       m.mars$fitting$GCV[m.mars$fitting$"0/1"== 0], col = 4)
legend("topright", legend = c("Growth","Pruning"), col = c(1, 4), pch = 1)

p.mars <- predict(m.mars,  test[, - y.position])

tab$Mars2 <- error(p.mars)

t(tab)


# random forest -----------------------------------------------------------

library(randomForest)
library(ranger)

# there is the possibility of checking if the number of tree in the forest are enough but not too much

# set.seed(3)
# set <- sample(c("train", "test"), replace = T,
#               prob = c(0.7, 0.3), size = nrow(train))
# train2 <- train[set == "train", ]
# set.seed(39)
# set <- sample(c("train", "test"), replace = T,
#               prob = c(0.7, 0.3), size = nrow(train2))
# train3 <- train2[set == "train", ]
# test3 <- train2[set == "test", ]
# set.seed(394)
# ranfo.try <- randomForest(y ~ .,
#                           data = train3,
#                           xtest = test3 %>% select(- y),
#                           ytest = test3$y,
#                           ntree = 200)
# plot(ranfo.try)

# selection of the number of variables a each split through out-of-bag
set.seed(3)
cols <- 2:(ncol(dat) - 1) # if there are too much variables is better to proceed with a greed
err <- matrix(NA, NROW(cols), 2) 
for(i in seq_along(cols)){
  cat(i, "/", NROW(cols), "...\n")
  m <- ranger(y ~ ., data = train, num.trees = 150, mtry = cols[i],
              max.depth = 2)
  err[i,] <- c(cols[i], m$prediction.error)
}

plot(err, type = "b", 
     xlab = "Number of variables considered at each split", 
     ylab = "Forecast error")
bestm <- err[which.min(err[, 2]), 1]
abline(v = bestm, lty = 2, col = 2)

set.seed(39)
m.ran <- ranger(y ~ ., data = train, importance = "impurity",
                num.trees = 150, mtry = bestm)

sort(m.ran$variable.importance, decreasing = T)[1:(ncol(dat) - 1)]
plot(m.ran$variable.importance, type = "b", 
     xlab = "Column index", ylab = "Variables importance")  
colnames(train %>% select(- y))

p.ran <- predict(m.ran, data = test)  
str(p.ran)

tab$Random_forest <- error(p.ran$predictions)

t(tab)

# projection pursuit -----------------------------------------------------------

set.seed(394)
set <- sample(c("train", "test"), replace = T, 
              prob = c(0.7, 0.3), size = nrow(train))
train2 <- train[set == "train", ]
test2 <- train[set == "test", ]

nterms <- 1:10 # + 5 if the model suggests the max number of rotations
err <- matrix(NA, NROW(nterms), 2)
for(i in seq_along(nterms)){
  cat(i, "/", NROW(nterms), "...\n")
  m <- ppr(y ~ ., data = train2, nterms = nterms[i])
  p <- predict(m, newdata = test2)
  e <- mean((p - test2$y)^2)
  err[i,] <- c(nterms[i], e)
}

plot(err, type = "b")
bestm <- err[which.min(err[, 2]), 1]
abline(v = bestm, lty = 2, col = 2)

set.seed(394)
m.ppr <- ppr(y ~ ., data = train, nterms = bestm)
p.ppr <- predict(m.ppr, newdata = test)  

tab$PPR <- error(p.ppr)

t(tab)


# risultati ---------------------------------------------------------------

e <- data.frame(t(tab))
knitr::kable(arrange(e, mae), digits = 4)




