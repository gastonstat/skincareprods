# =============================================================
# Import data
# =============================================================

# load 
library(plyr)
library(ggplot2)

toydata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)


library(DiscriMiner)
library(plsdepot)
library(turner)
library(matrixkit)

# 60% training
set.seed = 22222
training = sample(
  x = 1:nrow(toydata), 
  size = 20000)

train_size = ceiling(0.60 * nrow(toydata))
test_size = ceiling((nrow(toydata) - train_size) / 2)
valid_size = nrow(toydata) - train_size - test_size
set_sizes = sample(
  x = rep(1:3, times=c(train_size, test_size, valid_size)),
  size = nrow(toydata))
table(set_sizes)

# 60% train, 20% test, 20% validate
training <- (set_sizes == 1)
testing <- (set_sizes == 2)
validating <- (set_sizes == 3)


bincat = dummify(toydata$category)

X = cbind(bincat[training,], 
          num_ings=toydata$num_ings[training])
y = toydata$health[training]

pls1 = plsreg1(predictors=X, response=y, comps=7)

pls1$reg.coefs[1] + pls1$reg.coefs["dental-floss"] + 
  15* pls1$reg.coef["num_ings"]

plot(y, pls1$y.pred, pch=20, col="#adadad88", cex=0.5)

qplot(y, pls1$y.pred, geom="point", alpha=0.9) +
  geom_jitter()

