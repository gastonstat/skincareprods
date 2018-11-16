# =============================================================
# Import data
# =============================================================

# load 
library(plyr)
library(ggplot2)
library(DMwR)
library(rpart)
library(rpart.plot)
library(rattle)
library(FactoMineR)


toydata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

toydata$num_all = toydata$num_low + toydata$num_medium +
  toydata$num_high


# =============================================================
# Data sets for model prediction
# =============================================================

# 60% training
train_size = ceiling(0.60 * nrow(toydata))
test_size = ceiling((nrow(toydata) - train_size) / 2)
valid_size = nrow(toydata) - train_size - test_size
set.seed = 22222

set_sizes = sample(
  x = rep(1:3, times=c(train_size, test_size, valid_size)),
  size = nrow(toydata))
table(set_sizes)

# 60% train, 20% test, 20% validate
training <- (set_sizes == 1)
testing <- (set_sizes == 2)
validating <- (set_sizes == 3)


# =============================================================
# Regression Tree
# =============================================================


# regression tree model
my_tree = rpart(
  health ~ num_ings + fragrance + has_high + category, 
  data = toydata[training,],
  control = rpart.control(cp = 0.005)
)

# plot pretty tree
prettyTree(my_tree)

# another tree plot
fancyRpartPlot(my_tree)

# default plots
plot(my_tree)
text(my_tree, use.n = TRUE)

# R2 (not very useful)
rsq.rpart(my_tree)
my_tree_cp = printcp(my_tree)
rsq.val = round(1 - my_tree_cp[,c(3,4)], 4)


# prediction training set
tree_predictions = predict(my_tree, toydata[training,])
# Absolute Square Mean Error
mean(abs(tree_predictions - toydata[training,'health']))
# Mean Sqaure Error
mean((tree_predictions - toydata[training,'health'])^2)

# evaluation (kind of chafa)
regr.eval(toydata[training,'health'],
          tree_predictions, train.y = algae[training,'a1'])

# some boxplots
where = my_tree$where
boxplot(toydata$health[training] ~ where)


# not very useful plot (because of the ordinal scales)
plot(tree_predictions, toydata[training,'health'],
     main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty = 2)

# The issue is the scale in which response "score" is measured
# This ordinal scale seems more appropriate 
# for Correspondance Analysis
health_train_pred = predict(my_tree, toydata[training,])
pred_train_xtable = table(
  round(health_train_pred), 
  toydata$health[training])
pred_train_xtable
# correspondance analysis
ca_train = CA(pred_train_xtable)

# CA for testing set
# if we take scores as qualitative
health_pred = predict(my_tree, toydata[testing,])
test_xtable = table(round(health_pred,2), toydata$health[testing])

pred_test_xtable = table(round(health_pred), toydata$health[testing])
pred_test_xtable
# correspondance analysis
ca_test = CA(pred_test_xtable)

# mosaic plot
mosaicplot(pred_test_xtable)


# data frame for better CA plot
ca_test_dim = rbind(
  ca_test$col$coord[,1:2],
  ca_test$row$coord[,1:2]
)
df = data.frame(
  ca_test_dim,
  row.names = 1:nrow(ca_test_dim)
)
df$score = c(
  rownames(ca_test$col$coord),
  rownames(ca_test$row$coord))
df$type = rep(
  c("true", "pred"), 
  c(nrow(ca_test$col$coord), nrow(ca_test$row$coord)))

head(df)


# better CA plot
ggplot(data = df, aes(x = Dim.1, y= Dim.2, group = type)) +
  geom_line(aes(color = type), alpha = 0.3, size = 2) + 
  geom_text(aes(label = score, color = type), size = 7) +
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  ggtitle("Correspondance Analysis plot") +
  xlab(paste("Dim 1 (", round(ca_test$eig[1,2], 2), "%)", sep='')) +
  ylab(paste("Dim 2 (", round(ca_test$eig[2,2], 2), "%)", sep=''))
ggsave("/Users/Gaston/Desktop/ca_test.png",
       width = 10, height = 7.5)


# prediction testing set
test_predictions = predict(my_tree, toydata[testing,])
valid_predictions = predict(my_tree, toydata[validating,])
# Mean Sqaure Error
mean((test_predictions - toydata[testing,'health'])^2)
mean((valid_predictions - toydata[validating,'health'])^2)

# plot
plot(test_predictions, toydata[testing,'health'],
     main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values")
abline(0, 1, lty=2)




prp(my_tree)

fancyRpartPlot(my_tree)



health_hat = predict(my_tree, newdata=toydata[testing,])

plot(toydata$health[testing], health_hat)






cv.rpart <- function(form, train, test, ...) {
  m <- rpartXse(form, train, ...)
  p <- predict(m, test)
  mse <- mean((p - resp(form, test))^2)
  c(nmse = mse / mean((mean(resp(form, train)) - resp(form, test))^2))
}

res <- experimentalComparison(
  c(dataset(health ~ num_ings + fragrance + has_high + category, toydata)),
  c(variants('cv.rpart', se=c(0, 0.5, 1))),
  cvSettings(1, 10, 1234)
)

summary(res)

bestScores(res)



# ========================================================
# ROC 
fit.pr = predict(my_tree, newdata=toydata[testing,])
fit.pred = prediction(fit.pr, toydata$health[training])
fit.perf = performance(fit.pred,"tpr","fpr")
plot(fit.perf,lwd=2,col="blue",
     main="ROC:  Classification Trees on Adult Dataset")
abline(a=0,b=1)
# ========================================================


health_pred = predict(my_tree, toydata[testing,])

table(health_pred, toydata$health[testing])

[,1]                                [,2]
[1,] "baby-lotion"                       "a" 
[2,] "baby-shampoo"                      "b" 
[3,] "baby-soap-bath"                    "c" 
[4,] "baby-sunscreen"                    "d" 
[5,] "baby-wipes"                        "e" 
[6,] "bubble-bath"                       "f" 
[7,] "dental-floss"                      "g" 
[8,] "deodorants-antiperspirants-mens"   "h" 
[9,] "deodorants-antiperspirants-womens" "i" 
[10,] "feminine-moisturizer"              "j" 
[11,] "feminine-powder_deodorant"         "k" 
[12,] "fragrance-for-men"                 "l" 
[13,] "fragrance-for-women"               "m" 
[14,] "mouthwash"                         "n" 
[15,] "personal-cleansing"                "o" 
[16,] "shampoo"                           "p" 
[17,] "soap"                              "q" 
[18,] "toothpaste"                        "r" 


# =============================================================
# Random Forest
# =============================================================

#install.packages("randomForest")
library(randomForest)

toydata$group = factor(toydata$group)

my_forest = randomForest(
  x = toydata[,c(3,5,6,7)],
  y = toydata$health)

importance(my_forest)



# =============================================================
# Insight Charts
# =============================================================

# Average Score by Category
avg_health_categ_df = ddply(
  toydata, 
  .(category),
  summarize,
  q1 = quantile(health, probs = 0.25),
  avg = mean(health),
  q3 = quantile(health, probs = 0.75))

ord_avg_health = order(avg_health_categ_df$avg)

avg_health_categ_df[ord_avg_health,]

avg_score = mean(toydata$health)
avg_score = quantile(toydata$health, probs=0.75)


barcol = rep("#c994c7", nrow(avg_health_categ_df))
barcol[avg_health_categ_df$avg[ord_avg_health] <= 2] = "#dd1c77"
barcol[avg_health_categ_df$avg[ord_avg_health] > 4.2] = "#e7e1ef"

# vertical orientation
op = par(mar = c(2,3,5,2))
x <- barplot(
  avg_health_categ_df$avg[ord_avg_health], 
  col = barcol, border = NA, las = 2, ylim = c(0, 10))
y <- avg_health_categ_df$avg[ord_avg_health]
#abline(h = c(2, 4), col = rgb(0.7,0.7,0.7,0.7), lwd = 2)
abline(h = c(2, 4.3, avg_score), 
       col = c("#dd1c7799", "#c994c7bb", "#33333344"), 
       lwd = 2)
text(x, y + 0.1, adj=0, cex = 0.8, 
     labels = avg_health_categ_df$category[ord_avg_health],
     srt = 90, xpd = TRUE)
par(op)


#3f869b
barcol = rep("#f0ad4e", nrow(avg_health_categ_df))
barcol[avg_health_categ_df$avg[ord_avg_health] <= 2] = "#fd7d00"
barcol[avg_health_categ_df$avg[ord_avg_health] > 4.05] = "#d9d9d9"

# horizontal orientation
png(filename="/Users/Gaston/Desktop/barchart.png",
    width=900, height=700)
op = par(mar = c(2.5,3,2,2))
y <- barplot(
  avg_health_categ_df$avg[ord_avg_health], 
  horiz = TRUE, axes = FALSE,
  col = barcol, border = NA, xlim = c(0, 10))
axis(side = 1, at = 0:10, line = -0.3, col = "gray60", 
     col.axis = "gray30")
x <- avg_health_categ_df$avg[ord_avg_health]
#abline(h = c(2, 4), col = rgb(0.7,0.7,0.7,0.7), lwd = 2)
abline(v = c(2, 4, avg_score), 
       col = c("#fd7d00bb", "#f0ad4e88", "#77777744"), 
       lwd = 7)
text(x + 0.05, y, adj=0, cex = 1.1, 
     labels = avg_health_categ_df$category[ord_avg_health],
    xpd = TRUE)
title("Average Health Score by Product Category")
par(op)
dev.off()


# =============================================================
# Number of Ingredients
# =============================================================

# Average Score by Category
avg_ings_categ_df = ddply(
  toydata, 
  .(category),
  summarize,
  avg_score = mean(health),
  avg_ings = mean(num_ings),
  avg_high = mean(num_high),
  avg_medium = mean(num_medium),
  avg_low = mean(num_low),
  avg_all = mean(num_all))

avg_ings_categ_df 

with(avg_ings_categ_df, 
     plot(avg_ings, avg_all, cex=(10-avg_score)/2))
#with(avg_ings_categ_df, 
#     text(avg_ings, avg_all, labels=category, 
#          adj=0, cex=0.8, xpd=TRUE, pos=4))
with(avg_ings_categ_df, 
     text(avg_ings, avg_all, labels=rownames(avg_ings_categ_df), 
          adj=0, cex=0.8, xpd=TRUE))


xpos = c(
  4, 4, 4, 4, 3, 4, 3, 3, 2, 4, 
  4, 4, 2, 4, 1, 4, 2, 2, 2, 2,
  4, 3, 4, 4, 4, 4, 1, 4, 2, 1, 
  4, 1, 3, 4, 2, 2)
barcol = rep("#f0ad4eee", nrow(avg_ings_categ_df))
barcol[avg_ings_categ_df$avg_score <= 2] = "#fd7d00ee"
barcol[avg_ings_categ_df$avg_score > 4.05] = "#d9d9d9cc"
barcol[avg_ings_categ_df$avg_score > 6] = "#0b8db3cc"

a = min(avg_ings_categ_df$avg_score)
b = max(avg_ings_categ_df$avg_score)
(b - a) / b
rango = range(avg_ings_categ_df$avg_score)

pal = colorRampPalette(c("#fd7d00", "#0b8db3cc"))(20)

library(RColorBrewer)
ylgnbu = brewer.pal(9, "Oranges")[7:1]
ylgnbu[8] = "#393939"
mycol <- colorRampPalette(ylgnbu)(diff(c(1.72, 9)))
barcol = mycol[avg_ings_categ_df$avg_score]
barcol = paste(barcol, "dd", sep='')
#with(avg_ings_categ_df, 
#     plot(avg_ings, avg_all, cex=(10-avg_score)/2, col=barcol, pch=19))
with(avg_ings_categ_df, 
     plot(avg_ings, avg_all, col=barcol, cex=2, pch=19))
with(avg_ings_categ_df, 
     text(avg_ings, avg_all, labels=category, col = "gray20", 
          adj=0.5, cex=0.8, xpd=TRUE, pos=xpos))

# Avg Ingredients -vs- Avg Score
with(avg_ings_categ_df, 
     plot(avg_ings, avg_score, col=barcol, cex=2, 
          pch=19, ylim=c(0,9)))
with(avg_ings_categ_df, 
     text(avg_ings, avg_score, labels=category, 
          adj=0, cex=0.8, xpd=TRUE))


# Avg All Toxic Ings -vs- Avg Score
lpos = c(
  4, 4, 4, 4, 3, 4, 4, 3, 3, 4, 
  4, 4, 2, 4, 1, 1, 2, 2, 2, 1,
  4, 2, 4, 1, 4, 4, 1, 2, 2, 1, 
  4, 2, 1, 4, 2, 4)

png("/Users/Gaston/Desktop/num_ings_score.png", 
    width=900, height=700, res=75)
op = par(mar = c(4, 4.5, 3, 2))
with(avg_ings_categ_df, 
     plot(avg_ings, avg_score, col=barcol, cex=1.2*avg_all, 
          pch=19, xlim=c(4,28), ylim=c(0,10), 
          xlab = "", ylab = "", axes=FALSE))
axis(side=1, line=-1.4, col="white", col.ticks="gray20",
     col.axis="gray20")
axis(side=2, line=0, las=2, col="gray30", col.axis="gray20")
mtext(text = "Average number of ingredients", side=1, line=1.5)
mtext(text = "Average Health Score", side=2, line=2)
mtext("The more ingredients, the lower the Health Score", 
      side=3, col="gray15",
      cex=1.5, line=0.1, at=3, adj=0)
abline(h=0, v=0)
#with(avg_ings_categ_df, 
#     text(avg_ings, avg_score, labels=rownames(avg_ings_categ_df), 
#          adj=0, cex=0.8, xpd=TRUE))
with(avg_ings_categ_df, 
     text(avg_ings, avg_score, labels=abbreviate(category, 12),
          adj=0, cex=1.1, pos=lpos, xpd=TRUE))
legend(x=23.5, y=9.5, legend=c(" <= 2", "", " 3 - 4", "", " >= 5"), 
       pch=19,
       title = "Avg num of risky \ningredientss", bty="n",
       pt.cex=c(2,0,2.7,0,3.7), 
       col=c("#FEE6CE","white", "#FDAE6B", "white", "#D94801"))
par(op)
dev.off()


# Avg Ingredients -vs- Avg Score
with(avg_ings_categ_df, plot(avg_ings, avg_low))
with(avg_ings_categ_df, 
     text(avg_ings, avg_low, labels=category, 
          adj=0, cex=0.8, xpd=TRUE))


# =============================================================
# Category and Health
# =============================================================

ggplot(toydata, aes(x = factor(category), y = health)) +
  geom_violin()

ggplot(toydata, aes(x = factor(category), y = num_ings)) +
  geom_boxplot()


avg_ing_categ_df = ddply(
  toydata, 
  .(category),
  summarize,
  avg_ing = mean(num_ings),
  avg_health = mean(health))

ord_avg_ing = order(avg_ing_categ_df$avg_ing)

avg_ing_categ_df[ord_avg_ing,]


plot(avg_ing_categ_df$avg_ing, avg_ing_categ_df$avg_health,
     pch = 19, col = "gray90")
text(avg_ing_categ_df$avg_ing, avg_ing_categ_df$avg_health,  
     labels = abbreviate(avg_ing_categ_df$category, 10),
     cex = 0.8, col = "gray70", pos = 4, xpd = TRUE)


# colors like ggplot 
ggcolors <- function(n, alfa = 0.3) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alfa)[1:n]
}

ggplot(data = avg_ing_categ_df, 
       aes(x = avg_ing, y = avg_health, colour = avg_health)) +
  geom_point(size = 3) +
  scale_colour_gradientn(colours = ggcolors(7)) +
  geom_text(aes(label = category), hjust = -0.1, alpha=0.5) +
  xlim(0, 35) + 
  xlab("Average number of ingredients") +
  ylab("Average health score") +
  ggtitle("Health Score by Number of Ingredients")
ggsave(file = "/Users/Gaston/Desktop/avg_ing_cat.png",
       height = 7.5, width = 10, units = "in")





# =============================================================
# Boxplots
# =============================================================

with(toydata, boxplot(num_ings ~ has_high))

ggplot(data = toydata,
       aes(x = num_ings, colour = factor(has_high))) +
  geom_density(aes(fill = factor(has_high)), alpha = 0.5)


# adding category
ggplot(data = toydata,
       aes(x = num_ings, colour=factor(has_high))) +
  geom_density(aes(fill = factor(has_high)), alpha = 0.5) +
  xlab("Number of Ingredients") +
  facet_wrap(~ category, scales = "free_y") +
  guides(colour=guide_legend(title="Toxic"), 
         fill=FALSE)
ggsave(file = "/Users/Gaston/Desktop/num_ing_has_high.png",
       height = 7.5, width = 10, units = "in")


