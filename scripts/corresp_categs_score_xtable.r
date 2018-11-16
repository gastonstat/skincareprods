
# load 
library(plyr)
library(ggplot2)
library(FactoMineR)


toydata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)


categ_table = table(toydata$category)
categories = names(categ_table)
categories

score_values = c(0:8, 10)
categ_scores = matrix(0, length(categories), length(score_values))
dim(categ_scores)



# for each category
for (i in seq_along(categories)) {
  current_categ = categories[i]
  current_obs <- (toydata$category == current_categ)
  current_scores = toydata$health[current_obs]
  score_table = table(current_scores)
  which_scores = score_values %in% names(score_table) 
  # fill in matrix of scores by category
  categ_scores[i,which_scores] = as.vector(score_table)
}
rownames(categ_scores) = categories
colnames(categ_scores) = paste("s", score_values, sep='')

head(categ_scores)
ca_categ_scores = CA(categ_scores)

categ_scores_pull = cbind(
  S0 = rowSums(categ_scores[,1:2]),
  S2 = rowSums(categ_scores[,3:4]),
  S4 = rowSums(categ_scores[,5:6]),
  S6 = categ_scores[,7],
  S7 = rowSums(categ_scores[,8:10])
)

ca_categ_scores_pull = CA(categ_scores_pull)


xi = 1
yi = 2
x = c(
  ca_categ_scores_pull$row$coord[,xi],
  ca_categ_scores_pull$col$coord[,xi])
y = c(
  ca_categ_scores_pull$row$coord[,yi],
  ca_categ_scores_pull$col$coord[,yi])

plot(x, y, type = "n")
abline(h = 0, v = 0, col = "gray70")
points(x[1:36], y[1:36], col = "#54afc955", 
       pch = 20, cex = 2)
points(x[-c(1:36)], y[-c(1:36)], col = "#f3734e55", 
       pch = 20, cex = 3)
text(x[1:36], y[1:36], abbreviate(categories, 6), 
     col = "#54afc9")
text(x[-c(1:36)], y[-c(1:36)], 
     colnames(categ_scores_pull),
     col = "#f3734e")




