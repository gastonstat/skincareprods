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


# =============================================================
# Numer of Ingredients 
# =============================================================

# number of ingredients
summary(toydata$num_ings)

# examine distribution of number of ingredients
hist(toydata$num_ings)


# =============================================================
# Numer of Ingredients -vs- Health Score
# =============================================================

with(toydata, plot(num_ings, health))
abline(lm(toydata$health ~ toydata$num_ings), col = "orange", lwd = 2)

# correlation (without missing num_ings)
complete <- complete.cases(
  cbind(toydata[,c("health", "num_ings")])
)
cor(toydata$num_ings[complete], toydata$health[complete])

# number of ingedients and presence of high level
table(toydata$high)

summary(toydata$num_ings[toydata$health <= 2])
summary(toydata$num_ings[toydata$health > 2 & toydata$health <= 6])
summary(toydata$num_ings[toydata$health > 6])


# =============================================================
# Summary Statistics by product type (i.e. group)
# =============================================================

# average health score by category
ddply(
  toydata, .(category), summarize, 
  num = length(category),
  avg_score = mean(health, na.rm=TRUE),
  avg_ings = mean(num_ings, na.rm = TRUE),
  has_high = round(100 * sum(num_high, na.rm=TRUE) / length(category), 2),
  has_medium = round(100 * sum(num_medium, na.rm=TRUE) / length(category), 2),
  has_low = round(100 * sum(num_low, na.rm=TRUE) / length(category), 2)
)



# =============================================================
# Questions to answer
# =============================================================

# How many ingredients of high level of concern
# Top 10 common ingredients of high level of concern (in general)
# Top 5 ingredients of high level of concern (by category)

break_ingredients <- function(concern) {
  # split ingredients
  tmp = strsplit(concern, split = ",")
  # remove ingredient id
  lapply(tmp, function(x) gsub("\\d+-", "", x))
}


# Ingredients of high level of concern (in general)
x = lapply(toydata$high, break_ingredients)
high_ingredients = sort(table(unlist(x)), decreasing = TRUE)
high_ingredients
round(100 * high_ingredients / nrow(toydata), 3)


# Ingredients of high level of concern (by group)
groups = unique(toydata$group)
group_size = as.vector(table(toydata$group))
num_groups = length(group_size)
group_high_ings <- vector("list", num_groups)
group_high_ings_prop <- vector("list", num_groups)
for (g in 1L:num_groups) {
  aux_group_ings = toydata$high[toydata$group == groups[g]]
  break_aux_group_ings = unlist(break_ingredients(aux_group_ings))
  group_high_ings_table = table(break_aux_group_ings)
  group_high_ings[[g]] = sort(group_high_ings_table, decreasing = TRUE)
  group_high_ings_prop[[g]] = round(
    100 * table(break_aux_group_ings) / group_size[g], 3)
}
names(group_high_ings) = groups
names(group_high_ings_prop) = groups
group_high_ings
# in proportion
group_high_ings 
group_high_ings_prop


# What about 'fragrance'?
round(100 * with(toydata, table(fragrance, health)) / nrow(toydata), 2)

with(toydata, boxplot(health ~ fragrance))

ggplot(toydata, aes(x = factor(fragrance), y = health)) +
  +   geom_boxplot()

ggplot(toydata, aes(x = health)) +
  geom_histogram()


# Examine distribution of score by group
op = par(mar = c(10,3,2,2))
boxplot(
  formula = health ~ group, 
  data = toydata, col = "gray90",
  las = 2)
par(op)

ggplot(toydata, aes(x = health)) +
  geom_histogram(fill = "orange")


ggplot(toydata, aes(x = group, y = health, fill = group)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


ggplot(toydata, aes(x = group, y = health, fill = group)) +
  geom_boxplot() +
  geom_jitter(colour = "gray20", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


# Examine distribution of num ings by group
ggplot(toydata, aes(x = num_ings)) +
  geom_histogram(fill = "orange")

ggplot(toydata, aes(x = group, y = num_ings, fill = group)) +
  geom_boxplot() +
  geom_jitter(colour = "gray20", alpha = 0.3) +
  theme(axis.text.x = element_text(angle = 70, hjust = 1))


# average health score by group
ddply(
  toydata, .(group), summarize, 
  num = length(group),
  avg_score = mean(health, na.rm=TRUE),
  avg_ings = mean(num_ings, na.rm = TRUE),
  has_high = round(100 * sum(high_concern, na.rm=TRUE) / length(group), 2),
  has_medium = round(100 * sum(medium_concern, na.rm=TRUE)/ length(group), 2),
  has_low = round(100 * sum(low_concern, na.rm=TRUE) / length(group), 2),
  avg_high = mean(high_num),
  avg_medium = mean(medium_num),
  avg_low = mean(low_num)
)





# =============================================================
# Regression Tree
# =============================================================

library(rpart)
library(rpart.plot)

summary(toydata$num_ings)
table(toydata$group)
summary(toydata$fragrance)
table(toydata$has_high)


# 70% training
set.seed = 22222
training = sample(
  x = 1:nrow(toydata), 
  size = ceiling(0.70 * nrow(toydata)))

# 30% testing
testing = setdiff(1:nrow(toydata), training)


# regression tree model
my_tree = rpart(
  health ~ num_ings + fragrance + has_high + group, 
  data = toydata[testing,],
  method = "class"
)

plot(my_tree)
text(my_tree, use.n = TRUE)


# regression tree model
my_model = rpart(
  health ~ num_ings + fragrance + has_high + group, 
  data = toydata[testing,]
)

plot(my_model)
text(my_model, use.n = TRUE)

prp(my_model)

rsq.rpart(my_model)


health_pred = predict(my_model, toydata[testing,])

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

install.packages("randomForest")
library(randomForest)

toydata$group = factor(toydata$group)

my_forest = randomForest(
  x = toydata[,c(3,5,6,7)],
  y = toydata$health)

importance(my_forest)

# =============================================================
# Levels of concern
# =============================================================


b = get_ingredients(tooth$high)

# which contan triclosan
which_triclosan = unlist(lapply(b, function(x) x == "triclosan"))
tooth$name[which_triclosan]
num_ings[which_triclosan]
mean(num_ings[which_triclosan])



count_level(toydata$high)
count_level(toydata$medium)
count_level(toydata$low)

# proportion of ingredients in toothpastes based on level of concern
100 * count_level(tooth$high) / nrow(tooth)
100 * count_level(tooth$medium) / nrow(tooth)
100 * count_level(tooth$low) / nrow(tooth)


df1$num_ings[]


hist(df1$num_ings)
names(tooth)

mean(tooth$health, na.rm = TRUE)
median(tooth$health, na.rm = TRUE)
table(tooth$health)

summary(df1$num_ings[df1$hscore <= 2])
summary(df1$num_ings[df1$hscore > 2 & df1$hscore <= 6])
summary(df1$num_ings[df1$hscore > 6])
