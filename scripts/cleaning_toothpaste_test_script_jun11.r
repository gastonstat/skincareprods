
#set working directory
setwd("/Users/Gaston/Documents/Insight/data/good_guide/products")


# read toothpaste
tooth = read.csv(
  "ingredients-152770-toothpaste.csv",
  header = TRUE,
  stringsAsFactors = FALSE)





# read toothpaste
tooth = read.csv(
  "ingredients-152770-toothpaste.csv",
  header = TRUE,
  stringsAsFactors = FALSE)

# dimensions
dim(tooth)

# structure
str(tooth, vec.len = 1)

# examine health scores
table(tooth$health)
sum(is.na(tooth$health))

# function to count number of ingredients
ings = tooth$ingredients[1:5]

split_ingredients <- function(ingred) {
  unlist(strsplit(ingred, split = ","))
}

count_ingredients <- function(ingred) {
  tmp = unlist(strsplit(ingred, split = ","))
  length(tmp)
}



# =============================================================
# Numer of Ingredients -vs- Health Score
# =============================================================


num_ings = unlist(lapply(tooth$ingredients, count_ingredients))
hscore = tooth$health

num_ings[]

# inspect distribution of number of ingredients
hist(num_ings)
plot(num_ings, hscore)

# put in data frame
outlier = which(num_ings > 50)
df1 = data.frame(
  num_ings = num_ings[-outlier],
  hscore = hscore[-outlier])
df1 = df1[complete.cases(df1),]
lowess1 = with(df1, lowess(hscore, num_ings))

# lowess regression
plot(df1$num_ings, df1$hscore)
abline(lm(df1$hscore ~ df1$num_ings), col = "orange", lwd = 2)
#loess_fit = loess(df1$hscore ~ df1$num_ings)
#lines(df1$hscore, loess_fit$fitted, col = "blue") # lowess line (x,y)

cor(df1$num_ings, df1$hscore)


# =============================================================
# Levels of concern
# =============================================================

get_ingredients <- function(concern) {
  # split ingredients
  tmp = strsplit(concern, split = ",")
  # remove ingredient id
  lapply(concern, function(x) sub("\\d+-", "", x))
}

b = get_ingredients(tooth$high)

# which contan triclosan
which_triclosan = unlist(lapply(b, function(x) x == "triclosan"))
tooth$name[which_triclosan]
num_ings[which_triclosan]
mean(num_ings[which_triclosan])


count_level <- function(concern) {
  # select non empty levels of concern
  non_empty <- (concern != "")
  # split and count
  tmp = unlist(strsplit(concern[non_empty], split = ","))
  # remove ingredient id
  concern_tmp = gsub("\\d+-", "", tmp)
  concern_tmp = table(concern_tmp)
  sort(concern_tmp, decreasing = TRUE)
}

count_level(tooth$high)
count_level(tooth$medium)
count_level(tooth$low)

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
