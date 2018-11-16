

#set working directory
setwd("/Users/Gaston/Documents/Insight/data/good_guide/products")

# file names
current_prods = c(
  "ingredients-152648-deodorants-antiperspirants-mens.csv")


# read data
current = read.csv(
  current_prods[1],
  header = TRUE,
  stringsAsFactors = FALSE
)

# =============================================================

# split ingredients by ","
split_ingredients <- function(ingred) {
  unlist(strsplit(ingred, split = ","))
}

whats_init <- function(ingred) {
  # split ingredients
  tmp = split_ingredients(ingred)
  # remove ending dot
  tmp = gsub("\\.$", "", tmp)
  # remove (in)active ingredient(s):
  tmp = gsub("(inactive|active) ingredient[s]?:", "", tmp)
  # trim white spaces
  tmp = gsub("^\\s+", "", tmp)
  # output
  tmp[tmp != '']
}

# =============================================================


ingredient_list = lapply(current$ingredients, whats_init)

all_ingredients = unlist(ingredient_list)

table_all_ingredients = table(all_ingredients)

dim(table_all_ingredients)

vector_all_ingredients = as.vector(table_all_ingredients)
name_all_ingredients = names(table_all_ingredients)
third_quartile <- quantile(vector_all_ingredients, probs = 0.75)
above_3Q <- (vector_all_ingredients > third_quartile)
common_ingredients = vector_all_ingredients[above_3Q]
name_common_ingredients = name_all_ingredients[above_3Q]
dec_order = order(common_ingredients, decreasing=TRUE)
top_50_ingredients = common_ingredients[dec_order[1:50]]
name_50_ingredients = name_common_ingredients[dec_order]

top_50_ingredients[1:20]
name_50_ingredients[1:20]

# barchart top 20 common ingredients
op = par(mar = c(8, 3, 2, 2))
barplot(top_50_ingredients[1:20], names.arg=name_50_ingredients[1:20], 
        las = 2)
par(op)


# =============================================================

# Ingredients of high level of concern
high_concern_list = lapply(current$high, whats_init)

high_concern_ings = unlist(high_concern_list)

table_high_concern_ings = table(high_concern_ings)

dim(table_high_concern_ings)


# Ingredients of medium level of concern
medium_concern_list = lapply(current$medium, whats_init)
medium_concern_ings = unlist(medium_concern_list)
# remove id's
medium_concern_ings = gsub("^\\d+-", "", medium_concern_ings)
table_medium_concern_ings = table(medium_concern_ings)
dim(table_medium_concern_ings)

op = par(mar = c(8, 3, 2, 2))
barplot(table_medium_concern_ings, las = 2, border = NA)
par(op)
