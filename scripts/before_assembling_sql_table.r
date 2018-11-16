# =============================================================
# required functions
# =============================================================


split_ingredients_string <- function(ings_str) {
  # split ingredients
  unlist(strsplit(ings_str, split = ","))
}

# remove asterics
remove_asterics <- function(ings_vec) {
  gsub(
    pattern = "\\*",
    replacement = "",
    x = ings_vec)
}

remove_first_space <- function(ings_vec) {
  gsub(
    pattern = "^\\s",
    replacement = "",
    x = ings_vec)
}

remove_last_dot <- function(ings_vec) {
  gsub(
    pattern = "$\\.",
    replacement = "",
    x = ings_vec)
}

clean_split_ingredients <- function(ings_str) {
  # split ingredients string
  tmp = split_ingredients_string(ings_str)
  # remove asterics
  tmp = remove_asterics(tmp)
  tmp = remove_first_space(tmp)
  tmp = remove_last_dot(tmp)
  tmp
}

any_fragrance <- function(ings_vec) {
  fragrance = c("fragrance", "fragrances")
#  if (any(!is.na(match(fragrance, ings_vec)))) TRUE else FALSE
  if (any(!is.na(match(fragrance, ings_vec)))) 1 else 0
#  "fragance" %in% ings_vec
}

# tell me if products contain fragrance in their ingredients
has_fragrance <- function(ingreds) {
  # ingreds is a vector containing ingredients
  clean_ings = lapply(ingreds, clean_split_ingredients)
  unlist(lapply(clean_ings, any_fragrance))
}

# presence or absence of high concern ingredients
any_high <- function(high_concern_vec) {
  if (high_concern_vec == '') 0 else 1
}

# tell me if products contain high concern ingredients
has_high <- function(high_concern) {
  unlist(lapply(high_concern, any_high))
}

get_ingredients <- function(concern) {
  # split ingredients
  tmp = strsplit(concern, split = ",")
  # remove ingredient id
  lapply(concern, function(x) gsub("\\d+-", "", x))
}

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


length_nonempty <- function(x) {
  aux = unlist(x)
  if (length(aux) != 0) length(x) else 0
}

num_concern <- function(concern) {
  # anonym function
  tmp = strsplit(concern, split = ",")
  unlist(lapply(tmp, length_nonempty))
}



# =============================================================
# Import data
# =============================================================

# load 
library(plyr)
library(ggplot2)

toydata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/products_sql.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

str(toydata, vec.len = 1)

table(toydata$health)

# removing duplicated
duplicated_prods = duplicated(toydata$id_prod)
toydata = toydata[!duplicated_prods,]

# remove products with num_ings == 0 or > 50
# (0 ingredients equivalent to NA)
summary(toydata$num_ings)
hist(toydata$num_ings)
# 95% of ingredients have num of ings below
quantile(x=toydata$num_ings, probs=0.95)
sum(toydata$num_ings > 50, na.rm = TRUE) / nrow(toydata)
enough_ings <- (toydata$num_ings > 0 & toydata$num_ings <= 50)
toydata  = toydata[enough_ings,]


#toy = readLines("ingredients-152758-shampoo.csv", n=3086)
#toysplit = strsplit(toy, split=",")
#toyl = unlist(lapply(toysplit, length))
#summary(toyl[1:5])



# replaceing NAs by ""
toydata$high[is.na(toydata$high)] = ""
toydata$medium[is.na(toydata$medium)] = ""
toydata$low[is.na(toydata$low)] = ""

toydata$has_high = has_high(toydata$high)

# add columns with boolean level of concern
#toydata$high_concern <- toydata$high != ''
#toydata$medium_concern <- toydata$medium != ''
#toydata$low_concern <- toydata$low != ''

# add columns with number of ings based on level of concern
toydata$num_high = num_concern(toydata$high)
toydata$num_medium = num_concern(toydata$medium)
toydata$num_low = num_concern(toydata$low)

# fragrance boolean
toydata$fragrance = has_fragrance(toydata$ingredients)

# convert group to factor
#toydata$group = factor(toydata$group)


# remove products with incongruent number of ingredients
more_low_than_ings <- (toydata$num_low > toydata$num_ings)
more_medium_than_ings <- (toydata$num_medium  > toydata$num_ings)
more_high_than_ings <- (toydata$num_high > toydata$num_ings)

sum(more_low_than_ings) + 
  sum(more_medium_than_ings) + 
  sum(more_high_than_ings)

incongruent = c(
  which(more_low_than_ings),
  which(more_medium_than_ings))

toydata = toydata[-incongruent,]

with(toydata, cor(num_ings, num_low))
with(toydata, cor(num_ings, num_medium))
with(toydata, cor(num_ings, num_high))


# =================================================
# Save data file for MySQL
# =================================================

mysql_columns = c(
  "id_prod",
  "name",
  "category",
  "id_category",
  "health",
  "num_ings",
  "num_high",
  "num_medium",
  "num_low",
  "has_high",
  "fragrance"
)

str(toydata[,mysql_columns])

# export it
write.table(
  x = toydata[,mysql_columns],
  file = "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  sep = ",",
  row.names = FALSE
)


# check it
caredata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

