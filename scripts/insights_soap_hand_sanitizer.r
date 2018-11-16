
#set working directory
setwd("/Users/Gaston/Documents/Insight/data/good_guide/products")

# file names
current_prods = system("ls", intern = TRUE)

# product catgories
tmp = gsub("^(ingredients-\\d+-)", "", current_prods)
current_categ_prods = gsub("*\\.csv", "", tmp)
id_category = gsub("(ingredients-)|(-\\w+)|(\\.csv)", "", current_prods)


# soap
soap = read.csv(
  current_prods[32],
  header = TRUE,
  stringsAsFactors = FALSE
)
str(soap, vec.len = 1)

# hand sanitizer
hand = read.csv(
  current_prods[15],
  header = TRUE,
  stringsAsFactors = FALSE
)
str(hand, vec.len = 1)



# split ingredients by ","
split_ingredients <- function(ingred) {
  unlist(strsplit(ingred, split = ","))
}

# remove ingredient id
remove_id <- function(ings_vector) {
  gsub("^\\d+-", "", ings_vector)  
}

# test 
extract_concern <- function(some_concern_ings) {
  if (!is.na(some_concern_ings)) {
    some_concern_ings = split_ingredients(some_concern_ings)
    some_concern_ings = remove_id(some_concern_ings)
    return(some_concern_ings)
  } else {
    return("")
  }
}


# extract ingredients of high concern
get_concern <- function(data_frame, concern) {
  ext_con = lapply(
    data_frame[,concern],
    extract_concern
  )
  unlist(ext_con)
}

soap_high = get_concern(soap, "high")
hand_high = get_concern(hand, "high")

soap_med = get_concern(soap, "medium")
hand_med = get_concern(hand, "medium")



# extract ingredients of high concern
ings_high_list = lapply(
  soap[,"high"],
  extract_concern
)
# extract ingredients of medium concern
ings_medium_list = lapply(
  soap[,"medium"],
  extract_concern
)
# extract ingredients of low concern
ings_low_list = lapply(
  soap[,"low"],
  extract_concern
)


unlist(ings_high_list)


# number of ingredients 
number_ingredients <- function(ingred_list) {
  tmp = lapply(ingred_list, split_ingredients)
  unlist(lapply(tmp, length))
}

# total number of ingredients
total_ingredients <- function(ingred) {
  tmp = unlist(split_ingredients(ingred))
  tmp = unique(tmp)
  length(tmp)
}


health = current$health
num_ings = number_ingredients(current$ingredients)
less_60ings <- num_ings <= 60
plot(num_ings[less_60ings], health[less_60ings])
boxplot(num_ings[less_60ings] ~ health[less_60ings])

cor(num_ings[less_60ings], health[less_60ings])


a = current$ingredients[1:10]
b = split_ingredients(current$ingredients[100])
b

org = grep(
  pattern = "organic",
  x = current$name)

