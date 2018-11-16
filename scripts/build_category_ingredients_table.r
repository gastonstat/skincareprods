
#set working directory
setwd("/Users/Gaston/Documents/Insight/data/good_guide/products")

# file names
current_prods = system("ls", intern = TRUE)

# extract product catgories
tmp = gsub("^(ingredients-\\d+-)", "", current_prods)
current_categ_prods = gsub("*\\.csv", "", tmp)

# =============================================================

# split ingredients by ","
split_ingredients <- function(ings_string) {
  unlist(strsplit(ings_string, split = ","))
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

current_data = read.csv(
  current_prods[7],
  header = TRUE,
  stringsAsFactors = FALSE
)



concern_levels = c("high", "medium", "low")


for (i in 1L:length(current_prods)) {
  # read data
  current_data = read.csv(
    current_prods[i],
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  # list to store results
  list_ings = vector("list", length(concern_levels))
  
  # for each level of concern
  for (k in seq_along(concern_levels)) {
    # extract ingredients of given concern
    ingredient_list = lapply(
      current_data[,concern_levels[k]],
      extract_concern
    )
    all_ingredients = unlist(ingredient_list)
    non_empty_ings = sum(all_ingredients != '')
    if (non_empty_ings > 0) {
      table_ingredients = table(all_ingredients)
      name_ingredients = names(table_ingredients)
      quantity_ingredients = as.vector(table_ingredients)
      concern = rep(concern_levels[k], length(quantity_ingredients))
      categ = rep(current_categ_prods[i], length(quantity_ingredients))
      prop_products = 100 * quantity_ingredients / nrow(current_data)
      list_ings[[k]] = data.frame(
        category = categ,
        ingredient = name_ingredients,
        concern = concern,
        quantity = quantity_ingredients,
        proportion = prop_products
      )
    }
  }
  # assemble data frame
  tmp_df = do.call("rbind", list_ings)
  if (i == 1) {
    write.table(
      tmp_df, 
      "/Users/Gaston/Documents/Insight/data/good_guide/category_ingredients.csv",
      sep = ",", row.names = FALSE)
  } else {
    write.table(
      tmp_df, 
      "/Users/Gaston/Documents/Insight/data/good_guide/category_ingredients.csv",
      sep = ",", row.names = FALSE, 
      col.names = FALSE, append = TRUE)
  }
}  




# =============================================================
  


num_categs = length(current_prods)
ings_per_categ = vector("list", num_categs)
num_prods = rep(0, length(current_prods))


# give me a vector and I return it order



high_ings_df = data.frame(
  category = current_categ_prods, 
  num_high_ings = unlist(ings_per_categ),
  prop_high_ings = round(
    100 * unlist(ings_per_categ) / num_prods, 3)
)

ing_order = order(high_ings_df$prop_high_ings)
high_ings_df[ing_order,]

high_ings_df$num_high_ings

  
# table_all_ingredients = table(all_ingredients)
#  vector_all_ingredients = as.vector(table_all_ingredients)
#  name_all_ingredients = names(table_all_ingredients)
#  above_3Q <- (vector_all_ingredients > third_quartile)
#  common_ingredients = vector_all_ingredients[above_3Q]
#  name_common_ingredients = name_all_ingredients[above_3Q]
#  dec_order = order(common_ingredients, decreasing=TRUE)
  



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
