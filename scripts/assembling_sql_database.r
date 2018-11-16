

#set working directory
setwd("/Users/Gaston/Documents/Insight/data/good_guide/products")

# file names
current_prods = system("ls", intern = TRUE)

# product catgories
tmp = gsub("^(ingredients-\\d+-)", "", current_prods)
current_categ_prods = gsub("*\\.csv", "", tmp)
id_category = gsub("(ingredients-)|(-\\w+)|(\\.csv)", "", current_prods)



i = 37

current = read.csv(
  current_prods[i],
  header = TRUE,
  stringsAsFactors = FALSE
)
str(current, vec.len = 1)


table(current$health)



# split ingredients by ","
split_ingredients <- function(ingred) {
  unlist(strsplit(ingred, split = ","))
}

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



# name
# id_good_guide
# group
# category
# score
# number ingredients
# ingredients


# ------------------------------------------------
# export data for SQL table
# ------------------------------------------------

# these guys work: c(1:17,19:30)

for (i in 1:length(current_prods)) {
  # read data
  current_data = read.csv(
    current_prods[i],
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  # assemble data frame
  current_df = data.frame(
    name = current_data$name,
    id_prod = current_data$id_prod,
    category = rep(current_categ_prods[i], nrow(current_data)),
    id_category = rep(id_category[i], nrow(current_data)),
    health = current_data$health,
    num_ings = number_ingredients(current_data$ingredients),
    high = current_data$high,
    medium = current_data$medium,
    low = current_data$low,
#    num_high = number_ingredients(current_data$high),
#    num_medium = number_ingredients(current_data$medium),
#    num_low = number_ingredients(current_data$low),
    ingredients = current_data$ingredients,
    stringsAsFactors = FALSE
  )
  
  # define output file
  outfile = "/Users/Gaston/Documents/Insight/data/good_guide/products_sql.csv"
  
  # save it
  if (i == 1) {
    # first data frame
    write.table(
      current_df,
      file = outfile,
      sep = ",",
      row.names = FALSE
    )
  } else {
    # append following data frames
    write.table(
      current_df,
      file = outfile,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE,
      append = TRUE
    )    
  }
  # message
  print(current_prods[i])
}

# ------------------------------------------------


for (i in 1:length(current_prods)) {
  # read data
  current_data = read.csv(
    current_prods[i],
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  print(current_prods[i])
  print(summary(current_data$health))
  
  # assemble data frame
  current_df = data.frame(
    name = current_data$name,
    id_prod = current_data$id_prod,
    group = rep(current_categ_prods[i], nrow(current_data)),
    category = current_data$category,
    health = current_data$health,
    num_ings = number_ingredients(current_data$ingredients),
    high = current_data$high,
    medium = current_data$medium,
    low = current_data$low,
    ingredients = current_data$ingredients,
    stringsAsFactors = FALSE
  )
}

