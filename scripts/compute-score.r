
# destination to store data files
data_folder = "/Users/Gaston/Documents/Insight/data/good_guide/"

# read table of categories
categories = read.csv(
  paste(data_folder, "categories.csv", sep = ""),
  stringsAsFactors = FALSE)


# starting file to store results
output_file = paste(data_folder, "products/ingredients-",
                    categories$id[i], ".csv", sep = "")


fempow = read.csv(
  output_file, stringsAsFactors = FALSE)

dim(fempow)

summary(fempow$health)


fempow$ban 
fempow$ban[fempow$ban != ""]
fempow$restrict
fempow$high
fempow$medium
fempow$low






unique_ingredients <- function(ings) {
  aux = unlist(lapply(ings, function(x) strsplit(x, ",")))
  # remove id number
  gsub(
    pattern = "^\\d+-",
    replacement = "",
    x = unique(aux)
  )
}

extract_ingredients <- function(df) {
  concerns = c("low", "medium", "high", "ban")
  ings_list = vector("list", length = length(concerns))
  for (k in seq_along(concerns)) {
    ings_list[[k]] = unique_ingredients(df[,concerns[k]])
  }
  names(ings_list) = concerns
  
  data.frame(
    ingredient = unlist(ings_list),
    concern = rep(concerns, sapply(ings_list, length)),
    stringsAsFactors = FALSE
  )
}

extract_ingredients(fempow)


# list of known ingredients
known_ingredients = extract_ingredients(fempow)

# testing product
myingredients = c("hola", "naranja", "triclosan")


a = match(x = myingredients,
      table = known_ingredients$ingredient)

known_ingredients[a[!is.na(a)],2L]

