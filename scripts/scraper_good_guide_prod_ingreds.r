
require(XML)
require(stringr)

# ===============================================================
# required functions
# ===============================================================

# look for low-concern
xpath_low_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='low-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
}

# look for medium-concern
xpath_medium_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='medium-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
}

# look for high-concern
xpath_high_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='high-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
}

# regulatory ban
xpath_regulatory_ban <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='regulatory-ban']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
}
  
# use restriction
xpath_use_restriction <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='use-restriction']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
}

## Function to clean ingredient "href"
extract_ingredient_id <- function(hrefs) {
  gsub(
    pattern = "\\?",
    replacement = "",
    x = str_extract_all(
      gsub(
        pattern = "/ingredients/", 
        replacement = "", 
        x = hrefs), 
      "^.*\\?")
  )
}

search_concern <- function(html_doc, concern = "low") {
  type_concern = switch(
    concern,
    low = xpath_low_concern(html_doc),
    medium = xpath_medium_concern(html_doc),
    high = xpath_high_concern(html_doc),
    ban = xpath_regulatory_ban(html_doc),
    restriction = xpath_use_restriction(html_doc)
  )
  
  if (length(type_concern) > 0) {
    which_ingredients = grep("ingredients", type_concern)
    # extract ingredients 'id'
    all_ingredients = extract_ingredient_id(type_concern[which_ingredients])
    ingredients = unique(all_ingredients)
    ingredients = paste(ingredients, collapse = ",")
    return(ingredients)
  } else {
    return("")
  }
}


# product id
product_id <- function(html_doc) {
  id_href = xpathSApply(
    html_doc,
    "//section[@id='product']/div/meta[@content][1]",
    xmlGetAttr,
    "content"
  )
  gsub(
    pattern = "http://www.goodguide.com/products/", 
    replacement = "", 
    x = id_href)
}


# product name
product_name <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//div[@class='name']/h1",
    xmlValue
  )  
}

# product category
extract_category <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//span[@class='extra-info']/a",
    xmlGetAttr,
    "href"
  )
}


# category id number
category_id <- function(prod_cat) {
  if (length(prod_cat) != 0) {
    prod_cat = gsub("/categories/", "", prod_cat)
    prod_cat = str_extract(prod_cat, pattern = "\\d+")
  }
  else {
    prod_cat = ""
  }
  # output
  prod_cat
}


# category name
category_name <- function(prod_cat) {
  if (length(prod_cat) != 0) {
    prod_cat = gsub("/categories/", "", prod_cat)
    prod_cat = gsub("\\d+-", "", prod_cat)
  } else {
    prod_cat = ""
  }
  # output
  prod_cat
}


# product category
product_category <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//span[@class='extra-info']/a",
    xmlGetAttr,
    "href"
  )
}

# manufacturer
manufacturer <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//span[@itemprop='manufacturer']/span",
    xmlValue
  )  
}


manufacturer_id <- function(html_doc) {
  manufac_id = xpathSApply(
    html_doc,
    "//span[@itemprop='manufacturer']/span/a",
    xmlGetAttr,
    "href"
  )
  gsub(
    pattern = "/companies/",
    replacement = "",
    x = manufac_id
  )
}


# health rating container
health_rating <- function(html_doc) {
  ratings = xpathSApply(
    html_doc,
    "//td[@class='rating-container'][1]",
    xmlValue
  )
  as.numeric(ratings[1])
}

# extract ingredients
extract_ingredients <- function(html_doc) {
  str_ingredients = xpathSApply(
    html_doc,
    "//div[@data-hover-name='Package Label Ingredients']//p[2]",
    xmlValue
  )
  #
  tolower(str_ingredients)
}


# ===============================================================
# extract ingredients
# ===============================================================

# good guide url: browse categories
goodguide_url = "http://www.goodguide.com/"

# destination to store data files
data_folder = "/Users/Gaston/Documents/Insight/data/good_guide/"

# read table of product categories
categories = read.csv(
  paste(data_folder, "categories_pending.csv", sep = ""),
  stringsAsFactors = FALSE)

# column names for file
col_names = paste(
  "id_prod",
  "product",
  "id_category",
  "category",
  "id_comp",
  "company",
  "ingredients",
  "health",
  "low",
  "medium",
  "high",
  "ban",
  "restrict",
  sep = ",")


# scrape products in each category
# lets start with feminine powder deodorants
for (i in 11L:nrow(categories)) {
  # read table containing product ids
  data_file = paste(data_folder, "categories/",
                    categories$id[i], ".csv", sep = "")
  products = read.csv(data_file, stringsAsFactors = FALSE)

  # starting file to store results
  output_file = paste(data_folder, "products/ingredients-",
                      categories$id[i], ".csv", sep = "")
  # verbose
  print(paste("working on file: ", categories$id[i]))
  #cat(col_names, "\n", file = output_file)
  cat(
    "\"id_prod\",",
    "\"name\",",
    "\"id_category\",",
    "\"category\",",
    "\"id_comp\",",
    "\"company\",",
    "\"ingredients\",",
    "\"health\",",
    "\"low\",",
    "\"medium\",",
    "\"high\",",
    "\"ban\",",
    "\"restrict\"",
    "\n",
    sep = "", file = output_file)
  
  # parsing each product
    for (pro in 1L:nrow(products)) {
    prod_url = paste(goodguide_url, "products/", products$id[pro], sep = "")
    prod_html = htmlParse(prod_url)
    prod_categ = extract_category(prod_html)
    # populate file
    cat(
      "\"", product_id(prod_html), "\",",
      "\"", product_name(prod_html), "\",",
      "\"", category_id(prod_categ), "\",",
      "\"", category_name(prod_categ), "\",",
      "\"", manufacturer_id(prod_html), "\",",
      "\"", manufacturer(prod_html), "\",",
      "\"", extract_ingredients(prod_html), "\",",
      "\"", health_rating(prod_html), "\",",
      "\"", search_concern(prod_html, "low"), "\",",
      "\"", search_concern(prod_html, "medium"), "\",",
      "\"", search_concern(prod_html, "high"), "\",",
      "\"", search_concern(prod_html, "ban"), "\",",
      "\"", search_concern(prod_html, "restriction"),  "\"",
      "\n", sep = "",
      file = output_file,
      append = TRUE
    )
    # print message
    print(paste(categories$id[i], ":", pro, "out of", nrow(products)))
  } # end populating file
}

# stop i=26, pro=2413


# ===============================================================
# ===============================================================
# ===============================================================


# read product (html content)
product = htmlParse(
  "/Users/Gaston/Documents/Insight/data/honeymark-bath-salt-365014.html")

product = htmlParse(
  "http://www.goodguide.com/products/420054-rusk-sensories-calm-nourishing")


full_category = extract_category(product)

# testing everything
y = list(
  id_prod = product_id(product),
  name = product_name(product),
  id_category = category_id(full_category),
  category = category_name(full_category),
  man_id = manufacturer_id(product),
  company = manufacturer(product), 
  health = health_rating(product),
  low = search_concern(product, "low"),
  medium = search_concern(product, "medium"),
  high = search_concern(product, "high"),
  ban = search_concern(product, "ban"),
  restrict = search_concern(product, "restriction"),
  ingredients = extract_ingredients(product)
)



# extract ingredients
xpathSApply(
  product,
  "//div[@data-hover-name='Package Label Ingredients']//p[2]",
  xmlValue
)

xpathSApply(
  product,
  "//div[@class='headline-inner']//a",
  xmlGetAttr,
  "href"
)

# level of health
xpathSApply(
  product,
  "//div[@class='container']/ul[@class='rating-tree']/li/div[1]",
  xmlValue
)



xpathSApply(
  product,
  "//ul[@class='rating-tree expanded']/li//dt",
  xmlValue
)


b = xpathSApply(
  product,
  "//dd[@class='medium-health-concern']",
  xmlValue
)
length(b)




## Function to extract low concern ingredients
low_concern <- function(html_doc) {
  low_concern = xpathSApply(
    html_doc,
    "//dd[@class='low-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
  )
  if (length(low_concern) > 0) {
    which_ingredients = grep("ingredients", low_concern)
    # extract ingredient 'id'
    ingredients = extract_ingredient_id(low_concern[which_ingredients])
    return(ingredients)
  } else {
    return(NA)
  }
}
low_concern(product)



# look for low-concern
xpath_low_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='low-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
)}

# look for medium-concern
xpath_medium_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='medium-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
)}

# look for high-concern
xpath_high_concern <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//dd[@class='high-health-concern']//div[@class='ingredient-hover']//a",
    xmlGetAttr,
    "href"
)}




switch(
  "low",
  low = xpath_low_concern(product),
  medium = xpath_medium_concern(product),
  high = xpath_high_concern(product)
)


search_concern <- function(html_doc, concern = "low") {
  type_concern = switch(
    concern,
    low = xpath_low_concern(html_doc),
    medium = xpath_medium_concern(html_doc),
    high = xpath_high_concern(html_doc))
  
  if (length(type_concern) > 0) {
    which_ingredients = grep("ingredients", type_concern)
    # extract ingredient 'id'
    ingredients = extract_ingredient_id(type_concern[which_ingredients])
    return(ingredients)
  } else {
    return(NA)
  }
}

search_concern(product, "low")
search_concern(product, "medium")
search_concern(product, "high")





search_medium_concern <- function(html_doc) {
  medium_concern = xpath_medium_concern(html_doc)
  if (length(low_concern) > 0) {
    which_ingredients = grep("ingredients", low_concern)
    # extract ingredient 'id'
    ingredients = extract_ingredient_id(low_concern[which_ingredients])
    return(ingredients)
  } else {
    return(NA)
  }
}

search_high_concern <- function(html_doc) {
  high_concern = xpath_high_concern(html_doc)
  if (length(high_concern) > 0) {
    which_ingredients = grep("ingredients", low_concern)
    # extract ingredient 'id'
    ingredients = extract_ingredient_id(low_concern[which_ingredients])
    return(ingredients)
  } else {
    return(NA)
  }
}





b = low_concern(product)

str_extract_all(
  gsub(pattern="/ingredients/", replacement="", x=b), 
  "^.*?\\?")

str_extract_all(
  gsub(pattern="/ingredients/", replacement="", x=b), 
  "(^.*)\\?")


gsub(
  pattern = "\\?",
  replacement = "",
  x = str_extract_all(
    gsub(pattern="/ingredients/", replacement="", x=b), 
    "^.*\\?")
)


strsplit(b, split = "/")
library(stringr)
str_extract(string = b, pattern = "\\w+")



# low health concern ingredients
xpathSApply(
  product,
  "//dd[@class='low-health-concern']//div[@data-hover-name]",
  xmlGetAttr, 
  "data-hover-name"
)



xpathSApply(
  product,
  "//ul[@class='rating-tree expanded']/li//dl//li",
  xmlValue
)



# <span class='name with-children'>Human Health Impacts</span>
getNodeSet(
  product,
  "//span[@class='name with-children']"
)



# other negative aspects



# data adequacy

