
require(XML)
require(RCurl)

# good guide url: browse categories
goodguide_url = "http://www.goodguide.com/"

# destination to store data files
data_folder = "/Users/Gaston/Documents/Insight/data/good_guide/categories/"

# read table of categories
categories = read.csv(
  paste(
    "/Users/Gaston/Documents/Insight/data/good_guide/", 
    "categories_pending.csv", 
    sep = ""),
  header = TRUE,
  stringsAsFactors = FALSE
)



# ===============================================================
# Required Functions
# ===============================================================

# product titles
scrape_name <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//div[@class='name']/a",
    xmlGetAttr, "title")
}


# product Ids
scrape_id <- function(html_doc) {
  product_link = xpathSApply(
    html_doc,
    "//div[@class='name']/a",
    xmlGetAttr, "href")
  
  gsub(
    pattern = "/products/", 
    replacement = "", 
    x = product_link)
}


# product company
scrape_company <- function(html_doc) {
  xpathSApply(
    html_doc,
    "//div[@class='made-by-string']/a[1]",
    xmlValue)
}


# product company Id
scrape_comp_id <- function(html_doc) {
  company_link = xpathSApply(
    html_doc,
    "//div[@class='made-by-string']/a[1]",
    xmlGetAttr, 
    "href")
  
  gsub(
    pattern = "/companies/", 
    replacement = "",
    x = company_link)
}


# product prices
scrape_price <- function(html_doc) {
  price_string = xpathSApply(
    html_doc,
    "//div[@class='price']",
    xmlValue)
  
  gsub("[\n]||[\t]||\\s", "", price_string)
}


# product all ratings (overall, health, env, social)
# this doesn't work for products wth partial data
scrape_ratings <- function(html_doc) {
  ratings_vector = as.numeric(xpathSApply(
    html_doc,
    "//div[@class='value']",
    xmlValue))
  
  ratings = matrix(ratings_vector, ncol = 4, byrow = TRUE)
  colnames(ratings) = c(
    "overall", 
    "health",
    "environment",
    "society")
  
  ratings
}


product_data_frame <- function(html_doc) {
  # auxiliar data frame
  aux_df = data.frame(
    id = scrape_id(html_doc),
    name = scrape_name(html_doc),
    idcomp = scrape_comp_id(html_doc),
    company = scrape_company(html_doc),
    price = scrape_price(html_doc),
    stringsAsFactors = FALSE
  )
  # matrix of ratings
#  ratings = scrape_ratings(html_doc)
  # output
#  cbind(aux_df, ratings)  
  aux_df
}


#products_html = htmlParse(
#  "/Users/Gaston/Documents/Insight/data/feminine_powder.html")

#product_data_frame(products_html)

# ===============================================================
# Let's scrape all product ratings
# ===============================================================

col_names = paste(
  "id",
  "name",
  "idcomp",
  "company",
  "price",
#  "overall",
#  "health",
#  "environment",
#  "society",
  sep = ",")

# pagination: 15 products per page
number_categories = nrow(categories)
#i = 16

for (i in 9L:number_categories) {
    id = categories$id[i]
  output_file = paste(data_folder,
                      categories$id[i], ".csv", sep = "")
  # print message
  print(paste("file", categories$id[i]))
  # start file
  cat(col_names, "\n", file = output_file)
  # populate file
  for (pro in 1L:categories$pagination[i]) {
    # print pagination
    print(paste(categories$id[i], "| page", pro, "of", categories$pagination[i]))
    # assemble query term
    query = "http://www.goodguide.com/products?category_id=%s&page=%s"
    query_url = sprintf(query, id, pro)
    products_html = htmlParse(query_url)
    # get data frame with product information
    prods_df = product_data_frame(products_html)
    # append data frame to file
    write.table(prods_df, file = output_file, sep = ",", append = TRUE, 
              col.names = FALSE, row.names = FALSE)
  }
}


# categories that I may skip
# 30  concealer   9908 prods
# 31  eye-makeup 13419  
# 32  lipstick   14841
# 33  

categories$id[c(35:39, 41, 44)]
# I want these guys!!!
#"152642-after-shave","After Shave",335,23
#"152759-shaving-cream","Shaving Cream",348,24
#"283167-dental-floss","Dental Floss",177,12
#"152738-mouthwash","Mouthwash",435,29
#"152770-toothpaste","Toothpaste",783,53
#"152642-after-shave","After Shave",335,23
#"152720-hand-sanitizer","Hand Sanitizer",294,20
#"152759-shaving-cream","Shaving Cream",348,24


