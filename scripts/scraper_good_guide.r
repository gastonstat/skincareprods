

library(XML)
library(RCurl)


# good guide url: browse categories
goodguide_url = "http://www.goodguide.com/"

# destination
data_folder = "/Users/Gaston/Documents/Insight/data/"
goodguide_html = paste(data_folder, "good_guide_products_html.html", 
                       sep = "")
goodguide_html

# download html file
download.file("http://www.goodguide.com/browse", destfile = goodguide_html)


# ===============================================================
# Personal Care Products
# ===============================================================

# focus on "Personal Care" products only
gg_doc = htmlParse(goodguide_html)

# Personal Care Product Categories
personal_care_prods_categories = xpathSApply(
  gg_doc, 
  "//div[@id='vertical-personal-care']//dl/dt/a", 
  xmlValue)

# number of products in each category
number_prods_by_category = xpathSApply(
  gg_doc, 
  "//div[@id='vertical-personal-care']//dl/dd", 
  xmlValue)

# convert to numeric
number_prods_by_category = as.numeric(gsub(
  pattern = ",", 
  replacement = "", 
  x = number_prods_by_category))

# pers care prods categs links
personal_care_prods_categories_links = xpathSApply(
  gg_doc, 
  "//div[@id='vertical-personal-care']//dl/dt/a", 
  xmlGetAttr, "href") 

category_id = gsub(
  pattern = "/categories/", 
  replacement = "",
  x = personal_care_prods_categories_links
)

# data frame of product categories and their number
prods_categories_df = data.frame(
  id = category_id,
  category = personal_care_prods_categories,
  number = number_prods_by_category,
  pagination = ceiling(number_prods_by_category / 15),
  stringsAsFactors = FALSE
)


# save data frame 
gg_data_folder = "/Users/Gaston/Documents/Insight/data/good_guide/"
categs = paste(gg_data_folder, "categories.csv", sep = "")
write.csv(prods_categories_df, categs, row.names = FALSE)


# category with smallest number of products
which(prods_categories_df$Number == min(prods_categories_df$Number))
prods_categories_df[16,]


