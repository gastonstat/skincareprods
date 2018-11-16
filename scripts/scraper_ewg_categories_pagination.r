

library(XML)
library(RCurl)

# read reduced categories
ewg_cats = read.csv(
  "/Users/Gaston/Documents/Insight/data/ewg_reduce_categories.csv",
  header = TRUE,
  stringsAsFactors = FALSE)


# good guide url: browse categories
ewg_url = "http://www.ewg.org"


#doc = htmlParse("/Users/Gaston/Documents/Insight/data/ewg_toothpaste.html")



# extract title category
title_cat <- function(html_doc) {
  title_category = xpathSApply(
    html_doc,
    "//div[@id='righttoptitleandcats']",
    xmlValue
  )
  unlist(strsplit(title_category, " \\| "))
}

pagination_number <- function(html_doc) {
  str_page = xpathSApply(
    html_doc,
    "//span[@class='dark']",
    xmlValue
  )
  num_page = sub(pattern = ",", "", str_page)
  ceiling(as.numeric(num_page[[1]]) / 50)
}



output_file = "/Users/Gaston/Documents/Insight/data/ewg/ewg_categories.csv"

#cat(col_names, "\n", file = output_file)
cat(
  "\"category\",",
  "\"type\",",
  "\"link\",",
  "\"pagination\"",
  "\n",
  sep = "", file = output_file)


# for each major category (link)
for (k in 1L:nrow(ewg_cats)) {
    major = ewg_cats$category[k]
  # parse first page of major category
  doc = htmlParse(
    paste(ewg_url, ewg_cats$link[k], sep = ""))
  # get pagniation parameter for each major category
  title_categ_aux = title_cat(doc)
  # populate file
  cat(
    "\"", major, "\",",
    "\"", title_categ_aux[1], "\",",
    "\"", ewg_cats$link[k], "\",",
    "\"", pagination_number(doc), "\"",
    "\n", sep = "",
    file = output_file,
    append = TRUE
  )
}



