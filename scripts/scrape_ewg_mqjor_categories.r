
library(XML)


skin = htmlParse("/Users/Gaston/Documents/Insight/data/ewg_skin_deep.html")


links = xpathSApply(
  skin,
  "//li[@class='menuhover']//div[@class='row']//li/a",
  xmlGetAttr,
  "href")

links


categs = xpathSApply(
  skin,
  "//li[@class='menuhover']//div[@class='row']//li/a",
  xmlValue)

categs



xpathSApply(
  skin,
  "//li[@class='menuhover']/a[@class='submenus']",
  xmlValue)


xpathSApply(
  skin,
  "//li[@class='menuhover']//div[@class='row']/ul/li[contains(a)]",
  xmlValue)


xpathSApply(
  skin,
  "//div[@class='sub']/div[@class='row']/ul",
  xmlSize)


xpathSApply(
  skin,
  "//div[@class='sub']/div[@class='row']/ul/li[contains() = 'a']",
  xmlSize)


getNodeSet(
  skin,
  "//div[@class='sub']/div[@class='row']//a")



xpathSApply(
  skin,
  "//li[@class='menuhover']//div[@class='row']//li[contains() = 'a']",
  xmlValue)


xpathSApply(
  skin,
  "//li[@class='menuhover']//div[@class='row']//li/h2",
  xmlValue)


nodos = getNodeSet(
  skin,
  "//li[@class='menuhover']//div[@class='row']/ul")

lapply(nodos, )

lapply(nodos, xmlValue)




skin_df = data.frame(
  categs = categs,
  links = links,
  stringsAsFactors = FALSE)


write.table(
  skin_df,
  "/Users/Gaston/Documents/Insight/data/ewg_categories.csv",
  sep = ",",
  row.names = FALSE)



  