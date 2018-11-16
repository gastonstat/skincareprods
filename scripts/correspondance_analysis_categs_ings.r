
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


# =============================================================
# build text file of categories and ingredients of high level of concern
# =============================================================

# empty vector to store results
#categ_high = rep("", length(current_prods))
categ_ings = rep("", length(current_prods))
num_prod_by_categ = rep(0, length(current_prods))

for (i in 1L:length(current_prods)) {
  # read data
  current_data = read.csv(
    current_prods[i],
    header = TRUE,
    stringsAsFactors = FALSE
  )
  num_prod_by_categ[i] = nrow(current_data)
  
  # extract ingredients of high concern
  ings_high_list = lapply(
    current_data[,"high"],
    extract_concern
  )
  # extract ingredients of medium concern
  ings_medium_list = lapply(
    current_data[,"medium"],
    extract_concern
  )
  # extract ingredients of low concern
  ings_low_list = lapply(
    current_data[,"low"],
    extract_concern
  )
  aux_all_ingredients = c(
    unlist(ings_high_list),
    unlist(ings_medium_list),
    unlist(ings_low_list)
  )
  empty_ings <- (aux_all_ingredients == '')
  all_ingredients = aux_all_ingredients[!empty_ings]
  if (length(all_ingredients) != 0) {
#    all_ingredients = ""
#    categ_high[i] = paste(all_ingredients, collapse = " ")
    categ_ings[i] = paste(all_ingredients, collapse = " ")
  }
  #all_ingredients != ""
  #all_ingredients = all_ingredients[]
} 

# remove empty
#empty_categs <- (categ_high == '')
#categ_high = categ_high[!empty_categs]
#names(categ_high) = current_categ_prods[!empty_categs]
empty_categs <- (categ_ings == '')
categ_ings = categ_ings[!empty_categs]
names(categ_ings) = current_categ_prods[!empty_categs]


# save it
writeLines(
  categ_ings, 
  "/Users/Gaston/Documents/Insight/data/good_guide/categ_ings_vector.txt"
)


# =============================================================
# build text file of categories and ingredients of high level of concern
# =============================================================

# load text mining
library(tm)
library(FactoMineR)

# corpus from vector
not = which(current_categ_prods %in% 
               c('nail-polish', 'mouthwash', 'toothpaste', 'dental-floss'))
#mycorpus = Corpus(VectorSource(categ_ings[-not]))
mycorpus = Corpus(VectorSource(categ_ings))

  
# term document matrix
tdm = TermDocumentMatrix(mycorpus)

# inspect diag_dtm
# (79% sparsity, which means a lot of empty cells)
tdm

# convert as matrix
m1 = as.matrix(tdm)
colnames(m1) = current_categ_prods

# get word count
count_terms = rowSums(m1)
hist(count_terms, col="gray80")

# to simplify, we need to get the less sparsed terms
# for instance, let's get terms >= 90% quantile frequency
which_mfw <- count_terms >= quantile(count_terms, probs=0.80)
sum(which_mfw)

# most frequent terms
mfw = count_terms[which_mfw]

# Option 1: matrix with most frequent terms
mft_mat = m1[which_mfw,]
# first adjacency matrix
#adj_mat1 = m1 %*% t(m1)


# =============================================================
# Correspondance Analysis
# =============================================================

# correspondance analysis
my_ca = CA(mft_mat, graph=FALSE)
plot(my_ca)

barplot(100-my_ca$eig[,3])

# cluster analysis of categories
hc = hclust(dist(my_ca$col$coord), method="ward.D")
plot(hc, hang=-1)
hc6 = cutree(tree=hc, k=6)

library(ape)
library(igraph)
# convert to phylo object
phyl = as.phylo(hc)
# get edges
my_edges = phyl$edge
# convert to graph
my_graph = graph.edgelist(my_edges)
# extract layout (x-y coords)
my_lay = layout.fruchterman.reingold(my_graph)
my_lay = layout.auto(my_graph)

##plot(my_graph, layout=my_lay)
# minimum spanning tree
msp = minimum.spanning.tree(my_graph)

# PLOT
#plot(my_lay)
#text(my_lay[,1], my_lay[,2], labels=1:nrow(my_lay))
ggcolors <- function(n, alfa) {
  hues = seq(15,375,length=n+1)
  hcl(h=hues, l=65, c=100, alpha=alfa)[1:n]
}

txt_pal = ggcolors(6)[c(4,2,3,1,5,6)]
pch_pal = paste(txt_pal, "55", sep='')
txt_col = txt_pal[hc6]
pch_col = pch_pal[hc6]


pch_cex = 600 * (colMeans(m1) / num_prod_by_categ)
#pch_cex = log(colMeans(m1))
#pch_cex[pch_cex < 0.05] = 0.5

#png("/Users/Gaston/Desktop/category_cluster.png", width = 950, height = 700, res=300)
op = par(mar = c(1.5, 2.5, 1.5, 2.5))
plot(my_lay[,1], my_lay[,2], type="n", axes= FALSE,
     xlab = "", ylab = "")
segments(
  x0 = my_lay[my_edges[,1],1], 
  y0 = my_lay[my_edges[,1],2],
  x1 = my_lay[my_edges[,2],1],
  y1 = my_lay[my_edges[,2],2],
  col = "#dcdcdc55", lwd = 3.5
)
points(my_lay[1:36,1], my_lay[1:36,2], col=pch_col, 
       pch=19, cex=pch_cex)
points(my_lay[37:71,1], my_lay[37:71,2], col="gray90", 
       pch=19, cex=0.5)
text(my_lay[1:36,1], my_lay[1:36,2], col=txt_col,
     abbreviate(phyl$tip.label,15), cex=1.2, xpd = TRUE, font = 2)
par(op)
#dev.off()


# =============================================================
# Graph plot for website
# =============================================================

my_labels = c(
  "after-shave", "anti-aging",
  "deodorants-mens", "baby-lotion",
  "baby-shampoo", "baby-sunscreen",
  "baby-wipes", "bubble-bath",
  "conditioner", "feminine-moisturizer",
  "feminine-powder", "fragrance-men",
  "fragrance-women", "hair-spray", 
  "hand-sanitizer", "moisturizer",
  "mouthwash", "nail-polish", 
  "personal-cleansing", "shampoo",
  "shaving-cream", "sunless-tanning",
  "sunscreen <= 15", "sunscreen > 15",
  "tanning-oil", "toothpaste", 
  "baby-soap","concealer-foundation",
  "eye-makeup", "lipstick-balm",
  "scrubs-powders", "soap", 
  "body-wash-cleanser", "foot-care",
  "deodorants-womens", "dental-floss") 

ggcolors <- function(n, alfa) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alfa)[1:n]
}

txt_pal = ggcolors(6)[c(4,2,3,1,5,6)]
pch_pal = paste(txt_pal, "22", sep='')
txt_col = txt_pal[hc6]
pch_col = pch_pal[hc6]

pch_cex = 200 * (colMeans(m1) / num_prod_by_categ)

png("/Users/Gaston/Desktop/category_cluster.png",
    width = 950, height = 700, res=250)
op = par(mar = c(0.5, 1, 0.5, 1))
plot(my_lay[,1], my_lay[,2], type="n", axes= FALSE,
     xlab = "", ylab = "")
segments(
  x0 = my_lay[my_edges[,1],1], 
  y0 = my_lay[my_edges[,1],2],
  x1 = my_lay[my_edges[,2],1],
  y1 = my_lay[my_edges[,2],2],
  col = "#dcdcdc55", lwd = 1
)
points(my_lay[1:36,1], my_lay[1:36,2], col=pch_col, 
       pch=19, cex=pch_cex)
points(my_lay[37:71,1], my_lay[37:71,2], col="gray90", 
       pch=19, cex=0.1)
text(my_lay[1:36,1], my_lay[1:36,2], col=txt_col,
     my_labels, cex=0.45, xpd = TRUE, font = 1)
par(op)
dev.off()


# =============================================================
# Build json file for d3js graph
# =============================================================

pch_cex = 400 * (colMeans(m1) / num_prod_by_categ)

#null_nodes = paste("null_", 37:71, sep = '')
null_nodes = rep("", length(37:71))
node_names = c(phyl$tip.label, null_nodes)
node_names

node_colors = c(txt_col, rep("white", length(null_nodes)))

scores = c(
  round(700 * (colMeans(m1) / num_prod_by_categ)),
  rep(0.1, length(null_nodes))
)

groups = c(hc6, rep(7, length(null_nodes)))

outfile = "/Users/Gaston/Desktop/cluster_graph.json"
# first line
cat("{", "\n", file=outfile)
cat("  \"nodes\":[", "\n", file = outfile, append = TRUE)
# add names
for (i in 1:(length(node_names)-1)) {
  cat("    {\"name\":\"", node_names[i], 
      "\",\"group\":", groups[i]-1, 
      ",\"color\":\"", node_colors[i], "\"",
      ",\"score\":", scores[i], "},", 
      "\n", sep = '', file = outfile, append = TRUE)
}
cat("    {\"name\":\"", node_names[i], 
    "\",\"group\":", groups[i+1],
    ",\"color\":\"", node_colors[i+1], "\"",
    ",\"score\":", scores[i+1], "}", 
    "\n", sep = '', file = outfile, append = TRUE)
cat("  ],", "\n", file = outfile, append = TRUE)
# add links
cat("  \"links\":[", "\n", file = outfile, append = TRUE)
for (j in 1:(nrow(my_edges)-1)) {
  cat("    {\"source\":", my_edges[j,2]-1,
      ",\"target\":", my_edges[j,1]-1, 
      ",\"value\":1},", "\n",
      sep = '', file = outfile, append = TRUE)
}
cat("    {\"source\":", my_edges[j+1,2]-1,
    ",\"target\":", my_edges[j+1,1]-1, 
    ",\"value\":1}", "\n",
    sep = '', file = outfile, append = TRUE)
# end of file
cat("  ]", "\n", file = outfile, append = TRUE)
cat("}", file = outfile, append = TRUE)




