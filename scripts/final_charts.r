# =============================================================
# Import data
# =============================================================

# load
library(plyr)
library(ggplot2)

toydata = read.csv(
  "/Users/Gaston/Documents/Insight/data/good_guide/care_products.csv",
  header = TRUE,
  stringsAsFactors = FALSE
)

toydata$num_all = toydata$num_low + toydata$num_medium +
  toydata$num_high

# =============================================================
# Average Health Score by Category
# =============================================================

# Average Score by Category
avg_health_categ_df = ddply(
  toydata, 
  .(category),
  summarize,
  q1 = quantile(health, probs = 0.25),
  avg = mean(health),
  q3 = quantile(health, probs = 0.75))

# order by avg score
ord_avg_health = order(avg_health_categ_df$avg)
avg_health_categ_df[ord_avg_health,]

# overall score
avg_score = mean(toydata$health)
avg_score = quantile(toydata$health, probs=0.75)

# bar chart colors
barcol = rep("#f0ad4e", nrow(avg_health_categ_df))
barcol[avg_health_categ_df$avg[ord_avg_health] <= 2] = "#fd7d00"
barcol[avg_health_categ_df$avg[ord_avg_health] > 4.05] = "#d9d9d9"

library(RColorBrewer)
ylgnbu = brewer.pal(9, "Oranges")[7:1]
ylgnbu[8] = "#cdcdcd"
mycol <- colorRampPalette(ylgnbu)(diff(c(1.72, 9)))
barcol = mycol[avg_health_categ_df$avg[ord_avg_health]]
barcol = paste(barcol, "ee", sep='')


# horizontal orientation
png(filename="/Users/Gaston/Desktop/avg_score_barchart.png",
    width=900, height=700)
op = par(mar = c(3.5,3,2,2))
y <- barplot(
  avg_health_categ_df$avg[ord_avg_health], 
  horiz = TRUE, axes = FALSE,
  col = barcol, border = NA, xlim = c(0, 10))
axis(side = 1, at = 0:10, line = -0.3, col = "gray60", 
     col.axis = "gray30")
x <- avg_health_categ_df$avg[ord_avg_health]
#abline(h = c(2, 4), col = rgb(0.7,0.7,0.7,0.7), lwd = 2)
#abline(v = c(2, 4, avg_score, 8), 
#       col = c("#F16913dd","#fd7d00bb", "#FDD0A2aa", "#efefef"), 
#       lwd = 3)
lines(x=c(2,2), y=c(-1,max(y)+0.5), col="#F16913dd", lwd=3)
lines(x=c(4,4), y=c(-1, max(y)+0.5), col="#fd7d00bb", lwd=3)
lines(x=c(6,6), y=c(-1, max(y)+0.5), col="#FDD0A2aa", lwd=3)
lines(x=c(8,8), y=c(-1, max(y)+0.5), col="#efefef", lwd=3)
text(x + 0.05, y, adj=0, cex = 1.1, 
     labels = avg_health_categ_df$category[ord_avg_health],
     xpd = TRUE)
mtext("Average Health Score by Product Category", side=3,
      line=-0.5, at=0, adj=0, cex=1.5)
mtext("Average Health Score", side=1,
      line=2)
par(op)
dev.off()


## Export data for D3js
# expor data to csv file
category_score = data.frame(
  name = avg_health_categ_df$category[ord_avg_health],
  score = avg_health_categ_df$avg[ord_avg_health],
  color = barcol)

write.table(
  category_score, sep = ",",
  "/Users/Gaston/Documents/Insight/insightfl/app/static/category_score.csv",
  row.names = FALSE)


# =============================================================
# Number of Ingredients
# =============================================================

# Average Score by Category
avg_ings_categ_df = ddply(
  toydata, 
  .(category),
  summarize,
  avg_score = mean(health),
  avg_ings = mean(num_ings),
  avg_high = mean(num_high),
  avg_medium = mean(num_medium),
  avg_low = mean(num_low),
  avg_all = mean(num_all))

avg_ings_categ_df 


# Avg All Toxic Ings -vs- Avg Score
lpos = c(
  4, 4, 4, 4, 3, 4, 4, 3, 3, 4, 
  4, 4, 2, 4, 1, 1, 2, 2, 2, 1,
  4, 2, 4, 1, 4, 4, 1, 2, 2, 1, 
  4, 2, 1, 4, 2, 4)

ylgnbu = brewer.pal(9, "Oranges")[7:1]
ylgnbu[8] = "#393939"
mycol <- colorRampPalette(ylgnbu)(diff(c(1.72, 9)))
barcol = mycol[avg_ings_categ_df$avg_score]
barcol = paste(barcol, "dd", sep='')

png("/Users/Gaston/Desktop/num_ings_score.png", 
    width=900, height=700, res=75)
op = par(mar = c(4, 4.5, 3, 2))
with(avg_ings_categ_df, 
     plot(avg_ings, avg_score, col=barcol, cex=1.2*avg_all, 
          pch=19, xlim=c(4,28), ylim=c(0,10), 
          xlab = "", ylab = "", axes=FALSE))
axis(side=1, line=-1.4, col="white", col.ticks="gray20",
     col.axis="gray20")
axis(side=2, line=0, las=2, col="gray30", col.axis="gray20")
mtext(text = "Average number of ingredients", side=1, line=1.5)
mtext(text = "Average Health Score", side=2, line=2)
mtext("The more ingredients, the lower the Health Score", 
      side=3, col="gray15",
      cex=1.5, line=0.1, at=3, adj=0)
abline(h=0, v=0)
#with(avg_ings_categ_df, 
#     text(avg_ings, avg_score, labels=rownames(avg_ings_categ_df), 
#          adj=0, cex=0.8, xpd=TRUE))
with(avg_ings_categ_df, 
     text(avg_ings, avg_score, labels=abbreviate(category, 12),
          adj=0, cex=1.1, pos=lpos, xpd=TRUE))
legend(x=23.5, y=9.5, legend=c(" <= 2", "", " 3 - 4", "", " >= 5"), 
       pch=19,
       title = "Avg num of risky \ningredientss", bty="n",
       pt.cex=c(2,0,2.7,0,3.7), 
       col=c("#FEE6CE","white", "#FDAE6B", "white", "#D94801"))
par(op)
dev.off()



## Export data for D3js
# expor data to csv file
category_ings_score = data.frame(
  name = avg_ings_categ_df$category,
  ingredients = round(avg_ings_categ_df$avg_ings, 2),
  score = round(avg_ings_categ_df$avg_score, 2),
  color = barcol)

write.table(
  category_ings_score, sep = ",",
  "/Users/Gaston/Documents/Insight/insightfl/app/static/category_ings_score.csv",
  row.names = FALSE)
