library(ggplot2)
library(scales)
library(corrplot)
library(psych)
library(gplots)
library(vcd)

library(data.table)
TP <- fread("TP.csv", stringsAsFactors = T)

#plot of TP for all vars
library(plyr)
library(psych)
main = c("A","B","C","D","E","F","G","H","I","J","K")
multi.hist(TP,dcol="blue",main= c("a","b")) 

prod$aisle_id=NULL
prod$department_id=NULL

TP = merge(TP,prod, by = "product_id")

# how many items are in the orders
TP %>% 
  group_by(order_id) %>% 
  summarize(num_of_items = last(add_to_cart_order)) %>%
  ggplot(aes(x=num_of_items))+
  geom_histogram(stat="count",fill="gold") + 
  geom_rug() + 
  coord_cartesian(xlim=c(0,80))

#prod name
ggplot(data=TP, aes(TP$product_name)) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=5))+
  theme_bw() +
  xlab("Products") +
  ylab("Count")+
  ggtitle("Products") +
  theme(text=element_text(size=15))+
  geom_histogram(col="black", 
                 fill="gold",
                 aes(fill="...count..."))


#missing value 1
library("VIM")
aggr(TP)

#missing value 2
library(Amelia)
missmap(TP)
missmap(prior, legend = TRUE, col = c("navyblue","cornsilk"), main = "Missing Values",
        y.labels="At", y.at=1,
        y.cex = 0.8, x.cex = 0.8, csvar = NULL, tsvar =
          NULL, rank.order = TRUE)
