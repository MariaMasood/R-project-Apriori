getwd()
setwd("C:/Users/Dell/Desktop")
df_items <- read.csv("items.csv")
df_sorted <-df_items[order(df_items$CUSTOMER_ID), ]
df_sorted$CUSTOMER_ID <- as.numeric(df_sorted$CUSTOMER_ID) 
help("plyr")
install.packages("plyr", dependencies= TRUE)
if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}
library(plyr)
df_items <- ddply(df_items,c("CUSTOMER_ID","DATE"), function(df1)paste(df1$itemDescription, collapse = ","))
df_items$CUSTOMER_ID <- NULL
df_items$DATE <- NULL
#Rename column headers for ease of use
colnames(df_items) <- c("itemList")
write.csv(df_items,"ItemList.csv",quote = FALSE, row.names = TRUE)
install.packages("arules", dependencies=TRUE)
library(arules)

txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"));
basket_rules <- apriori(txn,parameter = list(sup = 0.01, conf = 0.5,target="rules"));

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
    detach(package:tm, unload=TRUE)
  }
           
inspect(basket_rules)
#Alternative to inspect() is to convert rules to a dataframe and then use View()
df_basket <- as(basket_rules,"data.frame")

library(arulesViz)
View(df_basket)
plot(basket_rules)
plot(basket_rules, method = "grouped", control = list(k = 5))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="graph", control=list(type="items"))
plot(basket_rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(basket_rules,measure=c("support","lift"),shading="confidence",interactive=T)
           itemFrequencyPlot(txn, topN = 5)