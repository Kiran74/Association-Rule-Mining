## load the library arules

library(arules)

##library(arulesViz)

store <- read.csv("Global Superstore.csv")

#Transforming the original dataset into transaction level dataset 
#Subset the data for order id and sub category
store<-store[,c("Order.ID","Sub.Category")]

## write the data in to file
write.csv(store ,"retail.csv")

#Convert the transaction level data into transaction format using "arules" package 
#trans <- as(df, "transactions")
transactions <- read.transactions(file = "retail.csv",format = "single",sep = ",",cols = c("Order.ID","Sub.Category"),rm.duplicates = T)

# To get an intial understanding of the data set check the plot of items indicating their frequency
itemFrequencyPlot(transactions,type="absolute",topN=17)
## From the plot we can see that Binders are purchased very frequently

## From the business point of view of store, to increase the sales the frequently purchased items need
## to be placed where customers can access it very easily 

## Building association rules using apriori algorithm 
rules_retail <- apriori(transactions, parameter = list(support = 0.0001, confidence = 0.8))

## Check the number of rules are generated
rules_retail

## check the first 10 rules
inspect(rules_retail[1:10])

## rules are not sorted lets sort the rules using the confidence and lift
rules_retail_sorted<-sort(rules_retail,by=c("confidence","lift"),decreasing = T)

## check the first 10 rules
inspect(rules_retail_sorted[1:10])

## appliances and copiers are bought frequently with other combinations

## Building association rules using apriori algorithm for the max length 3
rules_retail_3 <- apriori(transactions, parameter = list(support = 0.0001, confidence = 0.8,maxlen=3))

## Check the number of rules are generated

rules_retail_3

## we see ther are 0 rules generated for up to 3 items maximum, so let us change the parameter confidence 
## to check are there any rules with minimum confidence

rules_retail_3 <- apriori(transactions, parameter = list(support = 0.0001, confidence = 0.3,maxlen=3))

## Check the number of rules are generated

rules_retail_3

## the rules are generated for each confidence levels are as follows:
## Confidence(0.1:1820 rules
## Confidence(0.2):391 rules
## Confidence(0.3):39 rules
## For the Confidence greater than 0.3 we are not getting any rules for the length of 3 itemsets

## check the first 10 rules
inspect(rules_retail_3[1:10])

## Building association rules using apriori algorithm  for maxlength 4
rules_retail_4 <- apriori(transactions, parameter = list(support = 0.0001, confidence = 0.8,maxlen=4))

## Check the number of rules are generated

rules_retail_4

## we see ther are 0 rules generated for up to 3 items maximum, so let us change the parameter confidence 
## to check are there any rules with minimum confidence

rules_retail_4 <- apriori(transactions, parameter = list(support = 0.0001, confidence = 0.3,maxlen=4))

## Check the number of rules are generated

rules_retail_4

## the rules are generated for each confidence levels are as follows:
## Confidence(0.1:1820 rules
## Confidence(0.2):391 rules
## Confidence(0.3):39 rules
## For the Confidence greater than 0.3 we are not getting any rules for the length of 4 itemsets

## check the first 10 rules
inspect(rules_retail_3[1:10])

## let us have look at summary of the rules generated with out the maximum length of items 

summary(rules_retail)

##rule length distribution (lhs + rhs):sizes
##5   6   7 
##131 234  27 
## here we can clearly see that rules are generated only with itemsets 5,6 and 7 when the confidence is 80


## check the first 10 rules after sorting
inspect(rules_retail_sorted[1:10])

## appliances and copiers are bought frequently with other combinations

## let us see when people are tend to buy appliances 
rulesAppliancesrhs <- subset(rules_retail, subset = rhs %in% "Appliances" & lift > 1.2)
inspect(head(rulesAppliancesrhs, by=c("confidence")))
## People frequently bought appliances irrespective of the combinations so, keeping the appliances at
## multiple places in the store will make accessible to customers and resulting in purchasing more often

rulesApplianceslhs <- subset(rules_retail, subset = lhs %in% "Appliances" & lift > 1.2)
inspect(head(rulesApplianceslhs, by=c("confidence")))


## let us see when people are tend to buy copiers
rulesCopiersrhs <- subset(rules_retail, subset = lhs %in% "Copiers" & lift > 1.2)
inspect(head(rulesCopiersrhs, by=c("confidence")))

### Fasteners

rulesFasteners <- subset(rules_retail, subset = rhs %in% "Fasteners" & lift > 1.2)

inspect(head(rulesFasteners, n=5, by="confidence"))

##Fasteners are bought most of the times when Furnishings are bought.

### Art
rulesArt <- subset(rules_retail, subset = rhs %in% "Art" & lift > 1.2)

inspect(head(rulesArt, n=5, by="confidence"))
## we can see that arts are bought more frequently with the tables

### Chairs
rulesChairs <- subset(rules_retail, subset = rhs %in% "Chairs" & lift > 1.2)

inspect(head(rulesChairs, n=5, by="confidence"))

## we can see that chairs are bought more frequently with the tables

### Tables
rulesTables <- subset(rules_retail, subset = lhs %in% "Tables" & lift > 1.2)

inspect(head(rulesChairs, n=5, by="confidence"))
## Chairs are bought more frequntly with the chairs


## Papers

rulesPaper <- subset(rules_retail, subset = rhs %in% "Paper" & lift > 1.2)

inspect(head(rulesPaper, by="confidence"))

## whenever the chairs and tables are bought people tend to buy papers

## Envelopes

rulesPaper <- subset(rules_retail, subset = rhs %in% "Envelopes" & lift > 1.2)

inspect(head(rulesPaper, by="confidence"))

## Binders
rulesBinders <- subset(rules_retail, subset = lhs %in% "Binders" & lift > 1.2)

inspect(head(rulesBinders, n=5, by="confidence"))

## Storage

rulesStorage <- subset(rules_retail, subset = rhs %in% "Storage" & lift > 1.2)

inspect(head(rulesStorage, n=5, by="confidence"))


## Furnishings

rulesFurnishings <- subset(rules_retail, subset = lhs %in% "Furnishings" & lift > 1.2)

inspect(head(rulesFurnishings, n=5, by="confidence"))

## Accessories
rulesAccessories <- subset(rules_retail, subset = rhs %in% "Accessories" & lift > 1.2)

inspect(head(rulesFurnishings, n=5, by="confidence"))

## By changing the confidence and support to mine the other rules 
## Building association rules using apriori algorithm 
rules_retail_1 <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.3))

## Check the number of rules are generated
rules_retail_1

## check the first 10 rules
inspect(rules_retail_1[1:10])

## rules are not sorted lets sort the rules using the confidence and lift
rules_retail_sorted_1<-sort(rules_retail_1,by=c("confidence","lift"),decreasing = T)

## check the first 10 rules
inspect(rules_retail_sorted_1[1:10])

## Binders are purchased very frequently with many of the items. 
## So, one of the strategies could be to make different sections of Binders at 
## different places of the store rather than placing all the concentration at one place

## Let us see what people are tend to buy when they are purchasing Binders
rulesBinders_1 <- subset(rules_retail_1, subset = lhs %in% "Binders" & lift > 1.2)

inspect(head(rulesBinders_1, n=5, by="confidence"))
## people are tend to buy storage when they purchase Binders

## Let us see what people are tend to buy when they are purchasing Storage
rulesStorage_1 <- subset(rules_retail_1, subset = lhs %in% "Storage" & lift > 1.2)

inspect(head(rulesStorage_1, by="confidence"))

## Let us see what people are tend to buy when they are purchasing Storage
rulesArt_1 <- subset(rules_retail_1, subset = lhs %in% "Art" & lift > 1.2)

inspect(head(rulesArt_1, by="confidence"))

## whenever people are buying art are tend to buy binders 


