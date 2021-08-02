#read built in data groceries
library(arules)
class(Groceries)
inspect(head(Groceries, 3))
print(Groceries)
size(head(Groceries))

#convert 'transactions' to a list, note the LIST in CAPS
LIST(head(Groceries, 3))

#calculate frequent itemsets
frequentItems <- eclat (Groceries, parameter = list(supp = 0.07, maxlen = 15)) 

# calculates support for frequent items
inspect(frequentItems)


#plot frequent items
itemFrequencyPlot(Groceries, topN = 10, type = "absolute", main = "Item Frequency")

#Min support as 0.001, confidence as 0.8
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5))

#high- confidence rules.
rules_conf <- sort(rules, by = "confidence", decreasing = TRUE)

#show the support, lift and confidence for all rules
inspect(head(rules_conf))

#high-lift rules
rules_lift <- sort(rules, by = "lift", decreasing = TRUE)

#show the support, lift and confidence for all rules
inspect(head(rules_lift))

#maxlen = 3 limits the elements in a rule to 3
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.5, maxlen = 3))
inspect(rules[1:10])

#get subset rules in vector
subsetRules <- which(colSums(is.subset(rules, rules)) > 1)
length(subsetRules)    # > 3913 

#remove subset rules
rules <- rules[-subsetRules]

#get rules that lead to buying 'whole milk'
rules <- apriori(data = Groceries, parameter = list(supp = 0.001, conf = 0.08),
                 appearance = list(default = "lhs", rhs = "whole milk"),
                 control = list(verbose = F))
inspect(rules[1:10])

# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))

# those who bought 'milk' also bought..
rules <- apriori (data=Groceries, 
                  parameter=list (supp=0.001,conf = 0.15,minlen=2), 
                  appearance = list(default="rhs",lhs="whole milk"), 
                  control = list (verbose=F)) 

inspect(rules)

# 'high-confidence' rules.
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) 
inspect(head(rules_conf))