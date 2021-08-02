#reading the data file
mydata <- read.csv("Cosmetics.csv", header = T, colClasses = "factor")
View(mydata)


# Finding association rules
library(arules)
rules <- apriori(mydata)
summary(rules)


# Rules with specified parameter valus
rules <- apriori(mydata,parameter = list(minlen = 2, maxlen = 3, supp = .7, conf = .8))
inspect(rules)


# Finding interesting rules-1
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=3, conf=.7),
                 appearance=list(rhs=c("Foundation=Yes"),default="lhs"))
inspect(rules)

#Visulaizing rules
library(arulesViz)
plot(rules)
plot(rules, method = "grouped")
plot(rules, method = "graph", control = list(type = "items"))

# Finding interesting rules-2
rules <- apriori(mydata,parameter = list(minlen=2, maxlen=5,supp=.1, conf=.5),
                 appearance=list(rhs=c("Foundation=Yes"),lhs=c("Bag=Yes", "Blush=Yes",
                                                               "Nail.Polish=Yes", "Brushes=Yes", "Concealer=Yes", "Eyebrow.Pencils=Yes",
                                                               "Bronzer=Yes", "Lip.liner=Yes", "Mascara=Yes", "Eye.shadow=Yes",
                                                               "Lip.Gloss=Yes", "Lipstick=Yes", "Eyeliner=Yes"),default="none"))

quality(rules)<-round(quality(rules),digits=3)
inspect(rules)

# Finding redundancy
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(redundant)

#Removing redundant rules
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

# Graphs and Charts
library(arulesViz)
plot(rules)
plot(rules,method="grouped")
plot(rules,method="graph")

arules::itemFrequencyPlot(mydata, topN = 20, col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frquency', type = "relative",
                          ylab = "Item Frequency (Relative)")
