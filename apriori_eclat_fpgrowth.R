#load required library
library(arules)
library(arulesViz)
library(RColorBrewer)
data = read.transactions("Market_Basket_Optimisation.csv", header = FALSE)
str(data)

#applying apriori() function
rules <- apriori(data, parameter = list(supp = 0.01, conf = 0.2))

#applying inspect() function
inspect(rules[1:10])

#applying itemFrequencyPlot() function
arules::itemFrequencyPlot(data, topN = 20, 
                          col = brewer.pal(8, 'Pastel2'),
                          main = 'Relative Item Frequency Plot',
                          type = "relative",
                          ylab = "Item Frequency (Relative)")


#applying eclat() function
rules = eclat(data, parameter = list(support = 0.003, minlen = 2 ))

inspect(sort(rules, by = 'support')[1:10])

rules = eclat(data, parameter = list(support = 0.004, minlen = 2 ))

inspect(sort(rules, by = 'support')[1:10])