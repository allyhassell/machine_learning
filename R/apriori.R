## STAT318/462 Lab 6: Association Analysis using the Apriori algorithm


library(arules)

# convert the .csv data to a list of transactions
data = read.transactions(file = "Basket1.txt", format = "basket", sep = ",")
inspect(data)


# mine the frequent itemsets
FreqItemsets = apriori(data,
                       parameter = list(support = 0.4, target = "frequent itemsets"))

# view all the frequent itemsets
inspect(FreqItemsets)

# view the five frequent itemsets with the greatest support
inspect(sort(FreqItemsets, by = "support")[1:5])
itemFrequencyPlot(data, support = 0.1, cex.names = 0.8)


# mine the maximal frequent itemsets
MaxFreqItemsets = apriori(data,
                          parameter = list(support = 0.3,
                                           target = "maximally frequent itemsets"))

# view all the maximal frequent itemsets
inspect(MaxFreqItemsets)
summary(MaxFreqItemsets)


# mine the closed frequent itemsets
ClosedFreqItemsets = apriori(data,
                             parameter = list(support = 0.4,
                                              target = "closed frequent itemsets"))

# view all the closed frequent itemsets
summary(ClosedFreqItemsets)


# mine the association rules
rules = apriori(data,
                parameter = list(minlen = 2, support = 0.4,
                                 confidence = 0.8, target = "rules"))
# view all the rules
inspect(rules)
# view the five rules with the highest confidence
summary(rules)
inspect(sort(rules, by = "confidence")[1:5])


# find particular rules 
rules = apriori(data,
                 parameter = list(minlen = 2, support = 0.4,
                                  confidence = 0.8, target = "rules"),
                 appearance = list(default = "lhs", rhs = "Milk"))
summary(rules)
inspect(sort(rules, by = "confidence"))

# Use ?apriori, then click on 'APappearance' to see how to extract more complicated rules

########################################################################################
# These are much larger, real-world data sets.
# 1. Adult Census Income Database
data("Adult")
Adult

# 2. Groceries data set contains 1 month (30 days) of real-world point-of-sale 
# transaction data
data("Groceries")
Groceries


# 3. The **RealTransactionData_NovDec_2019.xlsx** data set contains 2 months of 
# real-world point-of-sale transaction data.
# This is a real dataset provided by Thomas Li, and obtained from an industry contact, 
# so it may be used for learning and research work at UC only.

# Please see the ReadMe file for data column info. You will need the Transaction ID 
# to match the transactions, and the Product Code to match the product - some data 
# cleaning is needed!
  
# Thomas has cleaned the November 2019 data and stored it in the file 
# **RealTransactionData_Nov_2019.csv**, with two columns: "Transaction ID", 
# and "Product Code". You can read in this transaction data as follows:
order_Nov19_trans = read.transactions(
  file = "RealTransactionData_Nov_2019.csv",
  format = "single",
  sep = ",",
  cols = c("Transaction ID", "Product Code"),
  rm.duplicates = T,
  header = T
)
summary(order_Nov19_trans)


# Finding frequent itemsets, with 2% occurance or more:

FreqItemsets = apriori(order_Nov19_trans,
                        parameter = list(support = 0.02, target = "frequent itemsets"))
inspect(FreqItemsets)

# You could now find frequent 2 and 3 itemsets, for example, and the association rules.

# To repeat this analysis for the December data, you would have to do the data 
# cleaning yourself :)

# Then report this back in terms of Product Name rather than Product Code.

# END OF THE LAB

