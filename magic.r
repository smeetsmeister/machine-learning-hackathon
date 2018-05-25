library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(plyr)

retail <- read_excel('test4.xlsx')
retail <- retail[complete.cases(retail), ]
retail <- retail %>% mutate(Description = as.factor(Description))
retail$Date <- as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
glimpse(retail)

retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                      function(df1)paste(df1$Description, 
                      collapse = ","))
itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")
write.csv(itemList,"test4.csv", quote = FALSE, row.names = TRUE)


tr <- read.transactions('test4.csv', rm.duplicates = FALSE, format = 'basket', sep=',')
tr
summary(tr)

rules <- apriori(tr, parameter = list(supp=0.001, conf=0.1))
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1:25])