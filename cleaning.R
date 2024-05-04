d <- marketing_campaign

#cleaning the data
character_columns <- sapply(d, is.character)
numeric_candidates <- character_columns & !sapply(names(d), function(x) x %in% c('Education', 'Marital_Status', 'Dt_Customer'))
d[, numeric_candidates] <- lapply(d[, numeric_candidates], as.numeric)

d$Kids <- as.numeric(d$Kidhome) + as.numeric(d$Teenhome)

d$Relationship <- 0
d$Relationship[d$Marital_Status %in% c("Together", "Married")] <- 1

d$Family_Size <- 0
for (i in 1:nrow(d)) {
  # Check if the marital status is 'In relationship'
  if (d$Relationship[i] == 1) {
    # Calculate family size if in a relationship
    d$Family_size[i] <- 2 + d$Kids[i]
  } else {
    # Calculate family size if not in a relationship
    d$Family_size[i] <- 1 + d$Kids[i]
  }
}

d$Dt_Customer <- as.Date(d$Dt_Customer)
d$Age <- 2024 - as.numeric(d$Year_Birth)
d$Collected <- as.Date("2014-12-07")
d$Days_is_client <- as.numeric(difftime(d$Collected, d$Dt_Customer, units = "days"))

d$MntTotal <- as.numeric(d$MntWines) + as.numeric(d$MntFruits) + as.numeric(d$MntMeatProducts) + as.numeric(d$MntFishProducts) + as.numeric(d$MntSweetProducts) + as.numeric(d$MntGoldProds)
d$NumAllPurchases <- as.numeric(d$NumWebPurchases) + as.numeric(d$NumCatalogPurchases) + as.numeric(d$NumStorePurchases)
d$AverageCheck <- round((as.numeric(d$MntTotal) / as.numeric(d$NumAllPurchases)), 1)
d$ShareDealsPurchases <- round((as.numeric(d$NumDealsPurchases) / as.numeric(d$NumAllPurchases)) * 100, 1)
d$TotalAcceptedCmp <- as.numeric(d$AcceptedCmp1) + as.numeric(d$AcceptedCmp2) + as.numeric(d$AcceptedCmp3) + as.numeric(d$AcceptedCmp4) + as.numeric(d$AcceptedCmp5) + as.numeric(d$Response)

median <- median(d$Income, na.rm = TRUE)
mean <- mean(d$Income, na.rm = TRUE)
dif <- mean - median
cat(paste("Income Median:", round(median), "\nIncome Mean:", round(mean), "\ndifference:", round(dif), "\n"))
d$Income[is.na(d$Income)] <- median

columns_to_drop <- c('ID', 'Year_Birth', 'Kidhome', 'Teenhome', 'Dt_Customer', 'Z_CostContact', 'Z_Revenue', 'Collected')
d <- d[, !names(d) %in% columns_to_drop]

d$Income <- ifelse(d$Income > 120000, 120000, d$Income)
d$AverageCheck <- ifelse(d$AverageCheck > 200, 200, d$AverageCheck)

d$ActiveDays <- d$Days_is_client - d$Recency

#write.csv(d, "C:\\Users\\seane\\Downloads\\dssl_project.csv")
