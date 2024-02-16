#this works out simple averages coefficients ie average over companies
#need to convert monthly returns to annual returns
#need to check if cumulative returns means log returns because i think this uses simple returns
#and we need to use log returns, if cumulative=log then i can easy fix it
#need to check if excess returns are better to use than only returns, ask Arcahnge when he's back
#need to add rolling windows, Jiayi and Shakeel
#this code generates filtered matrixes for the correlations which is what I think Shakeel was
#saying, and now we have to apply the James- Steinz inequalilty to shrink them 
#then put it back into a matrix/ this is his idea of 'average over time?'
#then ask him what do we do now?
#need to increase sample size first by like 2 to test with brute force writing combos downs
#then increase sample and don't need to double check code works

library(tidyquant)
library(timetk)
library(tidyverse)
library(GGally)
# Setting our stock symbols to a variable
tickers <- c("VALE", "PBR", "ITUB", "ABCB", "GIVSY", "EC")

# Download the stock price data

multpl_stocks <- tq_get(tickers,
                        from = "2013-01-01",
                        to = "2018-03-01",
                        get = "stock.prices")

multpl_stocks[is.na(multpl_stocks)]

#categorize companies into sector,Fsector and country

company_info <- data.frame(
  symbol = c("VALE", "PBR", "ITUB", "ABCB", "GIVSY", "EC"),
  Sector = c("Agribusiness", "Energy", "Bank", "Bank", "Insurance", "Energy"),
  Fsector = c("Non_Financial", "Non_Financial", "Financial", "Financial", "Financial", "Non_Financial"),
  Country = c("Brazil", "Brazil", "Brazil", "Brazil", "Colombia", "Colombia")
)

company_info

#Calculating the monthly returns for multiple stocks
multpl_stock_monthly_returns <- multpl_stocks %>%
  group_by(symbol) %>%                             # We are grouping the stocks by symbol
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               col_rename = 'returns')

# Calculating the correlation
company_corr <- multpl_stock_monthly_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cor()

#print correlation matrix
company_corr

# Initialize a matrix of zeros with the same dimensions as the correlation matrix
filtered_corr <- matrix(0, nrow = nrow(company_corr), ncol = ncol(company_corr))

# Loop through each row of the correlation matrix
for (i in 1:nrow(company_corr)) {
  for (j in 1:ncol(company_corr)) {
    # Get the symbols of the corresponding companies
    symbol1 <- rownames(company_corr)[i]
    symbol2 <- colnames(company_corr)[j]
    
    # Get the country and sector information for each company
    country1 <- company_info$Country[company_info$symbol == symbol1]
    country2 <- company_info$Country[company_info$symbol == symbol2]
    sector1 <- company_info$Sector[company_info$symbol == symbol1]
    sector2 <- company_info$Sector[company_info$symbol == symbol2]
    
    # Check if the companies are from different countries and sectors
    if (country1 != country2 & sector1 != sector2) {
      # Keep the correlation value unchanged
      filtered_corr[i, j] <- company_corr[i, j]
    }
  }
}

# Print the filtered correlation matrix
print(filtered_corr)

#calculate simple average ie average over companies
non_empty_cells <- sum(filtered_corr != 0, na.rm = TRUE)
sum_of_correlations <- sum(filtered_corr, na.rm = TRUE)
avg_correlation <- sum_of_correlations / non_empty_cells

print(avg_correlation)
#this is the average correlation for global ie different country, different sector

#///////////////////////////////////////////////////////////////////////////////

#This is for average country correlation coefficient ie same country, different sector
# Initialize a matrix of zeros with the same dimensions as the correlation matrix
filtered_corr_country <- matrix(0, nrow = nrow(company_corr), ncol = ncol(company_corr))

# Loop through each row of the correlation matrix
for (i in 1:nrow(company_corr)) {
  for (j in 1:ncol(company_corr)) {
    # Get the symbols of the corresponding companies
    symbol1 <- rownames(company_corr)[i]
    symbol2 <- colnames(company_corr)[j]
    
    # Get the country and sector information for each company
    country1 <- company_info$Country[company_info$symbol == symbol1]
    country2 <- company_info$Country[company_info$symbol == symbol2]
    sector1 <- company_info$Sector[company_info$symbol == symbol1]
    sector2 <- company_info$Sector[company_info$symbol == symbol2]
    
    # Check if the companies are from the same country but different sectors
    if (country1 == country2 & sector1 != sector2) {
      # Keep the correlation value unchanged
      filtered_corr_country[i, j] <- company_corr[i, j]
    }
  }
}

# Print the filtered correlation matrix
print(filtered_corr_country)

# Calculate the simple average correlation over non-empty cells
non_empty_cells_country <- sum(filtered_corr_country != 0, na.rm = TRUE)
sum_of_correlations_country <- sum(filtered_corr_country, na.rm = TRUE)
avg_correlation_country <- sum_of_correlations_country / non_empty_cells_country

print(avg_correlation_country)

#/////////////////////////////////////////////////////////////////////////////////////////
#The average Financial correlation Coefficient
# Initialize a matrix of zeros with the same dimensions as the correlation matrix
filtered_corr_financial <- matrix(0, nrow = nrow(company_corr), ncol = ncol(company_corr))

# Loop through each row of the correlation matrix
for (i in 1:nrow(company_corr)) {
  for (j in 1:ncol(company_corr)) {
    # Get the symbols of the corresponding companies
    symbol1 <- rownames(company_corr)[i]
    symbol2 <- colnames(company_corr)[j]
    
    # Get the country and sector information for each company
    country1 <- company_info$Country[company_info$symbol == symbol1]
    country2 <- company_info$Country[company_info$symbol == symbol2]
    sector1 <- company_info$Sector[company_info$symbol == symbol1]
    sector2 <- company_info$Sector[company_info$symbol == symbol2]
    fsector1 <- company_info$Fsector[company_info$symbol == symbol1]
    fsector2 <- company_info$Fsector[company_info$symbol == symbol2]
    
    # Check if the companies are from the same country and the same sector (financial sector) but different symbols
    if (symbol1 != symbol2 & country1 == country2 & sector1 == sector2 & fsector1 == "Financial" & fsector2 == "Financial") {
      # Keep the correlation value unchanged
      filtered_corr_financial[i, j] <- company_corr[i, j]
    }
  }
}

# Print the filtered correlation matrix
print(filtered_corr_financial)

# Calculate the simple average correlation over non-empty cells
non_empty_cells_financial <- sum(filtered_corr_financial != 0, na.rm = TRUE)
sum_of_correlations_financial <- sum(filtered_corr_financial, na.rm = TRUE)
avg_correlation_financial <- sum_of_correlations_financial / non_empty_cells_financial

print(avg_correlation_financial)

#/////////////////////////////////////////////////////////////////////////////////////////////
#NoN-financial correlation coefficient
# Initialize a matrix of zeros with the same dimensions as the correlation matrix
filtered_corr_non_financial <- matrix(0, nrow = nrow(company_corr), ncol = ncol(company_corr))

# Loop through each row of the correlation matrix
for (i in 1:nrow(company_corr)) {
  for (j in 1:ncol(company_corr)) {
    # Get the symbols of the corresponding companies
    symbol1 <- rownames(company_corr)[i]
    symbol2 <- colnames(company_corr)[j]
    
    # Get the country and sector information for each company
    country1 <- company_info$Country[company_info$symbol == symbol1]
    country2 <- company_info$Country[company_info$symbol == symbol2]
    sector1 <- company_info$Sector[company_info$symbol == symbol1]
    sector2 <- company_info$Sector[company_info$symbol == symbol2]
    fsector1 <- company_info$Fsector[company_info$symbol == symbol1]
    fsector2 <- company_info$Fsector[company_info$symbol == symbol2]
    
    # Check if the companies are from the same country and the same sector (non-financial sector) but different symbols
    if (symbol1 != symbol2 & country1 == country2 & sector1 == sector2 & fsector1 == "Non_Financial" & fsector2 == "Non_Financial") {
      # Keep the correlation value unchanged
      filtered_corr_non_financial[i, j] <- company_corr[i, j]
    }
  }
}

# Print the filtered correlation matrix
print(filtered_corr_non_financial)

# Calculate the simple average correlation over non-empty cells
non_empty_cells_non_financial <- sum(filtered_corr_non_financial != 0, na.rm = TRUE)
sum_of_correlations_non_financial <- sum(filtered_corr_non_financial, na.rm = TRUE)
avg_correlation_non_financial <- sum_of_correlations_non_financial / non_empty_cells_non_financial

print(avg_correlation_non_financial)

#says NaN as that's true in the current 6 stocks there is no pairs that match this
#testing corpcor package to apply James-Steinz inequality, I think it worked?
filtered_corr_country
install.packages("corpcor")
library(corpcor)
help(package="corpcor")
#need to get a lambda first, assuming uniform weight
L<-estimate.lambda(filtered_corr_country,verbose = TRUE)
L
cor.shrink(filtered_corr_country,lambda =L)
