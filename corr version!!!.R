#This works out simple averages coefficients ie average over companies and filters correlations
#Matrix's then applies the James-Steinz estimator for corr matrices (='average over time?')
#Need to check if excess returns are better to use than only returns, ask Arcahnge when he's back

#After applying cor.shrink to apply James-Steinz. What now? Shakeel

#Need to add rolling windows, I Don't think you can apply the rolling window
#corr to a matrix so I think, say we had 3 windows need to get 3 corr matrixs,
#each one showing the corr, for that window. obv if we had say many more windows than 3
#then we would end up with ALOT of matrices. Jiayi and Shakeel

#Need to increase sample size, when increase data time frame to say 2004-2020, need to 
#Filter out incomplete data as was getting NA's therefore James-Steinz was not working, 
#I fixed this by replacing NA's with 0, but I think need sample size to increase in order for 
#the shrinkage intensity to be not 1. ie need more companies!

#Code might not run when the 'length of log returns' is not the same for companies, it leads 
#to a single row+column being all 0, not a big issue as other values remain therefore can ignore.

library(tidyquant)
library(corpcor)
library(tidyverse)
library(timetk)

# Define the function to handle missing values in correlation matrices
handle_missing_values <- function(mat) {
  # Replace NA values with 0
  mat[is.na(mat)] <- 0
  return(mat)
}

# Setting our stock symbols to a variable
tickers <- c("VALE", "PBR", "ITUB", "ABCB", "GIVSY", "EC", "AMX", "CX", "MSFT", "AAPL", "GOOGL", "TSLA", "BAC","AVAL")

# Download the stock price data
multpl_stocks <- tq_get(tickers,
                        from = "2013-01-01",
                        to = "2018-03-01",
                        get = "stock.prices")

#maybe we need to add a definition here because sometimes companies like AXTELCPO do not show
#up in the matrix or company_info at all, maybe because they are not in the 'tidy-quant' package

# Remove rows with missing values from multpl_stocks
multpl_stocks <- multpl_stocks %>%
  na.omit()

company_info <- data.frame(
  symbol = c("VALE", "PBR", "ITUB", "ABCB", "GIVSY", "EC", "AMX", "CX", "MSFT", "AAPL", "GOOGL", "TSLA", "BAC","AVAL"),
  Sector = c("Agribusiness", "Energy", "Bank", "Bank", "Insurance", "Energy", "Telecommunications", "Construction", "Technology", "Technology", "Technology", "Technology", "Bank","Services"),
  Fsector = c("Non_Financial", "Non_Financial", "Financial", "Financial", "Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Financial","Non_Financial"),
  Country = c("Brazil", "Brazil", "Brazil", "Brazil", "Colombia", "Colombia", "Mexico", "Mexico", "USA", "USA", "USA", "USA", "USA","Mexico")
)

company_info

# Calculating the log returns for multiple stocks
multpl_stock_log_returns <- multpl_stocks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',  # Change to daily returns
               type = "log",
               col_rename = 'returns') %>%
  mutate(returns = returns * 252)  # Convert from daily to annual returns

# Find the minimum length of log returns
min_length <- multpl_stock_log_returns %>%
  group_by(symbol) %>%
  summarise(min_length = min(n()))

# Calculating the correlation
company_corr <- multpl_stock_log_returns %>%
  spread(symbol, value = returns) %>%
  tk_xts(silent = TRUE) %>%
  cor()

# Handle missing values in correlation matrices
company_corr <- handle_missing_values(company_corr)

# Print the correlation matrix
print(company_corr)

# Initialize a matrix of zeros with the same dimensions as the correlation matrix
filtered_corr_global <- matrix(0, nrow = nrow(company_corr), ncol = ncol(company_corr))

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
      filtered_corr_global[i, j] <- company_corr[i, j]
    }
  }
}

# Print the filtered correlation matrix
print(filtered_corr_global)

#calculate simple average ie average over companies
non_empty_cells <- sum(filtered_corr_global != 0, na.rm = TRUE)
sum_of_correlations <- sum(filtered_corr_global, na.rm = TRUE)
avg_correlation_global <- sum_of_correlations / non_empty_cells

print(avg_correlation_global)
#This is the average correlation for global i.e different country, different sector

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
#Initialize a matrix of zeros with the same dimensions as the correlation matrix
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
#//////////////////////////////////////////////////////////////////////////////////////

#This outputs a matrix after applying James- Steinz to show some idea for the average 'over time'
#For the global correlation coefficient:
#need to get a lambda first, assuming uniform weight (look into what this means)
filtered_corr_global
L1<-estimate.lambda(filtered_corr_global,verbose = TRUE)
L1
cor.shrink(filtered_corr_global,lambda =L1)

#James-Steinz applied country correlation coefficient(=average over time?)
filtered_corr_country
L2<-estimate.lambda(filtered_corr_country,verbose = TRUE)
L2
cor.shrink(filtered_corr_country,lambda =L2)

#James-Steinz applied financial correlation coefficient (=average over time?)
filtered_corr_financial
L3<-estimate.lambda(filtered_corr_financial,verbose = TRUE)
L3
cor.shrink(filtered_corr_financial,lambda =L3)

#James-Steinz applied non financial correlation coefficient(=average over time?)
filtered_corr_non_financial
L4<-estimate.lambda(filtered_corr_non_financial,verbose = TRUE)
L4
cor.shrink(filtered_corr_non_financial,lambda =L4)