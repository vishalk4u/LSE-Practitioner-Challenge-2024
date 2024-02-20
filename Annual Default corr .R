#This works out simple averages coefficients ie average over companies and filters correlations
#Matrix's then applies the James-Steinz estimator for corr matrices (='average over time?')

#After applying cor.shrink to apply James-Steinz. What now? (Shakeel)

#Then the code calculates default probabilities and takes the correlation
# to create a 'default correlation matrix'. Uses annual data, going to try do
# it with daily data to see if better results, as idk why but the probability
# of default comes out really small.

#Need to check if excess returns are better to use than only returns (Archange)

#Doubt: doesn't actually give log returns as if I run the 
# the function without type='log' then I get the same returns unless it 
# automatically works out log returns? Need to check. but on below website they
# said type= 'log' works in Example 1B.
# https://cran.r-project.org/web/packages/tidyquant/vignettes/TQ02-quant-integrations-in-tidyquant.html#example-1-use-quantmod-periodreturn-to-convert-prices-to-returns
# so let's just assume it works.

#Need to add rolling windows, I don't think you can apply the rolling window
# corr to a matrix so I think, say we had 3 windows need to get 3 corr matrixs,
# each one showing the corr, for that window. obv if we had say many more windows than 3
# then we would end up with ALOT of matrices. (Jiayi and Shakeel)

#Need to increase sample size, when increase data time frame to say 2004-2020, need to 
# filter out incomplete data as was getting NA's therefore James-Steinz was not working, 
# I fixed this by replacing NA's with 0, but I think need sample size to increase in order for 
# the shrinkage intensity to be not 1. ie need more companies!

#Code might not run when the 'length of log returns' is not the same for companies, it leads 
# to a single row+column being all 0, not a big issue as other values remain therefore can ignore.

#So in order for lengths to be the same,
# Need interpolation (Jiayi)


library(tidyquant)
library(timetk)
library(tidyverse)
library(corpcor)

# Define the function to handle missing values in correlation matrices
handle_missing_values <- function(mat) {
  # Replace NA values with 0
  mat[is.na(mat)] <- 0
  return(mat)
}

# Setting our stock symbols to a variable
tickers <- c("VALE", "PBR", "ITUB", "ABCB", "EC", "AMX", "CX", "MSFT", "AAPL", "GOOGL", "TSLA", "BAC","AVAL")

# Download the stock price data
multpl_stocks <- tq_get(tickers,
                        from = "2014-01-01",
                        to = "2019-01-01",
                        get = "stock.prices")

# Remove rows with missing values from multpl_stocks, instead need to 
# interpolate using zoo package as Shakeel said.
multpl_stocks <- multpl_stocks %>%
  na.omit()

company_info <- data.frame(
  symbol = c("VALE", "PBR", "ITUB", "ABCB", "EC", "AMX", "CX", "MSFT", "AAPL", "GOOGL", "TSLA", "BAC","AVAL"),
  Sector = c("Agribusiness", "Energy", "Bank", "Bank", "Energy", "Telecommunications", "Construction", "Technology", "Technology", "Technology", "Technology", "Bank","Services"),
  Fsector = c("Non_Financial", "Non_Financial", "Financial", "Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Non_Financial", "Financial","Non_Financial"),
  Country = c("Brazil", "Brazil", "Brazil", "Brazil", "Colombia", "Mexico", "Mexico", "USA", "USA", "USA", "USA", "USA","Mexico")
)

company_info

# Calculating the annual log returns for multiple stocks
multpl_stock_log_returns <- multpl_stocks %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'yearly', 
               type = "log",
               col_rename = 'returns')

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

#/////////////////////////////////////////////////////////////////////////
#Setting up data for default proability calculations for default correlation

#Function to calculate Distance-to-default

dtd <- function(mcap, debt, vol, r){
  if(debt==0) stop("Please provide a non-zero debt value")
  stopifnot(is.numeric(mcap),
            is.numeric(debt),
            is.numeric(vol),
            is.numeric(r))
  
  rho <- 1                 # forbearance
  Maturity <- 1
  
  ## Starting values of firm's market value and its volatility
  seed.V <- mcap + debt
  seed.sV <- mcap * vol / debt
  
  # Present value of debt
  debt <- debt * exp(-r)
  
  # Solving reverse Black-Scholes for market value of asset and
  # asset volatility  
  d1 <- function(V, debt, sV, Maturity) {
    num <- log(V/debt) + 0.5*sV*sV*Maturity
    den <- sV * sqrt(Maturity)
    num/den
  }
  
  d2 <- function(V, debt, sV, Maturity) {
    d1(V, debt, sV, Maturity) - sV*sqrt(Maturity)
  }
  
  
  ## Feed this function the parameter vector x.
  ## Error term computation:
  ## It returns the sum of squared errors for the two equations
  ## x[1] is V and x[2] is sV. 
  
  objective.function <- function(x, mcap, vol, debt, rho, Maturity){
    
    e1 <- -mcap + x[1]*pnorm(d1(x[1], debt*rho, x[2], Maturity)) -
      rho*debt*pnorm(d2(x[1], rho*debt, x[2], Maturity))
    
    e2 <- -vol*mcap + x[2]*x[1]*pnorm(d1(x[1], debt*rho, x[2], Maturity))
    
    (e1*e1) + (e2*e2)
  }
  
  # Solve it - Minimizing the error term
  res <- optim(c(seed.V, seed.sV),
               method="L-BFGS-B",
               fn=objective.function,
               lower=c(mcap, 0), upper=c(Inf, Inf),
               mcap=mcap, vol=vol, debt=debt, Maturity=Maturity,
               rho=rho)
  
  # Distance-to-default calculation
  dtd.v <- (res$par[1] - debt)/(res$par[1]*res$par[2])
  
  # Probability of default calculation
  pd <- pnorm(-dtd.v)
  
  return(list("dtd.v"=dtd.v, "asset.v"=res$par[1], "sigma.v"=res$par[2], "pd"=pd))
}

#Now to data.
# Aggregate data to yearly level, to get yearly adjusted prices and volume
# here to get the yearly figures, takes a simple average of daily figures
multpl_stocks_yearly <- multpl_stocks %>%
  group_by(symbol, year = lubridate::year(date)) %>%
  summarise(adjusted_yearly = mean(adjusted),
            volume_yearly = sum(volume))

# Add market_value_equity column, adjusted*volume=market value equity
multpl_stocks_yearly <- multpl_stocks_yearly %>%
  mutate(market_value_equity = adjusted_yearly * volume_yearly)

# Calculate a scaling factor (e.g., 1 million) to reduce all equity values
scaling_factor <- 1e6
multpl_stocks_yearly <- multpl_stocks_yearly %>%
  mutate(scaled_market_value_equity = market_value_equity / scaling_factor)

#To get the yearly standard deviation = equity volatility
multpl_stock_monthly_returns <- multpl_stocks %>%
  group_by(symbol) %>%                        
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               type="log",
               col_rename = 'returns') 

multpl_stock_monthly_returns<-multpl_stock_monthly_returns %>%
  mutate(year = year(date)) %>%
  group_by(symbol, year) %>%
  summarise(sd = sd(returns))

# Merge the two tables of s.d = volatility and adjusted price & volume
merged_table <- multpl_stocks_yearly %>%
  left_join(multpl_stock_monthly_returns, by = c("symbol", "year")) %>%
  rename(equity_volatility = sd)

# Now we have set up a table containing all symbols, adj price, volume,
# equity_value, scaled_equity_value, equity_volatility

# We have a data.frame where we can automatically take the values
# of debt for MSFT and use the values of equity_volatility and scaled
# equity_value from the merged table to calculate the dtd function
# for each yr for MSFT.

# Create a dataframe to store debt data for each company
# You guys need to manually put in the data here,unless there's another way
debt_data <- data.frame(
  symbol = c("VALE","PBR","ITUB","ABCB","EC","AMX","CX","MSFT","AAPL","GOOGL","TSLA","BAC","AVAL"),  # Specify symbols for each company
  debt_2014 = c(15976,60137,41838.65,72.103,8464.915,18547,8212.215,36121.0,14493.5,1614,9404,121569.5,2621),  # Provide debt data for each year for each company
  debt_2015 = c(19293,55780,38285.23,54.437,9729.945,16378.5,7975.62,38036.5,26664.5,997.5,9418.39,118382,2630),
  debt_2016 = c(17301.5,54298.5,33217.35,288.2745,31262.285,15078.5,6994.48,20278.5,37713.5,1967.5,5879.12,108411.5,3093.95),
  debt_2017 = c(14529,51124.5,35720.145,168.052,28802.5,16327.5,5031.84,13904.0,48603.5,1984.5,2068.378,113701,3200.75
  ),
  debt_2018 = c(1876.981, 11277,40335,31278.5,120.48,27234.175,13786,5429,10322.5,46867.5,2006,114696,3098.75)
)

# Create an empty dataframe to store results for all symbols
all_results <- data.frame(symbol = character(),
                          year = integer(),
                          dtd.v = numeric(),
                          asset.v = numeric(),
                          sigma.v = numeric(),
                          pd = numeric(),
                          stringsAsFactors = FALSE)

# Loop through each symbol
for (sym in unique(merged_table$symbol)) {
  # Filter the merged table to include only data for the current symbol
  symbol_data <- merged_table %>% filter(symbol == sym)
  
  # Get input debt values for the current symbol
  symbol_debt <- debt_data[debt_data$symbol == sym, ]
  debt_values <- c(symbol_debt$debt_2014, symbol_debt$debt_2015, symbol_debt$debt_2016, symbol_debt$debt_2017, symbol_debt$debt_2018)
  
  # Create a dataframe to store results for the current symbol
  symbol_results <- data.frame(symbol = rep(sym, 5),
                               year = unique(symbol_data$year),
                               dtd.v = NA,
                               asset.v = NA,
                               sigma.v = NA,
                               pd = NA,
                               stringsAsFactors = FALSE)
  
  # Loop through each year to calculate DTD, asset.v, sigma.v, and pd for the current symbol
  for (i in 1:nrow(symbol_results)) {
    # Subset data for the current year
    year_data <- symbol_data %>% filter(year == symbol_results$year[i])
    
    # Apply the dtd function to calculate DTD, asset.v, sigma.v, and pd
    dtd_result <- dtd(mcap = year_data$scaled_market_value_equity,
                      debt = debt_values[i],
                      vol = year_data$equity_volatility,
                      r = 0.05)
    
    # Store results in symbol_results dataframe
    symbol_results$dtd.v[i] <- dtd_result$dtd.v
    symbol_results$asset.v[i] <- dtd_result$asset.v
    symbol_results$sigma.v[i] <- dtd_result$sigma.v
    symbol_results$pd[i] <- dtd_result$pd
  }
  
  # Combine results for the current symbol with all_results dataframe
  all_results <- rbind(all_results, symbol_results)
}

# Print the results for all symbols, this shows each symbol and its dtd.v
#asset.v,sigma.v and p.d over the years 2014-2018
print(all_results)

#Now this code below creates the probabilty of default correlation matrix
# Get unique symbols
symbols <- unique(all_results$symbol)

# Create an empty matrix to store correlations
default_corr_matrix <- matrix(nrow = length(symbols), ncol = length(symbols), dimnames = list(symbols, symbols))

# Loop through each pair of symbols
for (i in 1:length(symbols)) {
  for (j in 1:length(symbols)) {
    # Get PD values for each symbol
    pd1 <- all_results$pd[all_results$symbol == symbols[i]]
    pd2 <- all_results$pd[all_results$symbol == symbols[j]]
    
    # Calculate correlation coefficient
    correlation <- cor(pd1, pd2)
    
    # Store correlation coefficient in the matrix
    default_corr_matrix[i, j] <- correlation
  }
}

# Print default correlation matrix
print(default_corr_matrix)


