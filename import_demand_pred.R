# load pre-requisites
setwd("F:/M.Sc. Semester III/Applied International Trade/Term Paper")
library(xlsx)
library(forecast)
library(strucchange)


# load and clean the data
quarterlyImports <- read.xlsx(file = 'trade_India_data.xlsx', sheetName = 'Trade_data',rowIndex = 8239:8319, colIndex = c(9,17), header = FALSE)
quarterlyGDP <- read.xlsx(file='trade_India_data.xlsx', sheetName = 'Trade_data', rowIndex = 873:953, colIndex = c(9,17), header = FALSE)
# rename column names for quarterly data
colnames(quarterlyGDP) <- c('Year','GDP')
colnames(quarterlyImports) <- c('Year','Imports')

# convert it into a time series 'ts' object
quarterly_imports <- ts(quarterlyImports$Imports, start = c(1996,2), end = c(2016,2), frequency = 4)
quarterly_gdp <- ts(quarterlyGDP$GDP, start = c(1996,2), end = c(2016,2), frequency = 4)


# diagnostic checks by plotting
plot(diff(log(quarterly_imports)), xaxt = 'n', col = 'red', ylab = 'diff(log(x))')
axis(1,at=seq(1996,2016,2))
lines(diff(log(quarterly_gdp)), col = 'blue')

plot(diff(log(quarterly_gdp)), xaxt = 'n')
axis(1,at=seq(1996,2016,2))


# check number of differences it would take for the data to become stationary
ndiffs(quarterly_imports)
ndiffs(log(quarterly_imports))

## check for structural breaks in quarterly_imports
bp.quarterly_imports <- breakpoints(quarterly_imports ~ 1)
summary(bp.quarterly_imports)

## the BIC chooses 5 breakpoints though
plot(bp.quarterly_imports)
breakpoints(bp.quarterly_imports)
plot(rice)
plot(quarterly_imports)
lines(breakpoints(bp.quarterly_imports))

## check for structural breaks in quarterly_imports
