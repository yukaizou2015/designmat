# Combine multiple rows in dataframe
# URL: http://stackoverflow.com/questions/14568662/paste-multiple-columns-together-in-r
# your starting data..
datatest <- data.frame('a' = 1:3, 'b' = c('a','b','c'), 'c' = c('d', 'e', 'f'), 'd' = c('g', 'h', 'i')) 

# columns to paste together
cols <- c( 'b' , 'c' , 'd' )

# create a new column `x` with the three columns collapsed together
datatest$x <- apply( datatest[ , cols ] , 1 , paste , collapse = "-" )

# remove the unnecessary rows
datatest <- datatest[ , !( names( datatest ) %in% cols ) ]