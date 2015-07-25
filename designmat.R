designmat <- function(x) {
        data <- read.csv("Hello.mat", stringsAsFactors = FALSE)
        write.table(data, paste(x,".mat", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)
        # Modify data to add in covariate
        data$V1[4] <- paste(1,0,100)
        # Current solution: extract the first 4 rows from original data, convert to matrix by cbind,
        # create a covariate matrix containing characters (also using cbind), rbind the two matrix,
        # convert to data frame, and then write into a file.
        # The 4 rows need to be updated since the new covariate matrix will contain different numbers.
}