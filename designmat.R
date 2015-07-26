designmat <- function(x) {
        data <- read.csv("Hello.mat", header = FALSE, stringsAsFactors = FALSE)
        # Codes for update:
                ori <- data$V1[5:length(data$V1)] # Extract the original matrix data
                cov <- as.character(c(1:10)) # Obtain the covariate data (This is just an example), need update based on the specific data
                data2 <- data.frame(cbind(ori, cov)) # Create a matrix by column-combining the original matrix data and the covariate data, and convert to dataframe
                cols <- names(data2) # Define the columns to be pasted
                data2$x <- apply( data2[ , cols] , 1, paste , collapse = " " ) # Combine the two components through paste function
                data3 <- data2[,!(names(data2) %in% cols)] # Update the dataframe with the previous two components excluded
                data4 <- c(data$V1[1:4], data3) # Create a character composed of strings of texts and data matrix
                        # Need to configure and update the text information, instead of using the old text information
        # Export data:        
        write.table(data4, paste(x,".mat", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)
}