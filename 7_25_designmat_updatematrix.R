# Read in the data
data <- read.csv("Hello.mat", header = FALSE, stringsAsFactors = FALSE)
# Read in the covariate sheet
Covdata <- read.xlsx("Covariate.xlsx", sheetIndex = 1)
        demean <- function(y) { # A function performing demean
                mCovdata <- apply(Covdata, 2, mean) # Calculate the mean of the covariate
                dmCovdata <- Covdata - mCovdata
                return(dmCovdata)
        }
        Covdata <- demean()
# Codes for creating a new heading
# Configure NW, NP, PP
makehead <- function(NW = 2 + ncol(Covdata), NP = nrow(Covdata), PP = "1 1") { # A function creating the heading
        NumWaves <- paste("/NumWaves", NW)
        NumPoints <- paste("/NumPoints", NP)
        PPheights <- paste("/PPheights", PP)
        Matrix <- "/Matrix"
        heading <- c(NumWaves, NumPoints, PPheights, Matrix)
        return(heading)
}
heading <- makehead() # Assign the output of makehead function to headingf
ori <- data$V1[5:length(data$V1)] # Extract the original matrix data
data2 <- cbind(ori, Covdata) # Create a matrix by column-combining the original matrix data and the covariate data, and convert to dataframe
cols <- names(data2) # Define the columns to be pasted
data2$x <- apply( data2[ , cols] , 1, paste , collapse = " " ) # Combine the two components through paste function
data3 <- data2[,!(names(data2) %in% cols)] # Update the dataframe with the previous two components excluded
data4 <- c(heading, data3) # Create a character composed of strings of texts and data matrix
data4 <- data.frame(cbind(data4)) # Create a matrix combining texts and data matrix by columns, and convert the matrix to dataframe