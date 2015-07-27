designmat <- function(filename = "design", Covdata = "Covariate.xlsx", demean = TRUE) {
## This is a function written to generate design.mat file automatically
## for TBSS analysis. A successful execution of this function requires
## 1) Using "design_ttest2 design" to first create a design.mat;
## 2) Create a "Covariate.xlsx" file and put in the same directory.

## Variables:
##      filename: the name of the file to be saved, whose default is set to "design"
##      Covdata: the file containing the covariate data, which should be a string name
##      demean: the option allowing the covariate to be demeaned if not

## The "demean" call is not very specific, as some covariate might not
## need to be demeaned, so this will be updated in the future.
        
## Don't know if two spaces will affect the permutation, remains to be tested

# Read in the data
data <- read.csv("design.mat", header = FALSE, stringsAsFactors = FALSE)

# Read in the covariate sheet
Covdata <- read.xlsx(Covdata, sheetIndex = 1)
        # Demean the covariates
        if (demean == TRUE) {
                demean <- function(y) { # A function performing demean
                        mCovdata <- apply(Covdata, 2, mean) # Calculate the mean of the covariate
                        dmCovdata <- Covdata - mCovdata
                        return(dmCovdata)
                }
                Covdata <- demean()
        }

# Create heading with NumWaves, NumContrasts, and PPheights configured
NumWaves <- paste("/NumWaves", 2 + ncol(Covdata))
NumPoints <- paste("/NumPoints", nrow(Covdata))
PPheights <- paste("/PPheights", "1 1")
Matrix <- "/Matrix"
heading <- c(NumWaves, NumPoints, PPheights, Matrix)

# Extract original matrix data
ori <- data$V1[5:length(data$V1)]
data2 <- cbind(ori, Covdata) # Create a matrix by column-combining the original matrix data and the covariate data, and convert to dataframe
cols <- names(data2) # Define the columns to be pasted
data2$x <- apply( data2[ , cols] , 1, paste , collapse = " " ) # Combine the two components through paste function
data2 <- data2[,!(names(data2) %in% cols)] # Update the dataframe with the previous two components excluded
data3 <- c(heading, data2) # Create a character composed of strings of texts and data matrix
data3 <- data.frame(cbind(data3)) # Create a matrix combining texts and data matrix by columns, and convert the matrix to dataframe

# Export data:        
write.table(data3, paste(filename,".mat", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)
}