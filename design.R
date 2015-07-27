design <- function(filename = "design", Covdata = "Covariate.xlsx", demean = TRUE) {
        designmat(filename, Covdata, demean)
        designcon(filename, NC)
}

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
                NC <<- 2 + ncol(Covdata) # For designcon function
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

designcon <- function(filename = "design", NC) {
        ## This is a function written to generate design.con file automatically
        ## for TBSS analysis. A successful execution of this function requires
        ## 1) Using "design_ttest2 design" to first create a design.con;
        ## 2) Specify how many contrasts are needed.
        ## In the future, both designcon.R and designmat.R will be merged so that
        ## one does not need to execute them separately to generate both files.
        
        ## Variables:
        ##      filename: the name of the file to be saved, whose default is set to "design"
        ##      NC: the number of contrasts needed. In design.R, NC do not need to be 
        ##          specified in the function since it will be configured using designmat function.
        
        # Read in the data
        data <- read.csv("design.con", header = FALSE, stringsAsFactors = FALSE)
        data2 <- data$V1[5:length(data$V1)] # Assign the matrix in data to data2 to avoid overwriting
        data2 <- data.frame(cbind(data2), stringsAsFactors = FALSE) # Convert the matrix to dataframe
        
        if (NC < 2) { # Determine if it worths to create the new design.con
                stop("Invalid Number of Contrasts, should be not less than 2")
        } else if (NC == 2) {
                data3 <- data
        } else {
                for (i in 1:(NC-2)) {
                        ncols <- length(strsplit(data2$data2[1], " ")[[1]]) # Get the number of columns of the matrix
                        ori <- data2$data2[1:length(data2$data2)] # Extract the old matrix data
                        EV1 <- c(ori, rep(paste(rep("0", ncols), collapse = " "), 2)) # Add two more rows of zeros
                        EV2 <- c(rep("0", length(data2$data2)), 1, -1) # Add the column of covariate contrasts
                        data2 <- data.frame(cbind(EV1,EV2)) # Column-combine EV1 and EV2, then convert to dataframe
                        cols <- names(data2) # Extract the name of the dataframe data2
                        data2$x <- apply(data2, 1, paste, collapse = " ") # Add in a new column x to the dataframe2 which pasted EV1 and EV2 together
                        data2 <- data2[,!(names(data2) %in% cols)] # Update data2 with previous EV1 and EV2 columns excluded
                        data2 <- data.frame(data2, stringsAsFactors = FALSE) # Convert data2 from matrix to dataframe
                }
                
                # Create heading with NumWaves, NumContrasts, and PPheights configured
                NumWaves <- paste("/NumWaves", NC)
                NumContrasts <- paste("/NumContrasts", NC)
                PPheights <- paste("/PPheights", "1 1")
                Matrix <- "/Matrix"
                heading <- c(NumWaves, NumContrasts, PPheights, Matrix)
                
                # Combine heading and updated matrix
                data3 <- cbind(c(heading, data2$data2))
                data3 <- data.frame(data3)
        }
        
        # Export data
        write.table(data3, paste(filename,".con", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)
}