designcon <- function(filename = "designtest", NC = 3) {
## This is a function written to generate design.con file automatically
## for TBSS analysis. A successful execution of this function requires
## 1) Using "design_ttest2 design" to first create a design.con;
## 2) Specify how many contrasts are needed.
## In the future, both designcon.R and designmat.R will be merged so that
## one does not need to execute them separately to generate both files.

        # Read in the data
        data <- read.csv("design.con", header = FALSE, stringsAsFactors = FALSE)
        data2 <- data$V1[5:length(data$V1)]
        data2 <- data.frame(cbind(data2), stringsAsFactors = FALSE)
        
        if (NC < 3) {
                stop("Invalid Number of Contrasts, should be not less than 3")
        } else {
                for (i in 1:(NC-2)) {
                        ncols <- length(strsplit(data2$data2[1], " ")[[1]]) # Configure columns number
                        ori <- data2$data2[1:length(data2$data2)] # Extract the old matrix data
                        EV1 <- c(ori, rep(paste(rep("0", ncols), collapse = " "), 2)) # Add two more rows of zeros
                        EV2 <- c(rep("0", length(data2$data2)), 1, -1) # Add the column of covariate contrasts
                        data2 <- data.frame(cbind(EV1,EV2)) # Column-combine EV1 and EV2, then convert to dataframe
                        cols <- names(data2) # Extract the name of the dataframe data2
                        data2$x <- apply(data2, 1, paste, collapse = " ") # Add in a new column to the dataframe2 with EV1 and EV2 pasted together
                        data2 <- data2[,!(names(data2) %in% cols)] # Update data2 with previous EV1 and EV2 columns excluded
                        data2 <- data.frame(data2, stringsAsFactors = FALSE) # Convert data2 from matrix to dataframe
                }
                
                # Configure NW, NC, PP
                NumWaves <- paste("/NumWaves", NC)
                NumContrasts <- paste("/NumContrasts", NC)
                PPheights <- paste("/PPheights", PP)
                Matrix <- "/Matrix"
                heading <- c(NumWaves, NumContrasts, PPheights, Matrix)
                
                # Combine heading and updated matrix
                data3 <- cbind(c(heading, data2$data2))
                data3 <- data.frame(data3)
                
                # Export data
                write.table(data3, paste(filename,".con", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)
        }
}