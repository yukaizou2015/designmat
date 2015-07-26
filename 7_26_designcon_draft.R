# 7_26 designcon.R
# Read in the data
data <- read.csv("design.con", header = FALSE, stringsAsFactors = FALSE)
data2 <- data$V1[5:length(data$V1)]
data2 <- data.frame(cbind(data2), stringsAsFactors = FALSE)

# Need a for loop need to enclose the codes below
for (i in 1:2) {
        ncols <- length(strsplit(data2$data2[1], " ")[[1]]) # Configure columns number
        ori <- data2$data2[1:length(data2$data2)]
        # If there is one covariate, rep is 2 times
        # If there are two covariate, rep is 4 times
        EV1 <- c(ori, rep(paste(rep("0", ncols), collapse = " "), 2))
        EV2 <- c(rep("0", length(data2$data2)), 1, -1) # need update
        data2 <- data.frame(cbind(EV1,EV2))
        cols <- names(data2)
        data2$x <- apply(data2, 1, paste, collapse = " ")
        data2 <- data2[,!(names(data2) %in% cols)]
        data2 <- data.frame(data2, stringsAsFactors = FALSE)
}
# Need to configure headings
# Configure NW, NC, PP
makehead <- function(NW = ncols + 1, NC = 4, PP = "1 1") { # A function creating the heading
        NumWaves <- paste("/NumWaves", NW)
        NumContrasts <- paste("/NumContrasts", NC)
        PPheights <- paste("/PPheights", PP)
        Matrix <- "/Matrix"
        heading <- c(NumWaves, NumContrasts, PPheights, Matrix)
        return(heading)
}
heading <- makehead()

# Combine heading and updated matrix
data3 <- cbind(c(heading, data2$data2))
data3 <- data.frame(data3)
# Export data
filename <- "Hellocon"
write.table(data3, paste(filename,".con", sep = ""), quote = FALSE, row.names = FALSE, col.names = FALSE)