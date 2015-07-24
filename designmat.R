designmat <- function(x) {
        data <- read.csv("Hello.mat")
        write.table(data, paste(x,".mat", sep = ""), quote = FALSE, row.names = FALSE)
        # Formatting of the first line "X.NumWaves.3" should be changed to "/NumWaves 3"
        # Modify data to add in covariate
}