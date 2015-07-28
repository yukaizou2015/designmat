# Check design.con and design.mat exists

if (file.exists("design.mat") & file.exists("design.con") == FALSE) {
        # Inspired from: https://stat.ethz.ch/pipermail/r-help/2003-July/036129.html
        group <- function()
        {
                cat('Number of subjects in the first group:', '\n')
                a <- scan("",n=1, quiet=TRUE)
                cat('Number of subjects in the second group:', '\n')
                b <- scan("",n=1, quiet=TRUE)
                message(paste("export FSLVERSION=4.1.9; design_ttest2 design", a, b))
        }
        group()
        
        system("export FSLVERSION=4.1.9; design_ttest2 design 5 5") # number subject to change
}