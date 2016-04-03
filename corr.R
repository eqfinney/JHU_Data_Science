corr <- function(directory, threshold=0) {
    ## INPUT
    ## directory: location of CSV files (char vector, length 1)
    ## threshold: number of completely observed observations required to compute
    ##    the correlation between nitrate and sulfate (numeric vector, length 1)
    
    ## OUTPUT
    ## A numeric vector of the correlations between nitrate and sulfate
    
    ## loop over all file names
    ## open the files
    ## read the tables
    ## determine if the tables have enough completely observed observations
    ## compute the correlation for all the cases
    
    id <- 1:332
    corvec <- vector()
    count <- 1
    for (fileid in seq_along(id)) {
        name <- paste(directory, '/', formatC(id[fileid], width=3, flag="0"), 
                      '.csv', sep="")
        data <- read.csv(name)
        nitrate <- data["nitrate"]
        sulfate <- data["sulfate"]
        compdf = complete(directory,id[fileid])
        if (compdf$nobs[[1]]>threshold) {
            newdata <- subset(data, !is.na(data$nitrate) & !is.na(data$sulfate))
            ##print(newdata)
            corvec[count] <- cor(newdata$nitrate, newdata$sulfate)
            count <- count +1
        }
        else {
          next
        }
    }
    
    return(corvec)
    
    }