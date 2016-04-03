complete <- function(directory, id=1:332) {
    ## INPUT
    ## directory: location of CSV files (char vector, length 1)
    ## id: monitor ID number (integer vector)
    
    ## OUTPUT
    ## Data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## where 'id' is monitor ID number and 'nobs' number of complete cases
    
    ## read in length of the vector and construct a 3xn data frame
    ## loop over all file names
    ## open the files
    ## read the tables
    ## put the monitor number into the data frame
    ## filter tables by !is.na(sulfates) && !is.na(nitrates)
    ## determine the length of this object and put it in the data frame
  
    size <- length(id)
    df <- data.frame(id=numeric(0), nobs=numeric(0))
    for (fileid in seq_along(id)) {
        name <- paste(directory, '/', formatC(id[fileid], width=3, flag="0"), 
                      '.csv', sep="")
        #print(name)
        data <- read.csv(name)
        #print('done reading data!')
        nitrate <- data["nitrate"]
        sulfate <- data["sulfate"]
        newdata <- data[!is.na(sulfate) & !is.na(nitrate)]
        #print(newdata)
        numcomplete <- length(newdata)/4
        df[fileid, ] <- c(id[fileid], numcomplete)
    }
    
    return(df)
}