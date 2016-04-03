pollutantmean <- function(directory, pollutant, id=1:332) {
    ## INPUT
    ## directory: location of CSV files (char vector, length 1)
    ## pollutant: name of pollutant (char vector, length 1)
    ## id: monitor ID number (integer vector)
    
    ## OUTPUT
    ## Mean of the pollutant across all monitors listed in id (ignoring NA)
    
    ## loop over all file names
    ## open the files
    ## read the tables
    ## determine their sum and their number of values
    ## calculate the mean value
    
    sum <- 0
    numvals <- 0
    
    for (fileid in seq_along(id)) {
        name <- paste(directory, '/', formatC(id[fileid], width=3, flag="0"), 
                      '.csv', sep="")
        #print(name)
        data <- read.csv(name)
        #print('done reading data!')
        poldata <- data[pollutant]
        sum <- sum + sum(poldata[!is.na(poldata)])
        #print(sum)
        numvals <- numvals + length(poldata[!is.na(poldata)])
        #print(numvals)
        }
    
    mean <- sum/numvals
    return(mean)
    }
