setwd("/Users/SJY/Desktop/SJY_proj/datasciencecoursera/course\ 2")

# Part 1
pollutantmean <- function(directory, pollutant, id = 1:332){
    file <- dir(path = directory, full.names = T)
    sum <- 0
    num <- 0
    for(i in id){
        data <- read.csv(file[i], header=T)
        sum <- sum + sum(data[pollutant][!is.na(data[pollutant])])
        num <- num + length(data[pollutant][!is.na(data[pollutant])])
    }
    mean <- sum/num
    return(mean)
}

# Part 2
complete <- function(directory, id = 1:332){
    file <- dir(path = directory, full.names = T)
    nobs <- c()
    for(i in id){
        data <- read.csv(file[i], header = T)
        nobs <- c(nobs, sum(complete.cases(data)))
    }
    result <- data.frame(id,nobs)
    return(result)
}

# Part 3
corr <- function(directory, threshold = 0){
    file <- dir(path = directory, full.names = T)
    com <- complete(directory, id = 1:332)
    com_over <- com[which(com$nobs > threshold),]
    id <- as.vector(com_over$id)
    corr <- c()
    for(i in id){
        data <- read.csv(file[i], header = T)
        datacom <- data[complete.cases(data),]
        corr <- c(corr, cor(datacom$sulfate, datacom$nitrate))
    }
    return(corr)
}
