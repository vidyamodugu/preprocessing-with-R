printLevelsOfFactors <- function(data) {
  for (i in 1:ncol(data)){
    if (is.factor(data[,i])){cat(names(data)[i], ': ', levels(data[,i]), '\n', '\n')}
  }
}


## Your instructor created the following function which you can use.
## This function only works if the dependent variable is numerical.
## An updated version will be provided to handle a categorical dependent
## variable, i.e. a factor, as soon as your instructor writes it.

## Run the code below to create the function and load it into memory.

plotDepvarVsX <- function(data, depVarCol=4, jit=FALSE, lin=FALSE){
  #  (1) Determine the column index number of the dependent variable
  #      by using the grep function.
  #  (2) Use the function by inputting the arguments:
  #      data       a dataframe
  #      depVarCol  the column index number of the dependent variable
  #      jit        jit=TRUE to jitter points on the graph (default is FALSE)
  #      lin        lin=TRUE to draw a smooth curve to visualize the possible
  #                 relationship between the dependent variable and each 
  #                 potential predictor variable (default is FALSE)
  library(ggplot2)
  depVar <- data[,depVarCol]
  plotData <- data[,-depVarCol]
  plot_list = list()
  for (i in 1:ncol(plotData)){
    keepObs <- (!is.na(depVar)) & (!is.na(plotData[,i]))
    if (is.factor(plotData[,i])){
      tempData <- data.frame(x=plotData[,i],y=as.numeric(depVar))
      library(doBy)
      tempData2 <- summaryBy(y ~ x, data=tempData, FUN=mean, na.rm=T)
      tempData2$origOrder <- 1:nrow(tempData2)
      tempData2 <- tempData2[order(tempData2$y.mean),]
      tempData2$newOrder <- 1:nrow(tempData2)
      
      tempData$x <- factor(tempData$x, levels(tempData$x)[tempData2$origOrder])
      p <- ggplot(data=data.frame(x=tempData$x, y=tempData$y), 
                  aes(x, y, ymin=min(tempData$y), ymax=max(tempData$y))) +  geom_point(color='blue')  + labs(x=names(plotData)[i], y=names(data)[depVarCol]) + theme(axis.text.x= element_text(angle = 45,hjust = 1))
    } else {
      p <- ggplot(data=data.frame(x=as.numeric(plotData[,i]), y=as.numeric(depVar)), 
                  aes(x, y, ymin=min(tempData$y), ymax=max(tempData$y), xmin=min(plotData[,i]), xmax=max(plotData[,i]))) +  geom_point(color='blue')  + labs(x=names(plotData)[i], y=names(data)[depVarCol]) 
    }
    
    
    if (lin==T){
      if (!is.factor(plotData[,i])){
        if (length(table(as.numeric(plotData[,i])))>10) {
          #          p <- p + geom_smooth(method='lm', formula = y ~ x + x*x)
          p <- p + stat_smooth(method = "gam", 
                               formula = y ~ x, col = "blue")
        } else {
          p <- p + geom_smooth(method='lm')
        }
      } else {
        p <- p + stat_summary(fun.y=mean, colour="blue", geom="line", group=1)
      }
      #      if (is.factor(plotData[,i])){
      #        p <- p + stat_summary(fun.y=mean, colour="blue", geom="line", group=1)
      #      }
      
    }
    if (jit==T){p <- p + geom_jitter(color='blue',width=.1, height=.1)}
    
    plot_list[[i]] <- p
  }
  p <- p + theme_classic()
  print(plot_list)
}


plotHistograms <- function(data){
  library(ggplot2)
  library(doBy)
  for (i in names(data)){
    if (is.factor(data[,i]) | is.character(data[,i])){
      if( is.character(data[,i])) {data[,i] <- factor(data[,i])}
      x <- sort(table(data[,i]), decreasing = T)
      if (length(x)>5) {
        x <- data.frame(Count=c(x[1:4], sum(x[5:length(x)])))
        rownames(x)[5] <- "Other"
      } else {
        x <- data.frame(Count=c(x[1:length(x)]))
      }
      
      x$level <- factor(rownames(x), levels=rownames(x))
      
      b <- ggplot(x, aes(level, Count)) + geom_bar(stat='identity')
      b <- b + labs(x = paste(i, '(factor)'))
      b <- b + theme_classic()+ theme(legend.position="none")
      b <- b + theme(axis.text.x= element_text(angle = 15,hjust = 1))
      b <- b +  ggtitle(paste(' (# of unique values)/n                      = ', round(length(unique(data[,i][complete.cases(data[,i])]))/length(data[,i][complete.cases(data[,i])]), 3), 
                              '\n', 
                              '(highest count)/(2nd highest count) =', round(x[1,1]/x[2,1], 1),
                              '\n', 'Number Missing = ', sum(is.na(data[,i])))) 
      print(b) 
    } else {
      cc <- cut(data[,i], breaks=10)
      x <- table(cc)
      x <- data.frame(Count=x)
      names(x) <- c('level', 'Count')
      x <- x[,c('Count','level')]
      
      b <- ggplot(x, aes(level, Count)) + geom_bar(stat='identity')
      b <- b + labs(x = paste(i, '(numeric)'))
      b <- b + theme_classic()+ theme(legend.position="none")
      b <- b + theme(axis.text.x= element_text(angle = 15,hjust = 1))
      b <- b +  ggtitle(paste(' (# of unique values)/n                      = ', round(length(unique(data[,i][complete.cases(data[,i])]))/length(data[,i][complete.cases(data[,i])]), 3), 
                              '\n', 
                              '(highest count)/(2nd highest count) =', round(sort(table(data[,i]), decreasing=T)[1]/sort(table(data[,i]), decreasing=T)[2], 1),
                              '\n', 'Number Missing = ', sum(is.na(data[,i])))) 
      print(b) 
      
    }
  }
}


transformSkewed <- function(data, cutoff=1, method='YeoJohnson'){
  # method can be "BoxCox" or "YeoJohnson" (default)
  # "BoxCox" requires the data to be non-negative.
  # Only transformed numeric variables.  
  # Factor variables are ignored.
  library(e1071)
  library(stringr)
  library(caret)
  cat('\n', 'Skewness of Numeric Variables', '\n')
  nn <- as.character(NULL)
  sk1<- as.numeric(NULL)
  dat<- as.numeric(NULL)
  for (i in names(data)){
    if (is.numeric(data[,i])){
      sk <- skewness(data[,i], na.rm=T)
      if (abs(sk)>cutoff){
        nn <- c(nn,i)
        sk1<- c(sk1,sk)
        preProcessOut <- preProcess(data[,c(i,i)], method=method)
        dat <- cbind(dat, predict(preProcessOut, data[,c(i,i)])[,1])
        colnames(dat)[ncol(dat)] <- i
        cat(paste(' ',round(sk,digits=2)), fill=TRUE, labels=paste0(str_pad(i, width=30, side='left')))
      }
    }
  }
  if (!is.null(ncol(dat))){
    for (i in colnames(dat)){
      hist(data[,i], main=paste('Not Transformed'), xlab=i)
      hist(dat[,i], main=paste('method=' , method), xlab=i)
    }    
  }
  return(list(skewedVariables=nn, skewness=sk1, tranformedData = dat))
}


detectOutliers <- function(data){
  # uses the 1.5IQR rule:
  # https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/box-whisker-plots/v/reading-box-and-whisker-plot
  # https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/box-whisker-plots/a/identifying-outliers-iqr-rule
  # dataframe must have 2 or more variables
  
  library(ggplot2)
  library(ggrepel)
  is_outlier <- function(x) {
    return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  }
  
  for (i in 1:ncol(data)){
    if (is.numeric(data[,i])){
      outlier = round(ifelse(is_outlier(data[,i]), data[,i], as.numeric(NA)),2)
      if (sum(is.na(outlier)) < length(outlier)){
        
        p <- ggplot(data,aes(x="", y=data[,i]))
        p <- p + geom_boxplot() + theme_classic()
        p <- p + geom_text_repel(aes(label=outlier),hjust=-0.3)
        p1 <- p + labs(x ='' , y = names(data)[i])
        
        p  <- ggplot(data, aes(data[,i])) + geom_histogram() + ggtitle('With Outliers')
        p2 <- p + labs(x ='' , y = '')
        
        p <- ggplot(data[is.na(outlier),],aes(x="", y=data[is.na(outlier),i]))
        p <- p + geom_boxplot() + theme_classic()
        p <- p + labs(x ='' , y = names(data)[i])
        p3 <- p
        
        p  <- ggplot(data[is.na(outlier),], aes(data[is.na(outlier),i])) + geom_histogram() + ggtitle('Without Outliers')
        p4 <- p + labs(x='', y='')
        
        library(cowplot)
        library(grid)
        
        print(plot_grid(p1, p2, p3, p4))
        
        
      } else {
        p <- ggplot(data,aes(x="", y=data[,i]))
        p <- p + geom_boxplot() + theme_classic()
        p <- p + geom_text(aes(label=outlier),hjust=-0.3)
        p1 <- p + labs(x ='' , y = names(data)[i])
        
        p  <- ggplot(data, aes(data[,i])) + geom_histogram() + ggtitle('No Outliers')
        p2 <- p + labs(x ='' , y = '')
        
        library(cowplot)
        library(grid)
        print(plot_grid(p1, p2))
        
      }
      
    }
  }
}

viewPercentMissing <- function(data){
  # calculates the percent missing for each variable
  # and sorts in decreasing order.
  percentMissing <- apply(data, 2, function(x){m <-sum(is.na(x))/length(x)})
  percentMissing <- sort(percentMissing, decreasing = T)
  View(cbind(percentMissing))
  return(percentMissing)
}


tuneKNearestNeighbor <- function(data, maxk=15){
  ## data is a data frame of 2 or more predictor variables
  ## This function will tune k in the k-nearest neighbor model
  ## k will be tuned independently for each predictor variable.
  ## The output contains the original data with the missings
  ## imputed with the optimal k used for each predictor variable.
  ## The argument maxk is the maximum number of neighbors that
  ## is included in the tuning grid for k in the caret train function.
  ## The output data set includes imputed values and centers and scales
  ## the data as well.
  data2 <- data
  data3 <- data[complete.cases(data),]
  varNames <- names(data3)
  for (v in varNames){
    columnNumber <- grep(v, names(data3))
    
    knnOut <- train(data3[,-columnNumber], 
                    data3[,columnNumber],
                    method = "knn", 
                    trControl = fitControl, tuneGrid = data.frame(k=2:maxk),
                    verboseIter = FALSE)
    preProcValues <- preProcess(data, method=c('center', 'scale', 'knnImpute'), k=as.numeric(knnOut$bestTune))
    data2[,v] <- predict(preProcValues, data)[,v]
    cat('\nimputed ', v, ' with k = ', as.character(knnOut$bestTune, '\n'))
  }
  return(data2)
}

