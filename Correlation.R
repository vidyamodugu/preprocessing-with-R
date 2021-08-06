rm(list=ls())
# Read the data from csv file
Phishing_data <- read.csv('/Users/vidyamodugu/Desktop/BA Major/Phishing_datasets.csv', header = T)
                          
                          
                          
                         names <- c("has_ip", "long_url", "short_service", "has_at",
                          "double_slash_redirect", "pref_suf", "has_sub_domain",
                          "ssl_state", "long_domain", "favicon", "port",
                          "https_token", "req_url", "url_of_anchor", "tag_links",
                          "SFH", "submit_to_email", "abnormal_url", "redirect",
                          "mouseover", "right_click", "popup", "iframe",
                          "domain_Age", "dns_record", "traffic", "page_rank",
                          "google_index", "links_to_page", "stats_report", "target") 
                          names(Phishing_data) <- names
                          str(Phishing_data)
                          summary(Phishing_data)
                          
                          
                          library(caret)
                          # Create training and testing partitions
                          train_in <- createDataPartition(y = Phishing_data$target,
                          p = 0.75, list = FALSE)
                          
                          training <- Phishing_data[train_in,]
                          testing <- Phishing_data[-train_in,]
                          
                          
                          nearZeroVar(Phishing_data, freqCut = 95/5,  saveMetrics = TRUE,
                          names = FALSE, foreach = FALSE, allowParallel = TRUE)
                          lowvariancecolumns <- nearZeroVar(Phishing_data)
                          filteredData <- Phishing_data[, -lowvariancecolumns]
                          my_num_data<-Phishing_data[,sapply(Phishing_data, is.numeric)]
                          my_num_data
                          my_num_data<-Phishing_data[,sapply(Phishing_data, is.numeric)]
                          my_num_data
                          
                          
                          Phishing_data<-as.factor(Phishing_data)
                          
                          library(corrplot)
                          library(caret)
                          correlations <- cor(Phishing_data,use="pairwise.complete.obs")
                          highCorr <- findCorrelation(correlations, cutoff = .75,verbose = FALSE)
                          length(highCorr)
                          Phishing_data<- Phishing_data[, -highCorr]
                          phishing_data <- cor(Phishing_data)
                          corrplot(phishing_data, method='circle')
                          plotHistograms(Phishing_data)
                          transformSkewed(Phishing_data)
                          plotHistograms(Phishing_data)
                          
                          preProcValues <- preProcess(Phishing_data, method=c('center', 'scale', 'yeo-johnson'), k=as.numeric(knnOut$bestTune))
                         ## Creating Dummy variables
                          
                          
                          
                           dummyInfo <- dummyVars(~ ., data = Phishing_data[,-1])
                          dummyInfo
                          dummyInfo2 <- predict(dummyInfo, Phishing_data[,-1])
                          dummyInfo2 <- data.frame(Phishing_data)
                          Phishing_data$target <- Phishing_data$target
                          str(Phishing_data)
  
                    library(snow)
                          library(doSNOW)
                          
                          cl <- makeCluster(3, type = "SOCK")
                          registerDoSNOW(cl)
                          
                          grep('Class', names(trainData))
                          
                          ctrl <- rfeControl(functions = ldaFuncs,
                                             method = "repeatedcv",
                                             n = 10,
                                             repeats = 10,
                                             verbose = FALSE)
                          
                          subsets <- 1:(ncol(segmentationData)-1)
                          
                          set.seed(1324)
                          rfeOut <- rfe(trainData[,-1], trainData[,1],
                                        sizes = subsets,
                                        rfeControl = ctrl)
                          
                         
                          
                          
                          
                          
                          
                          
                          
                          
                          
                        library(MASS)
                          
                          
                          fitControl2 <- trainControl(## 10-fold CV
                          method = "repeatedcv",
                          number = 10,
                          ## repeated ten times
                          repeats = 10 )
                          
                          lmOut <- train(target~., data = Phishing_data, 
                          method = "lda2", 
                          trControl = fitControl2,
                          verboseIter = TRUE,  tuneLength = 10)
                          
                          lmOut
                          
                          
                          
                          
                          ##Knn imputation using caret function
                          
                          
                          
                          ####Partial least square method
                          library(MASS)
                          fitControl <- trainControl(method = 'repeatedcv', repeats = 5,
                          number = 5, verboseIter = T)
                          
                          
                          pls.fit <- train(target~.,  data = training, preProcess=c('center','scale'),
                          method = "pls", trControl = fitControl,
                          tuneLength = 5)
                          pls.fit
                          pls.predict <- predict(pls.fit, testing[,-31])
                          postResample(pred = pls.predict,  obs = testing$target)
                          
                          plot(varImp(rf.fit))
                          
                          ####boosting method
                          gbm.fit <- train(target~.,  data = training, preProcess=c('center','scale'),
                          method = "gbm", trControl = fitControl,
                          tuneLength = 5)
                          gbm.fit
                          
                          rbfsvm.predict <- predict(gbm.fit, testing[,-31])
                          postResample(pred = rbfsvm.predict,  obs = testing$target)
                          plot(varImp(gbm.fit))
                          
                          
                          confusionMatrix(rbfsvm.predict, testing$target)
                          
                          ####random forest method
                          fitControl <- trainControl(method = 'repeatedcv', repeats = 5,
                          number = 5, verboseIter = T)
                          
                          
                          rf.fit <- train(target~.,  data = training, preProcess=c('center','scale'),
                          method = "rf", trControl = fitControl,
                          tuneLength = 5)
                          
                          plot(varImp(rf.fit))
                          
                          
                          # Predict the testing target
                          log.predict <- predict(log.fit, testing[,-31])
                          
                          log.fitconfusionMatrix(log.predict, testing$target)
                          