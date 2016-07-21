groom <- function( AnswerData, key){
  
  key <- c(key)
  
  AnswerData <- AnswerData[,is.element(colnames(AnswerData), rownames(key) )]
  
  AnswerData <- as.data.frame(
    lapply(AnswerData,function(x) 
      if(is.character(x)|is.factor(x)){ 
        gsub(",","",x) 
        
      }else x))
  AnswerData <- as.data.frame(
    lapply(AnswerData,function(x) 
      if(is.character(x)|is.factor(x)){ 
        gsub("\\(","",x) 
      get}else x))
  AnswerData <- as.data.frame(
    lapply(AnswerData,function(x) 
      if(is.character(x)|is.factor(x)){ 
        gsub(")","",x) 
      }else x))
  return(AnswerData)
}

scoreTF <- function(answers, key, df=prtFrame){
  maxScore <- sum(key$numAns)
  qScores <- c()
  invalid <- FALSE
  for(question in 1:length(answers)){
    qScoreMax <- key$numAns[question]
    
    given <- as.character( answers[1,question])
    
    correct <- as.character(key$Ans[question])
    num <- key$numAns[question]
    
    givenBIN <- binarize(given, num)
    correctBIN <-binarize(correct, num)	
    
    if(given %in% c("", " ")){
      invalid <- TRUE
    }
    for ( ans in 1:num){	
      if ( givenBIN[ans] == correctBIN[ans]){
        qScores <- c(qScores, 1)
      }else{
        qScores <- c(qScores, 0)	
      }	
    }
  }
  
  score <- signif(100*sum(qScores)/sum(key$numAns,4),4)
  if (invalid) { score <- -1}
  return(c(score, qScores))
}

getTF <- function(data, key, cull = T)
{
  TFscores <- c()
  data <- groom(data, key)
  numQ <- sum(key$numAns)
  tfFrame <- data.frame(matrix(ncol=numQ, nrow=1))
  colnames(tfFrame) <- ansNames
  for ( a in 1:nrow(data)){
    TFscores <- c(TFscores, scoreTF(data[a,])[1])
    tfFrame <- rbind(tfFrame, scoreTF(data[a,])[2:numQ+1])
  }
  
  tfFrame <- tfFrame[-1,]
  tfFrame <- cbind(tfFrame, TFscores)
  if (cull) tfFrame <- tfFrame[!(tfFrame$TFscores < 0),] 
  return(tfFrame)
}