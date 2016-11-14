# Christopher Snyder
# Central Dogma Concept Inventory Analysis
# CDCI-Analysis.R

#Read in the csv file containing the data, in order for this command
#to work, the csv file must be saved in the working directory, the folder that this
# rbeta program is saved in. 

#Version 4
CDCI1 <- read.csv("CDCI1pre.csv", header= TRUE, sep= "," )[2:27]
CDCI1post <- read.csv("CDCI-01post-new.csv", header= TRUE, sep= "," )[5:30]
CDCI2 <- read.csv("CDCI-02pre-corrected.csv", header= TRUE, sep= "," )[3:28]
CDCI3 <- read.csv("CDCI-03Ppre.csv", header= TRUE, sep= "," )[6:31]
CDCI4 <- read.csv("CDCI-04.csv", header= TRUE, sep= "," )[6:31]
CDCI5 <- read.csv("CDCI-05post.csv", header= TRUE, sep= "," )[7:32]
CDCI6 <- read.csv("CDCI-06post-revised.csv", header= TRUE, sep= "," )[6:31]
CDCI9 <- read.csv("CDCI9pre.csv", header= TRUE, sep= "," )[2:27]
CDCI10 <- read.csv("CDCI-10pre1.csv", header= TRUE, sep= "," )[6:31]
CDCI10post <- read.csv("CDCI-10Npost.csv", header= TRUE, sep= "," )[4:29]
Wright <- read.csv("Wright2015.csv", header= TRUE, sep= "," )[1:26]
Wrightpost <- read.csv("Wrightpost.csv", header= TRUE, sep= "," )[2:27]

version4 <- c(CDCI1, CDCI1post, CDCI2, CDCI3, CDCI4, CDCI5, CDCI6,CDCI9, CDCI10, CDCI10post, Wright, Wrightpost) 

#Version 5
CDCIIntro <- read.csv("CDCIIntro.csv", header= TRUE, sep= "," )[1:24]
CDCIGen   <- read.csv("CDCIGen.csv", header= TRUE, sep= "," )[1:24]

preIntro <- rbind( CDCI1, CDCI3, CDCI4, CDCI9)
postIntro <- rbind(CDCI5, CDCI6, CDCI1post)
preMid <- rbind(CDCI2, CDCI10, Wright)
postMid <- rbind(Wrightpost)


#BME + Biol 201

bio1     <-  read.csv("BIOL201 section 1 PRE.csv", header= TRUE, sep= "," )[1:24]
bio1post <-  read.csv("BIOL201 section 1 POST.csv", header= TRUE, sep= "," )[1:24]
bio2     <-  read.csv("BIOL201 section 2 PRE.csv", header= TRUE, sep= "," )[1:24]
bio2post <-  read.csv("BIOL201 section 2 POST.csv", header= TRUE, sep= "," )[1:24]
bio3     <-  read.csv("BIOL201 section 3 PRE.csv", header= TRUE, sep= "," )[1:24]
bio3post <-  read.csv("BIOL201 section 3 POST.csv", header= TRUE, sep= "," )[1:24]
bme      <-  read.csv("BME PRE.csv", header= TRUE, sep= "," )[1:24]
bmepost  <-  read.csv("BME POST.csv", header= TRUE, sep= "," )[1:24]
version5 <- c(bio1, bio1post, bio2, bio2post, bio3, bio3post, bme, bmepost)

###############################################################
#####
##### CHOOSE DATA AND KEY HERE
#####
###############################################################
data <- rbind(bio2, bme)
modelRelated <- c   ( 1,2,5,6,7,10,14,15,16,17.20,21)
nonModelRelated <- c( 3,4,8,9,11,12,13,19,22,23 )


numQ <- ncol(data)

#models <- c(1,2,5,6,7,9,10,13,14,15,16,17,18,19,20,21)
#noModels <- c(3,4,8,11,12,22,23)

V4Ans <- c(
"AD", "BE", "A", "A", "CD" , 
"ABCD", "A" , "C" , "AC","AB" ,
"A", "AC" , "CD" , "ABD" , "BD", 
"B", "CD" , "D" , "A" , "A" , 
"B" ,"C" , "A", "ABCD" )#
V4numAns <- c(5,5,3,3,4, 4,4,4,4,3, 4,5,5,4,4, 4,5,4,3,4, 3,4,4,4)

V5Ans <- c(
"AD",  "BE",   "A", "CD" , "ABCD",  
  "A",  "C",  "AC", "AB" ,    "A",
 "AC", "ACD","ABD", "BD",     "B",
"CD" , "BD" , "A" , "A" ,    "B" ,
"C" ,    "A", "ABCD" )
V5numAns <- c(5,5,3,4, 4,4,4,4,3, 4,5,5,4,4, 4,5,4,3,4, 3,4,4,4)

V5names <- colnames(data[1:length(V5Ans)])
V4names <- colnames(data[1:length(V4Ans)])

key <- data.frame(number = V4names, Ans = V4Ans,numAns = V4numAns, row.names = V4names)

getAnsNames <- function(myKey = key)
	{
	  ansNames <-  c()
	  for (q in 1:nrow(key))
	  {
	    
	    for (a in 1: myKey$numAns[q])
	    {
	      ansNames <- c(ansNames, paste(as.character(myKey$number[q]), LETTERS[a], sep = ""))
	    }
	  }
	  return( ansNames)
	}
# Remove unnecessary charcters from answer data
groom <- function( AnswerData, myKey = key)
	{
#	AnswerData <- AnswerData[,is.element(colnames(AnswerData), as.character(myKey$number))] 
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub(",","",x) }else x))
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub("\\(","",x) }else x))
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub(")","",x) }else x))
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub("\"","",x) }else x))
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub("\'","",x) }else x))
	AnswerData <- as.data.frame(lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ gsub(" ","",x) }else x))
	return(AnswerData)
	}

# generate scores treating each question as a binary correct / incorrect
scoreABS <- function(answers, focus=0, selection = c(1:length(answers)), myKey = key){
	qScores <- c()
	score <- 0 
	invalid <- FALSE
  for( a in selection)
    #{
    #if (is.element(names(answers[a]), as.character(key$number)))
    {
  		getgiven  <- as.character( answers[1,a])
  		if (a != focus){	
  			if (as.character( answers[1,a])  == as.character(myKey$Ans[a])){
  				score <- score + 1
  				qScores <- c(qScores, 1)
          }else if (as.character( answers[1,a]) %in% c("", " ")){ 
  				qScores <- c(qScores, 0)
  				invalid <- TRUE
  			}else{ qScores <- c(qScores, 0)}
  		}
#    } else print(names(answers[a]))
	}
	score <- round(100*score/length(answers),2)
	if(invalid){ score <- -1}
	return(c(score, qScores))	
	}
# generate scores so that the final score for each question is a fractionn representing the number of
# correct answer choices		
scorePRT <- function(answers, df=prtFrame, myKey = key){
	maxScore <- sum(myKey$numAns)
	qScores <- c()
	invalid <- FALSE
	for(question in 1:length(answers)){
		qScoreMax <- myKey$numAns[question]
		qScore <- qScoreMax
		given <- as.character( answers[1,question])
		correct <- as.character(myKey$Ans[question])
		num <- myKey$numAns[question]
		for(char in 1:nchar(correct)){
			corr <- substr(correct, char, char)
			if(!grepl(corr,given)){
				qScore = qScore - 1
			}
		}
		for(char in 1:nchar(given)){
			give <- substr(given, char, char)
			if(!grepl(give,correct)){
				qScore = qScore - 1
			}
		}
		if(given %in% c("", " ")){invalid <- TRUE}
		qScores <- c(qScores, round(qScore/num,2))
	}
	score <- round(100*sum(qScores)/length(answers),2)
	if (invalid) { score <- -1}
	return(c(score, qScores))
}
# transform a string containingchosen answers into a binary string containing a 1 for each chosen answer and a 0 
# 0 for each chosen answer. 		
binarize <- function(string, N)
	{
	binary <- rep(0,N)
	for( char in 1:N)
		{
		if(grepl(LETTERS[char], string)) binary[char] <- 1
		}
	return(binary)
	}
# generates scores so that each answer choice is treated as a separate true false 
scoreTF <- function(answers, df=prtFrame, myKey = key)
	{
	maxScore <- sum(key$numAns)
	qScores <- c()
	invalid <- FALSE
	for(question in 1:length(answers)){
		qScoreMax <- myKey$numAns[question]	
		given <- as.character( answers[1,question])
		correct <- as.character(myKey$Ans[question])
		num <- myKey$numAns[question]
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
	score <- signif(100*sum(qScores)/sum(myKey$numAns,4),4)
	if (invalid) { score <- -1}
	return(c(score, qScores))
	}
# produce a bar plot displaying the most common answers, the correct answer is highlighted in blue
qPlot <- function(AnswerData, question, crop=7, myKey = key)
	{
	these <- groom(AnswerData)[question]
	table <- sort(table(these))
	table <-(tail(table,crop))
	print(table)
	cols <- c("red","blue")
	correct <- names(table) == myKey$Ans[question]
	title <- paste("Question",as.character(question)) 
	
	barplot(table, main=title, col=cols[correct+1] , xlab = "Answer", ylab="Frequency")
	}

# gets the D value for a given question
# gets the proportion of the top 27% of scorers for a given question 
# as well as for the bottom 27% of scorers
getD <- function(question, max=FALSE)
	{
	modframe <- absFrame[,-question]
	modscores <- c()
	for(i in 1:length(modframe[[1]])){
		x <- table(as.numeric(modframe[i,]))
		sc <-  x[2]/sum(x)
		if (is.na(sc)){ sc <- 0}
		modscores <- c(modscores,round(sc,2))
	}
	cut <- round(length(modscores) *(27/100))
	modframe <- cbind(modframe,FOCUS=absFrame[,question],modscores)
	modframe <- modframe[order(modscores),]
	bottom <- head(modframe$FOCUS,cut)
	top <- tail(modframe$FOCUS, cut)
	T <- table(top)
	B <- table(bottom)
	Ucorrect <- (sum(T) - T[1])/ sum(T)
	if (max) { Ucorrect <- 1 }
	Lcorrect <- (sum(B)- B[1])/sum(B)
	return(Ucorrect - Lcorrect)
	}
# implementation of the Cohens D distance algorithm 
cohens_d <- function(x, y) 
	{
        lx <- length(x)- 1
        ly <- length(y)- 1
        md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
        csd <- lx * var(x) + ly * var(y)
        csd <- csd/(lx + ly)
        csd <- sqrt(csd)                     ## common sd computation
        cd  <- md/csd                        ## cohen's d
	}
# apply scorePRT to a collection of answer sets
getPRT <- function(data, cull = T, myKey = key)
	{
	  Prtscores <- c()
	  data <- groom(data)
	  numQ <- ncol(data)
	  prtFrame <- data.frame(matrix(ncol=length(data), nrow=1))
	  colnames(prtFrame) <- colnames(data)
	  for ( a in 1:nrow(data)){
	    Prtscores <- c(Prtscores, scorePRT(data[a,], myKey = key)[1])
	    prtFrame <- rbind(prtFrame, scorePRT(data[a,], myKey= key)[2:numQ+1])
	  }
	  prtFrame <- prtFrame[-1,]
	  prtFrame <- cbind(prtFrame, Prtscores)
	  if (cull) prtFrame <- prtFrame[!(prtFrame$Prtscores < 0),] 
	  return(prtFrame)
	}
# apply scoreAbs to a collection of answer sets
getAbs <- function(data, cull = T, myKey =  key)
	{
	Absscores <- c()
	data <- groom(data)
	numQ <- ncol(data)
	absFrame <- data.frame(matrix(ncol=length(data), nrow=1))
	colnames(absFrame) <- colnames(data)
	for ( a in 1:nrow(data))
		{	
		Absscores <- c(Absscores, scoreABS(data[a,], myKey = key )[1])
		absFrame <- rbind(absFrame, scoreABS(data[a,], myKey = key)[2:numQ+1])
		}
	absFrame <- absFrame[-1,]
	absFrame <- cbind(absFrame, Absscores)
	if (cull) absFrame <- absFrame[!(absFrame$Absscores < 0),] 
	return(absFrame)
	}
# apply scoreTF to a collection of answer sets
getTF <- function(data, cull = T, myKey = key)
	{
	TFscores <- c()
	data <- groom(data)
	numQ <- sum(key$numAns)
	tfFrame <- data.frame(matrix(ncol=numQ, nrow=1))
	colnames(tfFrame) <- getAnsNames(myKey)
	for ( a in 1:nrow(data))
		{  
	        TFscores <- c(TFscores, scoreTF(data[a,], myKey = key)[1])
	        tfFrame <- rbind(tfFrame, scoreTF(data[a,], myKey = key)[2:numQ+1])
	  	}
	tfFrame <- tfFrame[-1,]
	tfFrame <- cbind(tfFrame, TFscores)
	if (cull) tfFrame <- tfFrame[!(tfFrame$TFscores < 0),] 
	return(tfFrame)
	}



