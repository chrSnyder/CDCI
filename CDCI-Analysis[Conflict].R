

#Read in the csv file containing the data, in order for this command
#to work, the csv file must be saved in the working directory, the folder that this
# rbeta program is saved in. 

#Version 4
CDCI1 <- read.csv("CDCI1pre.csv", header= TRUE, sep= "," )[2:27]
CDCI1post <- read.csv("CDCI-01post-new.csv", header= TRUE, sep= "," )[5:30]
CDCI3 <- read.csv("CDCI-03Ppre.csv", header= TRUE, sep= "," )[6:31]
CDCI4 <- read.csv("CDCI-04.csv", header= TRUE, sep= "," )[6:31]
CDCI5 <- read.csv("CDCI-05post.csv", header= TRUE, sep= "," )[7:32]
CDCI6 <- read.csv("CDCI-06post-revised.csv", header= TRUE, sep= "," )[6:31]
CDCI2 <- read.csv("CDCI-02pre-corrected.csv", header= TRUE, sep= "," )[3:28]
CDCI9 <- read.csv("CDCI9pre.csv", header= TRUE, sep= "," )[2:27]
CDCI10 <- read.csv("CDCI-10pre1.csv", header= TRUE, sep= "," )[6:31]
CDCI10post <- read.csv("CDCI-10Npost.csv", header= TRUE, sep= "," )[4:29]
Wright <- read.csv("Wright2015.csv", header= TRUE, sep= "," )[1:26]
Wrightpost <- read.csv("Wrightpost.csv", header= TRUE, sep= "," )[2:27]

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


###############################################################
#####
##### CHOOSE DATA AND KEY HERE
#####
###############################################################
data <- rbind(bio2, bme)

modelRelated <- c   ( 1,2,5,6,7,10,14,15,16,17.20,21)
nonModelRelated <- c( 3,4,8,9,11,12,13,19,22,23 )


ClassData <- data[1]
AnswerData <- data[1:23]

numQ <- ncol(AnswerData)

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

Ans <- V5Ans
numAns <-V5numAns

key <- data.frame(Ans,numAns)
 
ansNames <- c(

"Q1A","Q1B","Q1C","Q1D","Q1E",
"Q2A","Q2B","Q2C","Q2D","Q2E",
"Q3A","Q3B","Q3C",
"Q4A","Q4B","Q4C","Q4D",
"Q5A","Q5B","Q5C","Q5D",

"Q6A","Q6B","Q6C","Q6D",
"Q7A","Q7B","Q7C","Q7D",
"Q8A","Q8B","Q8C","Q8D",
"Q9A","Q9B","Q9C",
"Q10A","Q10B","Q10C","Q10D",
"Q11A","Q11B","Q11C","Q11D","Q11E",

"Q12A","Q12B","Q12C","Q12D","Q12E",
"Q13A","Q13B","Q13C","Q13D",
"Q14A","Q14B","Q14C","Q14D",
"Q15A","Q15B","Q15C","Q15D",
"Q16A","Q16B","Q16C","Q16D","Q16E",
"Q17A","Q17B","Q17C","Q17D",

"Q18A","Q18B","Q18C",
"Q19A","Q19B","Q19C","Q19D",
"Q20A","Q20B","Q20C",
"Q21A","Q21B","Q21C","Q21D",
"Q22A","Q22B","Q22C","Q22D",
"Q23A","Q23B","Q23C","Q23D")

groom <- function( AnswerData){

AnswerData <- as.data.frame(
	lapply(AnswerData,function(x) 
		if(is.character(x)|is.factor(x)){ 
			gsub(",","",x) 
			 
		}else x))
AnswerData <- as.data.frame(
	lapply(AnswerData,function(x) 
		if(is.character(x)|is.factor(x)){ 
			gsub("\\(","",x) 
		}else x))
AnswerData <- as.data.frame(
	lapply(AnswerData,function(x) 
		if(is.character(x)|is.factor(x)){ 
			gsub(")","",x) 
		}else x))
return(AnswerData)
}

AnswerData <- groom(AnswerData)
ClassData <- groom(ClassData)


prtFrame <- data.frame(matrix(ncol=length(AnswerData), nrow=1))
colnames(prtFrame) <- colnames(AnswerData)

absFrame <- data.frame(matrix(ncol=length(AnswerData), nrow=1))
colnames(absFrame) <- colnames(AnswerData)

tfFrame <- data.frame(matrix(ncol=sum(key$numAns), nrow=1))
colnames(tfFrame) <- ansNames

scoreABS <- function(answers,df = absFrame,key = key, focus=0, selection = c(1:length(answers))){
	qScores <- c()
	score <- 0 
	invalid <- FALSE

	for( a in 1:length(answers)){
		given  <- as.character( answers[1,a])
    correct <- as.character(key$Ans[a])
    
		if (a != focus){	
			if (given  == correct ){
				score <- score + 1
				qScores <- c(qScores, 1)
			}else if (as.character( answers[1,a]) %in% c("", " ")){ 
				qScores <- c(qScores, 0)
				invalid <- TRUE
			}else{ qScores <- c(qScores, 0)}
		}
	}
	score <- round(100*score/length(answers),2)
	if(invalid){ score <- -1}
	
	return(c(score, qScores))	
}

scorePRT <- function(answers, df=prtFrame){
	maxScore <- sum(key$numAns)
	qScores <- c()
	invalid <- FALSE
	for(question in 1:length(AnswerData)){
    #print(paste ("Q" ,question))
		qScoreMax <- key$numAns[question]
		qScore <- qScoreMax
		given <- as.character( answers[1,question])
		correct <- as.character(key$Ans[question])
		num <- key$numAns[question]
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

binarize <- function(string, N){
	binary <- rep(0,N)
	for( char in 1:N){
		if(grepl(LETTERS[char], string)){
			binary[char] <- 1
		}
	}
	return(binary)
}

scoreTF <- function(answers, df=prtFrame){
	maxScore <- sum(key$numAns)
	qScores <- c()
	invalid <- FALSE
	for(question in 1:length(AnswerData)){
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
if(F){
Ascores <- c()
Pscores <- c()
TFscores <-c()

for ( a in 1:nrow(AnswerData)){
	Ascores <- c(Ascores, scoreABS(AnswerData[a,])[1])	
	Pscores <- c(Pscores, scorePRT(AnswerData[a,])[1])
	TFscores <- c(TFscores, scoreTF(AnswerData[a,])[1])
	prtFrame <- rbind(prtFrame, scorePRT(AnswerData[a,])[2:numQ+1])
	absFrame <- rbind(absFrame, scoreABS(AnswerData[a,])[2:numQ+1])
	tfFrame <- rbind(tfFrame, scoreTF(AnswerData[a,])[2:numQ+1])
}
prtFrame <- prtFrame[-1,]
absFrame <- absFrame[-1,]
tfFrame <- tfFrame[-1,]

FullData <- cbind(AnswerData, Ascores, Pscores, TFscores)
prtFrame <- cbind( prtFrame, Ascores, Pscores, TFscores)
absFrame <- cbind(absFrame, Ascores, Pscores, TFscores )
tfFrame <- cbind(tfFrame, Ascores, Pscores, TFscores )
include <- 0 


AnswerData <- AnswerData[!(AnswerData$Pscores < 0),] 
AnswerData <- AnswerData[!(AnswerData$Ascores < 0),] 
AnswerData <- AnswerData[!(AnswerData$TFscores < 0),] 


#Removing the responses that contain invalid answers
absFrame <- absFrame[!(absFrame$Ascores == -1),]
prtFrame <- prtFrame[!(prtFrame$Ascores == -1),]
tfFrame <- tfFrame[!(tfFrame$Ascores == -1),]
Ascores  <- Ascores[!(Ascores == -1)]
Pscores  <- Pscores[!(Pscores == -1)]
TFscores  <- TFscores[!(TFscores == -1)]

}
#if (1==2){#code cancelling block

qPlot <- function(question, crop=7, choice = "A"){
  these <- AnswerData[question]
	table <- sort(table(these))
	#print(table)
	table <-(tail(table,crop))
	cols <- c("red","blue")
	correct <- names(table) == key$Ans[question]
	title <- paste("Question",as.character(question)) 
	
	barplot(table, main=title, col=cols[correct+1] , xlab = "Answer", ylab="Frequency")
    
  A <- c()
  B <- c()
  C <- c()
  D <- c()
  E <- c()
  for(i in 1:length(these[[1]])){
    
    if(grepl("A", as.character(these[[1]][i]))){ A <- c(A, 1) }else{ A <- c(A,0) }
    if(grepl("B", as.character(these[[1]][i]))){ B <- c(B, 1) }else{ B <- c(B,0) }
    if(grepl("C", as.character(these[[1]][i]))){ C <- c(C, 1) }else{ C <- c(C,0) }
    if(grepl("D", as.character(these[[1]][i]))){ D <- c(D, 1) }else{ D <- c(D,0) }
    if(grepl("E", as.character(these[[1]][i]))){ E <- c(E, 1) }else{ E <- c(E,0) }
    
  }
    #print(" Answer Choice correlations with Absolute score" )
	#print(paste( "A",round(cor(A, AnswerData$Ascores),2)))
	#print(paste( "B",round(cor(B, AnswerData$Ascores),2)))
	#print(paste( "C",round(cor(C, AnswerData$Ascores),2)))
	if(key$numAns[question] > 3){
		#print(paste( "D",round(cor(D, AnswerData$Ascores),2)))
		if(key$numAns[question] > 4){
			#print(paste( "E",round(cor(E, AnswerData$Ascores),2)))
		}
	}
}

# gets the D value for a given question
#gets the proportion of the top 27% of scorers for a given question 
# as well as for the bottom 27% of scorers
getD <- function(question, max=FALSE){
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

if(F){
Pcors <- c()
Acors <- c()
Dif <- c()
Question <- c(1:numQ)
Ds <- c()
pDE <- c()



# Generate pearson correlation coefficients, difficulty levels, and separation values for each question
for(i in 1:numQ){
	Pcors <- c(Pcors,round( cor(prtFrame[[i]], Pscores),2))
	Acors <- c(Acors,round( cor(absFrame[[i]], Ascores),2))
	x <- table(absFrame[[i]])
	#print(paste(i, x[1],x[2],  x[2]/x[1], sep="  "))
	Dif <- c(Dif,round( x[1]/sum(x), 2))
	Ds <- c(Ds,round(getD(i),2))
	pDE <- c(pDE , round(getD(i, max=TRUE),2))
	
}


DE <-round(Ds/pDE, 3)

qProfiles <- data.frame(Question,"Partial Correlation"=Pcors[1:numQ],
				"Absolute Correlation"=Acors[1:numQ], "Difficulty Index"=Dif[1:numQ],
				 D=Ds[1:numQ] , D.E=DE[1:numQ])[order(Question),]
#boxplot(introTFFrame$TFscores,genTFFrame$TFscores)

aDif <- c()
aPcorr <- c()
aAcorr <- c()
aDs <- c()
apDE <- c()
for( question in 1:length(ansNames)){
	qTable <- table(tfFrame[question])
	aPcorr <- c(aPcorr,round( cor(tfFrame[question] , tfFrame$TFscores),2))
	aAcorr <- c(aAcorr, round(cor(tfFrame[question] , tfFrame$Ascores),2))
	aDif <- c(aDif,round(qTable[1]/sum(qTable),2))
	#aDs <- c(aDs, round(getD(question),3))
	#apDE  <- c(apDE, round(getD(question, max=TRUE),3))
}
#aDE <-round(aDs/apDE, 3)
Aprofiles <- data.frame(ansNames, aDif, aPcorr, aAcorr)#,  aD=aDs[1:length(ansNames)] , aD.E=aDE[1:length(ansNames)])
}
# calculates the cohens d vvalue between two variables
cohens_d <- function(x, y) {
    lx <- length(x)- 1
    ly <- length(y)- 1
    md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
    csd <- lx * var(x) + ly * var(y)
    csd <- csd/(lx + ly)
    csd <- sqrt(csd)                     ## common sd computation

    cd  <- md/csd                        ## cohen's d
}


# takes a dataframe of raw answerdata and returns a dataframe 
# of the scored data using the parial scoring method
getPRT <- function(data, cull = T)
{
  Prtscores <- c()
  data <- groom(data)
  numq <- ncol(data)
  prtFrame <- data.frame(matrix(ncol=length(data), nrow=1))
  colnames(prtFrame) <- colnames(data)
  for ( a in 1:nrow(data)){	
    Prtscores <- c(Prtscores, scorePRT(data[a,])[1])
    prtFrame <- rbind(prtFrame, scorePRT(data[a,])[2:numQ+1])
  }
  
  prtFrame <- prtFrame[-1,]
  prtFrame <- cbind(prtFrame, Prtscores)
  if (cull) prtFrame <- prtFrame[!(prtFrame$Prtscores < 0),] 
  return(prtFrame)
}

getABS <- function(data, cull = T)
{
  Absscores <- c()
  data <- groom(data)
  numq <- ncol(data)
  absFrame <- data.frame(matrix(ncol=length(data), nrow=1))
  colnames(absFrame) <- colnames(data)
  for ( a in 1:nrow(data)){  
    Absscores <- c(Absscores, scoreABS(data[a,], key=key)[1])
    absFrame <- rbind(absFrame, scoreABS(data[a,], key =key)[2:numQ+1])
  }
  
  absFrame <- absFrame[-1,]
  absFrame <- cbind(absFrame, Absscores)
  if (cull) absFrame <- absFrame[!(absFrame$Absscores < 0),] 
  return(absFrame)
}




