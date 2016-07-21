

#Read in the csv file containing the data, in order for this command
#to work, the csv file must be saved in the working directory, the folder that this
# rbeta program is saved in. 


Beta  <- read.csv("Beta-testers-raw-data-CommaD.csv", header= TRUE, sep= "," )
CDCI1 <- read.csv("CDCI1pre.csv", header= TRUE, sep= "," )
CDCI3 <- read.csv("CDCI-03Ppre.csv", header= TRUE, sep= "," )
CDCI4 <- read.csv("CDCI-04.csv", header= TRUE, sep= "," )
CDCI5 <- read.csv("CDCI-05post.csv", header= TRUE, sep= "," )
CDCI6 <- read.csv("CDCI-06post-revised.csv", header= TRUE, sep= "," )

CDCI2 <- read.csv("CDCI-02pre-corrected.csv", header= TRUE, sep= "," )
CDCI9 <- read.csv("CDCI9pre.csv", header= TRUE, sep= "," )
CDCI10 <- read.csv("CDCI-10pre1.csv", header= TRUE, sep= "," )
Wright <- read.csv("Wright2015.csv", header= TRUE, sep= "," )
# This command trims of the unnecessary columns that don't contain answer data
CDCI10 <- CDCI10[8:31]
CDCI3 <-CDCI3[8:31]
CDCI6 <-CDCI6[8:31]
CDCI4 <-CDCI4[8:31]
CDCI2 <- CDCI2[5:28]
Wright <- Wright[3:26]
CDCI5 <- CDCI5[9:32]
CDCI1 <- CDCI1[4:27]
CDCI9 <- CDCI9[4:27]
Beta <- Beta[4:27]
DataSet <- c(CDCI10, CDCI4, CDCI5,  Beta, Wright)

AnswerData <- rbind(CDCI5, CDCI6,CDCI10, Wright)#, CDCI5, Wright,CDCI3, CDCI9, CDCI10)

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


Ans <- c("AD", "BE", "A", "A", "CD" , "ABCD", "A" , "C" , "AC"
,"AB" , "A", "AC" , "CD" , "ABD" , "BD", "B", "CD" , "B" , "A" , "A" , "B" ,
"C" , "A", "ABCD" )

numAns <- c(5,5,3,3,4, 4,4,4,4,3, 4,5,5,4,4, 4,5,4,3,4, 3,4,4,4)

key <- data.frame(Ans,numAns)
 



prtFrame <- data.frame(matrix(ncol=length(AnswerData), nrow=1))
colnames(prtFrame) <- colnames(AnswerData)

absFrame <- data.frame(matrix(ncol=length(AnswerData), nrow=1))
colnames(absFrame) <- colnames(AnswerData)

scoreABS <- function(answers, focus=0){
	qScores <- c()
	score <- 0 
	invalid <- FALSE
	for( a in 1:length(answers)){
		given  <- as.character( answers[1,a])
		if (a != focus){	
			if (as.character( answers[1,a])  == as.character(key$Ans[a])){
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
		if(given %in% c("", " ")){
			 invalid <- TRUE
		}
		qScores <- c(qScores, round(qScore/num,2))
	}
	

	score <- round(100*sum(qScores)/length(answers),2)
	if (invalid) { score <- -1}
	return(c(score, qScores))
}



Ascores <- c()
Pscores <- c()

for ( a in 1:length(AnswerData[,1])){
	Ascores <- c(Ascores, scoreABS(AnswerData[a,])[1])	
	Pscores <- c(Pscores, scorePRT(AnswerData[a,])[1])
	prtFrame <- rbind(prtFrame, scorePRT(AnswerData[a,])[2:25])
	absFrame <- rbind(absFrame, scoreABS(AnswerData[a,])[2:25])
}
prtFrame <- prtFrame[-1,]
absFrame <- absFrame[-1,]

AnswerData <- cbind(AnswerData, Ascores, Pscores)
prtFrame <- cbind(prtFrame, Ascores, Pscores)
absFrame <- cbind(absFrame, Ascores, Pscores)
AnswerDAta <- AnswerData[!(AnswerData$Pscores == -1),] 

absFrame <- absFrame[!(absFrame$Ascores == -1),]
prtFrame <- prtFrame[!(prtFrame$Ascores == -1),]
Ascores  <- Ascores[!(Ascores == -1)]
Pscores  <- Pscores[!(Pscores == -1)]
scorePRTtable <-table(Pscores)
scoreABStable <-table(Ascores)


barplot(scoreABStable,space=1, main="Absolute Scores", col="red")
barplot(scorePRTtable,space=1, main="Partial Scores", col="blue")

hist(
Pscores,
breaks=40, 
col="blue",
xlim=c(0,100),
ylim=c(0,500),
main="Histogram of Absolute and Partial Scores",
ylab="Number of Students",
xlab="Absolute Score (Green) Partial Score (Blue)")

hist(
Ascores,
breaks=36 ,
add=T,
col=rgb(0, 1, 0, 0.5),
xlim=c(0,100), 
main="Histogram of Absolute and Partial Scores", 
xlab="Absolute Score")


qPlot <- function(question, crop=7){
	table <- sort(table(AnswerData[question]))
	print(table)
	
	table <-(tail(table,crop))
	
	cols <- c("red","blue")
	correct <- names(table) == key$Ans[question]
	title <- paste("Question",as.character(question+2)) 
	barplot(table, main=title, col=cols[correct+1] , xlab = "Answer", ylab="Frequency")
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
	bottom <- head(modframe[question],cut)
	top <- tail(modframe[,question], cut)
	T <- table(top)
	B <- table(bottom)
	Ucorrect <- T[2]/ sum(T)
	if (max) {
		Ucorrect <- 1
	}
	Lcorrect <- B[2]/sum(B)
	return(Ucorrect - Lcorrect)
}


Pcors <- c()
Acors <- c()
Dif <- c()
Question <- c(3:26)
Ds <- c()
pDE <- c()

for(i in 1:length(AnswerData)){
	Pcors <- c(Pcors,round( cor(prtFrame[[i]], Pscores),2))
	Acors <- c(Acors,round( cor(absFrame[[i]], Ascores),2))
	x <- table(absFrame[[i]])
	#print(paste(i, x[1],x[2],  x[2]/x[1], sep="  "))
	Dif <- c(Dif,round( x[1]/sum(x), 2))
	Ds <- c(Ds,round(getD(i),2))
	pDE <- c(pDE , round(getD(i, max=TRUE),2))
	
}
print(Ds)
print(pDE)

DE <-round(Ds/pDE, 3)

qProfiles <- data.frame(Question,"Partial Correlation"=Pcors[1:24],
				"Absolute Correlation"=Acors[1:24], "Difficulty Index"=Dif[1:24],
				 D=Ds[1:24] , D.E=DE[1:24])[order(Question),]
qProfiles
write.table(qProfiles, "CDCIquestions.txt", sep="\t")



