

#Read in the csv file containing the data, in order for this command
#to work, the csv file must be saved in the working directory, the folder that this
# rbeta program is saved in. 


Beta  <- read.csv("Beta-testers-raw-data-CommaD.csv", header= TRUE, sep= "," )
CDCI1 <- read.csv("CDCI1pre.csv", header= TRUE, sep= "," )
CDCI3 <- read.csv("CDCI-03Ppre.csv", header= TRUE, sep= "," )
CDCI4 <- read.csv("CDCI-04.csv", header= TRUE, sep= "," )
CDCI5 <- read.csv("CDCI-05post.csv", header= TRUE, sep= "," )
CDCI6 <- read.csv("CDCI-06post-revised.csv", header= TRUE, sep= "," )
CDCI1post <- read.csv("CDCI-01post-new.csv", header= TRUE, sep= "," )
CDCI2 <- read.csv("CDCI-02pre-corrected.csv", header= TRUE, sep= "," )
CDCI9 <- read.csv("CDCI9pre.csv", header= TRUE, sep= "," )
CDCI10 <- read.csv("CDCI-10Npost.csv", header= TRUE, sep= "," )
Wright <- read.csv("Wright2015.csv", header= TRUE, sep= "," )
Wrightpost <- read.csv("Wrightpost.csv", header= TRUE, sep= "," )
# This command trims of the unnecessary columns that don't contain answer data

CDCI10 <- CDCI10[4:29]
CDCI3 <-CDCI3[6:31]
CDCI6 <-CDCI6[6:31]
CDCI4 <-CDCI4[6:31]
CDCI2 <- CDCI2[3:28]
CDCI1post <- CDCI1post[5:30]
Wright <- Wright[1:26]
Wrightpost <- Wrightpost[2:27]
CDCI5 <- CDCI5[7:32]
CDCI1 <- CDCI1[2:27]
CDCI9 <- CDCI9[2:27]
Beta <- Beta[2:27]

preIntro <- rbind(Beta, CDCI1, CDCI3, CDCI4, CDCI9)

postIntro <- rbind(CDCI2, CDCI5, CDCI6, CDCI1post)

preMid <- rbind( CDCI10, Wright)

postMid <- rbind(Wrightpost)


###############################################################
#####
##### CHOOSE DATA HERE
#####
###############################################################
data <- rbind(preIntro)


ClassData <- data[1:2]
AnswerData <- data[3:26]

AnswerData <- as.data.frame( lapply(AnswerData,function(x) if(is.character(x)|is.factor(x)){ 
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
	score <- round(100*sum(qScores)/sum(key$numAns,4))
	if (invalid) { score <- -1}
	return(c(score, qScores))
}

ansNames <- c(

"Q3A","Q3B","Q3C","Q3D","Q3E",
"Q4A","Q4B","Q4C","Q4D","Q4E",
"Q5A","Q5B","Q5C",
"Q6A","Q6B","Q6C",
"Q7A","Q7B","Q7C","Q7D",
"Q8A","Q8B","Q8C","Q8D",

"Q9A","Q9B","Q9C","Q9D",
"Q10A","Q10B","Q10C","Q10D",
"Q11A","Q11B","Q11C","Q11D",
"Q12A","Q12B","Q12C",
"Q13A","Q13B","Q13C","Q13D",
"Q14A","Q14B","Q14C","Q14D","Q14E",

"Q15A","Q15B","Q15C","Q15D","Q15E",
"Q16A","Q16B","Q16C","Q16D",
"Q17A","Q17B","Q17C","Q17D",
"Q18A","Q18B","Q18C","Q18D",
"Q19A","Q19B","Q19C","Q19D","Q19E",
"Q20A","Q20B","Q20C","Q20D",

"Q21A","Q21B","Q21C",
"Q22A","Q22B","Q22C","Q22D",
"Q23A","Q23B","Q23C",
"Q24A","Q24B","Q24C","Q24D",
"Q25A","Q25B","Q25C","Q25D",
"Q26A","Q26B","Q26C","Q26D")


prtFrame <- data.frame(matrix(ncol=sum(key$numAns), nrow=1))
colnames(prtFrame) <- ansNames

absFrame <- data.frame(matrix(ncol=length(AnswerData), nrow=1))
colnames(absFrame) <- colnames(AnswerData)

Ascores <- c()
Pscores <- c()

for ( a in 1:length(AnswerData[,1])){
	Ascores <- c(Ascores, scoreABS(AnswerData[a,])[1])	
	Pscores <- c(Pscores, scoreTF(AnswerData[a,])[1])
	prtFrame <- rbind(prtFrame, scoreTF(AnswerData[a,])[2:97])
	absFrame <- rbind(absFrame, scoreABS(AnswerData[a,])[2:25])
}
prtFrame <- prtFrame[-1,]
absFrame <- absFrame[-1,]

AnswerData <- cbind(AnswerData, Ascores, Pscores)
prtFrame <- cbind(prtFrame, Ascores, Pscores)

AnswerData <- AnswerData[!(AnswerData$Pscores < 0),] 



prtFrame <- prtFrame[!(prtFrame$Ascores == -1),]

Pscores  <- Pscores[!(Pscores == -1)]

Ascores  <- Ascores[!(Ascores == -1)]

#returns the disrimintaor given a certain question index
getD <- function(question, max=FALSE){
	print(colnames(prtFrame)[question])
	modframe <- prtFrame[,-question]
	modscores <- c()
	for(i in 1:length(modframe[[1]])){
		x <- table(as.numeric(modframe[i,]))
		sc <-  (sum(x) - x[1])/sum(x)
		if (is.na(sc)){ sc <- 0}
		modscores <- c(modscores,round(sc,2))
	}
	cut <- round(length(modscores) *(27/100))
	modframe <- cbind(modframe,FOCUS=prtFrame[,question],modscores)
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


Dif <- c()
Pcorr <- c()
Acorr <- c()
Ds <- c()
pDE <- c()
if(2==1){
for( question in 1:(length(prtFrame)-2)){
	qTable <- table(prtFrame[question])
	Pcorr <- c(Pcorr,round( cor(prtFrame[question] , prtFrame$Pscores),2))
	Acorr <- c(Acorr, round(cor(prtFrame[question] , prtFrame$Ascores),2))
	Dif <- c(Dif,round(qTable[1]/sum(qTable),2))
	Ds <- c(Ds, round(getD(question),3))
	pDE  <- c(pDE, round(getD(question, max=TRUE),3))

}


DE <-round(Ds/pDE, 3)

Aprofiles <- data.frame(ansNames, Dif, Pcorr, Acorr,  D=Ds[1:96] , D.E=DE[1:96])
Aprofiles
}
#write.table(Aprofiles, "CDCIanswers.txt", sep="\t")


cohens_d <- function(x, y) {
    lx <- length(x)- 1
    ly <- length(y)- 1
    md  <- abs(mean(x) - mean(y))        ## mean difference (numerator)
    csd <- lx * var(x) + ly * var(y)
    csd <- csd/(lx + ly)
    csd <- sqrt(csd)                     ## common sd computation

    cd  <- md/csd                        ## cohen's d
}