# Christopher Snyder 


source("CDCI-Analysis.R")
cdci3_abs <-  getAbs(CDCI8[3:26])
cdci3_prt <-  getPRT(CDCI8[3:26])
numcorrect <- apply(cdci8_abs, 1, function(x){ table(x)[2]})

png("cdci_report.png")

barplot(table(numcorrect), ylab = "No. of students", xlab = "questions fully correct" )

dev.off()
png("prthist.png")

prt <- cdci8_prt$Prtscores
bottom <- length(prt[prt < 50])
sixty <- length(prt[prt >= 50& prt < 60])
seventy <- length(prt[prt >= 60 &prt < 70 ])
eighty <- length(prt[prt >= 70 &prt < 80 ])
ninety <- length(prt[prt >= 80 &prt < 90 ])
top  <- length(prt[prt >= 90])

distr <-  c(bottom, sixty, seventy, eighty, ninety, top)

barplot(distr,  xlab = "Partial credit score", ylab = "Frequency", main= "Score Range Distibution",  names.arg = c("< 50" , "50-59", "60-69", "70-79", "80-89", ">90"), col = "blue")
dev.off()

png("qplots1-5.png")
par(mfrow = c(3,2))

for (a in 1:5)
	{
	qPlot(CDCI3[3:26],a)
	}

par(mfrow = c(1,1))

dev.off()
png("qplots6-10.png")
par(mfrow = c(3,2))

for (a in 6:10)
	{
	qPlot(CDCI3[3:26],a)
	}

par(mfrow = c(1,1))

dev.off()
png("qplots11-15.png")
par(mfrow = c(3,2))

for (a in 11:15)
	{
	qPlot(CDCI3[3:26],a)
	}

par(mfrow = c(1,1))

dev.off()
png("qplots16-20.png")
par(mfrow = c(3,2))

for (a in 16:20)
	{
	qPlot(CDCI3[3:26],a)
	}

par(mfrow = c(1,1))

dev.off()

png("qplots21-24.png")
par(mfrow = c(3,2))

for (a in 21:24)
	{
	qPlot(CDCI3[3:26],a)
	}

par(mfrow = c(1,1))

dev.off()
