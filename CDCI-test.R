source("CDCI-Analysis.R")

#BME + Biol 201

bio1T     <-  read.csv("BIOL201 section 1 PRE.csv", header= TRUE, sep= "," )[1:24]
bio1postT <-  read.csv("BIOL201 section 1 POST.csv", header= TRUE, sep= "," )[1:24]
bio2T     <-  read.csv("BIOL201 section 2 PRE.csv", header= TRUE, sep= "," )[1:24]
bio2postT <-  read.csv("BIOL201 section 2 POST.csv", header= TRUE, sep= "," )[1:24]
bio3T     <-  read.csv("BIOL201 section 3 PRE.csv", header= TRUE, sep= "," )[1:24]
bio3postT <-  read.csv("BIOL201 section 3 POST.csv", header= TRUE, sep= "," )[1:24]
bmeT      <-  read.csv("BME PRE.csv", header= TRUE, sep= "," )[1:24]
bmepostT  <-  read.csv("BME POST.csv", header= TRUE, sep= "," )[1:24]


testData <-  rbind(bmeT, bmepostT)
prt <-getPRT(testData)
abs <-  getAbs(testData)
tf <- getTF(testData)

hist(prt$Prtscores)
