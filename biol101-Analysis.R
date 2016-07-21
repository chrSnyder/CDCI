# Christopher Snyder 
# Analysis of Carter IB pre and bio121 post IVV data

source("CDCI-analysis.R")
carter <- read.csv("pretestsIBCarter.csv", header = T)
bio121 <- read.csv("Bio121post.csv", header = T)

key <- read.csv("ivvprekey.csv", header = T)
names(key) <- c("number", "Ans", "numAns")

a <- getAbs(carter, myKey = key)
b <- getPRT(carter, myKey = key)
c <- getTF(carter, myKey = key)

x <- getAbs(bio121, myKey = key)
y <- getPRT(bio121, myKey = key)
z <- getTF(bio121, myKey = key)


write.table(a, "carter_abs.csv", row.names = F)
write.table(b, "carter_prt.csv", row.names = F)
write.table(c, "carter_tf.csv", row.names = F)

write.table(x, "bio121_abs.csv", row.names = F)
write.table(y, "bio121_prt.csv", row.names = F)
write.table(z, "bio121_tf.csv", row.names = F)

groom(carter)
