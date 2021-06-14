install.packages("ggplot2")
install.packages("readtext")
install.packages("psych")


library(readtext)
library(ggplot2)
library(psych)

# Set working directory
setwd("Documents/BookClub/BC2Clean")

#EITHER
# Parse makeup only sessions and load
#grep -v NM predict_ir_6bmsr4.txt > predict_ir_6bmsr4_m.txt
fname <- "predict_gn_6bmsr4_m.txt"
m_test <- read.delim(fname, sep = "", header = T, na.strings = " ", fill = T)
mr <- nrow(m_test)

#OR
# Parse 'difficult' sessions and load
#egrep "S1HD1|S1MK2|S1MK3|S7MK2|S7FM1|S10MK2|S10MK3|S10MK4|S14HD1|S20MK1|S21MK1" predict_ir_6bmsr4.txt > predict_ir_6bmsr4_mp.txt
fname <- "predict_an_6bmsr4_mp.txt"
m_test <- read.delim(fname, sep = "", header = F, na.strings = " ", fill = T)
mr <- nrow(m_test)

#Process
t2 <- m_test[,1:7]
t2[,8] <- FALSE

names(t2) <- c("Correct_class", "Score", "Guess_class", "TrustedVote", "VoteScore", "TrustedScore","File", "Flag")
for (i in 1:mr){
  if( t2$Correct_class[i] == t2$Guess_class[i] ){
    t2$Flag[i] <- TRUE
  }
}

t2r <- t2[t2$Flag == TRUE,]
t2w <- t2[t2$Flag != TRUE,]

# Set up thresholds
TVt = 0.5
VSt = 0.95

#Untrusted Accuracy
length(t2r$Score)/(length(t2r$Score) + length(t2w$Score))

#Trusted Accuracy Meta
(sum(t2r$TrustedVote >= TVt)+sum(t2w$TrustedVote < TVt))/(length(t2r$Score) + length(t2w$Score))
(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)+sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt))/(length(t2r$Score) + length(t2w$Score))
#Precision Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2w$TrustedVote >= TVt))
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))
#Recall Meta
sum(t2r$TrustedVote >= TVt)/(sum(t2r$TrustedVote >= TVt) + sum(t2r$TrustedVote < TVt)) #length(t2r$Score)
sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt)/(sum(t2r$TrustedVote >= TVt & t2r$VoteScore >= VSt) + sum(t2r$TrustedVote < TVt & t2r$VoteScore >= VSt))
#Specificity Meta
sum(t2w$TrustedVote < TVt)/(sum(t2w$TrustedVote < TVt) + sum(t2w$TrustedVote >= TVt)) #length(t2w$Score)
sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt)/(sum(t2w$TrustedVote < TVt & t2w$VoteScore >= VSt) + sum(t2w$TrustedVote >= TVt & t2w$VoteScore >= VSt))
