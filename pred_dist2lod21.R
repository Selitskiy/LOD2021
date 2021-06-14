install.packages("ggplot2")
install.packages("readtext")
install.packages("psych")
#install.packages("car")
install.packages("ggpubr")
#install.packages("dgof")
install.packages("kSamples")

library(readtext)
library(ggplot2)
library(psych)
#library(car)
library(ggpubr)
#library(dgof)
library(kSamples)

setwd("Documents/BookClub/BC2Clean")

probs1 <- c(.75,.9,.95)
probs2 <- c(.25,.1,.05)

#grep NM predict_ir_6bmsr1.txt > predict_ir_6bmsr1_nm.txt
fname <- "predict_an_6bmsr1_nm.txt"
nm_test <- read.delim(fname, sep = "", header = F, na.strings = " ", fill = T)
nr <- nrow(nm_test)
#for (i in 1:nr){
#  nm_test[i,26:47] <- sort(nm_test[i,5:25], decreasing = T)
#}
t1 <- nm_test[,1:3]
t1[,4] <- nm_test[,7]
t1[,5] <- FALSE
#t1[,6:8] <- nm_test[,26:28]
t1[,6] <- "N"
names(t1) <- c("Correct_class", "Score", "Guess_class", "File", "Flag", "Makeup")
for (i in 1:nr){
  if( t1$Correct_class[i] == t1$Guess_class[i] ){
    t1$Flag[i] <- TRUE
  }
}

tr <- t1[t1$Flag == TRUE,]
tw <- t1[t1$Flag != TRUE,]

#tr1 <- t1[t1$Flag == TRUE & t1$Correct_class == "S1",]
#tw1 <- t1[t1$Flag != TRUE & t1$Correct_class == "S1",]
#tr2 <- t1[t1$Flag == TRUE & t1$Correct_class == "S2",]
#tw2 <- t1[t1$Flag != TRUE & t1$Correct_class == "S2",]
#tr3 <- t1[t1$Flag == TRUE & t1$Correct_class == "S3",]
#tw3 <- t1[t1$Flag != TRUE & t1$Correct_class == "S3",]
#tr4 <- t1[t1$Flag == TRUE & t1$Correct_class == "S4",]
#tw4 <- t1[t1$Flag != TRUE & t1$Correct_class == "S4",]
#tr5 <- t1[t1$Flag == TRUE & t1$Correct_class == "S5",]
#tw5 <- t1[t1$Flag != TRUE & t1$Correct_class == "S5",]


#grep -v NM predict_ir_6bmsr1.txt > predict_ir_6bmsr1_m.txt
fname <- "predict_an_6bmsr1_m.txt"
m_test <- read.delim(fname, sep = "", header = T, na.strings = " ", fill = T)
mr <- nrow(m_test)
#for (i in 1:mr){
#  m_test[i,26:47] <- sort(m_test[i,5:25], decreasing = T)
#}
t2 <- m_test[,1:4]
t2[,4] <- m_test[,7]
t2[,5] <- FALSE
#t2[,6:8] <- m_test[,26:28]
t2[,6] <- "Y"
names(t2) <- c("Correct_class", "Score", "Guess_class", "File", "Flag","Makeup")
for (i in 1:mr){
  if( t2$Correct_class[i] == t2$Guess_class[i] ){
    t2$Flag[i] <- TRUE
  }
}

t2r <- t2[t2$Flag == TRUE,]
t2w <- t2[t2$Flag != TRUE,]


ggplot(tw, aes(x=Score)) + 
    #geom_histogram(data=tw, fill="green", binwidth = 0.01, alpha=0.6) +
    #geom_histogram(data=t2w, fill="red", binwidth = 0.01, alpha=0.6) +
  geom_density(data=tw, color="green", fill="green", adjust=0.25, alpha=0.6) +
  geom_density(data=t2w, color="red", fill="red", adjust=0.25, alpha=0.6) +
  #labs(title="Correctly (green) and wrongly (red) recognized image distributions (no-makeup training)",
  #     x="Softmax activations (probability/score)", y="Density") +
  xlim(0, 1.05) + ylim(0, 5)

describe(t2w$Score)
#describe(t2r$Score)

describe(tw$Score)
#describe(tr$Score)


twa <- rbind(tw, t2w)
ggboxplot(twa, x = "Makeup", y = "Score") 
#          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#          order = c("ctrl", "trt1", "trt2"),
#          ylab = "Weight", xlab = "Treatment")
ggplot(twa, aes(factor(Makeup), Score)) + 
  geom_violin()
#(scale="width")
#leveneTest(Score ~ Makeup, data = twa)
#fligner.test(Score ~ Makeup, data = twa)

alf <- c(.25, .1, .05) #, .01, .00003)
n <- length(tw$Score)
#twr <- sample(tw$Score, n)
#t2wr <- sample(t2w$Score, n)
m <- length(t2w$Score)
Dnm <- sqrt(-log(alf/2) * (1+m/n)/(2*m))
Dnm
ks.test(t2w$Score, tw$Score)

#ad.test(twr, t2wr)
#res <- aov(Score ~ Makeup, data=twa)
#summary(res)

#cvm.test(sort(tw$P1), sort(t2w$P1))
#kruskal.test(Score ~ Makeup, data = twa)
#wilcox.test(Score ~ Makeup, data = twa)
#cor.test(tw$Score, t2w$Score, method = "kendall")


thr <- quantile(tw$Score, probs1)
thr

#thrp <- quantile(t2w$Score, probs1)
#thrp

#thr2 <- quantile(tw$Score, probs2)
#thr2

#t2wr <- t2w$Score[t2w$Score <= thr[1]]
#twr <- tw$Score[tw$Score <= thr[1]]
#alf <- c(.25, .1, .05) #, .01, .00003)
#n <- length(twr)
#m <- length(t2wr)
#Dnm <- sqrt(-log(alf/2) * (1+m/n)/(2*m))
#Dnm
#ks.test(t2wr, twr)


#Prior
#Untrusted Accuracy
sum(tr$Score)/(length(tr$Score) + length(tw$Score))

#Trusted Accuracy Prior
#(sum(tr$Score > thr[1]))/(length(tr$Score) + length(tw$Score))
#(sum(tr$Score > thr[2]))/(length(tr$Score) + length(tw$Score))
#(sum(tr$Score > thr[3]))/(length(tr$Score) + length(tw$Score))
#Trusted Accuracy Prior TN
(sum(tr$Score > thr[1])+sum(tw$Score <= thr[1]))/(length(tr$Score) + length(tw$Score))
(sum(tr$Score > thr[2])+sum(tw$Score <= thr[2]))/(length(tr$Score) + length(tw$Score))
(sum(tr$Score > thr[3])+sum(tw$Score <= thr[3]))/(length(tr$Score) + length(tw$Score))
#Precision
sum(tr$Score > thr[1])/(sum(tr$Score > thr[1]) + sum(tw$Score > thr[1]))
sum(tr$Score > thr[2])/(sum(tr$Score > thr[2]) + sum(tw$Score > thr[2]))
sum(tr$Score > thr[3])/(sum(tr$Score > thr[3]) + sum(tw$Score > thr[3]))
#Recall
sum(tr$Score > thr[1])/length(tr$Score)
sum(tr$Score > thr[2])/length(tr$Score)
sum(tr$Score > thr[3])/length(tr$Score)

#Specificity/Percentile confirm
sum(tw$Score <= thr[1])/length(tw$Score)
sum(tw$Score <= thr[2])/length(tw$Score)
sum(tw$Score <= thr[3])/length(tw$Score)




#Posterior
#Untrusted Accuracy
sum(t2r$Score)/(length(t2r$Score) + length(t2w$Score))

#Trusted Accuracy Posterior, Posterior threshold
#(sum(t2r$P1 > thrp[1])+sum(t2w$P1 <= thrp[1]))/(length(t2r$P1) + length(t2w$P1))
#(sum(t2r$P1 > thrp[2])+sum(t2w$P1 <= thrp[2]))/(length(t2r$P1) + length(t2w$P1))
#(sum(t2r$P1 > thrp[3])+sum(t2w$P1 <= thrp[3]))/(length(t2r$P1) + length(t2w$P1))
#Precision
#sum(t2r$P1 > thrp[1])/(sum(t2r$P1 > thrp[1]) + sum(t2w$P1 > thrp[1]))
#sum(t2r$P1 > thrp[2])/(sum(t2r$P1 > thrp[2]) + sum(t2w$P1 > thrp[2]))
#sum(t2r$P1 > thrp[3])/(sum(t2r$P1 > thrp[3]) + sum(t2w$P1 > thrp[3]))
#Recall
#sum(t2r$P1 > thrp[1])/length(t2r$P1)
#sum(t2r$P1 > thrp[2])/length(t2r$P1)
#sum(t2r$P1 > thrp[3])/length(t2r$P1)

#Percentile verification
#sum(t2w$P1 <= thrp[1])/length(t2w$P1)
#sum(t2w$P1 <= thrp[2])/length(t2w$P1)
#sum(t2w$P1 <= thrp[3])/length(t2w$P1)


#Trusted Accuracy Posterior, Prior threshold
#(sum(t2r$Score > thr[1]))/(length(t2r$Score) + length(t2w$Score))
#(sum(t2r$Score > thr[2]))/(length(t2r$Score) + length(t2w$Score))
#(sum(t2r$Score > thr[3]))/(length(t2r$Score) + length(t2w$Score))
#Trusted Accuracy Posterior TN, Prior threshold
(sum(t2r$Score > thr[1])+sum(t2w$Score <= thr[1]))/(length(t2r$Score) + length(t2w$Score))
(sum(t2r$Score > thr[2])+sum(t2w$Score <= thr[2]))/(length(t2r$Score) + length(t2w$Score))
(sum(t2r$Score > thr[3])+sum(t2w$Score <= thr[3]))/(length(t2r$Score) + length(t2w$Score))
#Precision
sum(t2r$Score > thr[1])/(sum(t2r$Score > thr[1]) + sum(t2w$Score > thr[1]))
sum(t2r$Score > thr[2])/(sum(t2r$Score > thr[2]) + sum(t2w$Score > thr[2]))
sum(t2r$Score > thr[3])/(sum(t2r$Score > thr[3]) + sum(t2w$Score > thr[3]))
#Recall
sum(t2r$Score > thr[1])/length(t2r$Score)
sum(t2r$Score > thr[2])/length(t2r$Score)
sum(t2r$Score > thr[3])/length(t2r$Score)

#Specificity/Percentile verification
sum(t2w$Score <= thr[1])/length(t2w$Score)
sum(t2w$Score <= thr[2])/length(t2w$Score)
sum(t2w$Score <= thr[3])/length(t2w$Score)



#Trustd Accuracy
#sum(t2r$P2 < thr2[1] & t2r$P1 > thr[1])/(length(t2r$P2) + length(t2w$P2))
#sum(t2r$P2 < thr2[2] & t2r$P1 > thr[2])/(length(t2r$P2) + length(t2w$P2))
#sum(t2r$P2 < thr2[3] & t2r$P1 > thr[3])/(length(t2r$P2) + length(t2w$P2))
#Precision
#sum(t2r$P2 < thr2[1] & t2r$P1 > thr[1])/(sum(t2r$P2 < thr2[1] & t2r$P1 > thr[1]) + sum(t2w$P2 < thr2[1] & t2w$P1 > thr[1]))
#sum(t2r$P2 < thr2[2] & t2r$P1 > thr[2])/(sum(t2r$P2 < thr2[2] & t2r$P1 > thr[2]) + sum(t2w$P2 < thr2[2] & t2w$P1 > thr[2]))
#sum(t2r$P2 < thr2[3] & t2r$P1 > thr[3])/(sum(t2r$P2 < thr2[3] & t2r$P1 > thr[3]) + sum(t2w$P2 < thr2[3] & t2w$P1 > thr[3]))



#ggplot(tr, aes(x=P2)) + 
#    #geom_histogram(data=tw, fill="green", binwidth = 0.01, alpha=0.6) +
#    geom_histogram(data=tw1, fill="red1", binwidth = 0.01, alpha=0.6) +
#    geom_histogram(data=tw2, fill="red2", binwidth = 0.01, alpha=0.6) +
#    geom_histogram(data=tw3, fill="red3", binwidth = 0.01, alpha=0.6) +
#    geom_histogram(data=tw4, fill="red4", binwidth = 0.01, alpha=0.6) +
#    geom_histogram(data=tw5, fill="red", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tr1, fill="green1", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tr2, fill="green2", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tr3, fill="green3", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tr4, fill="green4", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tr5, fill="green", binwidth = 0.01, alpha=0.6) +
#  #geom_density(data=tw, color="red", fill="red", adjust=0.25, alpha=0.6) +
#  #geom_density(data=tr, color="green", fill="green", adjust=0.25, alpha=0.6) +
#  labs(title="Correctly (green) and wrongly (red) recognized image distributions (no-makeup training)",
#       x="Softmax activations (probability/score)", y="Density") +
#  xlim(0, .55) + ylim(0, 25)

#describe(tw$P2)
#describe(tr$P2)

#describe(tw1$P2)
#describe(tw2$P2)
#describe(tw3$P2)

#describe(tr1$P2)
#describe(tr2$P2)
#describe(tr3$P2)



#ggplot(tr, aes(x=P2)) + 
#  geom_histogram(data=tr, fill="green", binwidth = 0.01, alpha=0.6) +
#  geom_histogram(data=tw, fill="red", binwidth = 0.01, alpha=0.6) +
#  #geom_density(data=tw, color="red", fill="red", adjust=0.25, alpha=0.6) +
#  #geom_density(data=tr, color="green", fill="green", adjust=0.25, alpha=0.6) +
#  labs(title="Correctly (green) and wrongly (red) recognized image distributions (no-makeup training)",
#       x="Softmax activations (probability/score)", y="Density") +
#  xlim(0, 1.05) + ylim(0, 70)

#describe(tr$P2)
#describe(tw$P2)







#t1 <- acc[1:nr,]
#t1[5] <- FALSE
#t1[6] <- FALSE
#t1[7] <- FALSE
#t1[8] <- FALSE
##rep(FALSE, nr)

#names(t1) <- c("Correct_class", "Score", "Guess_class","File", "Flag", "Flag1", "Flag2", "Flag3")

#for (i in 1:nr){
#  if( t1$Correct_class[i] == t1$Guess_class[i] ){
#    t1$Flag[i] <- TRUE
#  }
#}

#tr <- t1[t1$Flag == TRUE,]
#tw <- t1[t1$Flag != TRUE,]

#data <- as.data.frame(t1)

##ggplot(data, aes(x=GuessScore, color=V26, fill=V26)) + 
##  geom_histogram(binwidth = 0.05, alpha=0.6)

#ggplot(tr, aes(x=Score)) + 
##  geom_histogram(data=tr, fill="green", binwidth = 0.02, alpha=0.6) +
##  geom_histogram(data=tw, fill="red", binwidth = 0.02, alpha=0.6) +
#  geom_density(data=tw, color="red", fill="red", adjust=0.25, alpha=0.6) +
#  geom_density(data=tr, color="green", fill="green", adjust=0.25, alpha=0.6) +
#  labs(title="Correctly (green) and wrongly (red) recognized image distributions (no-makeup training)",
#       x="Softmax activations (probability/score)", y="Density") +
#  xlim(0, 1.05) + ylim(0, 175)

#describe(tr$Score)

#describe(tw$Score)

#thr <- quantile(tw$Score, probs)

#thr

##tr$Flag1[tr$Score >= thr[1]] <- TRUE
##tr$Flag2[tr$Score >= thr[2]] <- TRUE
##tr$Flag3[tr$Score >= thr[3]] <- TRUE

#sum(tr$Score > thr[1])/(length(tr$Score) + length(tw$Score))
#sum(tr$Score > thr[2])/(length(tr$Score) + length(tw$Score))
#sum(tr$Score > thr[3])/(length(tr$Score) + length(tw$Score))



#fname2 <- "predict_5d2.txt"


#acc2 <- read.delim(fname2, sep = "", header = F, na.strings = " ", fill = T)

#nr2 <- nrow(acc2)
#t2 <- acc2[1:nr2,]
#t2[5] <- rep(FALSE, nr2)

#names(t2) <- c("Correct_class", "Score", "Guess_class","File", "Flag")

#for (i in 1:nr2){
#  if( t2$Correct_class[i] == t2$Guess_class[i] ){
#    t2$Flag[i] <- TRUE
#  }
#}

#tr2 <- t2[t2$Flag == TRUE,]
#tw2 <- t2[t2$Flag != TRUE,]

#data2 <- as.data.frame(t2)

#ggplot(tr2, aes(x=Score)) + 
#  #  geom_histogram(data=tr2, fill="green", binwidth = 0.02, alpha=0.6) +
#  #  geom_histogram(data=tw2, fill="red", binwidth = 0.02, alpha=0.6) +
#  geom_density(data=tw2, color="red", fill="red", adjust=0.25, alpha=0.6) +
#  geom_density(data=tr2, color="green", fill="green", adjust=0.25, alpha=0.6) +
#  labs(title="Correctly (green) and wrongly (red) recognized image distributions (makeup infused training)",
#       x="Softmax activations (probability/score)", y="Density") +
#  xlim(0, 1.05) + ylim(0, 175)

#describe(tr2$Score)

#describe(tw2$Score)

#thr2 <- quantile(tw2$Score, probs)

#thr2

#sum(tr2$Score > thr2[1])/length(tr2$Score) * length(tr2$Score)/(length(tr2$Score) + length(tw2$Score))
#sum(tr2$Score > thr2[2])/length(tr2$Score) * length(tr2$Score)/(length(tr2$Score) + length(tw2$Score))
#sum(tr2$Score > thr2[3])/length(tr2$Score) * length(tr2$Score)/(length(tr2$Score) + length(tw2$Score))



#Right
#ggplot(tr, aes(x=Score)) + 
#  #  geom_histogram(data=tr2, fill="green", binwidth = 0.02, alpha=0.6) +
#  #  geom_histogram(data=tw2, fill="red", binwidth = 0.02, alpha=0.6) +
#  geom_density(data=tr, color="green", fill="green", adjust=0.25, alpha=0.6) +
#  geom_density(data=tr2, color="green4", fill="green4", adjust=0.25, alpha=0.6) +
#  xlim(0.9, 1.00) + ylim(0, 50)


#wrong
#ggplot(tw, aes(x=Score)) + 
#  #  geom_histogram(data=tr2, fill="green", binwidth = 0.02, alpha=0.6) +
#  #  geom_histogram(data=tw2, fill="red", binwidth = 0.02, alpha=0.6) +
#  geom_density(data=tw, color="gray", fill="gray", adjust=0.25, alpha=0.6) +
#  geom_density(data=tw2, color="gray50", fill="gray50", adjust=0.25, alpha=0.6) +
#  xlim(0.85, 1.05) 
##+ ylim(0, 175)


#Export
#ec <- tr[,1:3]
#write.csv(ec, "correct1.csv", row.names=F)

#ew <- tw[,1:3]
#write.csv(ew, "wrong1.csv", row.names=F)

#ec2 <- tr2[,1:3]
#write.csv(ec2, "correct2.csv", row.names=F)

#ew2 <- tw2[,1:3]
#write.csv(ew2, "wrong2.csv", row.names=F)
