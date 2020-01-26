purchase_transition_PC <- read.csv("Purchase transitions_PC.csv")

PCpurprob<-purchase_transition_PC$Probability
#matPCpurprob= matrix(PCpurprob, nrow=6, ncol=6, byrow=TRUE)

setsexst=cbind(rep(1:6,by=6),PCpurprob,rep(1:6, each=6))
colvec= c(2:6,9)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for PC sessions ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:6), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:6){
  lines(setsexst[((i-1)*6+1):(i*6),1],setsexst[((i-1)*6+1):(i*6),2],col=colvec[i],lty=i,lwd=2)}
legend(4.8,0.75,c("Event","Eventdetals","Seatmap", "Cart","Checkout","Ordered"),col=colvec,lwd=c(2,2,2,2,2,2),text.font=4,lty=c(1:6),title="Page specifications")
#library(ggplot2)
#Markovplot_purchase_PC<-qplot(data= purchase_transition_PC, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for PC sessions ending in purchase") 


purchase_transition_Smartphone<- read.csv("Purchase transitions_Smartphone.csv")
Phonepurprob<-purchase_transition_Smartphone$Probability
#matPCpurprob= matrix(PCpurprob, nrow=6, ncol=6, byrow=TRUE)
setsexst=cbind(rep(1:6,by=6),Phonepurprob,rep(1:6, each=6))
colvec= c(2:6,9)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for Smartphone sessions ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:6), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:6){
  lines(setsexst[((i-1)*6+1):(i*6),1],setsexst[((i-1)*6+1):(i*6),2],col=colvec[i],lty=i,lwd=2)}
legend(4.8,0.75,c("Event","Eventdetals","Seatmap", "Cart","Checkout","Ordered"),col=colvec,lwd=c(2,2,2,2,2,2),text.font=4,lty=c(1:6),title="Page specifications")


#Markovplot_purchase_Smartphone<-qplot(data= purchase_transition_Smartphone, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for Smartphone sessions ending in purchase") 

purchase_transition_Tablet <- read.csv("Purchase transitions_Tablet.csv")
Tabpurprob<-purchase_transition_Tablet$Probability
#matPCpurprob= matrix(PCpurprob, nrow=6, ncol=6, byrow=TRUE)
setsexst=cbind(rep(1:6,by=6),Tabpurprob,rep(1:6, each=6))
colvec= c(2:6,9)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for Tablet sessions ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:6), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:6){
  lines(setsexst[((i-1)*6+1):(i*6),1],setsexst[((i-1)*6+1):(i*6),2],col=colvec[i],lty=i,lwd=2)}
legend(4.8,0.75,c("Event","Eventdetals","Seatmap", "Cart","Checkout","Ordered"),col=colvec,lwd=c(2,2,2,2,2,2),text.font=4,lty=c(1:6),title="Page specifications")


#Markovplot_purchase_Tablet<-qplot(data= purchase_transition_Tablet, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for Tablet sessions ending in purchase") 


Nopurchase_transition_PC <- read.csv("No purchase transitions_PC.csv")
PCnopurprob<-Nopurchase_transition_PC$Probability
#matPCpurprob= matrix(PCnopurprob, nrow=5, ncol=5, byrow=TRUE)
setsexst=cbind(rep(1:5,by=5),PCnopurprob,rep(1:5, each=5))
colvec= c(2:6)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for PC sessions not ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:5), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:5){
  lines(setsexst[((i-1)*5+1):(i*5),1],setsexst[((i-1)*5+1):(i*5),2],col=colvec[i],lty=i,lwd=2)}
legend(3.8,0.75,c("Event","Eventdetals","Seatmap", "Cart","Checkout"),col=colvec,lwd=c(2,2,2,2,2),text.font=4,lty=c(1:5),title="Page specifications")
#Markovplot_nopurchase_PC<-qplot(data= Nopurchase_transition_PC, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for PC sessions not ending in purchase") 


Nopurchase_transition_Smartphone <- read.csv("No purchase transitions_Smartphone.csv")
Phonenopurprob<-Nopurchase_transition_Smartphone$Probability
#matPCpurprob= matrix(PCnopurprob, nrow=5, ncol=5, byrow=TRUE)
setsexst=cbind(rep(1:5,by=5),Phonenopurprob,rep(1:5, each=5))
colvec= c(2:6)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for Smartphone sessions not ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:5), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:5){
  lines(setsexst[((i-1)*5+1):(i*5),1],setsexst[((i-1)*5+1):(i*5),2],col=colvec[i],lty=i,lwd=2)}
legend(4.2,0.52,c("Event","Eventdetals","Seatmap", "Cart","Checkout"),col=colvec,lwd=c(2,2,2,2,2),text.font=4,lty=c(1:5),title="Page specifications")
#Markovplot_nopurchase_Smartphone<-qplot(data= Nopurchase_transition_Smartphone, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for Smartphone not sessions ending in purchase") 


Nopurchase_transition_Tablet <- read.csv("No purchase transitions_Tablet.csv")
Tabnopurprob<-Nopurchase_transition_Tablet$Probability
#matPCpurprob= matrix(PCnopurprob, nrow=5, ncol=5, byrow=TRUE)
setsexst=cbind(rep(1:5,by=5),Tabnopurprob,rep(1:5, each=5))
colvec= c(2:6)
plot(setsexst[,1],setsexst[,2],xaxt= 'n', type="n",xlab="Page",ylab="Probability",main="Transition probabilities for Tablet sessions not ending in purchase",cex.main=1.4,cex.lab=1.4) #,ylab="Conditional probability of rank groups R2 given R1"
#text(setsexst[,1],setsexst[,2],label=rep(1:6, each=6),col=rep(colvec, each=6),cex=1.5)
axis(side=1, at=c(1:5), labels = c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout"))
#mtext(c("Event", "Eventdetails", "Seatmap", "Cart", "Checkout", "Ordered"), at=c(1:6), side=1)
for(i in 1:5){
  lines(setsexst[((i-1)*5+1):(i*5),1],setsexst[((i-1)*5+1):(i*5),2],col=colvec[i],lty=i,lwd=2)}
legend(4.2,0.52,c("Event","Eventdetals","Seatmap", "Cart","Checkout"),col=colvec,lwd=c(2,2,2,2,2),text.font=4,lty=c(1:5),title="Page specifications")
#Markovplot_nopurchase_Tablet<-qplot(data= Nopurchase_transition_Tablet, To.Page, Probability, color=From.Page, xlab= "To page", ylab="Transition probability", main = "Transition probabilities for Tablet sessions not ending in purchase") 


