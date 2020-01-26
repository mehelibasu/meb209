dev.cur()
dev.list()
dev.next(which = dev.cur())
dev.prev(which = dev.cur())
dev.off(which = dev.cur())
dev.set(which = dev.next())
graphics.off()
dev.cur()
dev.list()
dev.next(which = dev.cur())
dev.prev(which = dev.cur())
dev.off(which = dev.cur())
dev.set(which = dev.next())
dev.new(...)
graphics.off()
dev.off()

plot_patient <- function(x) {
  library(foreign)
  library(gplots)
  library(ggplot2)
  #xhetero <- plot(x$WEIGHT, x$TIMEDL,xlab = 'Dates', ylab = 'Weight', main = 'Weight variation per day')
  protein_weight <- plot(x$PROT, x$WEIGHT, pch=19, xlab="PROTEIN", ylab="WEIGHT")+
    abline(lm(x$WEIGHT~x$PROT),lwd=3, col="red")
  fat_weight <- plot(x$FAT, x$WEIGHT, pch=19, xlab="FAT", ylab="WEIGHT")+
    abline(lm(x$WEIGHT~x$FAT),lwd=3, col="red")
  sfat_weight <- plot(mydata$SFAT, mydata$WEIGHT, pch=19, xlab="SATURATED FAT", ylab="WEIGHT")+
    abline(lm(mydata$WEIGHT~mydata$SFAT),lwd=3, col="red")
  #windows()
  par(mfrow = c(1,3))
  #xhetero
  #return(xhetero)
  return(protein_weight)
  return(fat_weight)
  return(sfat_weight)
}
plot_patient(id1)



