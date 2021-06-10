library(ggplot2)

pcafile="GreSalAndModernPCA.eigenvec"
poplistfile="popLists.txt.csv"

pca=read.table(pcafile)
poplist=read.table(poplistfile)

pca[1:5,1:5]

colnames(pca) <- c("FID","IID","P1","P2","P3")


barplot(table(pca$FID),las=2,cex.names=.5)


plot(pca$P1,pca$P2,pch=19,col="grey")
points(subset(pca,FID=="PuBa",select = c(P1,P2)),cex=1.2,col="blue",pch=19)


plot(pca$P1,pca$P2,pch=19,col=as.integer(as.factor(poplist$V3)))
points(subset(pca,FID=="PuBa",select = c(P1,P2)),cex=1.2,col="orange",pch=19)

###########################################

ggplot(data=pca)+
  geom_point(aes(x=P1,y=P2))+
  theme_bw()


ggplot(data=pca)+
  geom_point(aes(x=P1,y=P2,col=poplist$V3))+
  theme_bw()


ggplot(data=pca)+
  geom_point(aes(x=P1,y=P2,col=poplist$V3))+
  labs(col = "Continent",x="PC1",y="PC2")+
  theme_bw()

ggplot(data=pca)+
  geom_point(aes(x=P1,y=P2,col=poplist$V3))+
  labs(col = "Continent",x="PC1",y="PC2")+
  geom_point(data=subset(pca,FID=="PuBa"),aes(x=P1,y=P2),col="navy")+
  theme_bw()
############### PCA Eurasia


pcafile <- "GreSalAndModernEurasiaPCA.eigenvec"
poplistfile="popLists.txt.csv"

pca <- read.table(pcafile)
poplist <- read.table(poplistfile)

pca

colnames(pca) <- c("FID","IID","P1","P2","P3")


barplot(table(pca$FID),las=2,cex.names=.5)


plot(pca$P1,pca$P2,pch=19,col="grey")
text(pca$P1,pca$P2,pca$FID)
points(subset(pca,FID=="PuBa",select = c(P1,P2)),cex=1.2,col="blue",pch=19)


plot(pca$P1,pca$P2,pch=19,col=as.integer(as.factor(poplist$V3)))
points(subset(pca,FID=="PuBa",select = c(P1,P2)),cex=1.2,col="orange",pch=19)


#MDS
MDSfile <- "GreSalAndModernEurasiaMDS.mds"

mds <- read.table(MDSfile)

head(mds)

mds <- read.table(MDSfile, header=T)

plot(mds$C1,mds$C2)


###ADMIXTURE

admQfile="GreSalAndModernEurasiaNoMissing.5.Q"
admPfile="GreSalAndModernEurasiaNoMissing.5.P"
admfamfile="GreSalAndModernEurasiaNoMissing.fam"

admQ=read.table(admQfile)
admfam=read.table(admfamfile)

barplot(t(admQ),space=0,border=NA,col=c("blue","red","purple","green","orange"))

avbypop <- aggregate(admQ,by = list(admfam$V1),FUN=mean)
avbypop
barplot(t(as.matrix(avbypop[,-1])),border=NA,space=0,
        names.arg = avbypop[,1],las=2,cex.names = .3)


#loop 

admQfile="ancient2merge_cleaned_nopugNoMissing.5.Q"
ancientlistfile="ancient2merge_cleaned_nopugNoMissing.fam"

admQ=read.table(admQfile)
ancientlist=read.table(ancientlistfile)

par(mar=c(20,2,2,2))
barplot(t(admQ),space=0,border=NA,names.arg = ancientlist$V1, las=2,cex.names=.5,
        col=c("blue","red","purple","green","orange"))


puglia=avbypop[avbypop$Group.1=="PuBa",]

pugliaAncient <- rbind(puglia[,-1],admQ)

namespugliaAncient <- c("Puglia",ancientlist$V1)

mdsAdm <- cmdscale(dist(pugliaAncient))

plot(mdsAdm)
text(mdsAdm,labels = namespugliaAncient)
points(mdsAdm[1,],col="red")
