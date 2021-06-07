pcafile="GreSalAndModernPCA.eigenvec"

pca=read.table(pcafile)

plot(pca$V3,pca$V4,pch=19,col=as.integer(as.factor(pca$V1)))


table(pca$V1)


