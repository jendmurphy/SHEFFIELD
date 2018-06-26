setwd("\\stfdata07\home\MI\Mip18jdm\ManW10\Desktop\SHEFFIELD\SHEFFIELD")

install.packages("ggfortify")
install.packages(c("caTools","caret","cluster","psych","gridExtra2"))


animacy = read.csv('https://raw.github.com/striatum/smi610/master/data/word_ratings.csv')

head(animacy)

str(animacy)

#  This command adds names to the bottom of the dendogram and clusters.
rownames(animacy) = animacy$Word

#  Taking subjective ratings of the words (columns 6 7 and 8)
#  Clusters based on these
#  Remember that column numbering starts from 1 in R not 0 (as in Python)
d = dist(animacy[,c(6,7,8)]) # matrix of distances
clus = hclust(d, method='complete')
plot(clus, main='Method: Complete', sub='', cex=0.6)

clus2 = hclust(d, method='centroid')
clus3 = hclust(d, method='ward.D')

par(mfrow=c(1,2))
plot(clus2, main='Method: Centroid', sub='')
plot(clus3, main='Method: Ward', sub='')

require(cluster, quietly=TRUE)

div = diana(d)
pltree(div, main='Divisive cluster analysis', sub='', cex=0.6)

cuts = cutree(div, 5)
cuts = cbind(rownames(animacy), cuts)
cuts = cuts[order(cuts[,2]),]
cuts