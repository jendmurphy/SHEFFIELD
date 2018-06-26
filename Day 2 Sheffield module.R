setwd("\\stfdata07\home\MI\Mip18jdm\ManW10\Desktop\SHEFFIELD\SHEFFIELD")

'''
SHEFFIELD MODULE DAY 2
Machine learning in R

'''

'''
HIERARCHICAL CLUSTERING 
'''
#  Install packages - use a column to list out multiple packages for installation
install.packages(c("ggplot2","ggthemes"))
install.packages(c("ggfortify","caTools","caret","cluster","psych","gridExtra2"))


animacy = read.csv('https://raw.github.com/striatum/smi610/master/data/word_ratings.csv')

head(animacy)

# This gives the structure of the dataframe
str(animacy)

#  This command adds names to the bottom of the dendogram and clusters.
rownames(animacy) = animacy$Word

#  Taking subjective ratings of the words (columns 6 7 and 8)
#  Clusters based on these
#  Remember that column numbering starts from 1 in R not 0 (as in Python)
d = dist(animacy[,c(6,7,8)]) # matrix of distances

#  Run clustering analysis.  d is the distances, method specified here
clus = hclust(d, method='complete')
plot(clus, main='Method: Complete', sub='', cex=0.6) # plotting the clustering

clus2 = hclust(d, method='centroid') # centroid method
clus3 = hclust(d, method='ward.D')   # ward method

#  Plotting clus 2 and clus 3 on one picture
par(mfrow=c(1,2)) # plotting two graphs next to each other
plot(clus2, main='Method: Centroid', sub='') #  plotting clus 2
plot(clus3, main='Method: Ward', sub='')     #  plotting clus 3

# Dont know what this does
require(cluster, quietly=TRUE)

#  Divisive cluster analysis
div = diana(d)
pltree(div, main='Divisive cluster analysis', sub='', cex=0.6)

cuts = cutree(div, 5)
cuts = cbind(rownames(animacy), cuts)
cuts = cuts[order(cuts[,2]),]
cuts

'''
KMEANS CLUSTERING
'''
