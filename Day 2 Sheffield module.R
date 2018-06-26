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

'''
SIMPLE LINEAR REGRESSION SECTION
'''
#  Script at https://raw.githubusercontent.com/striatum/smi610/master/src/simple_lin_regression.r
#  Notebook at 
# caTools is used in machine learning
# quietly = True suppresses annoying messages
# warn.conflicts = FALSE suppresses error messages
# require is a SYNONYM for library
require(caTools, quietly=TRUE, warn.conflicts=FALSE)

# read in data
# = is a synonm for <-
salary = read.csv('https://raw.github.com/striatum/smi610/master/data/salary_data.csv')

#  checkthe data out
head(salary)

# whats the structure of the dataframe?
str(salary)

set.seed(1234) # define a number for pseudo-random number generator

#  This splits the data into train and test
split = sample.split(salary$Salary, SplitRatio=2/3)
training_set = subset(salary, split == TRUE)
test_set = subset(salary, split == FALSE)

# FEATURE SCALING
# training_set = scale(training_set)
# test_set = scale(test_set)

#lm1 is a linear model - Salary is the outcome variable, YearsExperience is 
summary(lm1 <- lm(Salary ~
                    YearsExperience,
                  data=salary))

lm1 <- lm(Salary ~
            YearsExperience,
          data=training_set)

y_pred = predict(lm1, newdata=test_set)

require(ggplot2, quietly=TRUE, warn.conflicts=FALSE)
require(gridExtra, quietly=TRUE, warn.conflicts=FALSE)

p1 <- ggplot() +
  geom_point(data=training_set, aes(x=YearsExperience, y=Salary), colour='red') +
  geom_line(data=training_set, aes(x=YearsExperience, y=predict(lm1, newdata=training_set)), colour='blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

p2 <- ggplot() +
  geom_point(data=test_set, aes(x=YearsExperience, y=Salary), colour='red') +
  geom_line(data=training_set, aes(x=YearsExperience, y=predict(lm1, newdata=training_set)), colour='blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

grid.arrange(p1, p2, ncol=2)

cor.test(y_pred, test_set$Salary)
