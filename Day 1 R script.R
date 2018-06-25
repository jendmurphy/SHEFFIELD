
# Set working directory
setwd("//stfdata07/home/MI/Mip18jdm/ManW10/Desktop/SHEFFIELD/SHEFFIELD")

install.packages("tidyverse")
library("tidyverse")

#  Make an empty ggplot
ggplot()

#  Line chart
ggplot(data = economics,
       aes(x=date,y=unemploy))+
  geom_line()

#  Bar chart
ggplot(data = midwest,
       aes(x=state))+
  geom_bar()

#  histogram - specified bin
ggplot(data = diamonds,
       aes(x=price))+
  geom_histogram(bins = 10)

#  histogram - specified bin width
ggplot(data = diamonds,
       aes(x=price))+
  geom_histogram(binwidth = 500) # this specifies each bin as $500 dollars width

#  density
ggplot (data = diamonds,
        aes(x=price))+
  geom_density()

#  frequency polygon
ggplot (data = diamonds,
        aes(x=price))+
  geom_freqpoly()

#  Plotting two graphs on one set of axes
#  frequency polygon
ggplot (data = diamonds,
        aes(x=price))+
  geom_freqpoly()+
  geom_histogram(alpha = .3) #  change transparency  plot both on one plot

#  BOXPLOT
#  Now working with the diamonds data to plot the distribution of a continuous variable, by a categorical variable - BOXPLOT
ggplot(data=diamonds,
       aes(y=price, x=color))+
  geom_boxplot()

#  VIOLIN
#  Now working with the diamonds data to plot the distribution of a continuous variable, by a categorical variable - VIOLIN 
ggplot(data=diamonds,
       aes(y=price, x=color))+
  geom_violin()

#  DENSITY PLOT - different lines are different colours.  ggplot will take colour or color as a spelling (note that diamonds dataset includes a variable 'color' which is why it is spelt both ways here)
ggplot(data=diamonds,
       aes(x=price, colour=color))+
  geom_density()

#  SCATTERPLOT
ggplot(data = mpg,
       aes(x = displ, y = hwy))+
  geom_point()

#  The pokemon dataset!  Download from the internet
pokemon <- read.csv("https://bit.ly/2lwUJRB")

#  Make the pokemon dataset a tidy object
pokemon <- tbl_df(pokemon)



#  POKEMON 
#  Types of pokemon bar chart
ggplot(data = pokemon,
       aes(x = Type))+
  geom_bar()

#  HP amongst Pokemon density plot. colour by type
ggplot(data = pokemon,
       aes(x = HP, colour = Type))+
  geom_density()

#Attack and defense scatterplot
ggplot(data = pokemon,
       aes(x = Attack, y = Defense))+
  geom_point()

#  HP by type, amongst pokemon barchart
ggplot(data = pokemon,
       aes(x = HP, colour = Type))+
  geom_density()





