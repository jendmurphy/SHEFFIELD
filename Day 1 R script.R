
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
pokemon <- read.csv("http://bit.ly/2HZhfvs")

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

#  To turn the graph on its side, use cood_flip()
ggplot(pokemon, aes(Type))+
  geom_bar()+
  coord_flip()

#  Tidyverse is a library of pacages that make R a bit easier to use.
#  Piping is part f the tidyverse  pipe effecitvely means 'and then'

#  Draw a graph of average HP of different pokemon

#CTRL SHIFT M - gives the pipe command.
#%>% 

#  Doing some more graphs...
# HP
pokemon %>%  #  data set
  group_by(Type) %>%  #  group this data by type
  summarise(HPB = mean(HP)) %>%  #  using HPB to avoid ovewriting the variable HP
  ggplot(aes(x = Type, y = HPB))+
  geom_col()+   #  column chart which is like  a bar chart   same as: geom_bar(stat = "identity")
  coord_flip()

# Attack
pokemon %>%  #  data set
  group_by(Type) %>%  #  group this data by type
  summarise(AttackB = mean(Attack)) %>% 
  ggplot(aes(x = reorder(Type, AttackB),  #  REORDERS THE X BY THE VARIABLE ATTACK B
             y = AttackB))+
  geom_col()+   #  column chart which is like  a bar chart   same as: geom_bar(stat = "identity")
  coord_flip()

# Defense
pokemon %>%  #  data set
  group_by(Type) %>%  #  group this data by type
  summarise(DefenseB = mean(Defense)) %>% 
  ggplot(aes(x = reorder(Type, DefenseB),  #  REORDERS THE X BY THE VARIABLE ATTACK B
             y = DefenseB))+
  geom_col()+   #  column chart which is like  a bar chart   same as: geom_bar(stat = "identity")
  coord_flip()

# Using some filtering?  
install.packages("fivethirtyeight")
library(fivethirtyeight)

#  Look at fivethirtyeight github page, worth a read of the database.
#  Load the tarantino dataset
tarantino <- tarantino
ggplot(tarantino, aes(word))+
  geom_bar()

#  Draw a bar plot of the words used, only including ones that are used over 20 times.
tarantino %>% 
  group_by(word) %>%  #  group by the word
  summarise(howmany = n()) %>%  # n just means count
  filter(howmany >20) %>%  #  filters everything where howmany is >20
  na.omit %>%  #  Omits the NAs from the chart
  ggplot(aes(x = reorder(word, howmany),howmany))+
  geom_col()+  #  column chart, same as bar chart with stat = identity
  coord_flip() #

#  To colour in the graph so that the distirbution across the films is showed
tarantino_2 <- tarantino %>%  #  NOW THIE HOWMANY VARIABLE IS ADDED TO THE DATASET - 5 variables not 4.
  group_by(word) %>%  #  group by the word
  na.omit %>% 
  mutate(howmany = n()) %>%  # mutate is used to create a new variable from something that exists
  ungroup %>% 
  filter(howmany >20)  #  filters everything where howmany is >20
  
#  PLot the newly stored dataframe
ggplot(tarantino_2, aes(x = reorder(word, howmany),
                        fill = movie))+
  geom_bar()+  #  column chart, same as bar chart with stat = identity
  coord_flip()


#  Plot again - this time use dodge to split bars out
ggplot(tarantino_2, aes(x = reorder(word, howmany),
                        fill = movie))+
  geom_bar(position = "dodge")+  #  introduce dodge to split out the colour bars.
  coord_flip() 

#  PLot to use position adjustmnet FILL.  Makes all of the bars the same length, but removes the freqency - good if you want to know the distributiion but not bothered about the absolute numbers.
ggplot(tarantino_2, aes(x = reorder(word, howmany),
                        fill = movie))+
  geom_bar(position = "fill")+  #  introduce dodge to split out the colour bars.
  coord_flip() 


#  PRE LUNCH TASK
#  Plot of distribution of the words, by  film 
ggplot(tarantino_2, aes(x = movie,
                        fill = word))+
  geom_bar(position = "fill")
  coord_flip() 


#  Bar chart of different types of pokemon, acroding to which generation pokemon are introduced in.
pokemon %>% 
  group_by(Type) %>% 
  na.omit %>%
  mutate(howmany = n()) %>% 
  ungroup() %>% 
  ggplot(aes(Type, fill = generation))+
  geom_bar(position = "fill")+
  coord_flip()
  
#  Bar chart of different types of pokemon, acroding to which generation pokemon are introduced in.
pokemon %>% 
  group_by(Type) %>% 
  na.omit %>%
  mutate(howmany = n()) %>% 
  ungroup() %>% 
  ggplot(aes(Type, fill = generation))+
  geom_bar(position = "dodge")+
  coord_flip()


