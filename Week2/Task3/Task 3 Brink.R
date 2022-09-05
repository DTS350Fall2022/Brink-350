library(tidyverse)

?iris
head(iris)

#Plot 1
ggplot(iris)+
  geom_point(mapping = aes(x= Sepal.Length, y=Sepal.Width, color=Species, shape=Species))

#Plot 2
ggplot(iris)+
  geom_point(mapping =aes(x= Petal.Width, y=Petal.Length, color=Species, shape=Species))+
  facet_wrap(~ Species)

#Plot 3
ggplot(iris)+
  geom_point(mapping=aes(x= Petal.Length, y=Petal.Width, color=Species, shape=Species))+
  geom_smooth(method='lm',mapping=aes(x= Petal.Length, y=Petal.Width))

#Plot 4
ggplot(iris, aes(x=Sepal.Length, fill= Species))+
  geom_histogram(binwidth=.20, color='black')

#How do the length and width of the Sepals and Petals of the respective species relate and compare to each other?
#The variety of graphs used to solve the guiding question each served a purpose in showing a different angle to the data set, such as a line of best fit within a line graph or the vertical asymtote in the bar chart show the average sepal length.  

