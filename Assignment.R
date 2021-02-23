
# load packages -----------------------------------------------------------

library(tidyverse)
iris<-as_tibble(iris)

# assignment questions ----------------------------------------------------

#Question 1

flower_sample_size<-rename(
  iris,
  sepal_length=Sepal.Length,
  sepal_width=Sepal.Width,
  petal_length=Petal.Length,
  petal_width=Petal.Width,
  species=Species
)
flower_sample_size

#Question 2

mm<-mutate(
  flower_sample_size,
  petal_length = petal_length * 10,
  petal_width = petal_width * 10,
  sepal_length=sepal_length*10,
  sepal_width=sepal_width*10
)
mm

#Question 3

area<-mutate(mm,sepal_area = sepal_length*sepal_width,
             petal_area=petal_length*petal_width)
species_area<-select(area,sepal_area,petal_area,species
)
species_area

#Question 4

sepal_summary<-summarize(mm, sample_size = n(),
          max(sepal_length),
          min(sepal_length),
          range(sepal_length),
          median(sepal_length),
          q1=quantile(sepal_length, .25),
          q3=quantile(sepal_length, .75),
          IQR=IQR(sepal_length)
)
sepal_summary

#question 5

petals_grouped<-group_by(mm, petal_width)
petals_grouped

summarize(petals_grouped,mean_width=mean(petal_width))

petal_summary<-summarize(
           mm,
           sampl_size=n(),
           mean=mean(petal_width),
           sd=sd(petal_width),
           var=(petal_width),
           sem=mean/sqrt(sampl_size),
           ci_upper=mean+2*sem,
           ci_lower=mean-2*sem
)
petal_summary

#question 6

mm_striplot<-ggplot(data=mm)+
  geom_jitter(mapping=aes(x=species,y=petal_length), alpha =.4
)
mm_striplot

#question 7


