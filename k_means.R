library(factoextra)
library(NbClust)
library(tidyverse)


heart_disease <- read.csv("data/heart_disease_patients.csv")
head(heart_disease, 10)

lapply(heart_disease, is.numeric)

summary(heart_disease)

# remove id
heart_disease <- heart_disease[ , !(names(heart_disease) %in% c("id"))]

# scaling data and saving as a data frame
scaled <- scale(heart_disease)

# what does data look like now?
summary(scaled)

seed_val <- 10
set.seed(seed_val)

# select a number of clusters
k <- 5

# run the k-means algorithms
first_clust <- kmeans(scaled, centers = k, nstart = 1)

# how many patients are in each group
first_clust$size

eed_val <- 38
set.seed(seed_val)

# run the k-means algorithms
k <- 5
second_clust = kmeans(scaled, centers = k, nstart = 1)

# how many patients are in each group
second_clust$size

heart_disease$first_clust = first_clust$cluster
heart_disease$second_clust = second_clust$cluster

# load ggplot2
library(ggplot2)

# creating the plots of age and chol for the first clustering algorithm
plot_one <-
  ggplot(heart_disease, aes(
  x = age,
  y = chol,
  color = as.factor(first_clust))) +
  geom_point()

plot_one 

# creating the plots of age and chol for the second clustering algorithm
plot_two <-
  ggplot(heart_disease, aes(
  x = age,
  y = chol,
  color = as.factor(second_clust))) +
  geom_point()

plot_two


# Elbow method
fviz_nbclust(heart_disease, kmeans, method = "wss") +
     labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(heart_disease, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")





