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

# executing hierarchical clustering with complete linkage
hier_clust_1 <- hclust(dist(scaled), method = "complete")

# printing the dendrogram
plot(hier_clust_1)

# getting cluster assignments based on number of selected clusters
hc_1_assign <- cutree(hier_clust_1, 5)

# executing hierarchical clustering with complete linkage
hier_clust_2 <- hclust(dist(scaled), method = "single")

# printing the dendrogram
plot(hier_clust_2)

# getting cluster assignments based on number of selected clusters
hc_2_assign <- cutree(hier_clust_2, 5)

# adding assignments of chosen hierarchical linkage
heart_disease$hc_clust <- hc_1_assign

# remove 'sex', 'first_clust', and 'second_clust' variables
hd_simple <-
  heart_disease[,!(names(heart_disease) %in% c("sex", "first_clust", "second_clust"))]

# getting mean and standard deviation summary statistics
clust_summary <-
  do.call(data.frame,
  aggregate(. ~ hc_clust, data = hd_simple, function(x)
  c(avg = mean(x), sd = sd(x))))
clust_summary

# plotting age and chol
plot_one <-
  ggplot(heart_disease, aes(
  x = age,
  y = chol,
  color = as.factor(hc_clust)
  )) +
  geom_point()

plot_one 

# plotting oldpeak and trestbps
plot_two <-
  ggplot(heart_disease, aes(
  x = oldpeak,
  y = trestbps,
  color = as.factor(hc_clust)
  )) +
  geom_point()

plot_two
