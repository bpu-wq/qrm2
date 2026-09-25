#Question 1: The dataset movies1 is not the full population but a sample of the full population since it has only 505 observations. The full population is debatable but for instance be all movies ever published or all movies established by the source of the database.#
#Question 2a: Bruce Willis with 7 appearances as head actor#
movies <- read.delim("movies1 (1).tsv")
actor_counts <- sort(table(movies$first_actor), decreasing = TRUE)
head(actor_counts, 1)
#Question 2b: The average revenue for movies that starred Bruce Willis is $116,280,090 while the average revenue of all the movies listed in the dataset is $94,815,482 so it lies above the average of all movies in the dataset#
bruce_revenue <- mean(movies$revenue[movies$first_actor == "Bruce Willis"], na.rm = TRUE)
bruce_revenue
overall_revenue <- mean(movies$revenue, na.rm = TRUE)
overall_revenue
#Question 2C: The conclusion in the previous question is not highly trustworthy since only 7 observations are included. The law of large numnbers states that as you repeat a random experiment more times, the sample mean will converge toward the true population mean. As an outlier in an experiment with sample size 7 has a bigger impact on the mean than in an experiment with 1000 observations.#
#Question 3a: The Variance of the variable profits in millions is: 30463.13 so approximately 30463 in millions.#
movies$profits <- movies$revenue - movies$budget
movies$profits_millions <- movies$profits / 1000000
var(movies$profits_millions, na.rm = TRUE)
#Question 3b: The variance of this random sample is 5156.149 which is a lot lower than the previous computed variance of 30463.13.
set.seed(123)
movies_sample <- movies[sample(nrow(movies), 25), ]
var(movies_sample$profits_millions, na.rm = TRUE)
#Question 3c: The first few values of the vector are: 5156.149  3141.213  2110.105  3441.955 20740.620  1686.073.#
set.seed(123)
sample_vars <- numeric(100)
for (i in 1:100) {sample_vars[i] <- var(movies[sample(nrow(movies), 25), ]$profits_millions, na.rm = TRUE)}
head(sample_vars)
#Question 3d: The mean of sample_vars is 24473.26 and the standard deviation of sample_vars is 43563.22.
hist(sample_vars, main = "Distribution of Sample Variances", xlab = "Variance of Profits in Millions", col = "Orange", breaks = 20)
mean(sample_vars)
sd(sample_vars)
#Question 3e: No a sample of 25 is not sufficient to gain a reliable estimate of the population variance for profits. As you can see the histogram is heavily right skewed and the standard deviation is massive. This is because some blockbusters for example have a much higher profit and when included in such a small sample size drive up the standard deviation. Small samples fail to capture the regularity that these outliers occur. Hence why a sample size of 25 fails to generate a stable, reliable estimate of the population variance.#
