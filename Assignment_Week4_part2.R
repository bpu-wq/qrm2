#Question 2a: The male bar is slightly higher than the female bar. However, both bars are close to each other and the confidence intervals overlap so we can conclude there is no statistically significant difference.# 
df <- read.delim("movies1 (1).tsv")
library(ggplot2)
library(dplyr)
df_clean <- df %>% filter(!is.na(budget) & budget > 0 & !is.na(first_actor_gender))
ggplot(df_clean, aes(x = first_actor_gender, y = log(budget))) +
  stat_summary(fun = mean, geom = "bar", fill = c("blue", "red"), width = 0.5) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.1, fun.args = list(conf.int = .95)) +
  labs(
    title = "Mean log Budget by first actor gender",
    x = "First actor gender",
    y = "Mean log budget"
  ) +
  theme_minimal()
#Question 2b: As you can see is the Pr(>|t|) for first actor male 0.194 which is higher than the 0.05 that would make the difference statistically significant. the estimate is 0.3232 which would mean that movies with the first actor male have a higher log budget that those with first actor female. These findings show exactly what we saw in the bar chart at 2a. There is a difference but it is not significant.#
df_clean$FirstActorMale <- ifelse(df_clean$first_actor_gender == "male", 1, 0)
model_2b <- lm(log(budget) ~ FirstActorMale, data = df_clean)
summary(model_2b)
#Question 2c: Personally I did not find any errors in classification but my knowledge of actors is not all too big. If such an error was to be made, it is called a misclassification and its effect is the attenuation bias. It pushes the coefficients more towards zero leading to distorted conclusions.#