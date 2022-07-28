library(RedditExtractoR)
library(tidyverse)
library(writexl)
library(readxl)
library(fastText)
library(fastTextR)
library(irr)
library(broom)
library(questionr)

############## Constructing the variables ##############

df = read_xlsx("df.xlsx")

############## Variable: length of the message ##############

# Similar preprocessing steps are taken within the df to properly count the number of characters
df = df[!(df$uncivil=="NA"),]

#removing the comment text that was replied to
df$text = str_replace_all(df$text, "[&]gt;.*", "")

#removing websites, new lines, and other reddit syntax
df$text = str_replace_all(df$text, "\\n", "")
df$text = str_replace_all(df$text, "\\r", "")
df$text = str_replace_all(df$text, "////", "")
df$text = str_replace_all(df$text, ".(https:.*)", "")
df$text = str_replace_all(df$text, "\\[", "")
df$text = str_replace_all(df$text, "\\]", "")
df$text = str_replace_all(df$text, "[&]amp;", "")
df$text = str_replace_all(df$text, "nbsp;", "")
df$text = str_replace_all(df$text, "[&]lt", "")

# Removing rows without text.
df = df[!(df$text=="") | (df$text==" "), ]

# Variable: length of the message
df$length = nchar(df$text)

# checking the distribution and logging. adding plus one to prevent applying to a 0.
# It doesn't matter as much that it's not interpretable, since it's a control variable.

hist(df$length)

df$length_log = log(df$length +1)

hist(df$length_log)



############## Variable: descriptive (average incivility score over the past 48 hours) ##############


# Timestamp is in seconds, so calculate to how many seconds must be counted back to. 
past48h = 60*60*24*2
df$timestamp48h = df$timestamp - past48h

# checking if it's numeric.
df$uncivil = as.numeric(df$uncivil)
is.numeric(df$uncivil)
mean(df$uncivil)

# percentage of uncivil messages, excluding target post
df$descriptive = 0
for (i in 1:nrow(df)) {
  block <- subset(df, df$timestamp >= df$timestamp48h[i]) #after post
  block <- subset(block, block$timestamp < df$timestamp[i]) #before post
  df$descriptive[i] <- mean(block$uncivil)
}


#This excludes a number of posts: these are all threads/posts for which nothing was posted
#   in the previous 48. I will exclude these from my analysis, but I won't delete them;
# they are needed these for constructing other variables.


# checking the distribution. Because of interpretability reasons, I will not standardise.
hist(df$descriptive)


############## Variable: injunctive (number of upvotes for uncivil posts past 48 hours) ##############

# number of upvotes for uncivil posts in the past 48h, excluding target post.
df$injunctive = 0
for (i in 1:nrow(df)) {
  block <- subset(df, df$timestamp >= df$timestamp48h[i]) #after post
  block <- subset(block, block$timestamp < df$timestamp[i]) #before post
  block <- block[(block$uncivil == 1),]
  df$injunctive[i] <- sum(block$score)
}


# checking the distribution. Because of interpretability reasons, I will not standardise.
hist(df$injunctive)



############## Variable: contagion (replied to uncivil comment) ##############

df$contagion = NA


# for all posts/threads, the value will be NA, since these authors did not reply to anything
# for all parent comments, the value will be 0, since all threadposts are civil.
# for all child comments, the value will be matched to uncivil value of the comment that was replied to.


for (i in 1:nrow(df)) {
  
  # if comment_id == 0 and not contain "_"
  if(df$comment_id[i] == 0){
    df$contagion[i] = NA
  }
  
  # if comment_id != 0 and not contain "_"
  else if(df$comment_id[i] != 0 & str_detect(df$comment_id[i], "_", negate = TRUE)){
    df$contagion[i] = 0
  }
  
  # if comment_id contain "_"
  else if(df$comment_id[i] != 0 & str_detect(df$comment_id[i], "_")){
    target_id = df$comment_id[i]
    target_id = str_replace(target_id, "_.$", "")
    thread_id_df <- subset(df, df$thread_ID==(df$thread_ID[i]))
    
    df$contagion[i] = thread_id_df[(thread_id_df$comment_id == target_id), "uncivil"]
  }
  
  # else, "missing"
  else{
    print("Error in finding incivility of previous comment")}
}


# NA for the comments that replied to a deleted comment
df$contagion[df$contagion == "numeric(0)"] = NA

sum(is.na(df$contagion))


# checking the distribution

df$contagion = as.numeric(df$contagion)
hist(df$contagion)


############## Variable: incivility baseline ##############

df$baseline = 0
for (i in 1:nrow(df)) {
  block <- subset(df, df$user_ID == df$user_ID[i])
  block <- subset(block, block$timestamp < df$timestamp[i]) #before post
  df$baseline[i] <- mean(block$uncivil)
}

sum(is.na(df$baseline))


# Too many missing values So: if user didn't post prior to the target post, then he gets an 
# incivility baseline of 0, instead of NA. So first assigning all NA's a zero, and then assigning 
# missing users NA.


df$baseline[is.na(df$baseline)] = 0
df$baseline[is.na(df$user_ID)] = NA

sum(is.na(df$baseline))



# checking the distribution. Transformation techniques do not result in a better distribution.

hist(df$baseline)

#df$baseline_log = log(df$baseline + 1)
#df$baseline_st = scale(df$baseline, center = TRUE, scale = TRUE)
#df$baseline_sqrt = sqrt(df$baseline)

#hist(df$baseline_log)
#hist(df$baseline_st)
#hist(df$baseline_sqrt)



############## Analysis & assumptions  ##############


# creating the model
model = glm(uncivil ~ descriptive + injunctive + contagion + length_log + baseline, 
            data = df, 
            family = "binomial")
summary(model)



############## Assumption: Multicollinearity  ##############


# pearson for continuous variables
cor.test(df$descriptive, df$injunctive, method = "pearson", exact = FALSE)

# spearman for nominal or ordinal
cor.test(df$descriptive, df$contagion, method = "spearman", exact = FALSE)
cor.test(df$injunctive, df$contagion, method = "spearman", exact = FALSE)

# all are low enough (highest is .64), so assumption is probably met.


# Another way of checking, just to be sure: By means of VIF scores:
car::vif(model)
# no variable exceeds a VIF-value of 5, so no problems.


############## Assumption: linear relation between continuous predictors & logit  ##############

df_omit = na.omit(df)

probabilities = predict(model, type = "response")

# Select the continuous predictors
df_pred = df_omit[, c(13, 14, 11, 16)]
predictors = colnames(mydata)

# combine the logit with data
df_pred = df_pred %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

# scatterplotting the data
lin_plots = ggplot(df_pred, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(y = "Values of continuous predictors", x = "Log odds of uncivil message")
print(lin_plots)

# the relations between the continuous predictors (user incivility baseline, descriptive, injunctive, log of length)
# all look fairly linear, so assumption is met. 

ggsave("lin_plots.png")


############## Check assumption: influential values  ##############


# checking outliers by using Cook's distance & standardised residuals. Checking top 6 observations

plot(model, which = 4, id.n = 10)

model.data = augment(model) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(10, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = uncivil), alpha = .5) +
  theme_bw()


#residuals above 3 are problematic, so filter for that:
model.data %>% 
  filter(abs(.std.resid) > 3)


# no problematic outliers.



################# Analysis #################

# creating the apa table for descriptives

mydata = df[, c(8, 13, 14, 15, 11, 16)]

psych::describe(mydata)


# making two models, with and without control variables.

model1 = glm(uncivil ~ descriptive + injunctive + contagion, 
             data = df, 
             family = "binomial")
summary(model1)


model2 = glm(uncivil ~ descriptive + injunctive + contagion + length_log + baseline, 
             data = df, 
             family = "binomial")
summary(model2)

# model 2 (with control variables) has a better AIC, so I am going to use that one.


# calculating McFadden's Pseudo R2. This is measure of overall effect size, comparing the model
# with a nullmodel that only contains an intercept.

nullmod = glm(uncivil~1, data=df, family="binomial")
1-logLik(model2)/logLik(nullmod)

#calculating p-value
ll.null = model2$null.deviance/-2
ll.proposed = model2$deviance/-2

1-pchisq(2*(ll.proposed - ll.null), df=(length(model2$coefficients)-1))


# checking other R2's
rcompanion::nagelkerke(model2)


# chi-square can also be used for evaluating the model, calculating the difference
# between the expected values of a null model, and the actual model.

chisq = model2$null.deviance - model2$deviance

# p-value for chi-square, with 5 degrees of freedom for the number of predictors.
pchisq(chisq, 5, lower.tail = FALSE)


# Extracting the confident intervals
confint(model2)


# transforming coefficients to odds-ratio = more interpretable
questionr::odds.ratio(model2)