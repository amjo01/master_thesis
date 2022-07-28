library(RedditExtractoR)
library(tidyverse)
library(writexl)
library(readxl)
library(fastText)
library(fastTextR)
library(irr)
library(broom)
library(questionr)

###################### Data and sample selection ######################

#All threads of past year from 01-04-2021 till 12-05-2022, saving a copy of the raw data. 
#Warning: this code will give different results when ran at a different point in time.
links <- find_thread_urls(
  subreddit = "politiek",
  sort_by = "new",
  period = "year")

links$row_num <- seq.int(nrow(links))
sum(links$comments)


#Making random selection of threads for coding and saving a vector of the threads that were selected.
links = links[,3:10]
set.seed(1)
links_sample = links[sample(nrow(links), 40), ]

sum(links_sample$comments)

sample_selection = tibble(links_sample$row_num)
sample_selection = rename(sample_selection, "row_num" = 1)


# Unpacking, deleting missing comments and saving dataset
# Warning: if ran again, this will result in more deleted comments, since it extracts the current urls.
thread_sample = get_thread_content(links_sample$url)

comment_sample = thread_sample$comments
comment_sample = comment_sample[!(comment_sample$comment =="[deleted]"),]

post_sample = thread_sample$threads


############## Preparing thread sample ##############

# Removing unwanted columns and adding column for uncivil post. 
post_sample = post_sample %>%
  subset(select=-c(subreddit, upvotes, downvotes, up_ratio, cross_posts))
post_sample$uncivil = 0

# Making a df for thread id
threads = tibble(post_sample[,"url"])
threads = rename(threads, "url" = 1)

threads$thread_ID = 1:(nrow(threads))
threads$thread_ID = sub("^", "TID_", threads$thread_ID)

# Merging dfs for threads ids
post_sample = post_sample %>%
  merge(threads, by="url")
post_sample = post_sample[,-1]


# Extracting users, making unique user IDs
p_users = tibble(post_sample[,"author"])
c_users = tibble(comment_sample[,"author"])

p_users = rename(p_users, "author" = 1)
c_users = rename(c_users, "author" = 1)

users = rbind(p_users, c_users)
users = distinct(users)

users$user_ID = 1:(nrow(users))
users$user_ID = sub("^", "UID_", users$user_ID)

users[7,2] = NA


# Merging df's
post_sample = post_sample %>%
  merge(users, by="author")
post_sample = post_sample[,-1]

# Adding a column for future merging with comment sample
post_sample$comment_id = 0

post_sample = post_sample %>% 
  select(date:timestamp, thread_ID, user_ID, score, title, text, comment_id, uncivil) %>%
  unite(text, title, text, sep = " //// ")


############## Preparing comment sample ##############


# Providing comment sample with thread ID
comment_sample$url = threads$thread_ID[match(comment_sample$url, threads$url)]
comment_sample = rename(comment_sample, "thread_ID" = "url")

# Providing comments with existing user ID's
comment_sample = comment_sample %>%
  merge(users, by="author")
comment_sample = comment_sample[,-1]

# Removing unwanted columns and adding column for incivility score
comment_sample = comment_sample %>% 
  subset(select=-c(upvotes, downvotes)) %>%
  select(thread_ID:golds, comment_id:user_ID, comment) %>%
  rename("text" = "comment")
comment_sample$uncivil = 0

# Order the columns
comment_sample = comment_sample %>% 
  select(date:timestamp, thread_ID, user_ID, score, text, comment_id, uncivil)


############## Final sample for manual coding ##############

# Combine the dfs
sample_uc = rbind(comment_sample, post_sample)

# Create unique ID for merging the coding with the df later on.
sample_uc$matching = 1:(nrow(sample_uc))

# Order them randomly
set.seed(2)
rows = sample(nrow(sample_uc))
sample_uc = sample_uc[rows,]

icr_sample = sample_uc[1:110,]

#Two similar datasets for intercoder reliability (uc means uncoded).
write_xlsx(icr_sample, "icr_sample_uc_m.xlsx")
write_xlsx(icr_sample, "icr_sample_uc_r.xlsx")

#The larger dataset for manually coding
coding_sample = sample_uc[111:nrow(sample_uc),]
write_xlsx(coding_sample, "coding_sample_uc.xlsx")

