library(RedditExtractoR)
library(tidyverse)
library(writexl)
library(readxl)
library(fastText)
library(fastTextR)
library(irr)
library(broom)
library(questionr)

############## Checking Krippendorf's alpha ##############

# Removing the practice examples and making sure the df structure is correct
icr_sample_c_r = read_xlsx("icr_sample_c_r.xlsx")
icr_sample_c_m = read_xlsx("icr_sample_c_m.xlsx")

icr_sample_c_r = icr_sample_c_r[11:nrow(icr_sample_c_r),]
icr_sample_c_r$coder_id = "r"
icr_sample_c_r$text = 1:(nrow(icr_sample_c_r))

icr_sample_c_m = icr_sample_c_m[11:nrow(icr_sample_c_m),1:2]
icr_sample_c_m$coder_id = "m"
icr_sample_c_m$text = 1:(nrow(icr_sample_c_m))

icr_sample_both = rbind(icr_sample_c_m, icr_sample_c_r)
icr_sample_both = pivot_wider(icr_sample_both, id_cols = coder_id, names_from = text, values_from = uncivil)
icr_sample_both = select(icr_sample_both, -coder_id)

icr_sample_both = as.matrix(icr_sample_both)

# Krippendorf's alpha, at least α = .667
kripp.alpha(icr_sample_both, method = "nominal")
#alpha = .676


############## Preprocessig of text ##############

coded = read_xlsx("coding_sample_c.xlsx")
coded = coded[,-3]

#Removing NA's
coded = coded[!(coded$uncivil=="NA"),]

#removing the comment text that was replied to
coded$text = str_replace_all(coded$text, "[&]gt;.*", "")

#removing websites, new lines, and other reddit syntax
coded$text = str_replace_all(coded$text, "\\n", "")
coded$text = str_replace_all(coded$text, "\\r", "")
coded$text = str_replace_all(coded$text, "////", "")
coded$text = str_replace_all(coded$text, ".(https:.*)", "")
coded$text = str_replace_all(coded$text, "\\[", "")
coded$text = str_replace_all(coded$text, "\\]", "")
coded$text = str_replace_all(coded$text, "[&]amp;", "")
coded$text = str_replace_all(coded$text, "nbsp;", "")
coded$text = str_replace_all(coded$text, "[&]lt", "")

#formatting it to fasttext standards
coded$uncivil = sub("^", "__label__", coded$uncivil)

coded$text = paste(coded$uncivil, coded$text)
coded = coded[,-2]

write_xlsx(coded, "coded_min.xlsx")


# Extra pre-processing steps for improvement of the model:
#   Removing rows without text
coded = coded[!(coded$text=="") | (coded$text==" "), ]


# Removing remaining empty rows that were not deleted, by detecting vowels.
coded = coded[(grepl("a", coded$text)) | (grepl("e", coded$text)) | 
                (grepl("i", coded$text)) | (grepl("o", coded$text)) |
                (grepl("u", coded$text)),]

# Everything to lower case
coded$text = tolower(coded$text)


#formatting it to fasttext standards
coded$uncivil = sub("^", "__label__", coded$uncivil)

coded$text = paste(coded$uncivil, coded$text)
coded = coded[,-2]

write_xlsx(coded, "coded_final.xlsx")
# Warning: delete the first row in the excel file if that's the column header.
# Run model in Python.
