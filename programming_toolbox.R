library(tidyverse)

text <- readLines("text")

# remove blanks and quotes
rows_remove <- NULL
for (i in 1:length(text)) {
  if(str_length(text[i]) < 3) rows_remove <- c(rows_remove, i)
  text[i] <- paste0("\t", text[i])
}
text_clean <- text[-rows_remove]
text_clean <- str_remove_all(text_clean, "\"")
text_clean <- str_remove_all(text_clean, " - Repeating Form")
text_clean <- str_remove_all(text_clean, "<same as others>")

# order pages
df_order <- data.frame(seq = 1:length(text_clean), page = text_clean %>% word(-1) %>% parse_integer())
newseq   <- df_order %>% distinct(page, .keep_all = TRUE) %>% arrange(page) %>% pull(seq)
text_out <- text_clean[newseq]
text_out <- c("", text_out)

# write out
write_lines(text_out, "text")
remove(list = ls())
