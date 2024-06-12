### set wd to source file dir
setwd("~/work/Teaching/DSIER23/10-text-as-data")

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(xml2)
p_load(tidytext)
p_load(wordcloud2)
p_load(magrittr, tidyverse)

# PART 1: Overview of 
# GREP “Globally search a regular expression and print.” search for the presence of a string that matches a patter
grepl("DSIER", "DSIER 2023 Class it's very fun\n\n\t\t\t\t\t\t\t")

# Now let’s use the gsub command to remove all \t (\t usually denotes a tab character) while \n (starts on a new line)
gsub("\t", "", "DSIER 2023 Class it's very fun\n\n\t\t\t\t\t\t\t")
gsub("\t|\n", "", "DSIER 2023 Class it's very fun\n\n\t\t\t\t\t\t\t")

# none of the words in some_text start with the letter "P"
some_text<-c("This","topic","is","so","fun")
some_text[grep("^[T]", some_text)]

text_chunk<-c("[This topic is not so fun]")
# gsub("\","", text_chunk)]

# \ character has a literal meaning to R because it is part of something called a regular expression. To remove this character, and other characters like it, 
# we need to “escape” the character using single quotation marks wraped around a double \\ as follows:
text_chunk<-c("[This topic is not so fun]")
gsub('\\[|\\]',"", text_chunk)

# Excercises
# select the strings which ends in -ing in the following list c("working","former","book", "thinking")
# replace the -ing 

# COSINE SIMILARITY
s1 <- "The book is on the table"  
s2 <- "The pen is on the table"  
s3 <- "Put the pen on the book" 
sv <- c(s1=s1, s2=s2, s3=s3)

# Split sentences into words
svs <- strsplit(tolower(sv), "\\s+")

# Calculate term frequency tables (tf)
# (note that stacking vectors concatenates multiple vectors into a single vector)
termf <- table(stack(svs))

# calculate inverse document frequencies (idf)
idf <- log(1/rowMeans(termf != 0))
# get tf-idf
tfidf <- termf*idf

# Calculate dot products between the last tf-idf and all the previous
dp <- t(tfidf[,3]) %*% tfidf[,-3]

# Divide by the product of the euclidean norms do get the cosine similarity
cosim <- dp/(sqrt(colSums(tfidf[,-3]^2))*sqrt(sum(tfidf[,-3]^2)))
cosim

# PART 2: ANALYSE TRADE AGREEMENTS

if (!dir.exists("input/tota-master")) {
  # git repo URL
  url <- "https://github.com/mappingtreaties/tota/archive/refs/heads/master.zip"
  # assign name of the file to be saved locally
  destfile <- "input/tota.zip"
  # download.file() function to download the repository
  download.file(url, destfile)
  # once file is downloaded, you can use unzip() function to extract the files
  unzip(destfile, exdir="input/")
  file.remove(destfile)
}


# Step 1: Read XML Data
treaty_data <- read_xml("input/tota-master/xml/pta_1.xml")

# Step 2: Extract Information
# Convert XML data to tibble (data frame) and unnest the 'treaty' column
# Only keep rows where variable names (ie treaty_id) is 'date_signed' or 'parties_original'
info <- as_list(treaty_data) %>%
  tibble::as_tibble() %>%
  unnest_longer(treaty) %>%
  filter(treaty_id %in% c("date_signed", "parties_original"))

# Step 3: Extract Articles
# Find all XML nodes corresponding to 'article'
# Extract attributes and content, removing leading/trailing white space
articles <- treaty_data %>%
  xml_find_all("//article")
id <- articles %>%
  xml_attr("article_identifier") %>%
  as.character()
content <- articles %>%
  xml_text() %>%
  trimws()

# Step 4: Prepare Treaty Text Data Frame
# Create a data frame from the content and add year and parties columns
# Reorganize the columns and group by year and parties
# Summarize the content by pasting together the text of all articles for each group
treaty_text <- content %>%
  as.data.frame() %>%
  rename(content = ".") %>%
  mutate(year = unlist(filter(info, treaty_id=="date_signed")$treaty),
         parties = filter(info, treaty_id=="parties_original")$treaty) %>%
  ungroup() %>%
  select(content, year, parties) %>%
  group_by(year, parties) %>%
  summarise(treaty = paste(content, collapse = " // "), .groups = "keep")


# Find Most Frequent Words
# Convert the third column of the first row of 'treaty_text' to a data frame
# Unnest the tokens (words) in the 'treaty' column
# Count the frequency of each word and sort by frequency
# Display the top words (head() by default shows top 6)
treaty_text[1,3] %>% as.data.frame() %>% 
    unnest_tokens(word, treaty) %>%
    count(word, sort = TRUE) %>% head()
  
# Do the same after removing stop words
treaty_text[1,3] %>% as.data.frame() %>% 
  unnest_tokens(word, treaty) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  head()
# To Do :lemmatize!

# Do the same after removing stop words and numbers
data<- treaty_text[1,3] %>% as.data.frame() %>% 
  unnest_tokens(word, treaty) %>%
  anti_join(stop_words) %>% 
  filter(!grepl("[0-9]", word)) %>% 
  count(word, sort = TRUE)

# Represent in a Wordcount the main keywords
wordcloud2(data, color='random-light', backgroundColor="#152238")

# Define a function 'read_my_xml' to read XML data and perform text analysis
read_my_xml <- function(x) {
  # Message to console about the file being processed
  print(paste0("Working on ", x, "."))
  
  # Step 1: Load XML data
  treaty_data <- read_xml(x)
  
  # Step 2: Extract and filter relevant information
  info <- as_list(treaty_data) %>%
    tibble::as_tibble() %>%
    unnest_longer(treaty) %>%
    filter(treaty_id %in% c("date_signed", "parties_original"))
  
  # Step 3: Extract Articles
  articles <- treaty_data %>% xml_find_all("//article")
  id <- articles %>% xml_attr("article_identifier") %>% as.character()
  content <- articles %>% xml_text() %>% trimws()
  
  # Step 4: Prepare data
  # dreate a data frame from the content, add year and parties 
  # columns, group and summarize
  data <- content %>%
    as.data.frame() %>%
    rename(content = ".") %>%
    mutate(year = unlist(filter(info, treaty_id=="date_signed")$treaty),
           parties = filter(info, treaty_id=="parties_original")$treaty) %>%
    ungroup() %>%
    select(content, year, parties) %>%
    group_by(year, parties) %>%
    summarise(treaty = paste(content, collapse = " // "), .groups = "keep")
  
  # Step 5: Text Analysis
  # Unnest tokens, remove stop words, count word frequency, 
  # and filter for "services" and "investments"
  temp <- data[1,3] %>%
    as.data.frame() %>%
    unnest_tokens(word, treaty) %>%
    anti_join(stop_words) %>%
    count(word, sort = TRUE) %>%
    ungroup() %>%
    mutate(tot_words = sum(n)) %>%
    filter(word %in% c("services", "investments")) %>%
    mutate(n, tot_words, word, year = data$year, parties = data$parties) 
  
  # If temp has no rows, create a default row
  if (nrow(temp) == 0) {
    temp <- data.frame(n = 0, tot_words = 0, word = "NA", year = data$year, parties = data$parties)
  }
  
  # add year and parties to the data frame
  data <- temp %>% mutate(year = data$year, parties = data$parties)
  # clean up environment by removing unnecessary objects
  rm(articles, id, content, treaty_data)
  
  return(data)
}

# Define the directory where the XML files are located and get a of all XML files in the specified directory
file_directory <- "input/tota-master/xml"
my_files <- list.files(file_directory, full.names = TRUE)

# Randomly draw two files from the list of XML files
# Set a seed for reproducible random sampling
set.seed(123)
draw2 <- sample(my_files, 2)

# Apply the 'read_my_xml' function to the two drawn files and bind the results into a single data frame
dat <- map_df(draw2, read_my_xml)

# Step 4: Reshape the data to a wide format where each 'word' has its own column
table <- dat %>% spread(key = word, value = n)

# Step 5: Conduct a Chi-square test on the 'investments' and 'services' columns
# 'correct = FALSE' applies no continuity correction which is a small adjustment 
# used in certain statistical tests
chi_sq_result <- chisq.test(table$investments, table$services, correct = FALSE)

# Print the chi-square result
print(chi_sq_result)





