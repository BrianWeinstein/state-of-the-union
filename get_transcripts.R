
# Initialization ################################################################

# load packages
library(rvest)
library(dplyr)
library(readr)
library(jsonlite)
library(stringr)
library(progress)

# set working directory
setwd("~/Documents/state-of-the-union/")




# Get links to transcript pages ################################################################

# page containing links to transcripts
hub_page_url <- "http://www.presidency.ucsb.edu/sou.php"

# get transcript urls
transcript_urls <- read_html(x = hub_page_url) %>%
  html_nodes("span a , td.ver12 a") %>%
  html_attr("href")
transcript_urls <- data_frame(url = transcript_urls)

# remove non-urls (see footnote on http://www.presidency.ucsb.edu/sou.php#nixon1973)
transcript_urls <- transcript_urls %>%
  filter(url!= "#nixon1973")

# deduplicate urls
transcript_urls <- transcript_urls %>%
  filter(!duplicated(url))


# Get transcripts ################################################################

# define function to get transcripts and do light cleaning
get_transcript <- function(url) {
  
  # extract raw transcript text
  text <- url %>%
    read_html() %>%
    html_nodes(".displaytext , strong , .ver10+ .ver10 , p") %>%
    html_text()
  
  # identify where the "Citation:" begins
  citation_line <- which(str_detect(string = text, pattern = "^Citation:"))
  
  # extract president's name
  president <- text[citation_line+1] %>%
    str_extract(string = ., pattern = "^(.*)(?=:)")
  
  # extract title and date
  title_date <- text[citation_line+2]
  
  # remove "Citation:.*" from transcript
  text <- text[1:(citation_line - 1)]
  
  # collapse vector into one character string
  text_collapsed <- text %>%
    paste0(collapse = "\n")
  
  # create dataframe with metadata
  text_info <- data_frame(url = url,
                          president = president,
                          title_date = title_date,
                          transcript = text_collapsed)
  
  return(text_info)
  
}

pb <- progress_bar$new(
  format = "[:current]/[:total] [:bar] :percent eta: :eta",
  total = nrow(transcript_urls), clear = FALSE, width= 80)

# scrape transcripts
transcripts <- list()
for(i in 1:nrow(transcript_urls)){
  
  transcripts[[i]] <- get_transcript(url = transcript_urls$url[i])
  pb$tick()
  
}
transcripts <- bind_rows(transcripts)

# clean up title and date
transcripts_clean <- transcripts %>%
  mutate(title = str_extract(string = title_date, pattern = "\"(.*?)\""),
         title = str_replace_all(string = title, pattern = "(\"|,\")", replacement = ""),
         title = str_trim(string = title, side = "both")) %>%
  mutate(date = str_extract(string = title_date, pattern = ",\"(.*?)\\."),
         date = str_replace_all(string = date, pattern = "(,\"|\\.)", replacement = ""),
         date = str_trim(string = date, side = "both"),
         date = as.Date(date, format = "%B %d, %Y")) %>%
  select(date, president, title, url, transcript)

# get Trump 2018 SOTU transcript
trump_2018_sotu_url <- "https://www.cnn.com/2018/01/30/politics/2018-state-of-the-union-transcript/index.html"

trump_2018_sotu_text <- trump_2018_sotu_url %>%
  read_html() %>%
  html_nodes("div.zn-body__paragraph") %>%
  html_text() %>%
  str_trim(string = ., side = "both") %>%
  paste0(., collapse = "\n")

trump_2018_sotu <- data_frame(date = as.Date("2018-01-30"),
                              president = "Donald J. Trump",
                              title = "Address Before a Joint Session of the Congress on the State of the Union",
                              url = trump_2018_sotu_url,
                              transcript = trump_2018_sotu_text)

# combine Trump 2018 transcript with others
transcripts_clean <- bind_rows(transcripts_clean, trump_2018_sotu) %>%
  arrange(desc(date))




# Export ################################################################

write_csv(transcripts_clean, "transcripts.csv")
jsonlite::write_json(transcripts_clean, "transcripts.json", pretty = TRUE)
