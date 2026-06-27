library(rvest)
library(xml2)
library(dplyr)
library(httr)
library(tidyverse)
library(readr)

#####Read in CSV's that contain the url extension for sites that will be scraped
#####These urls were scraped in the Comics_Url script
#####Graphic novels are collected editions. Comics are individual comic issues. Naming convention comes
#####from DC.com URLs

comics <- read.csv("comics_url.csv")
graphic_novel <- read.csv("Graphic_Novels_url.csv")



######################################################################################
#########################Graphic novels###############################################

######Create tibble. I want to scrape whatever info we can, but pages have slight differences
######so not all headers will appear in all sites. I will address this by pulling in all the paragraph
######level text and parsing it later
gn_tibble <- tibble(Text = list(),
                    Characters = character(),
                    URL = character())



#####Loop to scrape info from urls
for (i in 1:nrow(graphic_novel)) {
  ####Defining the url for the graphic novel that we are currently scraping
  ext <- graphic_novel[i, 1]
  paste0("https://www.dc.com", ext) -> temp_link
  
  
  #####Setting a user agent and pulling data from the url
  #####as to not get blocked. Lines 38 to 42 come from Claude
  response <- GET(
    temp_link,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
  )
  page <- read_html(response)
  ####
  
  ######Pulling the hyperlinks from the page. The characters that star in a GN/ comic are
  ######clickable links on the page. I use the links to identify these characters.
  page |> html_elements("section") %>%
    html_elements("a") %>%
    html_attr("href") -> hyperlinks
  
  ####Creating a list of characters in each GN/ issue
  data.frame(link = hyperlinks) %>%
    dplyr::filter(., grepl("/characters/", link)) %>% mutate(link = gsub("/characters/", "", link)) %>%
    pull(link) %>% paste(., collapse = ", ") -> character_list
  
  #####Pulling the paragraph text.
  page |> html_elements("section") %>%
    html_elements("p") |> html_text2() -> Meta
  
  ###Creating a tibble row that contains the paragraph text as a list, the character list as an element,
  ###and the URL as an element.
  temp_tibble <- tibble(Text = list(Meta),
                        Characters = character_list,
                        URL = temp_link)
  
  ###add the tibble row to the full tibble
  bind_rows(gn_tibble, temp_tibble) -> gn_tibble
  
  #printing loop to know what iteration we're on
  print(paste0(i, "/", nrow(graphic_novel)))
  
  Sys.sleep(1)
  
}


###Saving tibble for future parsing
write_rds(gn_tibble, "gn.rds")



####Identify headers. They are in the paragraph text and can be identified because they end with ":"
####However, many descriptions and GN names also contain ":", so I filter the data to just look at
####strings with less than 15 characters
gn_tibble %>% select(Text) %>% unnest(Text) %>% as.data.frame() %>%
  filter(str_detect(Text, regex(":", ignore_case = TRUE))) %>% unique() %>%
  filter(nchar(Text) < 15) %>% select(Text) -> Col_List

print(Col_List)

###Two GN Names are less than 15 characters and contain ":". I filter them out to create a List of headers
###Three additional columns are added to the list
Col_List %>% filter(Text != "DC: Mech 2022" &
                      Text != "SUPERMAN: LOST") %>% add_row(.after = nrow(.), Text = "Characters:") %>%
  add_row(.after = nrow(.), Text = "Description:") %>%
  add_row(.after = nrow(.), Text = "URL:") -> Col_list

print(Col_list)

Col_list %>% str()


####Creating data frame for data scraped
data.frame(matrix(nrow = 0, ncol = nrow(Col_list))) %>% rename_at(vars(colnames(.)), ~ Col_list$Text) ->
  gn_df

###Loop to parse each page scrapes
for (i in 1:nrow(gn_tibble)) {
  ##extracting paragraph level text as a character vector
  gn_tibble[i, 1] %>% unlist() %>% as.character() -> temp_paragraph
  
  ####Characters and URL were saved to the tibble. We pull those directly for the data frame
  ####There is no header that defines the Description. However, it's the longest string saved
  ####in the paragraph, so it can be found easily. In addition, it's usually element 3, just not always
  gn_df[i, c("Characters:")] <- gn_tibble[i, c("Characters")]
  gn_df[i, c("Description:")] <- temp_paragraph %>% as.data.frame() %>% rename(loc = ".") %>%
    slice_max(str_length(loc), n = 1)
  gn_df[i, c("URL:")] <- gn_tibble[i, c("URL")]
  
  
  ###Define a function to find were the headers are located in the list
  heading_location <- function(x) {
    which(temp_paragraph == x)
  }
  
  ####Find the location of the headers. The values for each column are going to be the list
  ####of elements between the headers, so I identify the elements after and before each header
  sapply(Col_list[, 1], FUN = heading_location) %>% unlist() %>%
    as.data.frame() %>% rename(loc = ".") %>% rownames_to_column(., var = "Header") %>%
    arrange(loc) %>% mutate(text_start = loc + 1) %>%
    mutate(text_end = length(temp_paragraph)) -> text_locations
  
  if (nrow(text_locations) > 1) {
    text_locations[1:(nrow(text_locations) - 1), c("text_end")] <-
      (text_locations[2:(nrow(text_locations)), c("loc")] - 1)
  }
  
  #####I loop through each column (header) to define the element
  #text_locations$value <- NA
  for (j in 1:nrow(text_locations)) {
    Head <- text_locations[j, c("Header")]
    start <- text_locations[j, c("text_start")]
    end <- text_locations[j, c("text_end")]
    gn_df[i, Head] <- paste(temp_paragraph[start:end], collapse = ", ")
  }
  
  ###Keeping track of iteration
  print(paste0(i, "/", nrow(gn_tibble)))
}


####There seems to be one test page that got scraped. This is not a real GN, so we filter it out
gn_df %>% filter(
  `URL:` != "https://www.dc.com/graphic-novels/node-field-gn-series-title-vol-node-field-gn-volumenum"
) -> gn_df


###Save data frame
write_csv(gn_df, "GraphicNovel_df.csv")





######################################################################################
#########################Comics###############################################

######Same logic as with GN explained above
comic_tibble <- tibble(Text = list(),
                       Characters = character(),
                       URL = character())

####There's one dead link that got scraped. When I look at the the page this link was scrapped from,
####I find there was also the link "/comics/green-lantern-2005/green-lantern-56" with the same preview
####picture. I assume that the "/comics/green-lantern-56" was included by accident and
####"/comics/green-lantern-2005/green-lantern-56/" is the correct link, so I filter out the broken one.

comics %>% filter(link != "/comics/green-lantern-56") -> comics

#####Loop to scrape info from urls
for (i in 1:nrow(comics)) {
  ####Defining the url for the graphic novel that we are currently scraping
  ext <- comics[i, 1]
  paste0("https://www.dc.com", ext) -> temp_link
  
  
  #####Setting a user agent and pulling data from the url
  #####as to not get blocked. Lines 38 to 42 come from Claude
  response <- GET(
    temp_link,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
  )
  page <- read_html(response)
  ####
  
  ######Pulling the hyperlinks from the page. The characters that star in a comic are
  ######clickable links on the page. I use the links to identify these characters.
  page |> html_elements("section") %>%
    html_elements("a") %>%
    html_attr("href") -> hyperlinks
  
  ####Creating a list of characters in each issue
  data.frame(link = hyperlinks) %>%
    dplyr::filter(., grepl("/characters/", link)) %>% mutate(link = gsub("/characters/", "", link)) %>%
    pull(link) %>% paste(., collapse = ", ") -> character_list
  
  #####Pulling the paragraph text.
  page |> html_elements("section") %>%
    html_elements("p") |> html_text2() -> Meta
  
  ###Creating a tibble row that contains the paragraph text as a list, the character list as an element,
  ###and the URL as an element.
  temp_tibble <- tibble(Text = list(Meta),
                        Characters = character_list,
                        URL = temp_link)
  
  ###add the tibble row to the full tibble
  bind_rows(comic_tibble, temp_tibble) -> comic_tibble
  
  #printing loop to know what iteration we're on
  print(paste0(i, "/", nrow(comics)))
  
  Sys.sleep(1)
  
}


###Saving tibble for future parsing
write_rds(comic_tibble, "comic.rds")















####Identify headers. They are in the paragraph text and can be identified because they end with ":"
####However, many descriptions and GN names also contain ":", so I filter the data to just look at
####strings with less than 15 characters
comic_tibble %>% select(Text) %>% unnest(Text) %>% as.data.frame() %>%
  filter(str_detect(Text, regex(":", ignore_case = TRUE))) %>% unique() %>%
  filter(nchar(Text) < 16) %>% select(Text) -> Col_List

print(Col_List)

###4 comic Names are less than 15 characters and contain ":". I filter them out to create a List of headers
###Three additional columns are added to the list
Col_List %>% filter(
  Text != "ABC: A-Z 2005" &
    Text != "Enter: Ursa!"  &
    Text != "Parasite: Noun." &
    Text != "gen:LOCK 2019" &
    Text != "SUPERMAN: LOST" &
    Text != "DC: Mech 2022"
) %>% add_row(.after = nrow(.), Text = "Characters:") %>%
  add_row(.after = nrow(.), Text = "Description:") %>%
  add_row(.after = nrow(.), Text = "URL:") -> Col_list

print(Col_list)

Col_list %>% str()


####Creating data frame for data scraped
data.frame(matrix(nrow = 0, ncol = nrow(Col_list))) %>% rename_at(vars(colnames(.)), ~ Col_list$Text) ->
  comic_df

###Loop to parse each page scrapes
for (i in 1:nrow(comic_tibble)) {
  ##extracting paragraph level text as a character vector
  comic_tibble[i, 1] %>% unlist() %>% as.character() -> temp_paragraph
  
  ####Characters and URL were saved to the tibble. We pull those directly for the data frame
  ####There is no header that defines the Description. However, it's the longest string saved
  ####in the paragraph, so it can be found easily. In addition, it's usually element 3, just not always
  comic_df[i, c("Characters:")] <- comic_tibble[i, c("Characters")]
  comic_df[i, c("Description:")] <- temp_paragraph %>% as.data.frame() %>% rename(loc = ".") %>%
    slice_max(str_length(loc), n = 1)
  comic_df[i, c("URL:")] <- comic_tibble[i, c("URL")]
  
  
  ###Define a function to find were the headers are located in the list
  heading_location <- function(x) {
    which(temp_paragraph == x)
  }
  
  ####Find the location of the headers. The values for each column are going to be the list
  ####of elements between the headers, so I identify the elements after and before each header
  sapply(Col_list[, 1], FUN = heading_location) %>% unlist() %>%
    as.data.frame() %>% rename(loc = ".") %>% rownames_to_column(., var = "Header") %>%
    arrange(loc) %>% mutate(text_start = loc + 1) %>%
    mutate(text_end = length(temp_paragraph)) -> text_locations
  
  if (nrow(text_locations) > 1) {
    text_locations[1:(nrow(text_locations) - 1), c("text_end")] <-
      (text_locations[2:(nrow(text_locations)), c("loc")] - 1)
  }
  
  #####I loop through each column (header) to define the element
  #text_locations$value <- NA
  for (j in 1:nrow(text_locations)) {
    Head <- text_locations[j, c("Header")]
    start <- text_locations[j, c("text_start")]
    end <- text_locations[j, c("text_end")]
    comic_df[i, Head] <- paste(temp_paragraph[start:end], collapse = ", ")
  }
  
  ###Keeping track of iteration
  print(paste0(i, "/", nrow(comic_tibble)))
}


####About 2% of comics are missing a series and 10% of issues numbers are missing
sum(is.na(comic_df$`Series:`)) / nrow(comic_df)
sum(is.na(comic_df$`Volume/Issue #:`)) / nrow(comic_df)


####The ones not parsed from the text can be pulled from the url
comic_df %>% mutate(temp_column = gsub("https://www.dc.com/comics/", "", `URL:`)) %>%
  mutate(temp_column = gsub("-", " ", temp_column)) %>%
  mutate(`Series:` = ifelse(is.na(`Series:`), str_to_title((
    str_remove(temp_column, "\\d+$")
  )), `Series:`)) %>%
  mutate(`Volume/Issue #:` = ifelse((is.na(`Volume/Issue #:`)), (str_extract(
    temp_column, "\\d+$"
  )), `Volume/Issue #:`)) %>% select(-temp_column) -> comics_df_parsed


###Save data frame
write_csv(comics_df_parsed, "Comic_df.csv", col_names = TRUE)


