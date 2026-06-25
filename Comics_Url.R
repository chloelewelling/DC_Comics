library(rvest)
library(xml2)
library(dplyr)
library(httr)
#
# batman <- read_html("https://www.dc.com/comics/batman-2016/batman-32")
# batman_section <- batman |> html_elements("section")
# #html_structure(batman)
#
# batman_section |> html_elements("p") |> html_text2()->batman_tal_spec



#######We're scrapping from an interactive javascript list of comics ("https://www.dc.com/comics"). However, it seems to have a bug
#######where it doesn't display comic links after page 100. To address this, we filter based on date so we can use hyperlinks that have 
#######less than 100 pages of filtered content

ComicsList_01_01_1993_to_01_01_2005 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIxOTMzLTAxLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMDUtMDEtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_01_01_2005_to_09_01_2006 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDA1LTAxLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMDYtMDktMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_09_01_2006_to_03_01_2008 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDA2LTA5LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMDgtMDMtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_03_01_2008_to_09_01_2009 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDA4LTAzLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMDktMDktMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_09_01_2009_to_02_01_2011 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsInRvIjoiMjAxMS0wMi0wMVQyMzo1OTo1OS45OTlaIiwiZnJvbSI6IjIwMDktMDktMDFUMDA6MDA6MDAuMDAwWiJ9fQ%3D%3D"
ComicsList_02_01_2011_to_08_01_2012 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDExLTAyLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMTItMDgtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_08_01_2012_to_03_01_2014 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDEyLTA4LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMTQtMDMtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_03_01_2014_to_09_01_2015 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDE0LTAzLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMTUtMDktMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_09_01_2015_to_03_01_2017 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDE1LTA5LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMTctMDMtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_03_01_2017_to_08_01_2018 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDE3LTAzLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMTgtMDgtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_08_01_2018_to_01_01_2020 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDE4LTA4LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMjAtMDEtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_01_01_2020_to_09_01_2021 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDIwLTAxLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMjEtMDktMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_09_01_2021_to_01_01_2024 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDIxLTA5LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMjQtMDEtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_01_01_2024_to_06_01_2026 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDI0LTAxLTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjIwMjYtMDYtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"
ComicsList_06_01_2026_to_06_01_3000 <- "https://www.dc.com/comics?filters=eyJ0eXBlIjpbIkNvbWljQm9vayIsIkdyYXBoaWNOb3ZlbCJdLCJ1cGRhdGVkIjp7InR5cGUiOiJyYW5nZSIsImZyb20iOiIyMDI2LTA2LTAxVDAwOjAwOjAwLjAwMFoiLCJ0byI6IjMwMDAtMDYtMDFUMjM6NTk6NTkuOTk5WiJ9fQ%3D%3D"


######################################################
###Creating two data frames. Graphic_novels are the collected editions. Comics are the individual issues.

graphic_novels <- data.frame(link = character())
comics <- data.frame(link = character())


####Vector of hyperlinks to loop through
comic_links <- c(
  ComicsList_01_01_1993_to_01_01_2005,
  ComicsList_01_01_2005_to_09_01_2006,
  ComicsList_09_01_2006_to_03_01_2008,
  ComicsList_03_01_2008_to_09_01_2009,
  ComicsList_09_01_2009_to_02_01_2011,
  ComicsList_02_01_2011_to_08_01_2012,
  ComicsList_08_01_2012_to_03_01_2014,
  ComicsList_03_01_2014_to_09_01_2015,
  ComicsList_09_01_2015_to_03_01_2017,
  ComicsList_03_01_2017_to_08_01_2018,
  ComicsList_08_01_2018_to_01_01_2020,
  ComicsList_01_01_2020_to_09_01_2021,
  ComicsList_09_01_2021_to_01_01_2024,
  ComicsList_01_01_2024_to_06_01_2026,
  ComicsList_06_01_2026_to_06_01_3000
)



for (link_choice in comic_links) {
  for (i in 1:100) {
    
    paste0(link_choice, "&page=", i) -> temp_link
    
    ###We get blocked if we don't include this code
    response <- GET(
      temp_link,
      user_agent(
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
      )
    )
    page <- read_html(response)
    
    
    ####Pulling hyperlinks
    page |>
      html_elements("section") %>%
      html_elements("a") %>%
      html_attr("href") -> temp_links
    
    #####Using url to identify comic page links and graphic novel links. We don't want any of the dcuuniverseinfinite links
    #####since those are links ti the actual comics
    data.frame(link = temp_links) %>%
      dplyr::filter(., grepl("/comics/", link)) %>%
      dplyr::filter(., !grepl("dcuniverseinfinite.com", link)) -> temp_comics
    
    data.frame(link = temp_links) %>%
      dplyr::filter(., grepl("/graphic-novels/", link)) -> temp_gn
    
    
    ###Adding new comic urls from page. Current advertised comics show up on every page, so 
    ###we ensure that we're only looking at one row per comic
    graphic_novels %>% rbind(., temp_gn) %>% unique() -> graphic_novels
    comics %>% rbind(., temp_comics) %>% unique() -> comics
    
    
    ###Don't want to send requests too frequently
    Sys.sleep(3)
    
    ###I like keeping track of code (in case of failure)
    print(paste0(link_choice, "-", i))
  }
}

write.csv(graphic_novels, "graphic_novels_url.csv", row.names = FALSE)
write.csv(comics, "comics_url.csv", row.names = FALSE)

