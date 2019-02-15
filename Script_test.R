#load scripts 
library(dplyr)
library(readr)
url <- "https://github.com/jdauteri/HackberryGit/blob/master/song_requests.csv"
if (!file.exists("song_requests.csv")) {
  download.file(url, "song_requests.zip")
  unzip("song_requests.zip", files = "song_requests.csv")
}
all_mail <- read_csv("song_requests.csv")

#arranged by request
all_mail %>% 
  select (Sender_Location, Request) %>% 
  arrange(desc(Request))

#arranged by sender location
all_mail %>% 
  select (Sender_Location, Request) %>% 
  arrange(desc(Sender_Location))

#count frequency of each sender location
sender_count <- all_mail %>% 
  count(Sender_Location) %>% 
  arrange(desc(n)) 
#add head() to display only certain amount of results 

#count frequency of each recipient location
recipient_count <- all_mail %>% 
  count(Recipient_Location) %>% 
  arrange(desc(n)) 

# to count reasons for requests
reason_count <- all_mail %>% 
  select(Request, Request_reason) %>% 
  count(Request_reason) %>% 
  arrange(desc(n)) 

# to find the most songs requested for war
request_war <- all_mail %>% 
  select(Request, Request_reason) %>% 
  filter(Request_reason == "War") %>% 
  count(Request) %>% 
  arrange(desc(n)) 

#song count
song_count <- all_mail %>% 
  count(Request) %>% 
  arrange(desc(n))

#tops songs
top_songs <- all_mail %>% 
  count(Request) %>% 
  filter(n >= 4) %>% 
  arrange(desc(n)) 

#songs for war
french <- all_mail %>% 
  select(Request, Request_reason) %>% 
  filter(Request_reason == "War") %>% 
  count(Request) %>% 
  arrange(desc(n)) 

#songs by language--check unclear/unspecified
all_mail %>% 
  count(Language_Request)

#French songs frequencies
all_mail %>% 
  select(Request, Language_Request, Sender_Location) %>% 
  filter(Language_Request == "French") %>% 
  count(Request) %>% 
  arrange(desc(n))

#top request from each city
all_mail %>% 
  select(Sender_Location, Request) %>% 
  group_by(Sender_Location) %>% 
  top_n(1, Request) %>% 
  View

all_mail %>% 
  select(Recipient_Location, Request) %>% 
  group_by(Recipient_Location) %>% 
  count(Request) %>% 
  top_n(2, Request) %>% 
  View






