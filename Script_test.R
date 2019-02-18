#load scripts 
library(dplyr)
library(ggplot2)
library(readr)
url <- "https://github.com/jdauteri/HackberryGit/blob/master/song_requests.csv"
if (!file.exists("song_requests.csv")) {
  download.file(url, "song_requests.zip")
  unzip("song_requests.zip", files = "song_requests.csv")
}
all_mail <- read_csv("song_requests.csv")

#arranged by request
all_mail %>% 
  select (ID, Sender_Location, Request) %>% 
  arrange(desc(Request)) %>% 
  View

#arranged by sender location
all_mail %>% 
  select (ID, Sender_Location, Request) %>% 
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

#top 10 songs
all_mail %>% 
  count(Request) %>% 
  head(10) %>% 
  arrange(desc(n)) %>% 
  View

#top songs with visualization
top_songs$Request <- factor(top_songs$Request, levels = top_songs$Request[order(-top_songs$n)])
top_songs %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col(fill="beige", colour="gray") +
  coord_flip() +
  theme_minimal() +
  labs(title="Top 5 Song Requests", y = "Count", x = "Song Title")


#songs per city with visualization--can create a sonic profile of local listening and how that changed across the region
city <- all_mail %>% 
  filter(Sender_Location == "Iowa, LA") %>% 
  count(Request) %>% 
  arrange(desc(n))
city$Request <- factor(city$Request, levels = city$Request[order(-city$n)])
city %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col(fill="beige", colour="gray") +
  coord_flip() +
  theme_minimal() +
  labs(title="Iowa, LA Top 5 Songs", y = "Count", x = "Song Title")

#top songs recipient
city <- all_mail %>% 
  filter(Recipient_Location == "Sulphur, LA") %>% 
  count(Request) %>% 
  arrange(desc(n))
city$Request <- factor(city$Request, levels = city$Request[order(-city$n)])
city %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col(fill="beige", colour="gray") +
  coord_flip() +
  theme_minimal() +
  labs(title="", y = "Count", x = "Song Title")

#songs per city Hackberry (3 part-make city, then factor, then viz)
city <- all_mail %>% 
  filter(Sender_Location == "Hackberry, LA") %>% 
  count(Request) %>% 
  arrange(desc(n))
city$Request <- factor(city$Request, levels = city$Request[order(-city$n)])
city %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col() 




#songs for war
war <- all_mail %>% 
  select(Request, Request_reason) %>% 
  filter(Request_reason == "War") %>% 
  count(Request) %>% 
  arrange(desc(n))

#plot top 5 from french
war %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col()
 

#songs by language--check unclear/unspecified
all_mail %>% 
  count(Language_Request) %>% 
  ggplot(aes(x = Language_Request, y = n)) +
  geom_col() 

#French songs frequencies with visualization
all_mail %>% 
  select(Request, Language_Request, Sender_Location) %>% 
  filter(Language_Request == "French") %>% 
  count(Request) %>% 
  arrange(desc(n)) %>% 
  head(5) %>% 
  ggplot(aes(x = Request, y = n)) +
  geom_col() 


#top request from each city--not sure if this is working
all_mail %>% 
  select(Sender_Location, Request) %>% 
  group_by(Sender_Location) %>% 
  top_n(1, Request) %>% 
  View

#not sure what this is doing
all_mail %>% 
  select(Recipient_Location, Request) %>% 
  group_by(Recipient_Location) %>% 
  count(Request) %>% 
  top_n(2, Request) %>% 
  View

#could do this grouping by city and some kind of frequency chart?
all_mail %>% 
  filter(Sender_Location == "Iowa, LA") %>% 
  group_by(Language_Request) %>% 
  summarize(total_meetings = n())


##vis factor script city$Request <- factor(city$Request, levels = city$Request[order(-city$n)])




