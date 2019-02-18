all_mail %>% 
  select(ID, Sender_Location, Recipient_Location, Request)

all_mail %>% 
  select(starts_with("request"), -Request_Number)

all_mail %>% 
  count(Request) %>% 
  filter(n >= 4) %>% 
  arrange(desc(n))

all_mail %>% 
  filter(Request == "Any Song", 
         Request_reason == "French")

ggplot(all_mail, aes(x = Request)) +
  geom_bar()

##mapping tutorial http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

#attmpt at mapping
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

#create state data
usa <- map_data("usa")
states <- map_data("state")
dim(states)
head(states)
tail(states)
louisiana <- subset(states, region == "louisiana")

#get county data 
head(louisiana)
counties <- map_data("county")
la_county <- subset(counties, region == "louisiana")
head(la_county)

#plot LA
ggplot(data = louisiana) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "beige", color = "black") + 
  coord_fixed(1.3)

#plot LA no grid
la_base <- ggplot(data = louisiana, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "beige")
la_base + theme_nothing()

#plot LA counties
la_base + theme_nothing() + 
  geom_polygon(data = la_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA) 


library(ggmap)
map <- get_map(location = 'Europe', zoom = 4)
get_openstreetmap

geocoinstall.packages("googleAuthR")
de("Lake Charles, LA")
install.packages("googleAuthR")

geocode("the white house") %>% 
  View

set.seed(500)
df <- round(data.frame(
  x = jitter(rep(-95.36, 50), amount = .3),
  y = jitter(rep( 29.76, 50), amount = .3)
), digits = 2)
map <- get_googlemap('houston', markers = df, path = df, scale = 2)
ggmap(map, extent = 'device')

