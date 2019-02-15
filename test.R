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

