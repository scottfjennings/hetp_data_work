
##subset to each by = n record
hetp_each5<- hetp[seq(1, NROW(hetp), by = 5),]

go <- data.frame(table(hetp_each5$bird, hetp_each5$hr)) %>% 
  select(bird = Var1, hr = Var2, each5bird.total = Freq)
goo <- data.frame(table(hetp_each5$hr)) %>% 
  select(hr = Var1, each5total = Freq)
goof <- full_join(go, goo, by = c("hr")) %>% 
  mutate(each5perc = round(100*each5bird.total/each5total, 2))


fo <- data.frame(table(hetp$bird, hetp$hr)) %>% 
  select(bird = Var1, hr = Var2, bird.total = Freq)
foo <- data.frame(table(hetp$hr)) %>% 
  select(hr = Var1, total = Freq)
foof <- full_join(fo, foo, by = c("hr")) %>% 
  mutate(perc = round(100*bird.total/total, 2))



foofgoof <- full_join(goof, foof, by = c("bird", "hr")) %>% 
  mutate(eval = each5perc - perc)

ggplot(data = foofgoof, aes(x=hr, y = eval), group = bird)+
  geom_point(aes(color = bird))+
  geom_line(aes(color = bird))