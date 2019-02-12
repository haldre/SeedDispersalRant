library(tidyverse)
library(lubridate)

GoogleTrends <- read_csv("GoogleTrends.csv")
GoogleTrends$Week <- mdy(GoogleTrends$Week)
GoogleTrends <- rename(GoogleTrends, interest = 'Relative interest')
GoogleTrends$interest <- as.numeric(GoogleTrends$interest)

ggplot(GoogleTrends, aes(Week, interest, color= Interaction))+
  geom_line()+
  ylab("Relative interest")+
  scale_color_brewer(type = "qual", palette = 3, direction= -1)+
  theme_classic()

ggsave("GoogleTrends.pdf")
