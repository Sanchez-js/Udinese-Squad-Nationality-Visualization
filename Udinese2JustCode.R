rm(list = ls())
library(rvest)
udinese_main <- read_html("https://sofifa.com/team/55/udinese/?hl=en-US")
squad_info<-udinese_main%>%html_nodes(xpath='//*[@id="body"]/div[3]/div/div[2]/div[2]/table')%>%html_table()
squad_info_df <-as.data.frame(squad_info)
player_name <- squad_info_df$Name
player_nat<- udinese_main %>%html_elements("tbody.list")%>% html_elements("img.flag")%>% html_attr("title")
name_and_nat<-data.frame(player_name,player_nat[1:23])
instances_of_countries<-table(name_and_nat$player_nat)
country_count<-data.frame(instances_of_countries)
colnames(country_count) <- c("nation","count")
library(stringr)
country<-unique(player_nat)
money<-rep(c("$"),each=length(21))
countries2<-paste(money,country, collapse="")
new_coun<-str_split(countries2, " ")[[1]]
indices <- c(1,13,15,16)
result <- new_coun[-indices]
result[14] <- c("Senegal$")

library(tidyverse)
map.world <- map_data('world')
anti_join(country_count, map.world, by = c('nation' = 'region'))
map.play <- left_join( map.world, country_count, by = c('region' = 'nation')) 
ggplot(map.play, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = count))
library(tidyverse)
library(sf)
library(rvest)
library(stringr)
library(scales)
ggplot(map.play, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = count)) +
  guides(fill = guide_legend(reverse = T)) +
  labs(fill = 'Number of Players Per Nation'
       ,title = 'Udinese Squad Nationality'
       ,subtitle = '20/21 season'
       ,x = NULL
       ,y = NULL) +
  theme(text = element_text(color = '#000000')
        ,plot.title = element_text(size = 28)
        ,plot.subtitle = element_text(size = 14)
        ,axis.ticks = element_blank()
        ,axis.text = element_blank()
        ,panel.grid = element_blank()
        ,legend.position = c(.18,.36)
        ,legend.background = element_blank()
        ,legend.key = element_blank()
  ) +
  annotate(geom = 'text'
           ,label = 'Sofifa\nhttps://sofifa.com/team/55/udinese/?hl=en-US&col=pt&sort=desc'
           ,x = 18, y = -55
           ,size = 3
           ,color = '#000000'
           ,hjust = 'left'
  )+
  theme(legend.title = element_text(colour="black", face="bold"))
