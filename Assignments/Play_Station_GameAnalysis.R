library(tidyverse)
library(ggplot2)
#library(ggplot)
library(ggthemes)
library(viridis)
library(lubridate)
library(wordcloud)
library(qdap)
library(tm)
library(ngram)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(tidyr)

vgsales <- tbl_df(read.csv("vgsales.csv", stringsAsFactors = FALSE))

str(vgsales)
colSums(is.na(vgsales))
vgsales = vgsales[!(vgsales$Year %in% c("N/A","2017","2020")),]

ten_years = vgsales[(vgsales$Year %in% c("2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006")),]
ten_years
  



##  Genre Sale in Last Ten years

GenreSale = ten_years %>%
  
              group_by(Genre) %>%
              summarize(GlobalSales = sum(Global_Sales),
                       NASales = sum(NA_Sales),
                       EUSales = sum(EU_Sales), 
                       JapanSales = sum(JP_Sales)) 

GenreSale = melt(GenreSale,na.rm = FALSE,)
names(GenreSale) = c('Genre','SaleType','Sale')

 GenreSale

write_csv(GenreSale, path = "STAT515/GenreSale.csv")

ggplot(data=GenreSale,aes(x = Genre,y = Sale,fill=SaleType)) + 
  geom_bar(colour='black' , stat='identity', position='dodge') +  
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1,angle=45),
        plot.title = element_text(hjust=0.5)) + ## center 
  ggtitle('Genre Sale in last Ten Years (2006-2015)') + 
  scale_fill_brewer(palette = 'RdYlBu')+
  ylab('Sale')



# Video games sales growth

yearSale <-ten_years %>%
            group_by(Year) %>%
            summarize(NASales = sum(NA_Sales),
            EUSales = sum(EU_Sales), 
            JapanSales = sum(JP_Sales)) 


   salesyears <- melt(yearSale)
  names(salesyears) = c('Year','Saletype','amtsale')
  salesyears

ggplot(data=yearSale,aes(x = as_numeric2(Year),y = amtsale,color = Saletype)) +
  geom_line(size=2)+
  ggtitle("Sales in last 10 Years")+
  ylab('Sale')+
  xlab('Year')


write_csv(yearSale, path = "STAT515/yearSale.csv")




######Games Released per year


Gamesyear <- vgsales %>%
              group_by(Year) %>%
              summarize(Number_of_Games = n()) 

write_csv(Gamesyear, path = "STAT515/Gamesyear.csv")


  ggplot(aes(x = Year, y = Number_of_Games)) +
  geom_col(fill = "magenta4") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Games released per Year", x = "Year", y = "Sales (units)")





# ~~~~~~~~Top Ten Publishers in last Ten years

top_publisher <- ten_years %>% group_by(Publisher) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count)) %>% top_n(10)

write_csv(top_publisher, path = "STAT515/publisher.csv")

ggplot(data=top_publisher,aes(x=Publisher,y=Count,fill=Count)) +
  geom_bar(colour='black',stat='identity') + 
  theme_bw() +
  ggtitle('Top 20 Publisher') + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        plot.title = element_text(hjust=0.5)) + 
  scale_fill_distiller(palette = 'RdYlBu') + 
  coord_flip()
## coordinate flip bar == > barh



#Top Genre by sales



ten_years %>%
  gather("Region", "Value", c("NA_Sales", "EU_Sales", "JP_Sales")) %>%
  group_by(Region, Genre) %>%
  summarize(Sales = sum(Value)) %>%
  top_n(n = 3) %>%
  ggplot(aes(x = Region, y = Sales, group = Region, fill = Genre)) +
  geom_col(position = "stack") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Top Genre by Sales per Region")



nintendoplatforms = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sonyplatforms = c("PS","PS2","PSP","PS3","PS4","PSV")
segaplatforms = c("GEN","SCD","DC","GG")
msplatforms = c("XB","X360", "XOne")
otherplatforms = c("2600","3DO","NG","PCFX","TG16")


ten_years$Platformvendor[ten_years$Platform %in% nintendoplatforms] <- "Nintendo"
ten_years$Platformvendor[ten_years$Platform %in% sonyplatforms] <- "Sony"
ten_years$Platformvendor[ten_years$Platform %in% msplatforms] <- "Microsoft"
ten_years$Platformvendor[ten_years$Platform %in% segaplatforms] <- "Sega"
ten_years$Platformvendor[ten_years$Platform == "PC"] <- "Comp"
ten_years$Platformvendor[is.na(ten_years$Platformvendor)] <- "Other"

vgsales


temp1 <- ggplot(ten_years, aes(x=as_numeric2(Year))) + 
  geom_line(aes(y=..count.., color=Platformvendor), stat="bin", bins = 30, size=1.5) + 
  ggtitle("Released of Titles per Year") +
  xlab("Year ") + ylab("Released Titles count")
  

temp2 <-ggplot(aggregate(JP_Sales~Platformvendor + Year, data=ten_years, FUN=sum), aes(x=as_numeric2(Year))) + 
  geom_line(aes(y= JP_Sales, color=Platformvendor),size =1.5) + 
  ggtitle("Sold Units per Year of Release") +
  xlab("Year ") + ylab("Sold Units (in Million)")
  grid.arrange(temp1,temp2, ncol=1)  
  
  
  list_of_values <- c("Nintendo","Microsoft","Sony")
  SI_data <- filter(Platformvendor %in% list_of_values)  
  
  ten_years %>%
    filter(Platformvendor %in% list_of_values) %>%
    group_by(Platformvendor, Genre) %>%
    summarize(Sales = sum(EU_Sales)) %>%
    top_n(n = 3) %>%
    ggplot(aes(x = Platformvendor, y = Sales, group = Platformvendor, fill = Genre)) +
    geom_col(position = "stack") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title = "Top Genre by Sales per Company")
  
genresEUsales <-  ten_years %>%
  filter(Platformvendor %in% list_of_values) %>%
  group_by(Platformvendor, Genre) %>%
  summarize(Sales = sum(EU_Sales)) %>%
  top_n(n = 3)


write_csv(genresEUsales, path = "STAT515/genresEUsales.csv")

