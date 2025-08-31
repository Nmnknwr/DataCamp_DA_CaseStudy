library(data.table)
library(skimr)
library(tidyverse)
library(plotly)
library(priceR)

source("Functions.R")

dt <- fread("https://s3.amazonaws.com/talent-assets.datacamp.com/boat_data.csv")


skim(dt)

dt$view_pct <- 100*dt$`Number of views last 7 days`/sum(dt$`Number of views last 7 days`)

dt <- dt %>% 
  separate(Price,c("Currency","Amount"),sep=" ",remove=TRUE,convert=TRUE)

dt <- dt %>% 
  separate(Type,c("Condition","Fuel"),sep=",",remove=TRUE,convert=TRUE)

dt[Condition=="",Condition:=NA]

dt[Condition=="Diesel" & is.na(Fuel),Fuel:="Diesel"]
dt[Condition=="Diesel" & Fuel=="Diesel",Condition:=NA]

dt[Condition=="Unleaded" & is.na(Fuel),Fuel:="Unleaded"]
dt[Condition=="Unleaded" & Fuel=="Unleaded",Condition:=NA]

dt[Condition=="Electric" & is.na(Fuel),Fuel:="Electric"]
dt[Condition=="Electric" & Fuel=="Electric",Condition:=NA]

dt[,Condition := as.factor(Condition)]
dt[,Fuel := as.factor(Fuel)]
dt[,Material := as.factor(Material)]

conversions <- sapply(unique(dt$Currency),avg_ex) %>% 
  data.frame() %>% 
  rownames_to_column() %>%
  `colnames<-`(c('Currency','conv'))

dt <- dt %>% 
  left_join(conversions,by='Currency') %>%
  mutate(Amount_CAD = Amount*conv) %>% 
  select(-c(14))
  

dt[,Currency := as.factor(Currency)]

skim(dt)

hist(log(dt$`Number of views last 7 days`),xlab="Log of Total Views L7D",main = "Distribution of L7D views")
abline(v=mean(log(dt$`Number of views last 7 days`)),col="red",lwd=3)



dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(`Boat Type`)][order(-views)][1:10]
pie_plot(dt$`Boat Type`)

dt[Manufacturer!="", .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Manufacturer)][order(-views)]
pie_plot(dt$Manufacturer)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Material)][order(-views)]
pie_plot(dt$Material)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Location)][order(-views)]
pie_plot(dt$Location)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(`Year Built`)][order(-views)]
pie_plot(dt$`Year Built`)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Condition)][order(-views)]
pie_plot(dt$Condition)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Fuel)][order(-views)]
pie_plot(dt$Fuel)

dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(Currency)][order(-views)]
pie_plot(dt$Currency)


dtx <- log(dt$Amount_CAD)
dty <- dt$`Number of views last 7 days`
h <- hist(log(dt$Amount_CAD))
breaks <- data.frame("beg"=h$breaks[-length(h$breaks)],"end"=h$breaks[-1])
sums <- apply(breaks, MARGIN=1, FUN=function(x) { sum(dty[dtx >= x[1] & dtx < x[2] ]) })
h$counts <- sums
mean <- mean(log(dt$Amount_CAD))
median <- median(log(dt$Amount_CAD))
plot(h, ylab="Total Views (L7D)", main="Sum of L7D views Within Bins")
abline(v=mean,col="red",lwd=3)
abline(v=median,col="blue",lwd=3)

exp(mean)
exp(median)

skim(dt_converted)

hist(dt$`Number of views last 7 days`)

quantile(dt$`Number of views last 7 days`,c(.7,.8,.9,.95,.99))

dt$area <- dt$Length*dt$Width

all_boat_types <- as.data.frame(unique(unlist(strsplit(as.character(dt$`Boat Type`),","))))
names(all_boat_types)[1] <- "boat_type"

(per_boat_type <- dt %>% 
  separate_rows(`Boat Type`,sep=",") %>% 
  group_by(`Boat Type`) %>% 
  summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
  arrange(desc(views_per_listing)))

per_boat_type %>% 
  ggplot(aes(reorder(`Boat Type`,+views_per_listing),views_per_listing,group=`Boat Type`)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Boat Type",x="Number of views per listing",y="Boat Type") +
  theme(legend.position = "none") +
  theme_classic()

(per_manufacturer <- dt %>% 
    group_by(Manufacturer) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing))) ## To remove text from Manufacturer column? (take only 1st word?)

per_manufacturer %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(Manufacturer,+views_per_listing),views_per_listing,group=Manufacturer)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Manufacturer",x="Number of views per listing",y="Manufacturer") +
  theme(legend.position = "none") +
  theme_classic()


(per_material <- dt %>% 
    group_by(Material) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing)))

per_material %>% 
  ggplot(aes(reorder(Material,+views_per_listing),views_per_listing,group=Material)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Material",x="Number of views per listing",y="Material") +
  theme(legend.position = "none") +
  theme_classic()


(per_location <- dt %>% 
    group_by(Location) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing))) ## Cut location string to only country?

per_location %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(Location,+views_per_listing),views_per_listing,group=Location)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Location",x="Number of views per listing",y="Location") +
  theme(legend.position = "none") +
  theme_classic()


(per_year <- dt %>% 
    group_by(`Year Built`) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing)))

per_year %>% 
  top_n(20) %>% 
  ggplot(aes(reorder(`Year Built`,+views_per_listing),views_per_listing,group=`Year Built`)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Year Built",x="Number of views per listing",y="Year Built") +
  theme(legend.position = "none") +
  theme_classic()

(per_condition <- dt %>% 
    group_by(Condition) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing)))

per_condition %>% 
  filter(!(is.na(Condition))) %>% 
  ggplot(aes(reorder(Condition,+views_per_listing),views_per_listing,group=Condition)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by Condition",x="Number of views per listing",y="Condition") +
  theme(legend.position = "none") +
  theme_classic()


(per_fuel <- dt %>% 
    group_by(Fuel) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing)))

per_fuel %>% 
  filter(!(is.na(Fuel))) %>% 
  ggplot(aes(reorder(Fuel,+views_per_listing),views_per_listing,group=Fuel)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by type of Fuel",x="Number of views per listing",y="Fuel Type") +
  theme(legend.position = "none") +
  theme_classic()

(per_currency <- dt %>% 
    group_by(Currency) %>% 
    summarise(count_of_listings = n(),total_views = sum(`Number of views last 7 days`),views_per_listing = total_views/count_of_listings) %>% 
    arrange(desc(views_per_listing)))

per_currency %>% 
  ggplot(aes(reorder(Currency,+views_per_listing),views_per_listing,group=Currency)) +
  geom_col(color="black",fill="steel blue") +
  coord_flip() +
  labs(title="Views per listing by listing currency",x="Number of views per listing",y="Currency") +
  theme(legend.position = "none") +
  theme_classic()

dtx <- log(dt$Amount_CAD)
dty <- dt$`Number of views last 7 days`
h <- hist(log(dt$Amount_CAD))
breaks <- data.frame("beg"=h$breaks[-length(h$breaks)],"end"=h$breaks[-1])
sums <- apply(breaks, MARGIN=1, FUN=function(x) { sum(dty[dtx >= x[1] & dtx < x[2] ]) })
h$counts <- sums
mean <- mean(log(dt$Amount_CAD))
median <- median(log(dt$Amount_CAD))
plot(h, ylab="Total Views (L7D)", main="Distribution of L7D views as per Price",xlab = "Log(Price in CAD)")
abline(v=mean,col="red",lwd=3)
abline(v=median,col="blue",lwd=3)

dt %>% 
  arrange(desc(`Number of views last 7 days`)) %>% 
  top_n(50) %>% 
  group_by(`Boat Type`) %>% 
  summarise(listings = n(),views = sum(`Number of views last 7 days`), ) %>% 
  arrange(desc(views))