############################################################################################################
# Data Visualisation
# Farmers Market
# Author: Burcu Ozek
############################################################################################################

# Required libraries are downloaded
library(dplyr)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(reshape2)
library(tidyr)
library(RColorBrewer)

# Data is read
fmarket <- read.csv("~/Desktop/fmarket.csv", na.strings="", stringsAsFactors=FALSE)
View(fmarket)

# None of the rows is dublicated
fmarket[duplicated(fmarket)]
unique(fmarket)

# Convert social media columns binary
fmarket$Website[!is.na(fmarket$Website)] <- as.numeric(1) 
fmarket$Website[is.na(fmarket$Website)] <- as.numeric(0)
fmarket$Facebook[!is.na(fmarket$Facebook)] <- as.numeric(1) 
fmarket$Facebook[is.na(fmarket$Facebook)] <- as.numeric(0)
fmarket$Twitter[!is.na(fmarket$Twitter)] <- as.numeric(1)  
fmarket$Twitter[is.na(fmarket$Twitter)] <- as.numeric(0)
fmarket$Youtube[!is.na(fmarket$Youtube)] <- as.numeric(1)  
fmarket$Youtube[is.na(fmarket$Youtube)] <- as.numeric(0)
fmarket$OtherMedia[!is.na(fmarket$OtherMedia)] <- as.numeric(1) 
fmarket$OtherMedia[is.na(fmarket$OtherMedia)] <- as.numeric(0)

# Convert payment types columns binary
# First, I checked if there is another categorie other than Y or N 
as.factor(fmarket$Credit)
as.factor(fmarket$WIC)
as.factor(fmarket$WICcash)
as.factor(fmarket$SFMNP)
as.factor(fmarket$SNAP)

fmarket$Credit <- ifelse(fmarket$Credit == "Y", 1, 0)
fmarket$WIC <- ifelse(fmarket$WIC == "Y", 1, 0)
fmarket$WICcash <- ifelse(fmarket$WICcash == "Y", 1, 0)
fmarket$SFMNP <- ifelse(fmarket$SFMNP == "Y", 1, 0)
fmarket$SNAP <- ifelse(fmarket$SNAP == "Y", 1, 0)


My_Theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 14),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.text.y = element_text(size = 14),
  text=element_text(size=14))


# 1 ) Number of farmers markets by state
fmarket_number <- fmarket %>% 
  group_by(State)%>%
  summarise(total = n() )
fmarket_number
fmarket_number_names <- c("State","NumberOfMarket")
colnames(fmarket_number) <- fmarket_number_names
fmarket_number <- arrange(fmarket_number, desc(NumberOfMarket))
fmarket_number

ggplot(data=fmarket_number, aes(x=reorder(State,-NumberOfMarket), y=NumberOfMarket)) +
  geom_bar(stat="identity") + ggtitle("Number of Farmers Market by State") + xlab("States") + 
  ylab("Number of Farmers Market") +coord_flip() + My_Theme

ggsave("numbermarkets.png", device="png" ,width = 10, height = 10,  dpi = 450)



# 2) California NewYork States filtered, heatmap, payment type
top_states <- head(fmarket_number)
top_states

top_states_payment <- fmarket %>%
  group_by(State) %>%
  filter(State %in% top_states$State)  %>%
  summarise(tot_credit = sum(as.numeric(Credit)),tot_WIC = sum(as.numeric(WIC)), 
            tot_WICcrash =  sum(as.numeric(WICcash)), tot_SFMNP =  sum(as.numeric(SFMNP)),
            tot_SNAP =  sum(as.numeric(SNAP)))
  
top_states_payment

# Heat map for payment type based on top states

fmarket_top_payment_names <- c("State","Credit","WIC" , "WICCash" , "SFMNP" , "SNAP")
colnames(top_states_payment) <- fmarket_top_payment_names

heat_top_states_payment<-select(top_states_payment, State, Credit, WIC , WICCash , SFMNP , SNAP)
heat_top_states_payment

# Melt function is used to reshape the data
heat_top_states_payment<- melt(heat_top_states_payment, id = "State")
colnames(heat_top_states_payment)<- c("State","PaymentType" ,"Count")
heat_top_states_payment

p1<- ggplot(heat_top_states_payment, aes(PaymentType, State, fill= Count)) + 
  geom_tile()   +  ggtitle("Heat Map of Payment Type by Top States") + xlab("Payment Type") + 
  ylab("States") + My_Theme +coord_flip() 

ggsave("topstatespayment.png", device="png" ,width = 10, height = 10,  dpi = 450)



# 3) last 5 states and payment type

last_states <- tail(fmarket_number)
last_states

last_states_payment <- fmarket %>%
  group_by(State) %>%
  filter(State %in% last_states$State)  %>%
  summarise(tot_credit = sum(as.numeric(Credit)),tot_WIC = sum(as.numeric(WIC)), 
            tot_WICcrash =  sum(as.numeric(WICcash)), tot_SFMNP =  sum(as.numeric(SFMNP)),
            tot_SNAP =  sum(as.numeric(SNAP)))

last_states_payment

# Heat map for payment type based on last states

fmarket_last_payment_names <- c("State","Credit","WIC" , "WICCash" , "SFMNP" , "SNAP")
colnames(last_states_payment) <- fmarket_last_payment_names

heat_last_states_payment<-select(last_states_payment, State, Credit, WIC , WICCash , SFMNP , SNAP)
heat_last_states_payment

# Melt function is used to reshape the data
heat_last_states_payment<- melt(heat_last_states_payment, id = "State")
colnames(heat_last_states_payment)<- c("State","PaymentType" ,"Count")
heat_last_states_payment

p2 <-ggplot(heat_last_states_payment, aes(PaymentType, State, fill= Count)) + 
  geom_tile()   +  ggtitle("Heat Map of Payment Type by Last States") + xlab("Payment Type") + 
  ylab("States") + My_Theme +coord_flip()


ggsave("laststatespayment.png", device="png" ,width = 10, height = 10,  dpi = 450)

grid.arrange(p1, p2,ncol = 2)

ggsave("comparisonstatespayment.png", device="png" ,width = 10, height = 10,  dpi = 600)



# 4)  Social Media Accounts by Location (Bar Chart) 
fmarket_location <- fmarket %>% 
  group_by(fmarket$State)%>%
  summarise(tot_web = sum(as.numeric(Website)),tot_face = sum(as.numeric(Facebook)), 
            tot_twi =  sum(as.numeric(Twitter)), tot_you =  sum(as.numeric(Youtube)),
            tot_oth =  sum(as.numeric(OtherMedia)))

fmarket_location_names <- c("State","Website","Facebook" , "Twitter" , "Youtube" , "OtherMedia")
colnames(fmarket_location) <- fmarket_location_names

fmarket_location

bar<-select(fmarket_location, State,Website,Facebook , Twitter , Youtube , OtherMedia)

# Melt function is used to reshape the data
bar<- melt(bar, id = "State")
colnames(bar)<- c("State","SocialMedia" ,"Count")
bar

ggplot(data=bar, aes(x=SocialMedia, y=Count, fill = State)) +
  geom_bar(stat="identity") + ggtitle("Distribution of Social Media Accounts by States") + xlab("Social Media Type") + 
  ylab("Number of Account") + My_Theme 


ggsave("bar.png", device="png" ,width = 12, height = 12,  dpi = 1000)



# 5) Pie Chart for goods
product_names <- c("Organic"  ,     "Bakedgoods" ,   "Cheese"     ,   "Crafts"   ,     "Flowers"  ,     "Eggs" ,
                "Seafood"      , "Herbs"  ,       "Vegetables"  ,  "Honey"    ,     "Jams"     ,     "Maple",     
                "Meat"   ,       "Nursery" ,      "Nuts"     ,     "Plants"   ,     "Poultry"   ,
                "Prepared" ,     "Soap"    ,      "Trees"   ,      "Wine" ,          "Coffee"    ,    "Beans"  ,
                "Fruits"  ,      "Grains"  ,      "Juices"     ,   "Mushrooms" ,    "PetFood" ,
                "Tofu"    ,      "WildHarvested")

fmarket[ , 29:58 ][ fmarket[ , 29:58 ] == "Y" ] <- 1
fmarket[ , 29:58 ][ fmarket[ , 29:58 ] == "N" ] <- 0
fmarket[ , 29:58 ][ fmarket[ , 29:58 ] == "-" ] <- 0

#assumption : instead of - I assume 0

fmarket_product <- fmarket[ , 29:58]
for (i in 1:30)
{
  sum_product[,i] <- data.frame(sum(as.numeric(fmarket_product[ , i])))
}

colnames(sum_product) <- product_names
sum_product

pie(as.matrix(sum_product), labels = product_names, main="Pie Chart of Products") 




# 6) Top foods by cities

#top_products <- c("Bakedgoods", "Vegetables", "Fruits" ) 

product_city <- fmarket %>% 
  group_by(city)%>%
  summarise(bake = sum(as.numeric(Bakedgoods)),veg = sum(as.numeric(Vegetables)), 
            fruit =  sum(as.numeric(Fruits)))

product_city_names <- c("City","Bakedgoods", "Vegetables", "Fruits")
colnames(product_city) <- product_city_names

product_city<- melt(product_city, id = "City")
colnames(product_city)<- c("City","Product" ,"Count")
product_city

# Not a good plot therefore I did not include to the report 
ggplot(product_city, aes(Product, City, fill= Count)) + 
  geom_tile()   +  ggtitle("Heat Map of Top Product Type by Cities") + xlab("Product Type") + 
  ylab("City")


ggplot(product_city, aes(x=factor(Product), y=(Count))) + 
  geom_boxplot() + ggtitle("Most Preferred Products by Cities ") + xlab("Products") + 
  ylab("Counts") + My_Theme  

ggsave("box_city.png", device="png" ,width = 5, height = 5,  dpi = 300)




