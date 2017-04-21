library(ggvis)
library(dplyr)
library(lubridate)
library(plotly)
library(ggplot2)

##### Data Processing
## Combine datasets with different bedrooms
z_bed <- read.csv("0bed.csv", stringsAsFactors = FALSE)
one_bed<- read.csv("1bed.csv", stringsAsFactors = FALSE)
two_bed<- read.csv("2beds.csv", stringsAsFactors = FALSE)
three_bed<- read.csv("3beds.csv", stringsAsFactors = FALSE)
four_bed<- read.csv("4beds.csv", stringsAsFactors = FALSE)
five_bed<-read.csv("5beds.csv", stringsAsFactors = FALSE)
six_bed<- read.csv("6beds.csv", stringsAsFactors = FALSE)
s_e_bed<-read.csv("7&8beds.csv", stringsAsFactors = FALSE)

house<-do.call("rbind", list(z_bed, one_bed, two_bed, three_bed, four_bed, five_bed,
                                six_bed, s_e_bed))

## Get new variable month from Sale.Date
dates  = mdy_hms(house$Sale.Date)

days = data.frame(data = dates,
                  year = as.numeric(format(dates, format = "%Y")),
                  month = as.numeric(format(dates, format = "%m")),
                  day = as.numeric(format(dates, format = "%d"))
)
head(days)
head(dates)

new_house = cbind(house, days)
head(new_house)

## Get rid of year 2015 obs
new_house<-new_house %>% filter(year==2016)

colnames(new_house)[19]<-"Date"

## Calculate house age, new variable age
new_house$age<-2016-new_house$Year.Built

## Creat new variable class, which make ages into 13 groups
new_house$class<-"no information"

for(i in 1:length(new_house$age)){
  if(any(new_house$age[i]>100&new_house$age[i]<200)){
    new_house$class[i] <- "over 100 years"
  } 
  if(any(new_house$age[i]>90&new_house$age[i]<=100)){
    new_house$class[i] <- "91~100 years"
  }
  if(any(new_house$age[i]>80&new_house$age[i]<=90)){
    new_house$class[i] <- "81~90 years"
  }
  if(any(new_house$age[i]>70&new_house$age[i]<=80)){
    new_house$class[i] <- "71~80 years"
  }
  if(any(new_house$age[i]>60&new_house$age[i]<=70)){
    new_house$class[i] <- "61~70 years"
  }
  if(any(new_house$age[i]>50&new_house$age[i]<=60)){
    new_house$class[i] <- "51~60 years"
  }
  if(any(new_house$age[i]>40&new_house$age[i]<=50)){
    new_house$class[i] <- "41~50 years"
  }
  if(any(new_house$age[i]>30&new_house$age[i]<=40)){
    new_house$class[i] <- "31~40 years"
  }
  if(any(new_house$age[i]>20&new_house$age[i]<=30)){
    new_house$class[i] <- "21~30 years"
  }
  if(any(new_house$age[i]>10&new_house$age[i]<=20)){
    new_house$class[i] <- "11~20 years"
  }
  if(any(new_house$age[i]>0&new_house$age[i]<=10)){
    new_house$class[i] <- "1~10 years"
  }
  if(any(new_house$age[i]==0)){
    new_house$class[i] <- "new"
  }
  else {
    new_house$class[i] <- new_house$class[i]
  }
}

## Calculate Month Share (out of sales in the whole year 2016)
d<-new_house %>% compute_count(~factor(month))
d<-d %>% mutate (month=c(1:12))
d$month<-factor(d$month)
d<-d%>%mutate(percent=count_/952*100)
text <- c('5.57% share', '4.83% share', '9.03% share',
          "7.04% share", "11.1% share", "16.6% share",
          "10.4% share", "10.4% share", "6.72% share",
          "6.93% share", "6.41% share", "4.94% share")
d <- data.frame(d, text)
d
## Can be written into the dataset new_house or not  
#new_house<-left_join(new_house,d)
#new_house<-new_house %>% mutate(Market.Share=count_/952)
#new_house<-new_house %>% mutate(Market.Share.Percentage=Market.Share*100)


## Get new variable Sale.Mean.Price, it calculated the mean sale price
#for each month
new_house<-new_house %>% group_by(month) %>%  
  mutate(Sale.Mean.Price=mean(Sale.Price))

## Save my dataset
#write.csv(new_house, file = "FinalProject/NEW_house.csv", row.names = FALSE)





##### Plots

#### Tab 1 in shiny: Housing Sales vs Time (2 parts: sales by month, price by month)
#### plot title: House Sale by Month, 2016 Ames Iowa
#### interaction: can let user choose between bar or pie to display

### Sales by month
## Bar Chart
# ggvis
new_house %>% 
  compute_count(~factor(month)) %>%
  ggvis(x = ~x_, y = ~count_ ) %>%
  layer_bars(fill :="yellow") %>%
  layer_text(text := ~count_, prop("x", ~x_, scale = "xcenter"), y = ~count_ + .5, 
             fontSize := 18, align := "center") %>%
  scale_nominal("x", name = "xcenter", padding = .9, points = TRUE) %>%
  add_axis("x", title = "Month") %>% 
  add_axis("y", title = "Count of Sold House") 

# plotly
d %>% plot_ly(x = ~month, y = ~count_, type = 'bar', text = text,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
  layout(title = "2016 Ames Iowa House Sale by Month",
         xaxis = list(title = ""),
         yaxis = list(title = ""))


## Pie Chart
plot_ly(d, labels = ~month, values = ~count_, type = 'pie') %>%
  layout(title = '2016 Ames Iowa House Sale by Month',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

### Price by month
## Box-plot
# ggvis
new_house %>% ggvis(~month,~Sale.Price)   %>% layer_boxplots()
# plotly
new_house %>% plot_ly(x=~month, y = ~Sale.Price, type = "box") %>%
  layout(
    xaxis = list(range = c(0,13)),
    yaxis = list(range = c(0, 900000)))


## Add on: mean price to box-plot
# ggvis
new_house %>% ggvis(~month, y = ~Sale.Price) %>% 
  layer_boxplots() %>%
  layer_points(~month, ~Sale.Mean.Price, fill := "blue") %>%
  add_axis("x", title = "Month") %>%
  add_axis("y", title = "Sale Price")

# plotly: failed to add on to box-plot
new_house %>% plot_ly(x = ~month, y = ~Sale.Mean.Price,
                      # Hover text:
                      text = ~paste("Mean Price: ", Sale.Mean.Price, '$<br>Month:', month)) %>%
  add_trace(type="scatter") %>%
  layout(
    xaxis = list(range = c(0,13)),
    yaxis = list(range = c(0, 900000)))



#### Tab 2: Housing Sales vs House Attributes
#### add mine to Helena's part
#### interaction: 1) can let user choose only show points for one class or few classes
####              2) can let user choose add the smooth line or not
####              3) user can choose between smooth or linear model

### Price by year of built
## scatter plot with smooth line
# ggvis: the lines go crazy...
new_house %>% ggvis(~Year.Built, ~Sale.Price, fill = ~factor(class))  %>% 
  layer_points() %>% 
  add_axis("x", title = "Year of Built") %>% 
  add_axis("y", title = "Sale Price") %>% 
  scale_numeric("x", domain = c(1880, 2016), nice = FALSE, clamp = TRUE) %>%
  scale_numeric("y", domain = c(0, 900000), nice = FALSE, clamp = TRUE)%>%
  add_legend("fill", title = "Age of House")%>%
  layer_smooths()
 
# plotly
new_house %>% plot_ly(x = ~Year.Built, y = ~Sale.Price,
  # Hover text:
  text = ~paste("Price: ", Sale.Price, '$<br>Year:', Year.Built),
  color = ~class
) %>%
  add_trace(type="scatter")%>%
  layout(
    xaxis = list(range = c(1880, 2020)),
    yaxis = list(range = c(0, 900000))) %>% 
  add_lines(x=~Year.Built, y=~fitted(loess(Sale.Price~Year.Built)), showlegend=F, line=list(color="purple"))


#### Tab 3: Housing Sales vs Location 
#### (I actually think this should be the 2nd tab)


#### Tab 4: Housing Sales vs Assessed Value
#### plot title: Does Assessed Value tells the truth?
#### interaction: 1) user can choose just show one/few Sale.Condition
####              2) user can choose add on the line or not
####              3) user can choose between smooth or linear model

## Scatter plot with a smooth line
# ggvis
new_house %>% ggvis(~Assessed.Value,~Sale.Price, fill=~factor(Sale.Condition))  %>% 
  layer_points() %>% 
  add_axis("x", title = "Assessed Value") %>% 
  add_axis("y", title = "Sale Price") %>% 
  add_legend("fill", title = "House Style") %>%
  layer_smooths()

# plotly
fit<-lm(Sale.Price~Assessed.Value, data=new_house)
new_house %>% plot_ly(x = ~Assessed.Value, y = ~Sale.Price,
                      # Hover text:
                      text = ~paste("Sale Price: ", Sale.Price, 
                                    '$<br>Assessed Value:', Assessed.Value, 
                                    '$<br>Age:', class,
                                    '<br>Year of Built:', Year.Built)
                      
) %>%
  add_trace(type="scatter",color = ~Sale.Condition) %>% 
  add_lines(x=~Assessed.Value, y=fitted(fit), showlegend=F)



