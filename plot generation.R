set.seed(50)

library(tidyverse)
library(plotly)
library(ggthemes)
library(RColorBrewer)
library(leaflet)
library(formattable)
library(kableExtra)

#create the dataframe

dt<-data.frame(a = rnorm(100), b = rnorm(100, mean = 10, sd = 3),
               c = rnorm(100), d = rnorm(100, mean = 2, sd = 0.13), e = rnorm(100, mean = 539, sd = 10))

dt<-round(dt, 2)

names(dt)<-c("Density", "Air","Temperature","Pollution", "Co²")

dt$planet <- rbinom(100, 1, 0.5)

dt$planet<-as.factor(dt$planet)

dt$Initiatives<-c(1:100)


#generating plot

plot1<-ggplot(dt, aes(x =Density, y = Temperature, size = Air, alpha = 0.5)) + geom_point(col = "blue") + 
  geom_smooth(method = lm) + theme_pander()+ 
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust =0.5)) +
  labs(title = "Temperature by density population",subtitle = "Plot of the month",caption = "Data source: RG09") + 
  theme(legend.position = "bottom")
plot1

plot2<-ggplot(dt, aes(x = Co², y=planet, fill=planet)) + geom_boxplot(notch = T)  +
  labs(title = "Outliers by planet",subtitle = "Plot of the universe",caption = "Data source: RG09") + 
  geom_vline(xintercept =mean(dt$Co²), linetype="dashed", color = "red", size=1) + theme_classic()
plot2

plot3<-ggplot(dt, aes(Co²))+geom_histogram(color="black", fill="orange") +
  labs(title = "Distribution of the Co²",subtitle = "Plot for air",caption = "Data source: RG09") + theme_clean()
plot3

plot4 <-ggplot(dt, aes(x = Pollution)) + geom_density(color = "black", fill = "orange", alpha = 0.5) +
  labs(title = "Pol. density²",subtitle = "Breath",caption = "Data source: RG09") + theme_minimal()
plot4

plot5<-ggplot(dt, aes(x=planet, fill = planet))+geom_bar() + theme_bw()+
  labs(title = "Count of planet",caption = "Data source: RG09") + scale_fill_brewer("Dark2") + coord_flip()
ggplotly(plot5)

col<- scale_fill_brewer("blues")

plot6<-ggplot(dt, aes(y =Air, x = Initiatives, size = Air, col = planet, alpha = 0.8)) + 
  geom_point() +geom_smooth(method="glm", fill = "steelblue") + labs(title = "Galaxy", caption = "StarWars") +
  facet_grid(cols = vars(planet)) + scale_color_brewer(palette = "Dark2") + theme_bw()+ theme(legend.position="top")
plot6
ggplotly(plot6)

plot7<-ggplot(dt, aes(x=Initiatives, y = Air)) + geom_line(color = "red") + theme_clean() +  labs(title = "Tree of Life",caption = "Data source: RG09") + ylab("Air of life")
plot7

#generating simple map

icon<-makeIcon("640px-KPMG_logo.svg.png", iconWidth = 95, iconHeight = 38, shadowUrl = "https://home.kpmg/be/en/home.html" )
map1<-leaflet(dt)%>%addTiles() %>%addMiniMap()%>%addMarkers(lng=4.485433, lat=50.895773, popup="KPMG Belgium, I work there", icon = icon)%>% addProviderTiles(providers$nlmaps.pastel1)%>% 
  addMiniMap(tiles = providers$Esri.WorldStreetMap,toggleDisplay = TRUE)
map1

#generating a pretty table

sign_formatter <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))))
improvement_formatter <- formatter("span", style = x ~ style(font.weight = "bold", 
                                  color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))), 
                                   x ~ icontext(ifelse(x>0, "tag", "arrow-down"), x))

#icons (https://glyphicons.com/sets/basic/)

bold_color_formatter<-formatter("span", style = x ~ style(font.weight = "bold", color = "lightblue"))


pretty.dt<-formattable(dt,list(Pollution =color_tile("white", "green"), Temperature = color_bar("orange"), 
                    Density = sign_formatter, Air = improvement_formatter, planet = bold_color_formatter, Initiatives = percent))
pretty.dt.x<-as.datatable(pretty.dt)
pretty.dt


