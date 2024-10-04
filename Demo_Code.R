library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

World.Energy.Consumption <- 
  read.csv("J:/TY_Sem5/R Programming/R Mini Project/energy-data.csv")

# Research question
# We want to divide the countries of the world into 2 groups: 
# those in which the consumption of electricity from renewable sources 
# (biofuel, hydro, nuclear, solar, wind, other) exceeds 50% of the 
# total consumed and those in which it is less than 50%

df <- World.Energy.Consumption[,c('country', 
                                  'year',
                                  'biofuel_electricity', 
                                  'hydro_electricity',
                                  'nuclear_electricity',
                                  'solar_electricity',
                                  'wind_electricity', 
                                  'other_renewable_electricity',
                                  'coal_electricity',
                                  'gas_electricity',
                                  'oil_electricity')]

df$total <- rowSums(df[,3:11])
df <- df[-which(is.na(df$total)),]

df$biofuel_perc<-round(100*df$biofuel_electricity/df$total,2)
df$hydro_perc<-round(100*df$hydro_electricity/df$total,2)
df$nuclear_perc<-round(100*df$nuclear_electricity/df$total,2)
df$solar_perc<-round(100*df$solar_electricity/df$total,2)
df$wind_perc<-round(100*df$wind_electricity/df$total,2)
df$other_renewable_perc<-round(100*df$other_renewable_electricity/df$total,2)
df$coal_perc<-round(100*df$coal_electricity/df$total,2)
df$gas_perc<-round(100*df$gas_electricity/df$total,2)
df$oil_perc<-round(100*df$oil_electricity/df$total,2)

df$renewable<- rowSums(df[,13:18])
df$not_renewable<- rowSums(df[,19:21])

df1 <- df[,c(1:2,13:21)]
df1 <- melt(df1,id=c("country","year"))

df2 <- df[,c(1:2,22:23)]
df2 <- melt(df2,id=c("country","year"))

# Countries that consume more than 50% of non-renewable energy 
# (oil, gas, coal) are:

unique(df$country[df$renewable<50])

options(warn=-1)

df3<- df %>%
  filter(year==2020) %>%
  arrange(desc(total))

for (i in 1:nrow(df3)) {
  if (df3[i,22]<df3[i,23]) {
    c <-df3[i,1]
    
    g1<-df1 %>%
      filter(country==c) %>%
      ggplot(aes(year,value, colour=variable))+
      geom_line(size=1)+
      ggtitle(paste("Time series of electricity consumption in",c)) 
    
    g2<-df2 %>%
      filter(country==c,year==2020) %>%
      ggplot(aes(x = "", y = value, fill = variable)) +
      geom_col(color = "black") +
      geom_text(aes(label = value),
                position = position_stack(vjust = 0.5),size=2) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#EC754A","#BE2A3E"))+
      theme_void()+
      ggtitle("Percentage of renewable and non-renewable electricity", subtitle=paste("consumption in 2020 in",c)) 
    
    g3<-df1 %>%
      filter(country==c,year==2020) %>%
      mutate(variable= reorder(variable,value)) %>%
      ggplot(aes(variable,value, fill=variable))+
      geom_bar(stat = "identity")+
      geom_text(aes(label=value), hjust=0, size=2)+
      coord_flip()+
      guides(fill=FALSE)+
      ggtitle(paste("Percentage of energy sources consumed in 2020 in",c)) 
    
    grid.arrange(g2,g3,g1, nrow=3, ncol=1)   
    
  }
}

# Countries that consume more than 50% of renewable energy 
# (biofuel, hydro, nuclear, solar, wind, other) are:

unique(df$country[df$renewable>50])

df3<- df %>%
  filter(year==2020) %>%
  arrange(desc(total))

for (i in 1:nrow(df3)) {
  if (df3[i,22]>df3[i,23]) {
    c <-df3[i,1]
    
    g1<-df1 %>%
      filter(country==c) %>%
      ggplot(aes(year,value, colour=variable))+
      geom_line(size=1)+
      ggtitle(paste("Time series of electricity consumption in",c)) 
    
    g2<-df2 %>%
      filter(country==c,year==2020) %>%
      ggplot(aes(x = "", y = value, fill = variable)) +
      geom_col(color = "black") +
      geom_text(aes(label = value),
                position = position_stack(vjust = 0.5),size=2) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#3C8D53","#EACF65"))+
      theme_void()+
      ggtitle("Percentage of renewable and non-renewable electricity", subtitle=paste("consumption in 2020 in",c)) 
    
    g3<-df1 %>%
      filter(country==c,year==2020) %>%
      mutate(variable= reorder(variable,value)) %>%
      ggplot(aes(variable,value, fill=variable))+
      geom_bar(stat = "identity")+
      geom_text(aes(label=value), hjust=0, size=2)+
      coord_flip()+
      guides(fill=FALSE)+
      ggtitle(paste("Percentage of energy sources consumed in 2020 in",c)) 
    
    grid.arrange(g2,g3,g1, nrow=3, ncol=1)   
    
  }
}