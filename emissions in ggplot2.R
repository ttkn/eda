setwd("c:/users/pearl/documents/github/eda")
library(dplyr)
library(ggplot2)

df <- readRDS("summarySCC_PM25.rds")
df2 <- readRDS("source_classification_code.rds")

yearly_emissions <- tapply(df$Emissions, df$year, sum)

png(filename = "plot1.png", width = 480, height = 480)
barplot(yearly_emissions, main = "Yearly pm2.5 emmissions across United States", ylab = "Emissions in tons")
dev.off()

##### plot2

dfsplit <- group_by(df, fips, year)
#groups all data by county code, fips, then by year
pm2.5 <- summarise(dfsplit, pm2.5 = sum(Emissions))
#after data is grouped, take the sum of the emissions
baltimore <- arrange(filter(pm2.5, fips == 24510), year)

png(filename = "plot2.png", width = 480, height = 480)
barplot(baltimore$pm2.5, names.arg = baltimore$year, main = "Annual pm2.5 emmissions in Baltimore", ylab = "Emissions in tons")
dev.off()

###plot3

baltimore <- df %>%
  group_by(type, year, fips) %>%
  summarise (pm2.5 = sum(Emissions)) %>%
  filter(fips == 24510) %>%
  select(type, year, pm2.5)

png(filename = "plot3.png", width = 480, height = 480)
 ggplot(data=baltimore, aes(year, pm2.5)) + geom_line(aes(color=type)) + labs(title="Annual PM2.5 Emissions in Baltimore", y="emissions in tons") 
 dev.off()
 
#######plot4
 
x <- filter(df2, grepl("coal", df2$EI.Sector, ignore.case=TRUE)) #uses the second dataframe to filter sources of coal

coal <- data.frame(df %>% 
    filter(SCC %in% x$SCC) %>% #SCC is shared between the two dataframes; use one to find matching records in the other
    group_by(year) %>% 
    summarize(pm2.5 = sum(Emissions)))

png(filename = "plot4.png", width = 480, height = 480)
  ggplot(data=qwer, aes(year, pm2.5)) +
    geom_bar(stat="identity") +
    labs(title="U.S. emissions from coal, 1999-2008", y="emissions in tons") +
    scale_x_continuous(breaks=coal$year, labels=coal$year)
  dev.off()

######plot5
x <- filter(df2, grepl("vehicle", df2$EI.Sector, ignore.case = TRUE)) #now filtering for car emissions

cars <- data.frame(df %>%
    filter(SCC %in% x$SCC & fips == 24510) %>%
    group_by(year) %>%
    summarize(pm2.5 = sum(Emissions)))

png(filename = "plot5.png", width = 480, height = 480)
  ggplot(data=cars, aes(year, pm2.5)) +
    geom_bar(stat="identity") +
    labs(title="Vehicle emissions in Baltimore, 1999-2008", y="emissions in tons") +
    scale_x_continuous(breaks=coal$year, labels=coal$year)
dev.off()

###plot6
x <- filter(df2, grepl("vehicle", df2$EI.Sector, ignore.case = TRUE))

cars <- data.frame(df %>%
                     filter(SCC %in% x$SCC, fips %in% c("06037", 24510)) %>%
                     group_by(fips, year) %>%
                     summarize(pm2.5 = sum(Emissions)))

png(filename = "plot6.png", width = 480, height = 480)
ggplot(data=cars, aes(year, pm2.5)) +
  geom_bar(stat="identity", aes(colors=fips)) +
  labs(title="Vehicle emissions in Baltimore and Los Angeles, 1999-2008", y="emissions in tons") +
  scale_x_continuous(breaks=cars$year, labels=cars$year) +
  facet_wrap(~fips)
dev.off()
