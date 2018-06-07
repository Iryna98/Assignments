
library(tidyverse)
library(gvlma)




set.seed(123)
df1 <- as.data.frame(matrix(runif(1*50, min = 1, max = 10), ncol = 1)) %>%
    mutate(epsilon = rnorm(50, mean = 0, sd = 2)) %>%
    mutate(response = 3 - 2* V1  + epsilon) %>%
    mutate(group = ifelse(V1 <= 5, 1,2))

lo <- ggplot(df1, aes(y = V1, x = "group")) + geom_boxplot()
lo

reg1 <- lm(response ~ V1, data=df1)

summary(reg1)


gvmodel <- gvlma(lm(response ~ V1, data = df1))


with(df1,plot(V1, response, xlab = "Predictor", ylab = "Response")) 
abline(reg1,col="red")
new.dat <- data.frame(df1[1])
sorted<-data.frame(V1=new.dat[order(new.dat$V1),])
predictions <- predict(reg1, newdata=sorted, interval="confidence")
lines(data.matrix(sorted), predictions[,2],col="red",lty=2)
lines(data.matrix(sorted), predictions[,3],col="red",lty=2)


library(thematicmaps)
library(tidyverse)
library(stringr)


map_municipal <- read.csv2("nld_municipal_map.csv")
head(map_municipal)


AddMapLayer(MapPlot(), map_municipal)

pc4_locations <- read.csv2("nld_pc4_locations.csv")

str(pc4_locations)


schools <- read.csv2("schools.csv")

schools1 <- read.csv2("schools.csv") %>% mutate(PC4 = substr(POSTCODE, 0, 4))
select(schools1, PC4, DENOMINATIE)

school_loc <- merge(pc4_locations,schools1,by="PC4")
filter(school_loc, DENOMINATIE %in% c("Rooms-Katholiek", "Protestants-Christelijk"))

 
