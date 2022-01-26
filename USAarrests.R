library(sp)
library(raster)
library(datasets)


USArrests

dotchart(USArrests$Murder, labels =row.names(USA), main = "Density of Murder Crime in US States" ,color="blue",xlab = "Case per 100,000 Population", cex =0.5)

n <- USArrests[order(USArrests$Murder, decreasing=TRUE),]
n

dotchart(USArrests$Murder, labels =row.names(n), main = "Density of Murder Crime in US States" ,color="blue",xlab = "Case per 100,000 Population", cex =0.5)

#Correlation

cor(USArrests$UrbanPop,USArrests$Murder, method =c("pearson"))
cor(USArrests$UrbanPop,USArrests$Assault, method =c("pearson"))
cor(USArrests$UrbanPop,USArrests$Rape, method =c("pearson"))
cor(USArrests$Assault,USArrests$Murder, method =c("pearson"))
cor(USArrests,USArrests, method=c("pearson"))

pairs(USArrests,cor=TRUE, use="pairwise.complete.obs")

library(tidyverse)
library(knitr) 



#Descriptive Statistics

usa_arrests <- USArrests
usa_arrests %>%
  ggplot2 ::ggplot(
    aes(
      x = Murder
    )
  ) +
  ggplot2 ::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 3
  ) +
  ggplot2 :: stat_function(
    fun = dnorm,
    args = list(
      mean = usa_arrests %>% pull(Murder) %>% mean(),
      sd = usa_arrests %>% pull(Murder) %>% sd()
    ),
    color = "blue",
  )
  