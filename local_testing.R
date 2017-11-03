library(tidyverse)
library(corrplot)
library("PerformanceAnalytics")
library("plotly")
library(tidyverse)

data <- read.csv('./9Acc_RGB day.csv')[-1]
head(data)

#### gather data by IV and DV
dim(data %>% gather (variable, value, -(ACCESSION:DAY)))
dim(data)
head(data)

data2<- data %>% filter (TREATMENT=='Control')


#### summary stats of DV

# option 1
data %>% gather(variable, value, -(ACCESSION:DAY)) %>% group_by(ACCESSION,TREATMENT,DAY,variable) %>% summarize(mean_val = mean(value))

head(data)

#option 2
data %>% gather(variable, value, -(ACCESSION:DAY)) %>% group_by(TREATMENT,variable) %>% summarize(mean_val = mean(value))

#### melt variables to make new labels###
data %>% mutate(id = paste(ACCESSION, TREATMENT, DAY, sep = "_"))
head(data)

##### correlation result object
res <- cor(data[,4:10])
??corrplot


#########visulization methods##########

## corrplot
corrplot(res, type = "upper", order = "hclust", tl.col  = "black", tl.srt = 45)

### scatter plots
chart.Correlation(res, histogram=TRUE, pch=19)

##### heat maps
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)

###### testing for subsetting

my_data <-subset(data,select = c(COMPACTNESS,TREATMENT,ACCESSION,DAY))
fit <- lm(my_data[, 4] ~ my_data[, 1])
my_data[,5] <- coefficients(fit)[2]
my_data[,6] <- coefficients(fit)[1]
my_data[,7] <- summary(fit)$r.squared


melted_icecream <- melt(my_data,id=c('TREATMENT', 'ACCESSION'))
  sum_my_data <- summaryBy(value ~., data=melted_icecream) 
  return(sum_my_data)
})


plot_ly(data,x=data$PERIMETER, y=data$AREA, color=as.factor(data$ACCESSTION))

p<- ggplot(data, aes(PERIMETER, AREA)) + geom_point(aes(colour = ACCESSION, shape=TREATMENT))
ggplotly(p)


melted_icecream <- melt(my_data(), id=c(input$SelectIV,input$SelectGeno,input$SelectID, input$SelectTime))
sum_my_data <- summaryBy(value ~., data=melted_icecream) 

melted_icecream <- melt(data) 
sum_my_data <- summaryBy(value ~., data=melted_icecream)

#################  cor.mtest ##########################
cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}
res2 <- cor.mtest(my_data,0.99)
res2
################ rcorr #########################

library("Hmisc")
res3 <- rcorr(as.matrix(mtcars[,1:7]))
res3
STEM_HS <- read.csv("~/Desktop/STEM_HS.csv", stringsAsFactors=F,header = T)

STEM_HS2 <- t(STEM_HS[1:1000,]) %>% as_data_frame()

write(STEM_HS2,file='./test.csv')


mtcars
################## getting r and p value ##################

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

result <- flattenCorrMatrix(res3$r, res3$P)
result.type



read.csv(file=)