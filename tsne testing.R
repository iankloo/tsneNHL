library(tsne)
library(networkD3)
library(rCharts)
library(scales)
library(rvest)

#####bring in realnhl data
nhl <- read.csv('nhl.csv')

####bring in fake nhl data
#generate URLs
urls <- paste("https://www.huthq.com/16/players/?page=",rep(1:162), sep='')

#scrape function
scraper <- function(url) {
  table <- url %>%
    read_html() %>%
    html_nodes(css='table') %>%
    html_table()
  table <- as.data.frame(table)
}
#run scraper over all pages
nhl16Table <- do.call("rbind",lapply(urls, scraper))

#match with nhl players
#reformat names
nhl16Table$Player <- gsub(' ', '.', nhl16Table$Player)
for(i in 1:nrow(nhl16Table)){
  nhl$style[nhl16Table$Player[i] == nhl$Name] <- nhl16Table$Style[i]
  print(i)
}

####process data
nhlClean <- subset(nhl, select=c('Name', 'Gm', 'pos','Team','style', 'Salary', 'G60', 'A60', 'PenD', 'CF.', 'PDO','PSh.','ZSO.Rel', 'TOI.Gm'))

#probably need to remove these players
nhlClean <- nhlClean[which(is.na(nhlClean$PSh.) == FALSE),]
nhlClean <- nhlClean[which(is.na(nhlClean$Salary) == FALSE),]

#more than 30 games played 
nhlClean <- nhlClean[nhlClean$Gm > 30,]

#run t-sne procedure
tsneNHL <- tsne(nhlClean[7:14])

#clean up results
nhlDF <- as.data.frame(tsneNHL)
colnames(nhlDF) <- c('x', 'y')

#bring in name and position
nhlDF$name <- as.character(nhlClean$Name)
nhlDF$pos <- as.character(nhlClean$pos)

#flags for specific players
nhlDF$flag <- 0
nhlDF$flag[nhlDF$name == "Sidney.Crosby"] <- 1
nhlDF$flag[nhlDF$name == "Alex.Ovechkin"] <- 2
nhlDF$flag[nhlDF$name == "Kris.Letang"] <- 3

#rescale salary (to correspond to size)
nhlDF$salary <- nhlClean$Salary
nhlDF$salaryRescale <- rescale(nhlClean$Salary, to=c(1,20))

#reduce number of positions to offense and defense
nhlDF$posBin <- 'Offense'
nhlDF$posBin[nhlDF$pos %in% c('D','LD','DR')] <- 'Defense'

#bring in style
nhlDF$style <- nhlClean$style

#top 10 players
top10 <- c('Alex.Ovechkin', 'Sidney.Crosby','Steven.Stamkos', 'Jamie.Benn', 'John.Tavares',
           'Tyler.Seguin', 'Evgeni.Malkin', 'Claude.Giroux', 'Patrick.Kane', 'Corey.Perry')
nhlDF$top10 <- 0
nhlDF$top10[nhlDF$name %in% top10] <- 1
sum(nhlDF$top10)

nhlDF$goal <- nhlClean$G60
nhlDF$goalRescale <- rescale(nhlDF$goal, c(1,100))

####plot
plot <- nPlot(x ~ y, group='styles', data=nhlDF, type = 'scatterChart')
plot$chart(tooltipContent = "#! function(key, x, y, e){
  return '<b>Name</b>: ' + e.point.name 
} !#")
plot$chart(size = '#! function(d){return d.salary} !#')
plot







