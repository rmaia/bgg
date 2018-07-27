require(plyr)
require(rvest)
require(viridis)

gamedat <- vector('list', 1000)

# scraping boardgamegeek.com for basic game data and game ID, which is then
# used in the website's API

for(i in 1:1000){

  url <- paste0('https://boardgamegeek.com/browse/boardgame/page/', i)

  scrap <- read_html(url)
  
  links <- scrap %>% html_nodes('.collection_thumbnail') %>% html_children() %>% 
    html_attr('href')
  bggid <- links %>% strsplit('\\/') %>% lapply('[', 3) %>% unlist()
  
  allidurl <- paste0('https://www.boardgamegeek.com/xmlapi/boardgame/',
    paste(bggid, collapse=','),'?stats=1')

  gamedat[[i]] <- read_xml(allidurl)
  
  Sys.sleep(1)
  
}

# save top 5000 games to demonstrate code
saveRDS(gamedat, 'bggtop5000.RData')

# get basic info

allgames <- data.frame(year = lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    xml_nodes(y, 'yearpublished')) %>% lapply(function(z) xml_text(z)) %>% 
    unlist() ) %>% do.call(what=c, args=.)
)

allgames$name <- lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    (y %>% html_nodes('name'))[!is.na(y %>% html_nodes('name') %>% 
      html_attr('primary'))] %>% html_text())) %>% do.call(what=c, args=.) %>% unlist

allgames$rank <- lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    (y %>% html_nodes('rank'))[y %>% html_nodes('rank') %>% 
                                 html_attr('name') == 'boardgame'] %>% 
                                 html_attr('value'))) %>% 
  do.call(what=c, args=.) %>% unlist

allgames$geekrating <- lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    y %>% xml_node('bayesaverage') %>% html_text())) %>% 
  do.call(what=c, args=.) %>% unlist

allgames$bggid <- lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    y   %>% xml_attr('objectid'))) %>% 
  do.call(what=c, args=.) %>% unlist

# get category keywords

category <- lapply(gamedat, function(x)
  x %>% xml_children() %>% lapply(function(y) 
    xml_nodes(y, 'boardgamecategory')) %>% lapply(function(z) xml_text(z)) 
)
  
# combine into single dataframe
category <- do.call(c, category)

names(category) <- allgames$bggid

category <- ldply(category, cbind)

category <- as.data.frame.matrix(table(category))

#category <- category[allgames$bggid, ]

ccgames <- allgames[match(rownames(category), as.character(allgames$bggid))[
  !is.na(match(rownames(category), as.character(allgames$bggid)))
], ]

category <- category[ccgames$bggid, ]

all.equal(rownames(category), as.character(ccgames$bggid)) #TRUE

# select only categories that have 20+ games
toanalyze <- category[, colSums(category) > 20] 

letsrun <- cbind(toanalyze, geek=ccgames[,'geekrating'])
letsrun$geek <- as.numeric(as.character(letsrun$geek))

# run linear regression
mod1 <- lm( geek ~ ., data=letsrun, na.action='na.fail')

summary(mod1)

# Select top 100 games and bottom games
top100 <- category[which(as.numeric(as.character(ccgames$rank)) < 101),]
botgam <- category[which(as.numeric(as.character(ccgames$rank)) > 100), ]

topprop <- sort(colSums(top100), dec=T)/sum(sort(colSums(top100), dec=T))
botprop <- colSums(botgam)/sum(colSums(botgam))

botprop <- sort(botprop, dec=TRUE)
topprop <- topprop[names(botprop)]

# make barplot figure

pdf('categories.pdf', width=10, height=7)
plot <- barplot(rbind(botprop, topprop)[, 1:20], beside=T, xaxt='n', col=c('grey', 'tomato'), 
                ylab='Proportion of games with keyword', xlab='Game category', border=NA, ylim=c(0,0.11), 
                cex.lab=0.8, cex.axis=0.7)
box()
text(x=(plot[2, ] + plot[1, ])/2, y=-0.002, labels = names(botprop)[1:20], srt=60, adj=1, xpd=TRUE, cex=0.5)
legend('topright', legend=c('Top 100 games', 'Remaining games'), fill=c('tomato', 'grey'), bty='n', cex=0.8)
dev.off()


# trends in average ratings over time since 1980
allgames$year <- as.numeric(as.character(allgames$year))
allgames$geekrating <- as.numeric(as.character(allgames$geekrating))
allgames2 <- allgames[allgames$year > 1980, ]
allgames2 <- allgames2[allgames2$year < 2019, ]

palette <- setNames(viridis(39, alpha=0.4), 1980:2018)
allgames2$col <- palette[as.character(allgames2$year)] 

meangeek <- with(allgames2, tapply(geekrating, year, median, na.rm=T))

#write.csv(allg, 'allg.csv')
#write.csv(gattr_category, 'categories.csv')

# figure with annual trends

pdf('geekratingbyyear.pdf', width=10, height=7)
with(allgames2, plot(geekrating~year, pch=20, col=allgames2$col, 
    ylim=c(5,9), ylab='Average user rating', xlab='Year of release', , xaxt='n'))
axis(1, at=seq(1980, 2018, by=4))
points(meangeek~names(meangeek), pch=20, cex=1.5)
points(meangeek~names(meangeek), type='l')
dev.off()


