library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

#NYTIMES_KEY <- "5pIuDm9htRevqqAr2t1NA6a0r1QEoc4n" #old one I gave to the other group
NYTIMES_KEY <- "GZpXflME3ASOJ5aLWyqXNZUdtqZNWAxP"
term <- "Iran"
begin_date <- "20190330"
end_date <- "20190714"

baseurl <- paste0("http://api.nytimes.com/svc/search/v2/articlesearch.json?q=",term,
                  "&begin_date=",begin_date,"&end_date=",end_date,
                  "&facet_filter=true&api-key=",NYTIMES_KEY, sep="")

initialQuery <- fromJSON(baseurl)
maxPages <- round((initialQuery$response$meta$hits[1] / 10)-1) 

pages_2019 <- vector("list",length=maxPages)

for(i in 0:maxPages){
  nytSearch <- fromJSON(paste0(baseurl, "&page=", i), flatten = TRUE) %>% data.frame() 
  pages_2019[[i+1]] <- nytSearch 
  Sys.sleep(5) #I was getting errors more often when I waited only 1 second between calls. 5 seconds seems to work better.
}

iran_2019_articles <- rbind_pages(pages_2019)

save(iran_2019_articles,file="iran_2019_articles.Rdata")

colnames(iran_2019_articles) <- str_replace(colnames(iran_2019_articles),
                                                pattern='response\\.',replace='')
colnames(iran_2019_articles) <- str_replace(colnames(iran_2019_articles),
                                                pattern='docs\\.',replace='')
colnames(iran_2019_articles)

columns_to_remove <- c("status","copyright","print_page","source","_id","uri","headline.print_headline","headline.name","headline.seo","headline.sub",grep('meta',colnames(iran_2019_articles),value=TRUE),"abstract","multimedia")
columns_to_keep <- setdiff(colnames(iran_2019_articles),columns_to_remove)

iran_2019_abstracts <- as.vector(iran_2019_articles$abstract)

iran_2019_articles <- iran_2019_articles %>% select(columns_to_keep)

for(i in 1:ncol(iran_2019_articles))
{
  if(class(iran_2019_articles[,i]) == "list"){
    print(colnames(iran_2019_articles)[i])
    head(iran_2019_articles[,i],n=3)
  }
}
columns_to_keep
iran_2019_articles <- iran_2019_articles %>% select(setdiff(colnames(iran_2019_articles),"byline.person"))

num_rows_keywords <- unlist(lapply(iran_2019_articles$keywords,function(x)nrow(x)))

for(article_num in which(num_rows_keywords > 0)){
  this_article_keywords <- data.frame(Article.index = article_num,
                                      iran_2019_articles$keywords[[article_num]],
                                      stringsAsFactors=FALSE)
  if(exists("iran_2019_keywords_collapsed") == FALSE){iran_2019_keywords_collapsed <- this_article_keywords;next}
  iran_2019_keywords_collapsed <- rbind(iran_2019_keywords_collapsed,this_article_keywords)
}

head(iran_2019_keywords_collapsed);tail(iran_2019_keywords_collapsed)

iran_2019_articles <- data.frame(Targeted.topic = "2019 US Iran Tensions",
                                     Article.index = 1:nrow(iran_2019_articles),
                                     iran_2019_articles,
                                     stringsAsFactors=FALSE)

iran_2019_keywords_collapsed <- data.frame(Targeted.topic = "2019 US Iran Tensions",
                                               iran_2019_keywords_collapsed,
                                               stringsAsFactors=FALSE) 

articles_matching_keywords_info <- iran_2019_keywords_collapsed %>% filter(value == "Iran") %>% select(c("Targeted.topic","Article.index"))
articles_matching_keywords_info <- articles_matching_keywords_info[!duplicated(articles_matching_keywords_info),]

iran_2019_articles <- merge(iran_2019_articles,articles_matching_keywords_info,by=c("Targeted.topic","Article.index"))
table(iran_2019_articles$Targeted.topic)

iran_2019_articles$pub_date <- as.Date(substr(iran_2019_articles$pub_date,1,10))
iran_2019_articles <- iran_2019_articles %>% arrange(pub_date)

tmp = iran_2019_articles %>% 
  filter(Targeted.topic == "2019 US Iran Tensions") %>%
  select(c("pub_date","headline.main"))

summary(tmp)
head(tmp)
tmp
write_csv(tmp, "iran_2019_headlines.csv")

