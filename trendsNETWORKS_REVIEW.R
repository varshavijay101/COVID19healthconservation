library(dplyr)
library(ggplot2)
library(devtools)
dev_mode(on=T)
devtools::install_github("PMassicotte/gtrendsR")
library(gtrendsR)
library(reshape2)
library(changepoint)
library(tibble)
library(tidyr)
library(readr)
library(tidycovid19)
library(scales)
library(curl)
library(tseries)
library(tidytext)
library(igraph)
library(ggraph)
library(ggpubr)
library(stringr)
library(networktools)

load("covidgtrendsdata630.RData")

####Network Comparison Jan 2016-Dec 2019 and Jan 01 2020-May 1 2020
#all calls to Google Server are commented out and data are provided in
# the .RData file


# ##2016-2019
#top and rising queries are pulled for each baseline year to ensure
#that the baseline query network is as complete as possible
# 
# corona1617<-gtrends(keyword="coronavirus+corona virus+covid",
#                     geo = "US",
#                     time="2016-01-01 2016-12-31")
# corona1718<-gtrends(keyword="coronavirus+corona virus+covid",
#                     geo = "US",
#                     time="2017-01-01 2017-12-31")
# corona1819<-gtrends(keyword="coronavirus+corona virus+covid",
#                     geo = "US",
#                     time="2018-01-01 2018-12-31")
# corona1920<-gtrends(keyword="coronavirus+corona virus+covid",
#                     geo = "US",
#                     time="2019-01-01 2019-12-31")
# 
# pangolin1617<-gtrends(keyword="pangolin+pangolins",
#                       geo = "US",
#                       time="2016-01-01 2016-12-31")
# pangolin1718<-gtrends(keyword="pangolin+pangolins",
#                       geo = "US",
#                       time="2017-01-01 2017-12-31")
# pangolin1819<-gtrends(keyword="pangolin+pangolins",
#                       geo = "US",
#                       time="2018-01-01 2018-12-31")
# pangolin1920<-gtrends(keyword="pangolin+pangolins",
#                       geo = "US",
#                       time="2019-01-01 2019-12-31")
# 
# bat1617<-gtrends(keyword="bat+bats",
#                  geo = "US",
#                  category="66",
#                  time="2016-01-01 2016-12-31")
# bat1718<-gtrends(keyword="bat+bats",
#                  geo = "US",
#                  category="66",
#                  time="2017-01-01 2017-12-31")
# bat1819<-gtrends(keyword="bat+bats",
#                  geo = "US",
#                  category="66",
#                  time="2018-01-01 2018-12-31")
# bat1920<-gtrends(keyword="bat+bats",
#                  geo = "US",
#                  category="66",
#                  time="2019-01-01 2019-12-31")
# 
# wildtrade1617<-gtrends(keyword=c("wildlife trade+bushmeat"),
#                        geo = "US",
#                        time="2016-01-01 2016-12-31")
# wildtrade1718<-gtrends(keyword=c("wildlife trade+bushmeat"),
#                        geo = "US",
#                        time="2017-01-01 2017-12-31")
# wildtrade1819<-gtrends(keyword=c("wildlife trade+bushmeat"),
#                        geo = "US",
#                        time="2018-01-01 2018-12-31")
# wildtrade1920<-gtrends(keyword=c("wildlife trade+bushmeat"),
#                        geo = "US",
#                        time="2019-01-01 2019-12-31")
# 
# conservation1617<-gtrends(keyword="conservation",
#                           geo = "US",
#                           category="66",
#                           time="2016-01-01 2016-12-31")
# conservation1718<-gtrends(keyword="conservation",
#                           geo = "US",
#                           category="66",
#                           time="2017-01-01 2017-12-31")
# conservation1819<-gtrends(keyword="conservation",
#                           geo = "US",
#                           category="66",
#                           time="2018-01-01 2018-12-31")
# conservation1920<-gtrends(keyword="conservation",
#                           geo = "US",
#                           category="66",
#                           time="2019-01-01 2019-12-31")
coronarelated1617 <- as_tibble(corona1617$related_queries)
coronarelated1718 <- as_tibble(corona1718$related_queries)
coronarelated1819 <- as_tibble(corona1819$related_queries)
coronarelated1920 <- as_tibble(corona1920$related_queries)

pangolinrelated1617 <- as_tibble(pangolin1617$related_queries)
pangolinrelated1718 <- as_tibble(pangolin1718$related_queries)
pangolinrelated1819 <- as_tibble(pangolin1819$related_queries)
pangolinrelated1920 <- as_tibble(pangolin1920$related_queries)

batrelated1617 <- as_tibble(bat1617$related_queries)
batrelated1718 <- as_tibble(bat1718$related_queries)
batrelated1819 <- as_tibble(bat1819$related_queries)
batrelated1920 <- as_tibble(bat1920$related_queries)

wildtraderelated1617 <- as_tibble(wildtrade1617$related_queries)
wildtraderelated1718 <- as_tibble(wildtrade1718$related_queries)
wildtraderelated1819 <- as_tibble(wildtrade1819$related_queries)
wildtraderelated1920 <- as_tibble(wildtrade1920$related_queries)

consrelated1617 <- as_tibble(conservation1617$related_queries)
consrelated1718 <- as_tibble(conservation1718$related_queries)
consrelated1819 <- as_tibble(conservation1819$related_queries)
consrelated1920 <- as_tibble(conservation1920$related_queries)

taxacoronarelated1620<-rbind(coronarelated1617,coronarelated1718,coronarelated1819,coronarelated1920,pangolinrelated1617,pangolinrelated1718,pangolinrelated1819,pangolinrelated1920,batrelated1617,batrelated1718,batrelated1819,batrelated1920,consrelated1617,consrelated1718,consrelated1819,consrelated1920,wildtraderelated1617,wildtraderelated1718,wildtraderelated1819,wildtraderelated1920)
write.csv(taxacoronarelated1620,'relatedquery1620.csv')

#Search keywords used in network analysis
searchwords<-c("coronavirus","bat","pangolin","wildlife","conservation","trade","bushmeat","covid")


taxacoronarelated1620resolve<-taxacoronarelated1620%>%#resolve word conjugations
  mutate(value=str_replace(value,"bats","bat"))%>%
  mutate(value=str_replace(value,"pangolins","pangolin"))%>%
  mutate(value=str_replace(value,"corona virus","coronavirus"))

taxacoronarelatedbigram1620<-taxacoronarelated1620resolve%>%
  #search queries are decomposed into bigrams (two word combinations)
  # and the freqency is counted
  unnest_tokens(bigram, value, token = 'ngrams', n = 2,n_min=1,collapse=FALSE) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  #bigrams with common words are removed
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  #numeric queries are removed
  filter(is.na(as.numeric(word1)),is.na(as.numeric(word2)))%>%
  count(word1, word2,sort = TRUE) %>% 
  #single word queries are removed
  filter(!is.na(word1),!is.na(word2))

words1620 <-c(taxacoronarelatedbigram1620$word1,taxacoronarelatedbigram1620$word2)
word_counts1620 <- taxacoronarelated1620resolve %>% 
  select(value) %>% 
  unnest_tokens(word, value) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% stop_words$word)%>%
  #filter(word!="started")
  filter(word %in% words1620)%>%
  mutate(origin=if_else(word %in% searchwords,"search","other"))%>%
  mutate(origin2=if_else(origin == "other", " ", word))


#2020 Pandemic
# corona20<-gtrends(keyword="coronavirus+corona virus+covid",
#                   geo = "US",
#                   time="2020-01-01 2020-05-01")
# pangolin20 <- gtrends(keyword="pangolin+pangolins",
#                       geo = "US",
#                       time="2020-01-01 2020-05-01")
# bat20<-gtrends(keyword="bat+bats",
#                geo = "US",
#                category="66",
#                time="2020-01-01 2020-05-01")
# cons20<-gtrends(keyword="conservation",
#                 geo = "US",
#                 category="66",
#                 time="2020-01-01 2020-05-01")
# wildlifetrade20<-gtrends(keyword=c("/m/02pxwgl"),
#                          geo = "US",
#                          time="2020-01-01 2020-05-01")
# bushmeat20<-gtrends(keyword="bushmeat",
#                     geo = "US",
#                     time="2020-01-01 2020-05-01")

coronarelated20 <- as_tibble(corona20$related_queries)
pangolinrelated20<-as_tibble(pangolin20$related_queries)
batrelated20<-as_tibble(bat20$related_queries)
consrelated20<-as_tibble(cons20$related_queries)
wildtraderelated20<-as_tibble(wildlifetrade20$related_queries)
bushmeatrelated20<-as_tibble(bushmeat20$related_queries)
taxacoronarelated20<-rbind(coronarelated20,pangolinrelated20,batrelated20,consrelated20,wildtraderelated20,bushmeatrelated20)
write.csv(taxacoronarelated20,'relatedquery20.csv')

taxacoronarelated20resolve<-taxacoronarelated20%>%####resolve word conjugations
  mutate(value=str_replace(value,"bats","bat"))%>%
  mutate(value=str_replace(value,"pangolins","pangolin"))%>%
  mutate(value=str_replace(value,"corona virus","coronavirus"))%>%
  mutate(value=str_replace(value,"corona pangolin","coronavirus pangolin"))

taxacoronarelatedbigram20<-taxacoronarelated20resolve%>% 
  unnest_tokens(bigram, value, token = 'ngrams', n = 2,n_min=1,collapse=FALSE) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  filter(is.na(as.numeric(word1)),is.na(as.numeric(word2)))%>%
  count(word1, word2, sort = TRUE) %>% 
  filter(!is.na(word1),!is.na(word2)) 

words20<-c(taxacoronarelatedbigram20$word1,taxacoronarelatedbigram20$word2)

word_counts20 <- taxacoronarelated20resolve %>% 
  select(value) %>% 
  unnest_tokens(word, value) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% stop_words$word)%>%
  #filter(word!="started")
  filter(word %in% words20)%>%
  mutate(origin=if_else(word %in% searchwords,"search","other"))%>%
  mutate(origin2=if_else(origin == "other", " ", word))

################network comparisons#####
###New Nodes Coronavirus
newnodes<-setdiff(words20,words1620)

###Combine networks showing new nodes in different color
taxacoronarelatedbaseline<-taxacoronarelatedbigram1620%>%mutate(time="baseline")
taxacoronarelatedpandemic<-taxacoronarelatedbigram20%>%mutate(time="pandemic")
taxacoronacombine<-rbind(taxacoronarelatedbaseline,taxacoronarelatedpandemic)%>%
  select(-time)
taxacoronarelatedcombine<-rbind(taxacoronarelated20,taxacoronarelated1620)

wordscombine<-c(taxacoronacombine$word1,taxacoronacombine$word2)

word_countstotal<- taxacoronarelatedcombine %>% 
  select(value) %>% 
  unnest_tokens(word, value) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% stop_words$word)%>%
  #filter(word!="started")
  filter(word %in% wordscombine)%>%
  mutate(origin=if_else(word %in% searchwords,"search","other"))
#mutate(origin=case_when(word %in% searchwords~"search",
#word %in% words1620~"baseline",
#word %in% words20~"pandemic"))

###Baseline 2016 to 2020 network
related1620<-taxacoronarelatedbigram1620%>% 
  dplyr::mutate(weight=n)%>%#edge weights = bigram frequency
  graph_from_data_frame(vertices=word_counts1620)#convert to igraph object vertex size= word frequency


###including Pandemic Network 2020 Jan-May1
relatedcombine<-taxacoronacombine%>% 
  #dplyr::mutate(weight=n)%>%
  graph_from_data_frame()

###Pandemic Network 2020 
related20<-taxacoronarelatedbigram20%>%
  dplyr::mutate(weight=n)%>%
  graph_from_data_frame(vertices=word_counts20)


###mean network distance
mean_distance(related1620, directed=T)
mean_distance(relatedcombine, directed=T)
mean_distance(related20,directed=T)

####community detection
cluster1620<-cluster_infomap(related1620)
print(cluster1620)
#membership(cluster1620)
communities(cluster1620)
membership(cluster1620)
plot(cluster1620,related1620)
layout1620 <-layout.fruchterman.reingold(related1620)
plot(cluster1620, related1620, layout=layout1620, vertex.size=5,  edge.arrow.size=.2)
#cluster1620L<-cluster_louvain(related1620)
#print(cluster1620L)
#communities(cluster1620L)
#membership(cluster1620L)
#plot(cluster1620L,related1620)
clustercombine<-cluster_infomap(relatedcombine)
print(clustercombine)
#membership(clustercombine)
communities(clustercombine)
cluster20<-cluster_infomap(related20)
print(cluster20)
communities(cluster20)
layout20 <-layout.fruchterman.reingold(related20)
plot(cluster20, related20, layout=layout20, vertex.size=5,  edge.arrow.size=.2)

##degree centrality
cent1620<-sort(degree(related1620,loops=F,mode="all",normalized=F))%>%
  as.data.frame()%>%rownames_to_column()
colnames(cent1620)<-c('names','degree')
cent1620<-cent1620%>%dplyr::mutate(normdegree1620=degree/max(degree))
#centcombine<-sort(degree(relatedcombine))
cent20<-sort(degree(related20,loops=F,mode="all",normalized=F))%>%
  as.data.frame()%>%rownames_to_column()
colnames(cent20)<-c('names','degree')
cent20<-cent20%>%dplyr::mutate(normdegree20=degree/max(degree))

combinenormdegree<-left_join(cent1620,cent20,by="names")%>%drop_na()%>%
  select(names,normdegree1620,normdegree20)%>%
  mutate(mymean = rowMeans(cbind(normdegree1620,normdegree20)),deltadeg=abs(normdegree20-normdegree1620)) %>% 
  arrange(mymean) %>% 
  mutate(names=factor(names, names))

cor.test(combinenormdegree$normdegree1620,combinenormdegree$normdegree20,method="spearman")

novelnormdegree<-cent20%>%filter(names %in% newnodes)%>%
  arrange(normdegree20) %>% 
  mutate(names=factor(names, names))

#strength
strength1620<-sort(strength(related1620,loops=F,mode="all"))%>%
  as.data.frame()%>%rownames_to_column()
colnames(strength1620)<-c('names','strength')
strength1620<-strength1620%>%dplyr::mutate(normstrength1620=strength/max(strength))
#strengthcombine<-sort(strength(relatedcombine,loops=F,mode="all",normalized=F))
strength20<-sort(strength(related20,loops=F,mode="all"))%>%
  as.data.frame()%>%rownames_to_column()
colnames(strength20)<-c('names','strength')
strength20<-strength20%>%dplyr::mutate(normstrength20=strength/max(strength))

combinenormstrength<-left_join(strength1620,strength20,by="names")%>%drop_na()%>%
  select(names,normstrength1620,normstrength20)%>%
  mutate(mymean = rowMeans(cbind(normstrength1620,normstrength20)),deltadeg=abs(normstrength20-normstrength1620)) %>% 
  arrange(mymean) %>% 
  mutate(names=factor(names, names))

novelnormstrength<-strength20%>%filter(names %in% newnodes)%>%
  arrange(normstrength20) %>% 
  mutate(names=factor(names, names))


#closeness
closeness1620<-sort(closeness(related1620,normalized=TRUE))
closenesscombine<-sort(closeness(relatedcombine,normalized=TRUE))
closeness20<-sort(closeness(related20,normalized=TRUE))

#betweenness
betweenness1620<-sort(betweenness(related1620))
betweennesscombine<-sort(betweenness(relatedcombine))
betweenness20<-sort(betweenness(related20))

###simple path analysis
length(all_simple_paths(related1620, from="coronavirus", to=c("conservation"), mode = c("all")))
length(all_simple_paths(related20, from="coronavirus", to=c("conservation"), mode = c("all")))
all_simple_paths(related20, from="coronavirus", to=c("conservation"), mode = c("all"))

###########
#use for loop to bootstrap networks to caclulate confidence bounds for a test statistic 
#set number of boostrap samples
B <- 10
#get a character vector the graph's vertices
verts <- V(related20)
#create a vector to index which vertices belong to which subgraphs
#for now, assinging half to subgraph 1 and half to subgraph 2 to check that code runs
subs <- rep(0, length((verts)))
subs[1:length(verts)/2] <- 1
test_statistic1 <- mat.or.vec(B, length(verts))
for(i in 1:B){
  related20_resamp <- rewire(related20, with=keeping_degseq(niter = ecount(related20)*10, loops = FALSE))
  #get strength between each bertex and the other subgraphs using bridge centrality
  test_statistic1[i, ] <- as.vector(bridge(related20_resamp, communities=subs)$'Bridge Strength')
}
test_statistic1 <- rbind(verts, test_statistic1)[-1,]

#bootstrapped test statistic that gets at strength of connection between two subgraphs
#create toy subgraphs as example
verts_sub1 <- c("coronavirus", "covid")
verts_sub1b <- c("coronavirus")
verts_sub2 <- c("pangolin", "bat")
verts_sub3 <- c("conservation", "wildlife", "trade", "bushmeat")
test_stat_ec_20 <- mat.or.vec(B, 6)
test_stat_ec_1620 <- mat.or.vec(B, 6)
test_stat_path_20 <- mat.or.vec(B, 6)
test_stat_path_1620 <- mat.or.vec(B, 6)
for(i in 1:B){
  test_stat_ec_20[i, 1] <- edge_connectivity(related20, source=verts_sub1b, target=verts_sub2)
  test_stat_ec_20[i, 2] <- edge_connectivity(related20, source=verts_sub1b, target=verts_sub3)
  test_stat_ec_20[i, 3] <- edge_connectivity(related20, source=verts_sub2, target=verts_sub3)
  test_stat_path_20[i, 1] <- length(all_simple_paths(related20, from=verts_sub1b, to=verts_sub2, mode = c("all")))
  test_stat_path_20[i, 2] <- length(all_simple_paths(related20, from=verts_sub1b, to=verts_sub3, mode = c("all")))
  test_stat_path_20[i, 3] <- length(all_simple_paths(related20, from=verts_sub2, to=verts_sub3, mode = c("all")))
  related20_resamp <- rewire(related20, with=keeping_degseq(niter = ecount(related20)*10, loops = FALSE))
  test_stat_ec_20[i, 4] <- edge_connectivity(related20_resamp, source=verts_sub1b, target=verts_sub2)
  test_stat_ec_20[i, 5] <- edge_connectivity(related20_resamp, source=verts_sub1b, target=verts_sub3)
  test_stat_ec_20[i, 6] <- edge_connectivity(related20_resamp, source=verts_sub2, target=verts_sub3)
  test_stat_path_20[i, 4] <- length(all_simple_paths(related20_resamp, from=verts_sub1b, to=verts_sub2, mode = c("all")))
  test_stat_path_20[i, 5] <- length(all_simple_paths(related20_resamp, from=verts_sub1b, to=verts_sub3, mode = c("all")))
  test_stat_path_20[i, 6] <- length(all_simple_paths(related20_resamp, from=verts_sub2, to=verts_sub3, mode = c("all")))
  
  test_stat_ec_1620[i, 1] <- edge_connectivity(related1620, source=verts_sub1b, target=verts_sub2)
  test_stat_ec_1620[i, 2] <- edge_connectivity(related1620, source=verts_sub1b, target=verts_sub3)
  test_stat_ec_1620[i, 3] <- edge_connectivity(related1620, source=verts_sub2, target=verts_sub3)
  test_stat_path_1620[i, 1] <- length(all_simple_paths(related1620, from=verts_sub1b, to=verts_sub2, mode = c("all")))
  test_stat_path_1620[i, 2] <- length(all_simple_paths(related1620, from=verts_sub1b, to=verts_sub3, mode = c("all")))
  test_stat_path_1620[i, 3] <- length(all_simple_paths(related1620, from=verts_sub2, to=verts_sub3, mode = c("all")))
  related1620_resamp <- rewire(related1620, with=keeping_degseq(niter = ecount(related1620)*10, loops = FALSE))
  test_stat_ec_1620[i, 4] <- edge_connectivity(related1620_resamp, source=verts_sub1b, target=verts_sub2)
  test_stat_ec_1620[i, 5] <- edge_connectivity(related1620_resamp, source=verts_sub1b, target=verts_sub3)
  test_stat_ec_1620[i, 6] <- edge_connectivity(related1620_resamp, source=verts_sub2, target=verts_sub3)
  test_stat_path_1620[i, 4] <- length(all_simple_paths(related1620_resamp, from=verts_sub1b, to=verts_sub2, mode = c("all")))
  test_stat_path_1620[i, 5] <- length(all_simple_paths(related1620_resamp, from=verts_sub1b, to=verts_sub3, mode = c("all")))
  test_stat_path_1620[i, 6] <- length(all_simple_paths(related1620_resamp, from=verts_sub2, to=verts_sub3, mode = c("all")))
}  


################ Baseline and Pandemic network visualization (Fig. 3)##############
###create layout objects to preserve node locations between figures
set.seed(1)
layoutbase <- create_layout(graph=related1620, layout = "dh")%>%rename(xbase=x,ybase=y)
layoutpandemic<-create_layout(graph=related20, layout = "dh")%>%rename(xpando=x,ypando=y)
#layoutboth <- inner_join(layoutbase,layoutpandemic,by="name")%>%select(xbase,ybase,name)

layoutcombine<-create_layout(graph=relatedcombine,layout="fr")
layoutcombinebase<-layoutbase%>%left_join(layoutcombine,by="name")%>%
  select(-xbase,-ybase)
layoutcombinepando<-layoutpandemic%>%left_join(layoutcombine,by="name")%>%
  select(-xpando,-ypando)



xminrev<-min(layoutcombine$x)
xmaxrev<-max(layoutcombine$x)
yminrev<-min(layoutcombine$y)
ymaxrev<-max(layoutcombine$y)

baselinenetworkrev<-
  ggraph(related1620,layout = 'manual',x=layoutcombinebase$x,y=layoutcombinebase$y) +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +
  geom_node_point(aes(size = n,colour=origin))+#,color="blue") +
  geom_node_text(aes(filter = origin == "search", label = name,size=60),hjust=1.36,vjust=1.36,repel=TRUE,show.legend=FALSE)+
  # Force wide zoom for all plots
  expand_limits(x = c(xminrev, xmaxrev), y = c(yminrev, ymaxrev)) +
  theme_void() +
  theme(legend.position = 'none',panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  scale_color_manual(values=c("#006e99","#CC0000"))+
  labs(title = 'Baseline (2016 - 2019) related search queries')
baselinenetworkrev


pandemicnetworkrev<-
  ggraph(related20,x=layoutcombinepando$x,y=layoutcombinepando$y) +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE) +# need to adjust weights to match pandemic n this is for combined network-doesnt change between plots
  geom_node_point(aes(size = n,colour=origin),)+#,color="blue") +
  geom_node_text(aes(filter = origin == "search", label = name,size=22),hjust=1.33,vjust=1.33,repel=TRUE)+
  # Force wide zoom for all plots
  expand_limits(x = c(xminrev, xmaxrev), y = c(yminrev, ymaxrev)) +
  theme_void() +
  theme(legend.position=c(0.8,0.1),panel.border = element_rect(colour = "black", fill=NA, size=.5))+
  theme(legend.text = element_text(size=12))+
  scale_color_manual(values=c("#006e99","#CC0000"),labels=c("other","keyword"),name=NULL)+
  guides(size = FALSE)+
  labs(title = 'Pandemic (Jan. 2020 - Apr. 2020) related search queries')
pandemicnetworkrev


#Combined figure
ggarrange(baselinenetworkrev,pandemicnetworkrev,ncol=2)

###Node degree and network connectivity metrics (Fig. 4)####################3

quartz.options(width=4.14, height=6)
layout(matrix(c(1, 2), 2, 1, byrow = TRUE))
par(mar=c(1, 4, 3, 1))
plot(0, 0, col="white", xlim=c(0.95, 2.25), bty="n", xaxt="n", xlab=" ", ylab=" ", ylim=c(0, 3), cex.axis=0.8, yaxt="n")

deltanormdegree<-ggplot(combinenormdegree) +
  geom_segment(aes(x=names, xend=names, y=normdegree1620, yend=normdegree20), color="grey") +
  geom_point( aes(x=names, y=normdegree1620,shape="baseline"), color="black", alpha=1,size=1.6, bg="white") +
  geom_point( aes(x=names, y=normdegree20,shape="pandemic"), color="black", alpha=0.75,size=2) +
  coord_flip()+
  theme_bw() +
  scale_shape_manual(values=c(baseline=24, pandemic=16),name=NULL) +
  theme(
    #legend.position = c(0.9,0.1), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), text = element_text(size=9),
    axis.title=element_text(size=8)
  ) +
  #panel.border = element_blank()
  #panel.grid.major = element_line(colour="white") +
  #panel.background = element_rect(fill = "white") +
  xlab(" ") +
  ylab("Normalized node degree")

pandemicnormdegree<-ggplot(novelnormdegree) +
  geom_segment(aes(x=names, xend=names, y=0, yend=normdegree20), color="grey") +
  geom_point( aes(x=names, y=normdegree20), color="black", alpha=0.75,size=2, pch=16) +
  coord_flip()+
  theme_bw() +
  theme(
    legend.position = "none", panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), text = element_text(size=9),
    axis.title=element_text(size=8)
  ) +
  #panel.border = element_blank()
  #scale_x_discrete(limits = c(0, 10)) +
  xlab(" ") +
  ylab("Normalized node degree")

#quartz.options(width=3.54, height=3)
quartz.options(width=5.51, height=3.75)
layout(matrix(c(1), 1, 1, byrow = TRUE))
ggarrange(deltanormdegree,pandemicnormdegree,ncol=2,widths=c(1.5,1),common.legend=TRUE)

#quartz.options(width=3.54, height=4)
quartz.options(width=5.51, height=2)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE))

par(mar=c(1.5, 3.5, 1, 0.5))
plot(0, 0, col="white", xlim=c(0.95, 2.05), xaxt="n", xlab=" ", ylab=" ", ylim=c(0, 3), cex.axis=0.7, yaxt="n")

#points(c(2, 2, 2), c(test_stat_ec_20[1, 1], test_stat_ec_20[1, 2], test_stat_ec_20[1, 3]), pch=16)
#points(c(1, 1, 1), c(test_stat_ec_1620[1, 1], test_stat_ec_1620[1, 2], test_stat_ec_1620[1, 3]), pch=16)
lines(c(1, 2), c(test_stat_ec_1620[1, 1]+0.1, test_stat_ec_20[1, 1]+0.1), lwd=1.5, col=rgb(0, 0, 0, 0.3))
lines(c(1, 2), c(test_stat_ec_1620[1, 2], test_stat_ec_20[1, 2]), lwd=1.5, col=rgb(0, 0, 0, 0.3))
lines(c(1, 2), c(test_stat_ec_1620[1, 3], test_stat_ec_20[1, 3]), lwd=1.5, col=rgb(0, 0, 0, 0.3))
points(1, test_stat_ec_1620[1, 1]+0.1, pch=21, col=rgb(0, 0, 0, 1), bg=rgb(.75, .75, .75, 1))
points(1, test_stat_ec_1620[1, 2], pch=22, col=rgb(0, 0, 0, 1), bg=rgb(.75, .75, .75, 1))
points(1, test_stat_ec_1620[1, 3], pch=23, col=rgb(0, 0, 0, 1), cex=1, bg=rgb(.75, .75, .75, 1))
points(2, test_stat_ec_20[1, 1]+0.1, pch=21, col=rgb(0, 0, 0, 1), bg=rgb(.75, .75, .75, 1))
points(2, test_stat_ec_20[1, 2], pch=22, col=rgb(0, 0, 0, 1), bg=rgb(.75, .75, .75, 1))
points(2, test_stat_ec_20[1, 3], pch=23, col=rgb(0, 0, 0, 1), cex=1, bg=rgb(.75, .75, .75, 1))
mtext(side=2, line=2.1, "Edge connectivity", cex=0.7)
axis(side=2, at=c(0, 1, 2, 3), cex.axis=0.7)
mtext(line=0.25, side=1, "2016-2020                                            2020", cex=0.6)

par(mar=c(1.5, 3.5, 1, 0.5))
plot(0, 0, col="white", xlim=c(0.95, 2.05), xaxt="n", xlab=" ", ylab=" ", ylim=c(0, 250), cex.axis=0.6)
#points(c(2, 2, 2), c(test_stat_path_20[1, 1], test_stat_path_20[1, 2], test_stat_path_20[1, 3]), pch=16)
#points(c(1, 1, 1), c(test_stat_path_1620[1, 1], test_stat_path_1620[1, 2], test_stat_path_1620[1, 3]), pch=16)
lines(c(1, 2), c(test_stat_path_1620[1, 1], test_stat_path_20[1, 1]), lwd=1.5, col=rgb(0, 0, 0, 0.25))
lines(c(1, 2), c(test_stat_path_1620[1, 2]+8, test_stat_path_20[1, 2]), lwd=1.5, col=rgb(0, 0, 0, 0.25))
lines(c(1, 2), c(test_stat_path_1620[1, 3], test_stat_path_20[1, 3]), lwd=1.5, col=rgb(0, 0, 0, 0.25))
points(1, test_stat_path_1620[1, 1], pch=21, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(1, test_stat_path_1620[1, 2]+8, pch=22, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(1, test_stat_path_1620[1, 3], pch=23, col=rgb(0, 0, 0, 0.75), cex=1, bg=rgb(.75, .75, .75, 1))
points(2, test_stat_path_20[1, 1], pch=21, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(2, test_stat_path_20[1, 2], pch=22, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(2, test_stat_path_20[1, 3], pch=23, col=rgb(0, 0, 0, 0.75), cex=1, bg=rgb(.75, .75, .75, 1))
mtext(side=2, line=2.1, "Number of paths", cex=0.7)
text(1.05, -18, "2016-2020", cex=0.7)
text(2, -18, "2020", cex=0.7)
mtext(line=0.25, side=1, "2016-2020                                            2020", cex=0.6)

quartz.options(width=5.51, height=2)
par(mar=c(1, 1, 1, 1))
plot(1, 1, col="white", xlim=c(1, 10.25), ylim=c(0, 2), xaxt="n", yaxt="n", bty="n", xlab=" ", ylab=" ")
points(8, 1, pch=21, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(4.5, 1, pch=22, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
points(1, 1, pch=23, col=rgb(0, 0, 0, 0.75), bg=rgb(.75, .75, .75, 1))
text(9.4, 1, "coronavirus-host species", cex=0.65)
text(5.9, 1, "coronavirus-conservation", cex=0.65)
text(2.4, 1, "conservation-host species", cex=0.65)


###################### Node Strength Plot (Fig. S3)#############################
timecols<- c("normstrength1620"="blue","normstrength20"="red")
deltanormstrength<-ggplot(combinenormstrength) +
  geom_segment(aes(x=names, xend=names, y=normstrength1620, yend=normstrength20), color="grey") +
  geom_point( aes(x=names, y=normstrength1620, color="normstrength1620"), alpha=.8,size=3 ) +
  geom_point( aes(x=names, y=normstrength20, color="normstrength20"), alpha=.8,size=3 ) +
  coord_flip()+
  scale_color_manual(values=timecols, labels=c("baseline","pandemic"),name=NULL)+
  theme_bw() +
  #theme(
  #legend.position = "bottom"
  #) +
  xlab("") +
  ylab("")

pandemicnormstrength<-ggplot(novelnormstrength) +
  geom_segment(aes(x=names, xend=names, y=0, yend=normstrength20), color="grey") +
  geom_point( aes(x=names, y=normstrength20, color="normstrength20"), alpha=.8,size=3 ) +
  coord_flip()+
  theme_bw() +
  scale_color_manual(values=timecols, breaks=c("baseline","pandemic"),name=NULL)+
  theme(
    legend.position = "none"
  ) +
  xlab("") +
  ylab("")

ggarrange(deltanormstrength,pandemicnormstrength,ncol=2,widths=c(1.5,1),common.legend=TRUE)

