setwd('/nfs/vvijay-data/Covidgtrends')
library(dplyr)
library(ggplot2)
#library(reshape2)
#library(tibble)
#library(tidyr)
#library(readr)
#library(curl)
library(tidytext)
library(igraph)
library(ggraph)
#library(ggpubr)
#library(stringr)
#library(networktools)
library(visNetwork)

#load RData file
load("~/covidgtrends/covidgtrendsdata630.RData")


####create interactive visNetwork plot for combined network
word_countstotalrev<- taxacoronarelatedcombine %>% 
  select(value) %>% 
  unnest_tokens(word, value) %>% 
  count(word, sort = TRUE) %>% 
  filter(!word %in% stop_words$word)%>%
  #filter(word!="started")
  filter(word %in% wordscombine)%>%
  #mutate(origin=if_else(word %in% searchwords,"search","other"))
mutate(origin=case_when(word %in% searchwords~"search",
word %in% words1620~"other",
word %in% words20~"novel pandemic"))

bigram1620<-taxacoronarelatedbigram1620%>%
  rename(basen=n)
bigram20<-taxacoronarelatedbigram20%>%
  rename(pandn=n)

taxacoronacombinejoin<-taxacoronacombine%>%left_join(bigram1620,by=c("word1","word2"))%>%
  left_join(bigram20,by=c("word1","word2"))
taxacoronacombinejoin[is.na(taxacoronacombinejoin)]<-0
taxacoronacombinejoin<-taxacoronacombinejoin%>%dplyr::mutate(deltan=pandn-basen)

relatedcombinerev<-taxacoronacombinejoin%>% 
  dplyr::mutate(weight=n)%>%
  graph_from_data_frame(vertices=word_countstotalrev)

datacombinenet <- toVisNetworkData(relatedcombinerev)
# nodes<-datacombinenet$nodes
# edges<-datacombinenet$edges
# #nodes$color.background <-c("blue", "yellow", "red")[nodes$origin]
# #nodes$color.border <- "black"
# nodes$group<-nodes$origin
# #nodes$label<-nodes$name
# #nodes$size<-nodes$n
# edges$width <- edges$weight
# edges$label<-edges$weight
# edges$group <- edges$change
# #vis.nodes$color.highlight.background <- "orange"
# #vis.nodes$color.highlight.border <- "darkred"
# set.seed(123)
# visNetwork(nodes, edges, height = "700px", width = "100%") %>%
#   visIgraphLayout(layout = "layout_with_fr") %>%
#   # red triangle with shadow for group "search"
#   visGroups(groupname = "search", 
#             color = list(background = "lightgreen", 
#                         border = "black",
#                         highlight = "red"),
#             shape = "circle", 
#             shadow = list(enabled = TRUE), size=20) %>% 
#   # darkblue circle for group "other"
#   visGroups(groupname = "other", 
#             color=list(background = "lightblue", 
#                  border = "black",
#                  highlight = "red"),
#             shape = "circle", size=10)%>%
#   # blue circle for group "novel pandemic"
#   visGroups(groupname = "novel pandemic", 
#             color=list(background = "lightyellow", 
#                  border = "black",
#                  highlight = "red"),
#             shape = "circle", size=10)%>%
#   #visLegend()%>%
#   #visNodes(size = n) %>%
#   visOptions(selectedBy = "origin",
#     highlightNearest = list(enabled = T, hover = T), 
#              nodesIdSelection = T)
###networktoshiny
require(shiny)
require(visNetwork)
server <- function(input, output) {
  nodes<-datacombinenet$nodes
  edges<-datacombinenet$edges
  #nodes$color.background <-c("blue", "yellow", "red")[nodes$origin]
  #nodes$color.border <- "black"
  nodes$group<-nodes$origin
  #nodes$label<-nodes$name
  #nodes$size<-nodes$n
  edges$width <- edges$weight
  edges$label<-edges$deltan
  edges$color <- "blue"
  edges$color[edges$deltan > 0] <- "red"
  ledges <- data.frame(color = c("blue","red"), 
                      label = c("decrease","increase")) 
  output$network <- renderVisNetwork({
    visNetwork(nodes, edges, height = "100%", width = "100%",main = "Google Health-Environment Search Network (Combined Baseline and Pandemic)") %>%
      visIgraphLayout(layout = "layout_with_fr",randomSeed=123) %>%
      # red triangle with shadow for group "search"
      visGroups(groupname = "search", 
                color = list(background = "lightgreen", 
                             border = "black",
                             highlight = "yellow"),
                shape = "circle", 
                shadow = list(enabled = TRUE), size=20) %>% 
      # darkblue circle for group "other"
      visGroups(groupname = "other", 
                color=list(background = "lightblue", 
                           border = "black",
                           highlight = "yellow"),
                shape = "circle", size=10)%>%
      # blue circle for group "novel pandemic"
      visGroups(groupname = "novel pandemic", 
                color=list(background = "lightyellow", 
                           border = "black",
                           highlight = "yellow"),
                shape = "circle", size=10)%>%
      visEdges(shadow = FALSE,smooth =TRUE,arrows ="to")%>%
      visLegend(useGroups = F, addEdges = ledges,position="")%>%
      #visNodes(size = n) %>%
      visOptions(selectedBy = "origin",
                 highlightNearest = list(enabled = T, degree = 0, hover = T,algorithm="all"), 
                 nodesIdSelection = T)
  })
}

ui <- fluidPage(
  visNetworkOutput("network")
)

shinyApp(ui = ui, server = server)

