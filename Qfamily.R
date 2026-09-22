rm(list=ls())
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(hrbrthemes)
library(ggplot2)
library(scales)
library(DescTools)
library(fs)

setwd(paste(fs::path_home(),'/OneDrive/manuscripts/qanoncasualties/family/data',sep=""))

library(data.table)
library(stringr)
results_bert = fread("./results_hdbscan_qanoncasualties_0.005_mcp.tsv",header=TRUE,sep="\t",quote=FALSE)
results_all_sentences = fread("./all_sentences_qanoncasualties.tsv",header=TRUE,sep="\t",quote=FALSE)
unique_sentences = fread("./uniques_qanoncasualties.tsv",header=FALSE,sep="\t",quote=FALSE)
hdbscan_results = fread("./hdbscan_qanoncasualties_0.005_mcp.tsv",header=TRUE,sep="\t",quote=FALSE)


submissions = fread(paste(fs::path_home(),"/languageDatasets/reddit/subreddits23/QAnonCasualties_submissions.tsv",sep=""),header=FALSE,quote="")
comments = fread(paste(fs::path_home(),"/languageDatasets/reddit/subreddits23/QAnonCasualties_comments.tsv",sep=""),header=FALSE,quote="")
all_posts = rbind(submissions,comments)
colnames(all_posts) = c("Author","Date","ID","Ups","Downs","TEXT")
all_posts$Date = as.Date(as.POSIXct(all_posts$Date))


#index is zero based for python, populate all sentences with topics and dates
results_all_sentences$Date = all_posts[results_all_sentences$index+1]$Date
results_all_sentences$Ups = all_posts[results_all_sentences$index+1]$Ups
results_all_sentences$Topic = hdbscan_results[match(results_all_sentences$sentence,unique_sentences$V1)]


proportions_table = data.table(hdbscan=c(),upvotes=c(),clusterSize=c())
english_classified = filter(results_all_sentences,Topic!="-1")

### draw up proportions table and create thumbnail graphs
library(lubridate)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(data.table)
customPalette = c("#ff4700")
my_theme <- function(){
  list(
    theme_ipsum_rc(),
    scale_color_manual(values = customPalette),
    scale_linetype_manual(values = c(1))
  )
}

topics = unique(english_classified$Topic)
for(cluster in topics) {
  cluster_sentences = filter(results_all_sentences,Topic==cluster)
  
  theGraphData <- cluster_sentences %>% mutate(Date = floor_date(Date, unit = "month")) %>% group_by(Date) %>% summarize(Posts = n())
  ragg::agg_png(paste("./thumbnails/timeseries_",cluster,".png",sep=""), width = 176, height = 99, units = "px", res = 60, scaling=1)
  tmp = ggplot(data=theGraphData,aes(x=Date,y=Posts)) +
    stat_smooth(method="loess",size=1.4,se=FALSE,span=0.2)+
    my_theme()+theme(legend.position = "none")+
    scale_y_log10(labels = label_number(accuracy = 1),limits = c(1,500),breaks=c(1, 10, 100))+
    scale_x_date(breaks = "2 year", date_labels =  "%Y",name="Day")+
    theme(legend.position = "none",axis.text.x=element_text(size=12),axis.title.y = element_blank(),axis.text.y = element_text(size=12),axis.title.x= element_blank(),plot.margin = margin(t = 0.5,r = 0.2,b = 1,l = 0.2,unit = "mm"))
  print(tmp)
  dev.off()

  topic_percentOfUpvotes = sum(cluster_sentences$Ups,na.rm=TRUE)/sum(results_all_sentences$Ups,na.rm=TRUE)
  proportions_table = rbind(proportions_table,data.table(hdbscan=c(cluster),upvotes=c(topic_percentOfUpvotes),clusterSize=c(length(cluster_sentences$Ups)/length(results_all_sentences$Ups))))
  
  library(RColorBrewer)
  set1mod = brewer.pal(n = 6, name = "Set1")[4]
  my_bartheme <- function(){
    list(
      theme_ipsum_rc(),
      scale_fill_manual(values =  set1mod),
      scale_color_manual(values = set1mod)
    )
  }

  bardata = data.table(type=c("upvotes"),percent=round(c(topic_percentOfUpvotes)*100,1))
  tmp_barplot = ggplot(data=bardata,aes(x=type,y=percent,fill=type,color=type)) +
    geom_col(show.legend = FALSE)+
    my_bartheme()+coord_flip()+geom_text(aes(label = paste(percent,"%",sep="")),size = 9,hjust = -0.1, show.legend=FALSE)+
    scale_y_continuous(labels = label_number(accuracy = 1),limits = c(0,20),breaks=c(5, 15))+
    theme(legend.position = "none",axis.title.x = element_blank(),axis.text.x=element_text(size=12),axis.title.y = element_blank(),axis.text.y = element_blank(),plot.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1,unit = "mm"))
  ragg::agg_png(paste("./thumbnails/barplot_",cluster,".png",sep=""), width = 176, height = 99, units = "px", res = 50, scaling=1)
  print(tmp_barplot)
  dev.off()

  
}


# do legends
library(grid)
library(gridExtra)
library(cowplot)
bardata = data.table(type=c("upvotes"),percent=round(c(topic_percentOfUpvotes)*100,2))
tmp_barplot = ggplot(data=bardata,aes(x=type,y=percent,fill=type,color=type)) +
  geom_col(show.legend = FALSE)+
  my_bartheme()+coord_flip()+geom_text(aes(label = paste(percent,"%",sep="")),size = 9,hjust = -0.1, show.legend=FALSE)+ylim(0,13)+
  theme(legend.position = "none",axis.title.x = element_blank(),axis.text.x=element_blank(),axis.title.y = element_blank(),axis.text.y = element_blank(),plot.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1,unit = "mm"))
tmp_barplotLEGEND = tmp_barplot + theme(legend.position = "right", legend.title = element_blank(), text = element_text(family='serif',size=19))+geom_col(show.legend = TRUE)+scale_fill_manual(breaks=c("upvotes"),values =  set1mod)+scale_color_manual(breaks=c("upvotes"),values =  set1mod)
legend <- cowplot::get_plot_component(tmp_barplotLEGEND, 'guide-box-right', return_all = TRUE)
tmplabel = grid.newpage()
ragg::agg_png(paste("./thumbnails/barplot_legend.png"), width = 176, height = 30, units = "px", res = 60, scaling=1)
grid.draw(legend)
dev.off()

library(grid)
library(gridExtra) 
tmp_timeseriesLEGEND = tmp + theme(legend.position = "top",legend.title=element_blank(),legend.text = element_text(family='serif',size=20),legend.key.spacing.x = unit(7, "mm"),legend.margin=margin(0, 18, 0, 0))
legend <- cowplot::get_plot_component(tmp_timeseriesLEGEND, 'guide-box-top', return_all = TRUE)
tmplabel = grid.newpage()
ragg::agg_png(paste("./thumbnails/timeseries_legend.png"), width = 176, height = 30, units = "px", res = 50, scaling=1)
grid.draw(legend)
dev.off()





# export results and images to html
library(xtable)
tmp_results_bert = results_bert
tmp_results_bert = tmp_results_bert[,!c("Name")]
tmp_results_bert$Timeline = paste("<img src=\"",getwd(),"/thumbnails/timeseries_",results_bert$Topic,".png\" /img>",sep="")
tmp_results_bert$Socials = paste("<img src=\"",getwd(),"/thumbnails/barplot_",results_bert$Topic,".png\" /img>",sep="")
# remove noise category
tmp_results_bert = tmp_results_bert[-(which(tmp_results_bert$Topic==-1)),]
tmp_html = gsub(pattern="/img&gt;", replacement="/>",gsub(pattern="&lt;img",replacement="<img",print(xtable(tmp_results_bert), include.rownames=FALSE,type="html")))
# no need for timeseries legend
replacementString = paste("<th> Timeline <br><img src='",getwd(),"/thumbnails/timeseries_legend.png' /img></th>",sep="")
tmp_html = gsub(pattern="<th> Timeline </th>", replacement=replacementString,tmp_html)
replacementString2 = paste("<th> Socials <br><img src='",getwd(),"/thumbnails/barplot_legend.png' /img></th>",sep="")
tmp_html = gsub(pattern="<th> Socials </th>", replacement=replacementString2,tmp_html)
cat(tmp_html,file="./htmlTables/htmlTable.html")





### let's extract full posts with highlighted rep sentences
non_noise_reps = filter(results_bert,Topic!='-1')
extracted_posts = data.table(Topic=c(),Date=c(),Post=c())
for(entry in 1:length(non_noise_reps$Topic)) {
  # remove first and last python array bracket and single quote brackets
  repsentences = str_sub(non_noise_reps[entry,]$Representative_Docs,3,-3)
  repsentences = unlist(str_split(repsentences,"', '"))
  for(sentence in repsentences) {
    thePost = all_posts[unique_sentences[which(unique_sentences$V1==sentence)]$V2+1]
    thePostText = str_replace_all(thePost$TEXT,pattern=fixed(sentence),replacement=paste("<font color='red'>",sentence,"</font>",sep=""))
    thePostText = str_replace_all(thePostText,pattern="\342\200\231",replacement="'")
    thePostText = str_replace_all(thePostText,pattern="\342\235\244",replacement="'")
    extracted_posts = rbind(extracted_posts,data.table(Topic=c(non_noise_reps[entry,]$Topic),Date=c(thePost$Date),Post=c(thePostText)))
  }
}
extracted_posts=extracted_posts[order(extracted_posts$Topic)]
extracted_posts$Date = paste(as.Date(extracted_posts$Date),"",sep="")

library(xtable)
tmp_html = str_replace_all(pattern=fixed("&lt;/font&gt;"), replacement="</font>",str_replace_all(pattern=fixed("&lt;font color='red'&gt;"),replacement="<font color='red'>",print(xtable(extracted_posts), include.rownames=FALSE,type="html")))
cat(tmp_html,file="./htmlTables/htmlTable_posts.html")


table1 = proportions_table
table1$upvotes = paste(round(table1$upvotes*100,2),"%",sep="")
table1$clusterSize = paste(round(table1$clusterSize*100,2),"%",sep="")




