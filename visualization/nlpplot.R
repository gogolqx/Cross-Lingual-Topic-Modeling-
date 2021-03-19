library(data.table)
library(magrittr)
library(tidyr)
library(ggplot2)
library(plotly)
library(GGally)
library(quantreg)
library(wesanderson)
library(ggpubr)
library(cowplot)
library(dplyr)
require(ggpubr)
require(gridExtra)
require(ggpubr)
library(forcats)
library(gtable)
library(ggstance)
library(packHV)
library(xtable)
library(quantreg)
######################################
#import files
ny_news <- fread(sprintf("./all_sentiments_nytimes_news.csv"))
ny_comments <- fread(sprintf("./all_sentiments_nytimes_comments.csv"))
spiegel_news <- fread(sprintf("./all_sentiments_spiegel_news.csv"))
spiegel_comments <- fread(sprintf("./all_sentiments_spiegel_comments.csv"))
#check length of files
dim(spiegel_news)
dim(ny_news)
head(ny_news)
dim(ny_comments)
dim(spiegel_comments)
unlist(clusters)
unique(spiegel_news$cluster)
quantile(ny_news$scores,0.25)
#define cluster label names 

#clusters_old <- list("1"='Planting & gardening', "3"='Chemicals & Cancer', "4"= 'Meat & Animals', "5"='Taste & Food', "6"='Health & Diet', "7"='Retail', "8"='Scientific Research', "9"= 'GMO', "10"='Skandals', "11"='Agriculture', "12"= 'Polices', "13"='Economy')
clusters <- list("0"='Planting & gardening', "1"='Retail',"3"= 'GMO label & bio-products' , "5"='Taste & food', "6"='Chemicals & cancer',"7"='Genetic research',  "8"='Health & diet',  "10"='Governance & public policy', "11"='Meat & animal feeding', "12"= 'Agriculture', "13"='Price & consumption')
######################################
#for global sentiment analysis
# create the histgram & boxplot for global sentiment scores
preprocess <-function(filename){
  docs <- fread(sprintf('./%s.csv',filename))
  docs <- docs%>%filter(scores!=0)
  docs <- docs %>% group_by(cluster) %>% mutate(clustermean=mean(scores))
  docs <- docs  %>% add_count(cluster) #total num of sentences in this cluster
  docs <- data.table(docs)
  docs<-docs[,std_scores := scale(scores,center=F,scale=T)]
  docs<-docs[,group:=ifelse(scores < 0, "neg", ifelse(scores > 0,"pos", "neutral"))]
  docs[,cluster_weight := n/dim(docs)[1]]
  docs<- data.table(docs)
  print(head(docs))
  mean <- round(mean( docs[,scores]),3)
  std_mean <- round(mean( docs[,std_scores]),3)
  vlines <- data.frame(statistic=c("mean","score = 0"), x=c(mean,0))
  # plot overview of sentiment scores distribution
  plot_overview_unstd <- docs %>% ggplot(aes(x = scores)) +
    geom_histogram(binwidth = 0.05, aes(y = (..density..)*0.05),colour = "black", fill = "white")+
   # ylim(c(-0.2, 0.4))+
    scale_fill_manual(values = c("#00AFBB", "grey", "#FC4E07"))+
    #geom_vline(xintercept = 0,linetype="dotted",col = "black") +
    #geom_vline(xintercept = quantile(docs$scores,0.75),linetype="dotted",alpha=0.4) +
    #geom_vline(xintercept = quantile(docs$scores,0.25),linetype="dotted",alpha=0.4) +
    geom_vline(data=vlines,
               aes(linetype=statistic, xintercept=x, color=NA),
               color=c("darkblue"),
               show.legend=TRUE) +
    scale_y_continuous(breaks=seq(0, 0.4, by = 0.1),limits = c(-0.1,0.4))+
    scale_x_continuous(breaks=seq(-1, 1, by = 0.1),limits = c(-1,1))+
    labs(x ="scores", y = "densities")+
    #geom_vline(xintercept =mean,aes( col = "darkblue"),linetype=2,show.legend=TRUE) +
   # geom_vline(aes(xintercept =mean, col = NA),linetype=2,col = "darkblue",show.legend = T) +
    geom_boxploth(aes(y = -0.05),outlier.colour='grey',stat="boxplot", position = "dodge2",width = 0.05,  alpha = .5) +labs(title = paste0("Histgram and boxplot of ",filename," with mean = ",mean))+theme(plot.title = element_text(hjust = 0.5))
  #print(quantile(docs$scores,0.75))
 # print(quantile(docs$scores,0.25))
  print(quantile(docs$scores,0.75)-quantile(docs$scores,0.25))
  # plot overview  per topic of sentiment scores distribution
  plot_overview_topics <- docs %>% ggplot(aes(x = scores)) +
    geom_histogram(binwidth = 0.05, aes(y = (..density..)*0.05),colour = "black", fill = "white")+
    ylim(c(-0.2, 0.4))+
    scale_fill_manual(values = c("#00AFBB", "grey", "#FC4E07"))+
    geom_vline(xintercept = 0,linetype="dotted",col = "black") +
    geom_vline(xintercept =mean, col = "darkblue",linetype=2) +
    labs(x ="scores", y = "densities")+
    geom_boxploth(aes(y = -0.1),outlier.colour='grey',stat="boxplot", position = "dodge2",width = 0.05,  alpha = .5) +labs(title = paste0("Histgram (per topic) and boxplot of ",filename," with mean = ",mean))+facet_wrap(~cluster,ncol = 4,labeller = labeller(cluster = unlist(clusters))) +theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste0('output/overview_unstd_',filename,'.pdf'),plot_overview_unstd )
  ggsave(paste0('output/overview_per_topic_',filename,'.pdf'),plot_overview_topics )
  print(head(print))
  
# create boxplot for global sentiment scores
  plot_cluster <- docs %>% 
    ggplot(aes(factor(cluster),y=scores,fill=clustermean)) +
    geom_hline(linetype="dashed", color = "darkgrey",yintercept = 0)+
    scale_x_discrete(labels=clusters)+
    geom_jitter(alpha=0.05,size=0.5)+
    geom_boxplot(notchwidth=0.1,varwidth=TRUE,outlier.shape = NA,aes(weight=cluster_weight))+
    scale_color_gradient2(midpoint = 0, low = "blue", mid = "grey",high = "#e03531", space = "Lab")+
    scale_fill_gradient2(midpoint = 0, low = "blue", mid = "grey",high = "#e03531", space = "Lab")+
    geom_point(aes(y=clustermean,factor(cluster)),size = 1,shape=3,stroke=1,alpha=0.6)+ 
    theme(plot.title = element_text(size =10),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank())
  ggsave(paste0('output/all_boxplot_',filename,'.pdf'),plot_cluster)
}
#  
preprocess('all_sentiments_nytimes_news')
preprocess('all_sentiments_nytimes_comments')
preprocess('all_sentiments_spiegel_news')
preprocess('all_sentiments_spiegel_comments')
0.032-0.061
# calculate the cluster mean and  cluster weight for datatset
statistic <- function(combined){
  combined <- combined %>% group_by(cluster) %>% mutate(clustermean=mean(scores))
  combined <- combined  %>% add_count(cluster)
  combined <- data.table(combined)
  combined[,cluster_weight := n/dim(combined)[1]]
  
  return (combined)
}
# generate boxplot for comments and news per topic
compare_news_comments <-function(newsfile,commentsfile){
  news <- fread(sprintf('./%s.csv',newsfile))
  news <- news%>%filter(scores!=0)
  print(unique(news$cluster))
  comments <- fread(sprintf('./%s.csv',commentsfile))
  comments <- comments%>%filter(scores!=0)
  news<- data.table(news)
  news_n <- count(news)
  comments<- data.table(comments)
  comm_n <- count(comments)
  scale<-comm_n/news_n
  news<- do.call("rbind", replicate(as.integer(scale), news, simplify = FALSE))
  print('new news')
  print(count(news))
  comments <- statistic(comments)
  news<- statistic(news)
  news[,type:='news']
  comments[,type:='comment']
  l = list(news,comments)
  combined <- rbindlist(l)
  combined<-data.table(combined)
  print('combined')
print( scale)
  sum_com <-combined%>% group_by(type,cluster)%>% summarise(sum_weights=sum(cluster_weight))
  
  sum_com <-sum_com %>% group_by(type) %>%mutate(relative_weight_per_type=sum_weights/sum(sum_weights))
  sum_com<-data.table(sum_com)
print(sum_com)
  xtable(sum_com)
  combined <-merge(combined,sum_com,by=c('cluster','type'),all.x = TRUE, all.y = FALSE)

  # plot the boxplot that to compare the news and comments per topic
  plot_cluster <- combined %>% ggplot(aes(x=type,y=scores,weight=relative_weight_per_type)) +
    geom_hline(linetype="dashed", color = "darkgrey",yintercept = 0)+
    scale_x_discrete(labels=clusters)+
    geom_jitter(alpha=0.05,size=0.1)+
    #boxplot(width=c(10,0.2,3,4,5))+
    geom_boxplot(varwidth=TRUE,outlier.shape = NA, aes(fill=(clustermean)))+
    geom_point(aes(y=clustermean),size = 1,shape=3,alpha=0.6)+
    facet_wrap(~cluster,ncol = 11,labeller = labeller(cluster = unlist(clusters)))+
    scale_fill_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+ 
    scale_color_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    theme(plot.title = element_text(size =10),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank()) +
    coord_cartesian(clip = "off")   
  ggsave(paste0('output/all_boxplot_combined',newsfile,'.pdf'),width = 20, height = 4,plot_cluster)
  return (list(data.table(combined),(sum_com)))
}

ny <-  compare_news_comments('all_sentiments_nytimes_news','all_sentiments_nytimes_comments')
sp <- compare_news_comments('all_sentiments_spiegel_news','all_sentiments_spiegel_comments')
#tb <-as.data.table(ny[2])
#tb_1 <- tb[type=='comment',] #commment
#tb_1 <- tb_1[order(cluster),]
#tb_1<-tb_1[,name:=clusters]
#tb_1 <- tb_1[order(-relative_weight_per_type),]
#tb_1 <- tb_1[,c(4,5)]
#xtable(tb_1)


# check if there is a significant relation between the number of sentences and the variety 
isrelated<-function(data,filename){
  combined_data <- data.table(data)
  combined_data[,sum_weights:=NULL]
  combined_comment <- combined_data %>% filter(type=='comment') %>% group_by(doc_id,cluster)  %>%mutate(n_sentence_per_doc =n(),quantile_min=quantile(scores,probs = 0.15),quantile_max=quantile(scores,probs = 0.85),quantile=quantile_max-quantile_min,var_new=var(scores))
combined_comment<- data.table(combined_comment)

combined_news <- combined_data %>% filter(type=='news') %>% group_by(doc_id,cluster)  %>%mutate(n_sentence_per_doc =n(),quantile_min=quantile(scores,probs = 0.15),quantile_max=quantile(scores,probs = 0.85),quantile=quantile_max-quantile_min,var_new=var(scores))
combined_news<- data.table(combined_news)
# select the first datapoint in each subset which has the same quantile and n_sentence_per_doc
#analyse the relationship between #sentences and quantile
point_size<-0.3
meth <- 'lm'
  compact <- combined_comment[,.SD[1],by =c('cluster','doc_id')]
  compact_news <- combined_news[,.SD[1],by =c('cluster','doc_id')]
  print(head(compact))
  mean_quantile_news = median(compact_news$quantile)
  mean_quantile = median(compact$quantile)
  print(mean_quantile)
  print("mean_quantile news")
  print(mean_quantile_news)
  print(mean_quantile_news-mean_quantile)
  plot1 <- compact %>% filter(n_sentence_per_doc>1) %>%ggplot(aes(n_sentence_per_doc,quantile))+geom_point(size=point_size)+
    geom_smooth(method = meth,linetype=3,size=point_size,alpha=0.5) +
    facet_wrap(~cluster,ncol = 4,labeller = labeller(cluster = unlist(clusters)))+
    ylim(c(0,1))+
    ggtitle(sprintf('Number of sentences in comments with sentiment variance per topic (%s)' ,filename))+ 
    theme(plot.title = element_text(size =10,hjust = 0.5))
  plot2 <- compact %>% filter(n_sentence_per_doc>1) %>%ggplot(aes(n_sentence_per_doc,var_new))+
    geom_point(size=point_size)+
    ylim(c(0,0.3))+
    geom_smooth(method = meth,linetype=3,size=point_size,alpha=0.5)+
    facet_wrap(~cluster,ncol = 4,labeller = labeller(cluster = unlist(clusters)))+
    labs(x ="number of sentence per document", y = "sentiment variance")+
    ggtitle(sprintf('Number of sentences in comments with sentiment variance per topic (%s)' ,filename))+ 
    theme(plot.title = element_text(size =10,hjust = 0.5))
#combine two plots in one graph
#b<-plot_grid(plot1, plot2,  ncol = 1, nrow = 2)
  hlines <- data.frame(statistic="median of variance", y=mean_quantile)
  plot3 <- compact %>% filter(n_sentence_per_doc>1) %>% ggplot(aes(n_sentence_per_doc,quantile))+
    geom_hline(data=hlines,
               aes(linetype=statistic, yintercept=y, color=NA),
               alpha=0.8,
               color=c("darkgrey"),
               show.legend=TRUE) +
    geom_point(size=0.5)+
    ylim(c(0,1))+
    scale_y_continuous(breaks=seq(0, 1.2, by = 0.1),limits = c(0,1.2))+
    
    geom_smooth(method = "loess",aes(col= 'blue'),alpha=0.4,linetype=3)+
    scale_colour_manual(name="",values ='blue',labels= 'loess smoothing')+
    labs(x ="number of sentences per document", y = "sentiment variance")+
    ggtitle(sprintf('Number of sentences in comments with sentiment variance (%s)' ,filename))+ theme(plot.title = element_text(size =10,hjust = 0.5))
  ggsave(paste0('output/num_commentsentences_quantile_percluster',filename,'.pdf'),plot1,width = 8, height = 4)
  ggsave(paste0('output/num_commentsentences_var_percluster',filename,'.pdf'),plot2,width = 8, height = 4)
  ggsave(paste0('output/num_commentsentences_quantile',filename,'.pdf'),plot3)
  return(compact)
}
#
compact_ny <- isrelated(combined_ny,'nytimes')
compact_sp <-isrelated(combined_sp,'spiegel')
# join comments and new
join_com_news <-function(combined_data){
  combined_data <- data.table(combined_data)
  combined_data[,sum_weights:=NULL]
  combined_data <- combined_data %>% group_by(doc_id,type,cluster)  %>%mutate(n_sentence_per_doc =n(),quantile_min=quantile(scores,probs = 0.15),quantile_max=quantile(scores,probs = 0.85),quantile=quantile_max-quantile_min,var_new=var(scores))
  combined_data<- data.table(combined_data)
  # select the first datapoint in each subset which has the same quantile and n_sentence_per_doc
  #analyse the relationship between #sentences and quantile
  compact <- combined_data[,.SD[1],by =c('cluster','doc_id','type')]
  compact <-data.table(compact)
  return(compact)
}
# add rowname to column name
add_rowname2col <- function(x,colname){
  x[sapply(x, is.null)] <- NA
  x <- unlist(x)
  x <- data.table(x)
  
  setnames(x,'x',colname)
  x[,cluster:= seq(0,13)] # modify the cluster indexes
  return(x)
}

# generate the correlation between news and comments via means/weight/quantile
cor_plot <- function(combined_data,corpusname,pvalue=1){
  ny_news <- join_com_news(combined_data)[type=='news']
  ny_comm<- join_com_news(combined_data)[type=='comment']
  cluster_index <- unique(ny_news[,cluster])
  print(cluster_index)
  means_cor <- list()
  weights_cor <- list()
  quantiles_cor<- list()
  for (i in cluster_index){
    ny_news_i<- ny_news[cluster==i,c('mean','weight','doc_id','var_new','quantile')]
    ny_comm_i<- ny_comm[cluster==i,c('mean','weight','doc_id','var_new','quantile')]
    merged <-merge(ny_news_i,ny_comm_i,by='doc_id',all=F)
  #hist(merged[,quantile.x]) #[,mean.x]
    if(dim(merged)[1]>1){
      res_mean <- cor.test(merged[,mean.x],merged[,mean.y])
      if (res_mean$p.value<pvalue)
        mean_cor <-res_mean$estimate
      else
        mean_cor <- 0
   #print(cor.test(merged[,mean.x],merged[,mean.y]))
      # In R there is no array[0]. Thus +1
      means_cor[i+1] <- mean_cor 
      res_weight <- cor.test(merged[,weight.x],merged[,weight.y],method = 'spearman')
      if (res_weight$p.value<pvalue)
        weight_cor <-res_weight$estimate
      else
        weight_cor <- 0
      weight_cor <- res_weight$estimate
      weights_cor[i+1] <- weight_cor
      res_quantile <- cor.test(merged[,quantile.x],merged[,quantile.y],method = 'spearman')
      if (res_weight$p.value<pvalue)
        quantile_cor <- res_quantile$estimate
      else
        quantile_cor <- 0
      quantiles_cor[i+1] <- quantile_cor
      }
  }
  print(means_cor)
  means_cor<-add_rowname2col(means_cor,'sentiment_polarity_mean_corr')
  print(means_cor)
  weights_cor<-add_rowname2col(weights_cor,'topic_domiance_corr')
  quantiles_cor<-add_rowname2col(quantiles_cor,'sentiment_variance_corr')
  merged1 <- merge(means_cor,weights_cor)
  merged2 <-merge(merged1,quantiles_cor)
  merged2 <- data.table(merged2)
  
  merged2_melt <- melt(merged2,id.vars = 'cluster')
  merged2_melt <- data.table(merged2_melt)
  merged2_melt<-merged2_melt[cluster!=2 &cluster!=4 & cluster!=9]
  
  p<- ggplot(merged2_melt,aes(x = reorder(factor(cluster), -value),y=value)) +
    geom_bar(stat='identity', width = 0.5,position='dodge',aes(fill = value ))+
    scale_y_continuous(name="correlation coefficient")+
    facet_grid(~variable)+
    scale_x_discrete(labels=clusters)+
    scale_fill_gradient2(name = "coefficient",midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    ggtitle(sprintf('Correlation between news and comments (%s)' ,corpusname))+
    theme(plot.title = element_text(size =10,hjust = 0.5),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank())
  ggsave(paste0('output/cor_bar_new_',corpusname,'.pdf'),width = 8, height = 4,p)
}

cor_plot(combined_ny,'NYTimes',1)
cor_plot(combined_sp,'Spiegel',1)
######################################
#for individual doc (news and comments)

plot_box <-function(article,comment,filename){
  myTitle1 <- paste("Sentiment distribution of news ",filename)
  myTitle2 <- paste("Sentiment distribution of comments ",filename)
  plot1 <- article %>% ggplot(aes(y=scores,factor(cluster),fill=mean,weight=weight))+
    scale_x_discrete(labels=clusters)+
    ylim(c(-1,1))+
    
    geom_hline(linetype="dashed", color = "darkgrey",yintercept = 0)+
    geom_boxplot( notchwidth=0.1,varwidth=0.1,outlier.shape = NA)+
    scale_color_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    scale_fill_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    geom_point(aes(y=mean,factor(cluster)),size = 1,shape=3,stroke=1,alpha=0.6)+ 
    labs(x ="cluster", y = "sentiment score")+
    geom_jitter(alpha=0.1)+
    ggtitle(myTitle1)+
    theme(plot.title = element_text(size =10),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank())
  
  plot2<- comment %>% ggplot(aes(y=scores,factor(cluster),fill=mean,weight=weight))+
    scale_x_discrete(labels=clusters)+
    ylim(c(-1,1))+
    geom_hline(linetype="dashed", color = "darkgrey",yintercept = 0)+
    geom_boxplot(notchwidth=0.1,varwidth=0.1,outlier.shape = NA)+
    geom_point(aes(y=mean,factor(cluster)),size = 1,shape=3,alpha=0.6)+
    scale_fill_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    scale_color_gradient2(midpoint = 0, low = "#e03531", mid = "grey",high = "#0e8420", space = "Lab")+
    labs(x ="cluster", y = "sentiment score")+
    geom_jitter(alpha=0.05,size=0.2)+
    ggtitle(myTitle2)+ 
    theme(plot.title = element_text(size =10),axis.ticks.x=element_blank(),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank())
  #plot two in one graph
  b<-plot_grid(plot1, plot2,  ncol = 1, nrow = 2)
  ggsave(paste0('output/boxplot_',filename,'.pdf'),b,width = 8, height = 8)
  }
#indexes of the docs to plot

indexs_sp <-c(37,39,40,46,54, 56,59,62,65,69,71, 72,74,76,78,81,84, 93,101,102,104,106, 107,108,112,114,115,117, 118,122,138,140,141,142)
indexs_ny <-c(40, 57, 76, 127, 131, 133, 147, 162, 167, 175, 177, 196, 214, 216, 222, 229, 231, 245, 254, 259, 264, 282, 304, 309, 316, 320, 321, 322, 323, 324, 325, 326)
#indexs_ny<-c(175)
#indexs_sp<-c(112)
# generate boxplot for each doc
for (ind in indexs_sp){num<- ind
  comments <- fread(sprintf("./output_article/comments_spiegel_%s.csv",num))
  comments <- comments%>%filter(scores!=0)
  article <- fread(sprintf("./output_article/article_spiegel_%s.csv",num))
  plot_box(article,comments,sprintf('spiegel_filter_%s',num))
  }

for (ind in indexs_ny){num<- ind
comments <- fread(sprintf("./output_article/comments_nytimes_%s.csv",num))
comments <- comments%>%filter(scores!=0)
article <- fread(sprintf("./output_article/article_nytimes_%s.csv",num))
plot_box(article,comments,sprintf('nytimes__filter_%s',num))
}


#backup
# generate one doc for testing
comments_40 <- fread(sprintf("./output_df_article_comments_sentiment/comments_nytimes_%s.csv",40))
compact_ny %>% filter(doc_id==40)
myTitle2 <- paste("Sentiment distribution of comments ",'nytimes_test')
single_plot <- comments %>% ggplot(aes(y=scores,factor(cluster),fill=mean,weight=weight))+
  scale_x_discrete(labels=clusters)+
  geom_boxplot(notchwidth=0.1,varwidth=0.1,outlier.shape = NA)+
  geom_point(aes(y=mean,factor(cluster)),size = 1,shape=3,alpha=0.6)+
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "grey",high = "#e03531", space = "Lab")+
  scale_color_gradient2(midpoint = 0, low = "blue", mid = "grey",high = "#e03531", space = "Lab")+
  labs(x ="cluster", y = "sentiment score")+
  geom_jitter(alpha=0.1,size=0.5)+
  ggtitle(myTitle2)+ 
  theme(plot.title = element_text(size =10),axis.text.x = element_text(size=6,angle = 90, vjust = 0.5, hjust=1),axis.title.x = element_blank())
single_plot
