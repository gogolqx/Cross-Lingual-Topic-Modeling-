import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import date, datetime, timedelta

from util import DocType, Polarity, Source, OptimalKClustersConfig

def get_sentences_counts(assignments, start_year = 2007, end_year = 2017):
  # Get data for plotting
  end_year = end_year + 1
  start_datetime_str = str(start_year) + '-01-01'
  end_datetime_str = str(end_year - 1) + '-12-31'
  start_datetime = pd.to_datetime(start_datetime_str)
  end_datetime = pd.to_datetime(end_datetime_str)

  years = [range(start_year, end_year)]
  totalMonths = 12 * (np.max(years) - np.min(years) + 1)
  months = [str(np.min(years) + (i // 12)) + '-' +  str(i % 12 + 1).zfill(2) for i in range(totalMonths)]

  # Convert assignments into panda dataframe
  df = pd.DataFrame.from_dict(assignments)
  df['month'] = df['date'].str[:-3]
  df['date'] = pd.to_datetime(df['date'])

  # df.head()

  nytimes_articles = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) & (df['com_id'] == DocType.NON_COMMENT.value)]
  nytimes_comments = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) &(df['com_id'] != DocType.NON_COMMENT.value)]
  quora_articles = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) & (df['com_id'] == DocType.NON_COMMENT.value)]
  quora_comments = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) &(df['com_id'] != DocType.NON_COMMENT.value)]
  spiegel_articles = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] == DocType.NON_COMMENT.value)]
  spiegel_comments = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] != DocType.NON_COMMENT.value)]

  nytimes_articles_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]
  nytimes_comments_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]
  quora_articles_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]
  quora_comments_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]
  spiegel_articles_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]
  spiegel_comments_positive = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] > 0)]

  nytimes_articles_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]
  nytimes_comments_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]
  quora_articles_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]
  quora_comments_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]
  spiegel_articles_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]
  spiegel_comments_neutral = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] == 0)]

  nytimes_articles_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]
  nytimes_comments_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.NYTIMES) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]
  quora_articles_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]
  quora_comments_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.QUORA) &(df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]
  spiegel_articles_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] == DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]
  spiegel_comments_negative = df[(df['date'] >= start_datetime) & (df['date'] <= end_datetime) & (df['corpus_name'] == Source.SPEIGEL) & (df['com_id'] != DocType.NON_COMMENT.value) & (df['sentiment'] < 0)]

  print('During {} to {}'.format(start_datetime_str, end_datetime_str))
  print('Number of senteces for New York Times articles:', len(nytimes_articles))
  print('Number of senteces for New York Times comments:', len(nytimes_comments))
  print('Number of senteces for Quora articles:', len(quora_articles))
  print('Number of senteces for Quora comments:', len(quora_comments))
  print('Number of senteces for Der Spiegel articles:', len(spiegel_articles))
  print('Number of senteces for Der Spiegel comments:', len(spiegel_comments))

  # Massage the data for plotting
  def get_sentence_count_by_date_per_cluster(df, months = months):
    grouped_df = df.groupby(['cluster', 'month']).count().astype(int)['global_id']
    grouped_df = pd.DataFrame(grouped_df)
    grouped_df.index = grouped_df.index.set_names(['cluster', 'month'])
    grouped_df.reset_index(inplace = True)  
    
    month_list = grouped_df['month']
    
    pivot_df = grouped_df.pivot(index = 'cluster', columns = 'month')
    
    for month in months:
      if month not in month_list.values:
        pivot_df[('global_id',month)] = 0

    return pivot_df.reindex(sorted(pivot_df.columns), axis=1).fillna(0)

  freq_nytimes_articles = get_sentence_count_by_date_per_cluster(nytimes_articles).values
  freq_nytimes_comments = get_sentence_count_by_date_per_cluster(nytimes_comments).values
  freq_quora_articles = get_sentence_count_by_date_per_cluster(quora_articles).values
  freq_quora_comments = get_sentence_count_by_date_per_cluster(quora_comments).values
  freq_spiegel_articles = get_sentence_count_by_date_per_cluster(spiegel_articles).values
  freq_spiegel_comments = get_sentence_count_by_date_per_cluster(spiegel_comments).values

  freq_nytimes_articles_positive = get_sentence_count_by_date_per_cluster(nytimes_articles_positive).values
  freq_nytimes_comments_positive = get_sentence_count_by_date_per_cluster(nytimes_comments_positive).values
  freq_nytimes_articles_neutral = get_sentence_count_by_date_per_cluster(nytimes_articles_neutral).values
  freq_nytimes_comments_neutral = get_sentence_count_by_date_per_cluster(nytimes_comments_neutral).values
  freq_nytimes_articles_negative = get_sentence_count_by_date_per_cluster(nytimes_articles_negative).values
  freq_nytimes_comments_negative = get_sentence_count_by_date_per_cluster(nytimes_articles_negative).values

  return months, freq_nytimes_articles, freq_nytimes_comments, freq_quora_articles, freq_quora_comments, freq_spiegel_articles, freq_spiegel_comments, freq_nytimes_comments_positive, freq_nytimes_comments_neutral, freq_nytimes_comments_negative


def get_simple_timeseries_plot(source: str, timeseries: list, y_articles: np.array, y_comments: np.array, colors = ['purple', 'plum'], ymax = 50, figsize = (16, 8), title_fontsize = 20, label_fontsize = 12, xticks = None):
  for i in range(OptimalKClustersConfig.k_with_garbage): 
    if i not in OptimalKClustersConfig.garbage_clusters:
      # plot
      # plt.plot(timeseries, cluster0_nytimes_article)
      # plt.plot(timeseries, cluster0_nytimes_comment)
      plt.figure(figsize = figsize)
      plt.title(OptimalKClustersConfig.clusters_with_garbage[i] + ' discussed in ' + source, fontsize = title_fontsize)
      plt.ylim(0, ymax)
      plt.ylabel('Precentage(%) in occurrences', fontsize = label_fontsize)
      plt.xlabel('Timeseries by month', fontsize = label_fontsize)
      plt.plot(timeseries, y_articles[i] / sum(y_articles[i]) * 100, label = 'sentences in news', color = colors[0])
      # plt.fill(timeseries, y_articles[i] / sum(y_articles[i]) * 100, facecolor = colors[0], alpha = 0.25)
      plt.plot(timeseries, y_comments[i] / sum(y_comments[i]) * 100, label = "sentences in readers' comments", color = colors[1])
      # plt.fill(timeseries, y_comments[i] / sum(y_comments[i]) * 100, facecolor = colors[1], alpha = 0.25)
      
      if xticks is not None:
      	plt.xticks(xticks)
      plt.legend()
      # beautify the x-labels
      plt.gcf().autofmt_xdate()
      
      plt.show()
      
#import matplotlib.pyplot as plt

#def get_simple_timeseries_plot(source: str, timeseries: list, y_articles: np.array, y_comments: np.array, colors = ['purple', 'plum'], ymax = 50, figsize = (16, 8), title_fontsize = 20, label_fontsize = 12, xticks = None):
#  for i in range(OptimalKClustersConfig.k_with_garbage): 
#    if i not in OptimalKClustersConfig.garbage_clusters:
#      # plot
#      # plt.plot(timeseries, cluster0_nytimes_article)
#      # plt.plot(timeseries, cluster0_nytimes_comment)
#      plt.figure(figsize = figsize)
#      plt.title(OptimalKClustersConfig.clusters_with_garbage[i], fontsize = title_fontsize)
#      plt.ylim(0, ymax)
#      plt.ylabel('Precentage(%) in occurrences', fontsize = label_fontsize)
#      plt.xlabel('Months', fontsize = label_fontsize)
#      plt.plot(timeseries, y_articles[i] / sum(y_articles[i]) * 100, label = 'sentences in news', color = colors[0])
#      # plt.fill(timeseries, y_articles[i] / sum(y_articles[i]) * 100, facecolor = colors[0], alpha = 0.25)
#      plt.plot(timeseries, y_comments[i] / sum(y_comments[i]) * 100, label = "sentences in readers' comments", color = colors[1])
#      # plt.fill(timeseries, y_comments[i] / sum(y_comments[i]) * 100, facecolor = colors[1], alpha = 0.25)
#      
#      if xticks is not None:
#      	plt.xticks(xticks)
#      plt.legend(fontsize = 15)
#      # beautify the x-labels
#      plt.gcf().autofmt_xdate()
#      
#      plt.show()      
