# -*- coding: utf-8 -*-
"""topwords.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1nARN_lqqucduHpMNnlNbtQVM8kv7Zvo9
"""
import numpy as np
from util import Source, Stopwords

from sklearn.feature_extraction.text import TfidfVectorizer
from scipy.special import rel_entr

import matplotlib.pyplot as plt
from wordcloud import WordCloud
import random


def get_tfidf(stopwords: Stopwords, documents: np.array, vocabulary = None, global_vectorizer = None):
  """
  Parameters:
    stopwords: frozenset
      choice in Stopwords class 
    documents: np.array
      list of documents
    vocabulary: mapping or iterable, default = None
      either a Mapping (e.g., a dict) where keys are terms and values are indices in the feature matrix, or an iterable over terms. If not given, a vocabulary is determined from the input documents
  Returns:
    vectorizer.vocabulary_: dict
      a mapping of terms to feature indices
    vectorizer.get_feature_names(): list
      a list of feature names
    tfidf.toarray()[0]: 1d array
    vectorizer: TfidfVectorizer object
  """ 
  if global_vectorizer is None:
    vectorizer = TfidfVectorizer(stop_words = stopwords, norm = 'l1', vocabulary = vocabulary)
    idf = vectorizer.fit(documents)
  else:
    vectorizer = global_vectorizer
  tfidf = vectorizer.transform([' '.join(documents)])

  return vectorizer.vocabulary_, vectorizer.get_feature_names(), tfidf.toarray()[0], vectorizer


def get_clarity_score(x, y):
  """
  x, y: 1d array of same length
    array of tdidf scores of a cluster and global tdidf scores respectively
  """
  return rel_entr(x, y) / np.log(2)


def get_sort_scores(x):
  """ 
  x: 1d array
    array of scores
  """
  return np.sort(x)[::-1]


def get_sort_indeces(x):
  """ 
  x: 1d array
    array of scores
  """
  return x.argsort()[::-1]


def get_topwords(k: int, sentences: np.array, labels: np.array, stopwords: Stopwords, use_global_vectorizer = False):
  """
  k: int
  	number of clusters
  sentences: 1d np.array
    an array of sentences (of a language or a source)
  labels: 1d np.array
    an array of cluster labels corresponding to sentences
  stopwords: frozenset
    choice in Stopwords class
  use_global_vectorizer: boolean
    if True, use global idf (idf calculated from all sentences of corpus, i.e. all sentences from a language or a source)
    if False, use cluster idf (idf calculated from all sentences labelled as same cluster in the corpus)
  """
  # print('Global')
  vocabulary, bag_of_words, tfidf, global_vectorizer = get_tfidf(stopwords, sentences)

  tfidf_k = []
  for i in range(k):
    # print('Cluster', i)
    sentences_i = sentences[np.where(labels == i)]

    if use_global_vectorizer:
      _, _, t, _ = get_tfidf(stopwords, sentences_i, vocabulary = vocabulary, global_vectorizer = global_vectorizer)
    else:
      _, _, t, _ = get_tfidf(stopwords, sentences_i, vocabulary = vocabulary)
    tfidf_k.append(t)

  tfidf_k = np.array(tfidf_k)
  clarity_score = np.apply_along_axis(get_clarity_score, 1, tfidf_k, y = tfidf) # a row indicates tfidf for a cluster
  
  sorted_scores = np.apply_along_axis(get_sort_scores, 1, clarity_score)
  sorted_indeces = np.apply_along_axis(get_sort_indeces, 1, clarity_score)
  sorted_words = np.array(bag_of_words)[sorted_indeces]

  print('Top words and their corresponding clarity scores for k = {} are ready!'.format(k))
  return sorted_words, sorted_scores


def grey_color_func(word, font_size, position, orientation, random_state=None, **kwargs):
  return "hsl(0, 43%%, %d%%)" % random.randint(60, 100)


def print_wordcloud(k: int, sorted_words: np.array, sorted_scores: np.array, max_words = 300):
  size = (100, 60)
  cols, rows = 4, 4
  fig, axs = plt.subplots(rows, cols, figsize = size)

  for i in range(k):
    d = {}
    for word, score in zip(sorted_words[i][:max_words], sorted_scores[i][:max_words]):
      d[word] = score
    wordcloud = WordCloud(width = 1000, height = 600, max_words = max_words, relative_scaling = 1, normalize_plurals = True).generate_from_frequencies(d)

    row = i // cols
    col = i  % cols
    axs[row,col].imshow(wordcloud.recolor(color_func=grey_color_func, random_state=3), interpolation='bilinear')
    axs[row,col].set_title('cluster {}'.format(i))