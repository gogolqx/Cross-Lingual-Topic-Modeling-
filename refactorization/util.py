# -*- coding: utf-8 -*-
"""util.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1BpuYZIMKiZOv-FsGhPqYLJq28NTe7PxO
"""

import numpy as np
import json
import re

import nltk
nltk.download('stopwords')
nltk.download('punkt')
from nltk.corpus import stopwords

from nltk.stem.snowball import EnglishStemmer
from nltk.stem.snowball import GermanStemmer

from enum import Enum


class DocType(Enum):
  NON_COMMENT = -1
  ARTICLE = 0
  COMMENT = 1
  ALL = 2
  
class Language(Enum):
  EN = 1
  DE = 2
  
class Polarity(Enum):
  NEUTRAL = 0
  POSITIVE = 1
  NEGATIVE = 2

class Source():
  NYTIMES = 'nytimes'
  QUORA = 'quora'
  SPEIGEL = 'spiegel'

class Stopwords():
  EN_NLTK = stopwords.words('english')
  DE_NLTK = stopwords.words('german')

  # The most high term-frequency words with term-frequency larger than 5000
  EN_TFHIGH = frozenset(['the', 'and', 'to', 'of', 'is', 'in', 'that', 'organic', 'it', 'are', 'not', 'for',
                         'you', 'food', 'as', 'have', 'be', 'on', 'with', 'they', 'or', 'but', 'this', 'from',
                         'more', 'we', 'do', 'can', 'there', 'if', 'by', 'at', 'all', 'foods', 'about',
                         'what', 'has', 'will', 'so', 'their', 'an', 'your', 'would', 'than', 'people', 'no',
                         'which', 'like', 'was', 'one', 'some', 'my'])
  DE_TFHIGH = frozenset(['die', 'und', 'der', 'ist', 'das', 'nicht', 'von', 'in', 'es', 'sie', 'zu', 'den',
                         'ich', 'auch', 'mit', 'zitat', 'ein', 'sich', 'für', 'auf', 'man', 'sind', 'dass',
                         'aber', 'werden', 'wie', 'im', 'nur', 'oder', 'wenn', 'eine', 'so', 'bei', 'als',
                         'wird', 'aus', 'was', 'dem', 'noch', 'bio', 'an', 'dann', 'haben', 'kann', 'da',
                         'hat', 'mehr', 'wir', 'um', 'mal', 'doch', 'schon', 'ja', 'nach', 'sein', 'keine',
                         'immer', 'einen', 'des', 'gibt', 'hier', 'diese', 'durch'])
  
class OptimalKClustersConfig():
  clusters_with_garbage = ['Planting and gardening', 'Retail', 'Garbage 1', 'GMO label and bio-products', 'Garbage 2',
                           'Taste and food', 'Chemicals and cancer', 'Genetic research', 'Health and diet', 'Garbage 3',
                           'Governance and public policy', 'Meat and animal feeding', 'Agriculture', 'Price and consumption', 'Garbage 4']
  k_with_garbage = len(clusters_with_garbage)

  # define the index of garbage_clusters
  garbage_clusters = [2, 4, 9, 14]
  clusters = [c for c in clusters_with_garbage if 'Garbage' not in c]
  k = len(clusters)
  

def load_kmean_labels(path):
  labels = np.load(path).astype(int)
  k = max(labels) + 1
  print('Number of {}-means labels: {}'.format(k, len(labels)))
  return k, labels

def load_sentences(path):
  with open(path, 'r') as f:
    loaded_data = json.load(f)
  print('Sentences file - loaded')

  sentences = []
  for item in loaded_data:
    for key, value in item.items():
      sentences.append(value)
  print('Done - appended all sentences')
  print('Number of tokenized sentences from corpus:', len(sentences))
  return sentences

def load_sentences_index(path, source: Source):
  with open(path, 'r') as f:
    loaded_data = json.load(f)
  print('Sentences index file - loaded')

  indeces = []
  for item in loaded_data:
    if item['corpus_name'] in source:
      indeces.append(item['global_id'])
  print('Done - appended all sentences indeces')
  print('Number of indeces from corpus {}: {}'.format(source, len(indeces)))
  return indeces

def replace_url(sentence):
  return re.sub(r'<a[\s\w\/~=#\'\":.,@?;&%()+-]*\>[\s\w\/~=#\"\':.,@?;&%()+-]*<\/a>', 'url', sentence)

def get_stemmed_sentences(lanuage: Language, stopwords: Stopwords, sentences):
  token_pattern = re.compile(r'(?u)\b\w\w+\b')
  if lanuage == Language.EN:
    stemmer = EnglishStemmer()
  else:
    stemmer = GermanStemmer()

  s = []
  for sentence in sentences:
    s.append(' '.join([stemmer.stem(word) for word in token_pattern.findall(sentence.lower()) if word not in stopwords]))

  return np.array(s)


# encoder for numpy to json format
# reference: https://stackoverflow.com/questions/50916422/python-typeerror-object-of-type-int64-is-not-json-serializable/50916741
class NumpyEncoder(json.JSONEncoder):
  def default(self, obj):
    if isinstance(obj, np.integer):
      return int(obj)
    elif isinstance(obj, np.floating):
      return float(obj)
    elif isinstance(obj, np.ndarray):
      return obj.tolist()
    else:
      return super(NumpyEncoder, self).default(obj)
    
def list2json(input_list, output_path):
  output_file = open(output_path, 'w', encoding = 'utf-8')
  json.dump(input_list, output_file, ensure_ascii = False, cls = NumpyEncoder)
  output_file.close()