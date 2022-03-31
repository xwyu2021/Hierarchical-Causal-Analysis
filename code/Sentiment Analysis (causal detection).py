#!/usr/bin/env python
# coding: utf-8

# In[ ]:


## logistic regression vs RNN
import pandas as pd
import numpy as np
import tensorflow as tf
from tensorflow import keras
import nltk
import string ## string operations
from nltk.corpus import stopwords # module for stop words
from nltk.stem import PorterStemmer 
from nltk.tokenize import TweetTokenizer
import re
import spacy
import textacy
from spacy.matcher import Matcher
from nltk import RegexpParser
from nltk import word_tokenize, pos_tag, ne_chunk, sent_tokenize
from nltk import Tree
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')


# In[ ]:


####### 1. logistic regression ##########
data = pd.read_csv(r'./texts.csv')
label = pd.read_csv(r'./label.csv')
d = data["x"].tolist()
train = d[:280]
test = d[280:len(d)]
y = label["y"].tolist()
trainy = y[:280]
testy = y[280:len(d)]


# In[ ]:


def build_freqs(texts,ys):
    
    freqs = {}
    for y,text in zip(ys,texts):
        tokens = word_tokenize(str(text))
        for word in tokens:
            pair = (word, y)
            if pair in freqs:
                freqs[pair] += 1
            else:
                freqs[pair] = 1
    return freqs
def extract_features(text,freqs):
    #tweet: a list of words for a single tweet
    # tokenize, stems, remove stopwords
    word_1 = word_tokenize(str(text))
    x = np.zeros((1,3))
    x[0,0] = 1
    for word in word_1:
        x[0,1] += freqs.get((word,1.0),0)
        x[0,2] += freqs.get((word,0.0),0)
    assert(x.shape == (1,3))
    return x


# In[ ]:


X = np.zeros((len(train),3))
for i in range(len(train)):
    X[i,:] = extract_features(train[i],freqs)
Y = np.array([trainy]).T


# In[ ]:


def sigmoid(z):
    return 1/(1 + np.exp(-z))
def gradientdescent(x,y,theta, alpha, iters):
    m = x.shape[0]
    for i in range(iters):
        z = np.dot(x,theta)
        h = sigmoid(z)  ## ./ to have floating points
        J = - (1./m) * (np.dot(y.transpose(), np.log(h)) + np.dot((1-y).transpose(), np.log(1-h)))
        theta = theta - (alpha/m) * np.dot(x.transpose(),(h-y))
    J = float(J)
    return J, theta


# In[ ]:


J, theta = gradientdescent(X,Y,np.zeros((3,1)),1e-5,10000)
print(f"cost after training is {J:.8f}")
print(f"the estimators are {[round(t,10) for t in np.squeeze(theta)]}")


# In[ ]:


def predict_text(text, freqs, theta):
    x = extract_features(text,freqs)
    return sigmoid(np.dot(x,theta))

def test_logistic_regression(test_x,test_y,freqs,theta):
    y_hat = []
    for text in test_x:
        y_pred = predict_text(text, freqs, theta)
        if y_pred > 0.5:
            y_hat.append(1)
        else:
            y_hat.append(0)
    ct = 0
    for i in range(len(test_y)):
        if y_hat[i] == test_y[i]:
            ct += 1
            
    accuracy = ct/ len(test_x)
    return accuracy


# In[ ]:


myaccuracy = test_logistic_regression(test,testy,freqs,theta)
print(myaccuracy) ## 0.86


# In[ ]:


########## 2. Recurrent Neural Network

labeleddata = pd.read_csv(r'./labeled.csv')
numeric_feature_names = ['x', 'y']
numeric_feature = labeleddata[numeric_feature_names]
tfdata = tf.convert_to_tensor(numeric_feature)
from collections import Counter
vocabulary = Counter()
for x,y in tfdata:
    tftext = tf.strings.split(x)
    vocabulary.update(list(tftext.numpy()))
myvocabulary = [word for word, count in vocabulary.most_common()[:460]]
## replace words by id
words = tf.constant(myvocabulary)
word_ids = tf.range(len(myvocabulary),dtype=tf.int64)
## create a look up table
vocab_init = tf.lookup.KeyValueTensorInitializer(words,word_ids)
num_oov_buckets = 10
table = tf.lookup.StaticVocabularyTable(vocab_init,num_oov_buckets)


# In[ ]:


data = pd.read_csv(r'./texts.csv')
inputfeature = data.pop("x")
inputfeature.values
label = pd.read_csv(r'./label.csv')
target = label.pop("y")
target.values
mydata = tf.data.Dataset.from_tensor_slices((inputfeature.values[:280], target.values[:280]))
testdata = tf.data.Dataset.from_tensor_slices((inputfeature.values[280:len(inputfeature.values)], target.values[280:len(inputfeature.values)]))


# In[ ]:


def process(x,y):
    return tf.strings.split(x),y
train_set = mydata
nowset = train_set.batch(32).map(process)
def encode_words(X_batch,y_batch):
    return table.lookup(X_batch),y_batch
nowset = nowset.map(encode_words).prefetch(1)
test_set = testdata
nextset = test_set.batch(32).map(process)
nextset = nextset.map(encode_words).prefetch(1)


# In[ ]:


embed_size = 128
vocab_size=460
model = keras.models.Sequential([
    keras.layers.Embedding(vocab_size + num_oov_buckets,embed_size,input_shape=[None]),
    keras.layers.GRU(128,return_sequences = True),
    keras.layers.GRU(128),
    keras.layers.Dense(1,activation="sigmoid")
])


# In[ ]:


model.compile(loss = "binary_crossentropy",
             optimizer = "adam",
             metrics=["accuracy"])
history = model.fit(nowset,epochs=5) ## 0.9893
mse_test = model.evaluate(nextset) ## 0.9767

