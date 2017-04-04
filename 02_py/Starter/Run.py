#
# Identifying Duplicate Questions
# https://www.kaggle.com/anokas/quora-question-pairs/data-analysis-xgboost-starter-0-35460-lb
#

import numpy as np
import pandas as pd
import os
import gc
import matplotlib.pyplot as plt
import seaborn as sns

pal = sns.color_palette()

# Check input data files
print('# File sizes')
for f in os.listdir('../../data/in/'):
    if 'zip' not in f:
        print(f.ljust(30) + str(round(os.path.getsize('../../data/in/' + f) / 1000000, 2)) + 'MB')

# Read data
df_train = pd.read_csv('../../data/in/train.csv')
print(df_train.head())

# Quick analysis of train data set
print('Total number of question pairs for training: {}'.format(len(df_train)))
print('Duplicate pairs: {}%'.format(round(df_train['is_duplicate'].mean()*100, 2)))

qids = pd.Series(df_train['qid1'].tolist() + df_train['qid2'].tolist())
print('Total number of questions in the training data: {}'.format(len(np.unique(qids))))
print('Number of questions that appear multiple times: {}'.format(np.sum(qids.value_counts() > 1)))

# Histogram drawing
plt.figure(figsize=(12, 5))
plt.hist(qids.value_counts(), bins=50)
plt.yscale('log', nonposy='clip')
plt.title('Log-Histogram of question appearance counts')
plt.xlabel('Number of occurences of question')
plt.ylabel('Number of questions')
print()

