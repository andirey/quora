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
