# Quora Question Pairs

**Can you identify question pairs that have the same intent ?**

The **goal** of this competition is to **predict** which of the provided pairs of questions contain **two questions with the same meaning**. 

# Ground truth

The **ground truth** is the **set of labels** that have been supplied by human experts. The ground truth labels are inherently subjective, as the true meaning of sentences can never be known with certainty. 

Human labeling is also a **'noisy'** process, and reasonable people will disagree. As a result, the ground truth labels on this dataset should be taken to be **'informed' but not 100% accurate**, and may include **incorrect labeling**. We believe the labels, on the whole, to represent a reasonable consensus, but this may often not be true on a case by case basis for individual items in the dataset.

# Data fields

**id** - the id of a training set question pair

**qid1** - unique id of question #1 in pair

**qid2** - unique id of question #2 in pair

Available only in train.csv

**question1** - the full text of question #1

**question2** - the full text of question #2

**is_duplicate** - the target variable

Set to **1** if question1 and question2 have essentially the same meaning, and **0** otherwise.

# Model

## Example of tf–idf

Have to be change

More webs again:

https://en.wikipedia.org/wiki/Tf%E2%80%93idf
