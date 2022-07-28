import fasttext
import pandas as pd
import csv
from datetime import datetime
from sklearn.model_selection import KFold

# path setting
path =r'...'

# as df
data_excel = pd.read_excel(path+r"\coded_final.xlsx")

# k fold
kf = KFold(n_splits=5, shuffle=False)
label_0 = []
label_1 = []

print(f"Model started training at {datetime.now()}")
start=datetime.now()

for train_indices, test_indices in kf.split(data_excel):
    # saving train and test data as txt files
    data_excel.iloc[list(train_indices), :].to_csv(path + r"\train_data.txt", index=False,
                                                   sep=' ', header=None, quoting=csv.QUOTE_NONE,
                                                   quotechar="", escapechar=" ")

    data_excel.iloc[list(test_indices), :].to_csv(path + r"\test_data.txt", index=False,
                                                  sep=' ', header=None, quoting=csv.QUOTE_NONE,
                                                  quotechar="", escapechar=" ")

    model = fasttext.train_supervised(input=path+r"\train_data.txt", lr=1, dim=300,
                                      verbose=1, epoch=25,
                                      pretrained_vectors=path+r"\cc.nl.300.vec")

    # testing the model
    metrics = model.test_label(path + r"\test_data.txt")

    label_0.append(metrics['__label__0'])
    label_1.append(metrics['__label__1'])

# save the metrics
label_0_df = pd.DataFrame(label_0)
label_0_df.rename(columns={'precision': 'precision_0',
                           'recall': 'recall_0',
                           'f1score': 'f1score_0'},
                  inplace=True)
label_1_df = pd.DataFrame(label_1)
label_1_df.rename(columns={'precision': 'precision_1',
                           'recall': 'recall_1',
                           'f1score': 'f1score_1'},
                  inplace=True)

all_label_df = pd.concat([label_0_df, label_1_df], axis=1)
all_label_df.to_excel(path + r"performance_metrics.xlsx")

print(f"Model finished training at {datetime.now()}")
duration = datetime.now() - start
print(f"Total training time consisted of: {duration}")
