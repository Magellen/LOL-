# -*- coding: utf-8 -*-
"""
Created on Sun Apr 23 14:54:22 2017

@author: XMKZ
"""

import numpy as np
np.random.seed(1337)  # for reproducibility
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
import pandas as pd

xdata = pd.read_csv('D:/大四下/data science/internship/house/X.csv', header=0)
ydata = pd.read_csv('D:/大四下/data science/internship/house/3class.csv', header=0)
X, y = xdata, ydata
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)
stdsc = StandardScaler()
X_train_std = stdsc.fit_transform(X_train)
X_test_std = stdsc.fit_transform(X_test)

X_train_std= X_train_std.astype('float32')
X_test_std= X_test_std.astype('float32')
y_train=y_train.astype('float32')
y_test= y_test.astype('float32')
y_train = np_utils.to_categorical(y_train, num_classes=2)
y_test = np_utils.to_categorical(y_test, num_classes=2)
'''
ydata = np.array(ydata)
xdata = np.array(xdata)
ydata = ydata-1
xdata= xdata.astype('float32')
ydata= ydata.astype('float32')
X_train = xdata[0:7600,:]
X_test = xdata[7601:10671,:]
y_train = ydata[0:7600]
y_test = ydata[7601:10671]
'''

model = Sequential([
    Dense(5, input_dim=13),
    Activation('relu'),
    Dense(3),
    Activation('softmax')
])

rmsprop = RMSprop(lr=0.0001)

model.compile(optimizer=rmsprop,
              loss='categorical_crossentropy',
               metrics=['accuracy'])

print('Training ------------')
# Another way to train the model
model.fit(X_train_std, y_train, epochs=1000, verbose=2,  shuffle=True, batch_size=1000)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(X_test_std, y_test)

print('test loss: ', loss)
print('test accuracy: ', accuracy)
'''
pre = model.predict_classes(X_test)
print(sum(pre==y_test)/3000)
'''