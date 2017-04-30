# -*- coding: utf-8 -*-
"""
Created on Sun Apr 16 11:47:47 2017

@author: XMKZ
"""

import numpy as np
np.random.seed(1337)  # for reproducibility
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
import pandas as pd

xdata = pd.read_csv('D:/大四下/data science/final/TX.csv', header=0)
ydata = pd.read_csv('D:/大四下/data science/final/TY.csv', header=0)
ydata = np.array(ydata)
xdata = np.array(xdata)
xdata= xdata.astype('float32')
ydata= ydata.astype('float32')-1
X_train = xdata[0:3830,:]
X_test = xdata[3831:5471,:]
y_train = ydata[0:3830,]
y_test = ydata[3831:5471,]
y_train = np_utils.to_categorical(y_train, num_classes=2)
y_test = np_utils.to_categorical(y_test, num_classes=2)

model = Sequential([
    Dense(5, input_dim=60),
    Activation('relu'),
    Dense(2),
    Activation('softmax'),
])

rmsprop = RMSprop(lr=0.0001)

model.compile(optimizer=rmsprop,
              loss='categorical_crossentropy',
              metrics=['accuracy'])

print('Training ------------')
# Another way to train the model
model.fit(X_train, y_train, epochs=1000, verbose=2, validation_split=0.1, shuffle=True, batch_size=1000)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(X_test, y_test)

print('test loss: ', loss)
print('test accuracy: ', accuracy)

pre = model.predict_classes(X_test)
y_test1 = y_test[:,1]
print(sum(pre==y_test1)/3000)