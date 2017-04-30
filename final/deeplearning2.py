# -*- coding: utf-8 -*-
"""
Created on Wed Apr 12 21:07:00 2017

@author: XMKZ
"""

import numpy as np
np.random.seed(1337)  # for reproducibility
from keras.utils import np_utils
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
import pandas as pd

xdata = pd.read_csv('D:/大四下/data science/final/VX.csv', header=0)
ydata = pd.read_csv('D:/大四下/data science/final/VY.csv', header=0)
ydata = np.array(ydata)
xdata = np.array(xdata)
ydata = ydata-1
xdata= xdata.astype('float32')
ydata= ydata.astype('float32')
X_train = xdata[0:7600,:]
X_test = xdata[7601:10671,:]
y_train = ydata[0:7600]
y_test = ydata[7601:10671]


model = Sequential([
    Dense(5, input_dim=52),
    Activation('relu'),
    Dense(2),
    Activation('softmax')
])

rmsprop = RMSprop(lr=0.001)

model.compile(optimizer=rmsprop,
              loss='categorical_crossentropy',
               metrics=['accuracy'])

print('Training ------------')
# Another way to train the model
model.fit(X_train, y_train, epochs=100, verbose=2, validation_split=0.1, shuffle=True, batch_size=1000)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(X_test, y_test)

print('test loss: ', loss)
print('test accuracy: ', accuracy)

pre = model.predict_classes(X_test)
print(sum(pre==y_test)/3000)