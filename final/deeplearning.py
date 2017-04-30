# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import numpy as np
import pandas as pd
np.random.seed(1337) 
from keras.models import Sequential
from keras.layers import Dense, Activation
from keras.optimizers import RMSprop
from keras.utils import np_utils
data = pd.read_csv('D:/大四下/data science/final/try2.csv', header=0)
data = np.array(data)
X_train = data[0:7000,1:120]
X_test = data[7001:11000,1:120]
y_train = data[0:7000,0]
y_test = data[7001:11000,0]
y_train = np_utils.to_categorical(y_train, num_classes=2)
y_test = np_utils.to_categorical(y_test, num_classes=2)


model = Sequential([
    Dense(5, input_dim=120),
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
model.fit(X_train, y_train, epochs=100, verbose=2, batch_size=100)

print('\nTesting ------------')
# Evaluate the model with the metrics we defined earlier
loss, accuracy = model.evaluate(X_test, y_test)

print('test loss: ', loss)
print('test accuracy: ', accuracy)

pre = model.predict_classes(X_test)
y_test1 = y_test[:,1]
print(sum(pre==y_test1)/3999)