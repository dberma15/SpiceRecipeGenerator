# -*- coding: utf-8 -*-
"""
Created on Thu Dec 21 10:27:24 2017

@author: bermads1
"""

import os
os.environ["KERAS_BACKEND"] = "tensorflow"
import numpy as np
from tqdm import tqdm
import matplotlib.pyplot as plt

from keras.layers import Input
from keras.models import Model, Sequential
from keras.layers.core import Reshape, Dense, Dropout, Flatten
from keras.layers.advanced_activations import LeakyReLU
from keras.layers.convolutional import Conv2D, UpSampling2D
from keras.datasets import mnist
from keras.optimizers import Adam
from keras import backend as K
from keras import initializers
import scipy.misc
import pandas
K.set_image_dim_ordering('th')

# GAN starting input conditions. Can be changed (I should try changing this to 10)
randomDim = 100

# Load in data from spiceration.csv, the file contianing the parsed data from the recipe blog.

X_train=pandas.read_csv("spiceratio.csv")

spicenames=X_train.columns
X_train=np.array(X_train)

# Optimizer
adam = Adam(lr=0.0002, beta_1=0.5)

# Generator
generator = Sequential()
generator.add(Dense(100, input_dim=randomDim, kernel_initializer=initializers.RandomNormal(stddev=0.02)))
generator.add(Dense(75,activation="relu"))
generator.add(LeakyReLU(0.2))
generator.add(Dense(60,activation="relu"))
generator.add(LeakyReLU(0.2))
generator.add(Dense(46,activation="softmax"))
generator.compile(loss='binary_crossentropy', optimizer=adam)
# Discriminator
discriminator = Sequential()
discriminator.add(Dense(40, activation='relu', input_shape=(46,)))
discriminator.add(Dropout(0.3))
discriminator.add(Dense(40, activation='relu'))
discriminator.add(Dropout(0.3))
discriminator.add(Dense(1, activation='sigmoid'))
discriminator.compile(loss='binary_crossentropy', optimizer=adam,metrics=['accuracy'])


# Combined network
discriminator.trainable = False
ganInput = Input(shape=(randomDim,))
x = generator(ganInput)
ganOutput = discriminator(x)
gan = Model(inputs=ganInput, outputs=ganOutput)
gan.compile(loss='binary_crossentropy', optimizer=adam)

dLosses = []
gLosses = []

# Plot the loss from each batch
def plotLoss(epoch):
    dAccuracy=[x[1] for x in dLosses]
    dLoss=[x[0] for x in dLosses]
    plt.figure(figsize=(10, 8))
    plt.plot(dLoss, label='Discriminitive loss')
    plt.plot(dAccuracy,label='Discriminitive Accuracy')
    plt.plot(gLosses, label='Generative loss')
    plt.xlabel('Epoch')
    plt.ylabel('Loss')
    plt.legend()
    plt.savefig('dcgan_loss_epoch_%d.png' % epoch)


def train(epochs=1, batchSize=128):
    batchCount = X_train.shape[0] / batchSize
    print('Epochs:', epochs)
    print('Batch size:', batchSize)
    print('Batches per epoch:', batchCount)

    for e in range(1, epochs+1):
        print('-'*15, 'Epoch %d' % e, '-'*15)
           # Get a random set of input noise and images
        noise = np.random.normal(0, 1, size=[batchSize, randomDim])
        spiceBatch = X_train[np.random.randint(0, X_train.shape[0], size=batchSize)]
        # Generate fake MNIST images
        generatedRecipes = generator.predict(noise)
        X = np.concatenate([spiceBatch, generatedRecipes])
        # Labels for generated and real data
        yDis = np.zeros(2*batchSize)
        # One-sided label smoothing
        yDis[:batchSize] = 0.9
        # Train discriminator
        discriminator.trainable = True
        dloss = discriminator.train_on_batch(X, yDis)
        # Train generator
        noise = np.random.normal(0, 1, size=[batchSize, randomDim])
        yGen = np.ones(batchSize)
        discriminator.trainable = False
        gloss = gan.train_on_batch(noise, yGen)
        print(discriminator.evaluate(X,yDis))
        # Store loss of most recent batch from this epoch
        dLosses.append(dloss)
        gLosses.append(gloss)

	generator.save("spice_gan_generator.h5")
    discriminator.save("spice_gan_discriminator.h5")
    noise = np.random.normal(0, 1, size=[batchSize, randomDim])
    spiceBatch = X_train[np.random.randint(0, X_train.shape[0], size=batchSize)]
    # Generate fake MNIST images
    generatedRecipes = generator.predict(noise)
    spiceblends=pandas.DataFrame(generatedRecipes)
    spiceblends.columns=spicenames
    spiceblends=round(spiceblends,2)
    rowsum=spiceblends.sum(axis=1)
    spiceblends=spiceblends.divide(rowsum,axis=0)
    spiceblends.to_csv("generatedspiceblends.csv",index=False)
    # Plot losses from every epoch
    plotLoss(e)

if __name__ == '__main__':
    train(125, 1000)