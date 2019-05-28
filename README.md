# Scala Machine Learning

Branch | Status | CodeCov
-|-|-
[master](https://github.com/wei-1/scala-machine-learning/tree/master) | [![Build Status](https://travis-ci.org/Wei-1/Scala-Machine-Learning.svg?branch=master)](https://travis-ci.org/Wei-1/Scala-Machine-Learning) | [![Codecov](https://codecov.io/gh/wei-1/scala-machine-learning/branch/master/graph/badge.svg)](https://codecov.io/gh/wei-1/scala-machine-learning)
[develop](https://github.com/wei-1/scala-machine-learning/tree/develop) | [![Build Status](https://travis-ci.org/Wei-1/Scala-Machine-Learning.svg?branch=develop)](https://travis-ci.org/Wei-1/Scala-Machine-Learning) | [![Codecov](https://codecov.io/gh/wei-1/scala-machine-learning/branch/develop/graph/badge.svg)](https://codecov.io/gh/wei-1/scala-machine-learning)


## Light Weight Scala Machine Learning Library

A very light weight Scala machine learning library that provide some basic ML algorithms in Scala. The repo is served as a algorithm gallery. Please enjoy and dive into the algorithm that you will like to learn in its basic level.


## Dev-Environment

- Scala 2.12

- Sbt 1.2


## This package includes

### Classification :

- [x] Naive Bayesian Decision

- [x] K-Nearest Neighborhood (KNN)

- [x] Gaussian Process Classification

- [x] Linear Classification

- [x] Linear Support Vector Machine (linear-SVM)

- [x] Perceptron

- [x] Decision Tree

- [x] Random Forest

### Boost :

- [x] Naive Boost

- [x] Weighted Boost

### Regression :

- [x] Multiple Linear Regression

- [x] Multivariate Linear Regression - GD

### Clustering :

- [x] Hierarchical

- [x] DBSCAN

- [x] HDBSCAN

- [x] BIRCH

- [x] K-Means

- [x] EM Cluster

- [x] Density Peak Cluster

### Neural Net & Deep Learning :

- [x] Neural Network (NN)

- [x] Restricted Boltzmann Machine (RBM)

- [x] Deep Belief Network (DBN)

- [x] Long Short-Term Memory (LSTM)

- [x] Neural Turing Machine - Memory Searching Cognition [(Code in a Different Repo)](https://github.com/Wei-1/Scala-NTM)

### Optimization :

- [x] Gene Algorithm (GA)

- [x] Minimax

- [x] Monte Carlo Tree Search

### Reinforcement Learning :

- [x] Naive Feedback

- [x] Q-Learning

- [x] Q-Neural Learning

- [x] Deep Q-Network (DQN)

- [x] Double DQN (D-DQN)

- [x] Asynchronous Advantage Actor-Critic (A3C)

### Feature Analysis :

- [x] Student-T Test

- [x] ANOVA

- [x] Linear Discriminant Analysis

- [x] Quadratic Discriminant Analysis


## TODO

- [ ] Alpha-go Zero (MCTS-NN) - Deep Reinforcement Learning


## Installation Process

1. [Install sbt](https://www.scala-sbt.org/download.html)

2. Clone this project

```
git clone https://github.com/Wei-1/Scala-Machine-Learning.git
```


## Test

    sbt test


## Build Jar

    sbt assembly
