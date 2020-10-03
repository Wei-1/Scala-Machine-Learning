# Scala Machine Learning

Branch | Status | CodeCov
-|-|-
[master](https://github.com/wei-1/scala-machine-learning/tree/master) | [![Build Status](https://travis-ci.org/Wei-1/Scala-Machine-Learning.svg?branch=master)](https://travis-ci.org/Wei-1/Scala-Machine-Learning) | [![codecov](https://codecov.io/gh/Wei-1/Scala-Machine-Learning/branch/master/graph/badge.svg)](https://codecov.io/gh/Wei-1/Scala-Machine-Learning)
[develop](https://github.com/wei-1/scala-machine-learning/tree/develop) | [![Build Status](https://travis-ci.org/Wei-1/Scala-Machine-Learning.svg?branch=develop)](https://travis-ci.org/Wei-1/Scala-Machine-Learning) | [![codecov](https://codecov.io/gh/Wei-1/Scala-Machine-Learning/branch/develop/graph/badge.svg)](https://codecov.io/gh/Wei-1/Scala-Machine-Learning)


## Light Weight Scala Machine Learning Library

A very light weight Scala machine learning library that provide some basic ML algorithms in Scala. The repo is served as a algorithm gallery. Please enjoy and dive into the algorithm that you will like to learn in its basic level.


## Dev-Environment

- Scala 2.12

- Sbt 1.2


## This package includes

### Classification :

- [x] Naive Bayesian Decision [[Code]](src/main/scala/algorithm/classification/BayesianDecision.scala) [[Usage]](src/test/scala/algorithm/classification/BayesianDecisionTest.scala)

- [x] K-Nearest Neighborhood (KNN) [[Code]](src/main/scala/algorithm/classification/KNN.scala) [[Usage]](src/test/scala/algorithm/classification/KNNTest.scala)

- [x] Gaussian Process Classification [[Code]](src/main/scala/algorithm/classification/GaussianProcess.scala) [[Usage]](src/test/scala/algorithm/classification/GaussianProcessTest.scala)

- [x] Linear Classification [[Code]](src/main/scala/algorithm/classification/LinearClassification.scala) [[Usage]](src/test/scala/algorithm/classification/LinearClassificationTest.scala)

- [x] Linear Support Vector Machine (linear-SVM) [[Code]](src/main/scala/algorithm/classification/LinearSVM.scala) [[Usage]](src/test/scala/algorithm/classification/LinearSVMTest.scala)

- [x] Perceptron [[Code]](src/main/scala/algorithm/classification/Perceptron.scala) [[Usage]](src/test/scala/algorithm/classification/PerceptronTest.scala)

- [x] Decision Tree [[Code]](src/main/scala/algorithm/classification/DecisionTree.scala) [[Usage]](src/test/scala/algorithm/classification/DecisionTreeTest.scala)

- [x] Random Forest [[Code]](src/main/scala/algorithm/classification/RandomForest.scala) [[Usage]](src/test/scala/algorithm/classification/RandomForestTest.scala)

- [x] Extreme Learning Machine [[Code]](src/main/scala/algorithm/classification/ExtremeLearning.scala) [[Usage]](src/test/scala/algorithm/classification/ExtremeLearningTest.scala)

### Boost :

- [x] Naive Boost [[Code]](src/main/scala/algorithm/classification/NaiveBoost.scala) [[Usage]](src/test/scala/algorithm/classification/NaiveBoostTest.scala)

- [x] Weighted Boost [[Code]](src/main/scala/algorithm/classification/WeightedBoost.scala) [[Usage]](src/test/scala/algorithm/classification/WeightedBoostTest.scala)

### Regression :

- [x] Multiple Linear Regression [[Code]](src/main/scala/algorithm/regression/MultipleLinearRegression.scala) [[Usage]](src/test/scala/algorithm/regression/MultipleLinearRegressionTest.scala)

- [x] Multivariate Linear Regression - GD [[Code]](src/main/scala/algorithm/regression/MultivariateLinearRegression.scala) [[Usage]](src/test/scala/algorithm/regression/MultivariateLinearRegressionTest.scala)

- [x] Stochastic Gradient Decent [[Code]](src/main/scala/algorithm/regression/MultivariateLinearRegression.scala) [[Usage]](src/test/scala/algorithm/regression/MultivariateLinearRegressionTest.scala)

### Clustering :

- [x] Hierarchical [[Code]](src/main/scala/algorithm/clustering/Hierarchical.scala) [[Usage]](src/test/scala/algorithm/clustering/HierarchicalTest.scala)

- [x] DBSCAN [[Code]](src/main/scala/algorithm/clustering/DBSCAN.scala) [[Usage]](src/test/scala/algorithm/clustering/DBSCANTest.scala)

- [x] HDBSCAN [[Code]](src/main/scala/algorithm/clustering/HDBSCAN.scala) [[Usage]](src/test/scala/algorithm/clustering/HDBSCANTest.scala)

- [x] BIRCH [[Code]](src/main/scala/algorithm/clustering/BIRCH.scala) [[Usage]](src/test/scala/algorithm/clustering/BIRCHTest.scala)

- [x] K-Means [[Code]](src/main/scala/algorithm/clustering/KMean.scala) [[Usage]](src/test/scala/algorithm/clustering/KMeanTest.scala)

- [x] EM Cluster [[Code]](src/main/scala/algorithm/clustering/EMCluster.scala) [[Usage]](src/test/scala/algorithm/clustering/EMClusterTest.scala)

- [x] Density Peak Cluster [[Code]](src/main/scala/algorithm/clustering/DensityPeakCluster.scala) [[Usage]](src/test/scala/algorithm/clustering/DensityPeakClusterTest.scala)

### Neural Net & Deep Learning :

- [x] Neural Network (NN) [[Code]](src/main/scala/algorithm/deeplearning/NeuralNetwork.scala) [[Usage]](src/test/scala/algorithm/deeplearning/NeuralNetworkTest.scala)

- [x] Restricted Boltzmann Machine (RBM) [[Code]](src/main/scala/algorithm/deeplearning/RBM.scala) [[Usage]](src/test/scala/algorithm/deeplearning/RBMTest.scala)

- [x] Deep Belief Network (DBN) [[Code]](src/main/scala/algorithm/deeplearning/DBN.scala) [[Usage]](src/test/scala/algorithm/deeplearning/DBNTest.scala)

- [x] Long Short-Term Memory (LSTM) [[Code]](src/main/scala/algorithm/deeplearning/LSTM.scala) [[Usage]](src/test/scala/algorithm/deeplearning/LSTMTest.scala)

- [x] Neural Turing Machine - Memory Searching Cognition [[Code (in another Repo)]](https://github.com/Wei-1/Scala-NTM)

### Optimization :

- [x] Gene Algorithm (GA) [[Code]](src/main/scala/algorithm/optimization/GeneAlgorithm.scala) [[Usage]](src/test/scala/algorithm/optimization/GeneAlgorithmTest.scala)

- [x] Minimax [[Code]](src/main/scala/algorithm/optimization/Minimax.scala) [[Usage]](src/test/scala/algorithm/optimization/Minimax.scala)

- [x] Monte Carlo Tree Search (MCTS) [[Code]](src/main/scala/algorithm/optimization/MCTS.scala) [[Usage]](src/test/scala/algorithm/optimization/MCTSTest.scala)

- [x] Epsilon Greedy Search [[Code]](src/main/scala/algorithm/optimization/EpsilonGreedy.scala) [[Usage]](src/test/scala/algorithm/optimization/EpsilonGreedyTest.scala)

- [x] Upper Confidence Bound [[Code]](src/main/scala/algorithm/optimization/UpperConfidenceBound.scala) [[Usage]](src/test/scala/algorithm/optimization/UpperConfidenceBoundTest.scala)

### Reinforcement Learning :

- [x] Naive Feedback [[Code]](src/main/scala/algorithm/reinforcement/NaiveFeedback.scala) [[Usage]](src/test/scala/algorithm/reinforcement/NaiveFeedbackTest.scala)

- [x] Q-Learning [[Code]](src/main/scala/algorithm/reinforcement/QLearning.scala) [[Usage]](src/test/scala/algorithm/reinforcement/QLearningTest.scala)

- [x] Q-Neural Learning [[Code]](src/main/scala/algorithm/reinforcement/QNeuralLearning.scala) [[Usage]](src/test/scala/algorithm/reinforcement/QNeuralLearningTest.scala)

- [x] Deep Q-Network (DQN) [[Code]](src/main/scala/algorithm/reinforcement/DQN.scala) [[Usage]](src/test/scala/algorithm/reinforcement/DQNTest.scala)

- [x] Dueling DQN (D-DQN) [[Code]](src/main/scala/algorithm/reinforcement/DDQN.scala) [[Usage]](src/test/scala/algorithm/reinforcement/DDQNTest.scala)

- [x] Asynchronous Advantage Actor-Critic (A3C) [[Code]](src/main/scala/algorithm/reinforcement/A3C.scala) [[Usage]](src/test/scala/algorithm/reinforcement/A3CTest.scala)

- [x] Prioritized Experience Replay (PER-DQN) [[Code]](src/main/scala/algorithm/reinforcement/PER.scala) [[Usage]](src/test/scala/algorithm/reinforcement/PERTest.scala)

### Feature Analysis :

- [x] Student-T Test [[Code]](src/main/scala/algorithm/analysis/StudentT.scala) [[Usage]](src/test/scala/algorithm/analysis/StudentTTest.scala)

- [x] ANOVA [[Code]](src/main/scala/algorithm/analysis/ANOVA.scala) [[Usage]](src/test/scala/algorithm/analysis/ANOVATest.scala)

- [x] Linear Discriminant Analysis [[Code]](src/main/scala/algorithm/analysis/LDA.scala) [[Usage]](src/test/scala/algorithm/analysis/LDATest.scala)

- [x] Quadratic Discriminant Analysis [[Code]](src/main/scala/algorithm/analysis/QDA.scala) [[Usage]](src/test/scala/algorithm/analysis/QDATest.scala)

### Feature Transformation :

- [x] One Hot Encoding [[Code]](src/main/scala/algorithm/transformation/OneHot.scala) [[Usage]](src/test/scala/algorithm/transformation/OneHotTest.scala)


## TODO

- [ ] Rainbow - Deep Reinforcement Learning

- [ ] Alpha-go Zero (MCTS-NN) - Deep Reinforcement Learning

- [ ] Neural Architect Search (NAS) - Neural Network & Deep Learning


## Installation Process

1. [Install sbt](https://www.scala-sbt.org/download.html)

2. Clone this project


## Test

    sbt test


## Build Jar

    sbt assembly
