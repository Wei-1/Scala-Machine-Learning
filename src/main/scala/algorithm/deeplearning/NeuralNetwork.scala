// Neural Network - The Hard Way
// Wei Chen
// 2018-10-02
// From Google's NN-Playground in TypeScript to Scala (merging Plyaground, State, and NN)
package com.scalaml.algorithm

/**
 * A node in a neural network. Each node has a state
 * (total input, output, and their respectively derivatives) which changes
 * after every forward and back propagation run.
 */
class Node(
    /**
     * Creates a new node with the provided id and activation function.
     */
    val id: String = null,
    /** Activation function that takes total input and returns node's output */
    val activation: ActivationFunction = null,
    val initZero: Boolean = false
) {
    /** List of input links. */
    var inputLinks = Array[Link]()
    var bias: Double = if(initZero) 1e-8 else 0.1
    /** List of output links. */
    var outputLinks = Array[Link]()
    /** Node input and output. */
    var totalInput: Double = 0.0
    var output: Double = 0.0
    /** Error derivative with respect to this node's output. */
    var outputDer: Double = 0.0
    /** Error derivative with respect to this node's total input. */
    var inputDer: Double = 0.0
    /**
     * Accumulated error derivative with respect to this node's total input since
     * the last update. This derivative equals dE/db where b is the node's
     * bias term.
     */
    var accInputDer: Double = 0.0
    /**
     * Number of accumulated err. derivatives with respect to the total input
     * since the last update.
     */
    var numAccumulatedDers: Double = 0.0

    /** Recomputes the node's output and returns it. */
    def updateOutput(): Double = {
        // Stores total input into the node.
        totalInput = bias + inputLinks.map(link => link.weight * link.source.output).sum
        output = activation.output(totalInput)
        output
    }

    /** Debug node */
    override def toString: String = id + "_" + outputLinks.map(_.weight).mkString(",")
}

/** Built-in error functions */
trait ErrorFunction {
    def error(output: Double, target: Double): Double
    def der(output: Double, target: Double): Double
}
object SQUARE extends ErrorFunction { // Square error only
    override def error(output: Double, target: Double): Double =
        0.5 * Math.pow(output - target, 2)
    override def der(output: Double, target: Double): Double =
        output - target
}

/** Built-in activation functions */
trait ActivationFunction {
    def output(x: Double): Double
    def der(x: Double): Double
}
object TANH extends ActivationFunction {
    override def output(x: Double): Double = Math.tanh(x)
    override def der(x: Double): Double = 1 - Math.pow(output(x), 2)
}
object RELU extends ActivationFunction {
    override def output(x: Double): Double = Math.max(0, x)
    override def der(x: Double): Double = if(x <= 0) 0 else 1
}
object SIGMOID extends ActivationFunction {
    override def output(x: Double): Double = 1 / (1 + Math.exp(-x))
    override def der(x: Double): Double = {
        val out = output(x)
        out * (1 - out)
    }
}
object LINEAR extends ActivationFunction {
    override def output(x: Double): Double = x
    override def der(x: Double): Double = 1
}

/** Build-in regularization functions */
trait RegularizationFunction {
    def output(w: Double): Double
    def der(w: Double): Double
}
object L1 extends RegularizationFunction {
    override def output(w: Double): Double = Math.abs(w)
    override def der(w: Double): Double = if(w < 0) -1 else if(w > 0) 1 else 0
}
object L2 extends RegularizationFunction {
    override def output(w: Double): Double = 0.5 * Math.pow(w, 2)
    override def der(w: Double): Double = w
}

/**
 * A link in a neural network. Each link has a weight and a source and
 * destination node. Also it has an internal state (error derivative
 * with respect to a particular input) which gets updated after
 * a run of back propagation.
 */
class Link(
    /**
    * Constructs a link in the neural network initialized with random weight.
    *
    * @param source The source node.
    * @param dest The destination node.
    * @param regularization The regularization function that computes the
    *     penalty for this weight. If null, there will be no regularization.
    */
    val source: Node,
    val dest: Node,
    val regularization: RegularizationFunction = null,
    val initZero: Boolean = false
) {
    val id: String = source.id + "-" + dest.id
    var weight: Double = if(initZero) 1e-8 else Math.random() - 0.5
    var isDead: Boolean = false
    /** Error derivative with respect to this weight. */
    var errorDer: Double = 0.0
    /** Accumulated error derivative since the last update. */
    var accErrorDer: Double = 0.0
    /** Number of accumulated derivatives since the last update. */
    var numAccumulatedDers: Double = 0.0
}

/**
 * A wrapper for all neural network functions
 */
class NeuralNetwork {
    var networkShape: Array[Int] = Array(4, 2)
    var activation: ActivationFunction = TANH
    var outputActivation: ActivationFunction = LINEAR
    var regularization: RegularizationFunction = null
    var inputIds: Array[String] = Array[String]()
    var initZero: Boolean = false // fix 0.0 for testing
    var index: Int = 0
    var updateIndex: Int = 0
    var batchSize: Int = 10
    var learningRate: Double = 0.03
    var regularizationRate: Double = 0.0
    var network = Array[Array[Node]]()
    /**
     * Builds a neural network.
     *
     * @param networkShape The shape of the network. E.g. [1, 2, 3, 1] means
     *   the network will have one input node, 2 nodes in first hidden layer,
     *   3 nodes in second hidden layer and 1 output node.
     * @param activation The activation function of every hidden node.
     * @param outputActivation The activation function for the output nodes.
     * @param regularization The regularization function that computes a penalty
     *     for a given weight (parameter) in the network. If null, there will be
     *     no regularization.
     * @param inputIds List of ids for the input nodes.
     */
    def config(
        _networkShape: Array[Int] = networkShape,
        _activation: ActivationFunction = activation,
        _outputActivation: ActivationFunction = outputActivation,
        _regularization: RegularizationFunction = regularization,
        _inputIds: Array[String] = inputIds,
        _initZero: Boolean = initZero,
        _index: Int = index,
        _updateIndex: Int = updateIndex,
        _batchSize: Int = batchSize,
        _learningRate: Double = learningRate,
        _regularizationRate: Double = regularizationRate
    ): Boolean = {
        try {
            /** Parameters */
            networkShape = _networkShape
            activation = _activation
            outputActivation = _outputActivation
            regularization = _regularization
            inputIds = _inputIds
            initZero = _initZero
            index = _index
            updateIndex = _updateIndex
            batchSize = _batchSize
            learningRate = _learningRate
            regularizationRate = _regularizationRate
            /** Network */
            val numLayers = networkShape.length
            var id = 1
            /** List of layers, with each layer being a list of nodes. */
            network = Array[Array[Node]]()
            for(layerIdx <- 0 until numLayers) {
                val isOutputLayer: Boolean = layerIdx == (numLayers - 1)
                val numNodes = networkShape(layerIdx)
                val currentLayer = (for(i <- 0 until numNodes) yield {
                    var nodeId = id.toString()
                    if(layerIdx == 0 && inputIds.size == numNodes) {
                        nodeId = inputIds(i)
                    } else {
                        id += 1
                    }
                    val node = new Node(
                        nodeId,
                        if(isOutputLayer) outputActivation else activation,
                        initZero
                    )
                    if(layerIdx >= 1) {
                        // Add links from nodes in the previous layer to this node.
                        network(layerIdx - 1).foreach { prevNode =>
                            val link = new Link(prevNode, node, regularization, initZero)
                            prevNode.outputLinks :+= link
                            node.inputLinks :+= link
                        }
                    }
                    node
                }).toArray
                network :+= currentLayer
            }
            true
        } catch { case e: Exception =>
            Console.err.println(e)
            false
        }
    }
    /**
     * Runs a forward propagation of the provided input through the provided
     * network. This method modifies the internal state of the network - the
     * total input and output of each node in the network.
     *
     * @param network The neural network.
     * @param inputs The input array. Its length should match the number of input
     *     nodes in the network.
     * @return The final output of the network.
     */
    def forwardProp(
        inputs: Array[Double]
    ): Array[Double] = {
        val inputLayer = network.head
        if(inputs.length != inputLayer.length) {
            Console.err.println("The number of inputs must match the number of nodes in the input layer")
            System.exit(1)
        }
        // Update the input layer.
        for(i <- 0 until inputLayer.length) {
            val node = inputLayer(i)
            node.output = inputs(i)
        }
        network.drop(1).foreach { currentLayer =>
            // Update all the nodes in this layer.
            for(node <- currentLayer) node.updateOutput()
        }
        getOutputNodes.map(_.output)
    }

    /**
     * Runs a backward propagation using the provided target and the
     * computed output of the previous call to forward propagation.
     * This method modifies the internal state of the network - the error
     * derivatives with respect to each node, and each weight
     * in the network.
     */
    def backProp(
        targets: Array[Double],
        errorFunc: ErrorFunction = SQUARE
    ): Unit = {
        val outputNodes = network.last
        if(targets.size != outputNodes.size) {
            Console.err.println(s"Outputs(${targets.size}) must match the output layer(${outputNodes.size})")
            System.exit(1)
        }
        // The output node is a special case. We use the user-defined error
        // function for the derivative.
        for((node, target) <- outputNodes.zip(targets)) {
            node.outputDer = errorFunc.der(node.output, target)
        }

        // Go through the layers backwards.
        for(layerIdx <- network.length - 1 to 1 by -1) {
            val currentLayer = network(layerIdx)
            // Compute the error derivative of each node with respect to:
            // 1) its total input
            // 2) each of its input weights.
            for(node <- currentLayer) {
                node.inputDer = node.outputDer * node.activation.der(node.totalInput)
                node.accInputDer += node.inputDer
                node.numAccumulatedDers += 1
            }

            // Error derivative with respect to each weight coming into the node.
            for(node <- currentLayer) {
                for(link <- node.inputLinks) {
                    if(!link.isDead) {
                        link.errorDer = node.inputDer * link.source.output
                        link.accErrorDer += link.errorDer
                        link.numAccumulatedDers += 1
                    }
                }
            }
            if(layerIdx > 1) {
                val prevLayer = network(layerIdx - 1)
                for(node <- prevLayer) {
                    // Compute the error derivative with respect to each node's output.
                    node.outputDer = 0.0
                    for(output <- node.outputLinks) {
                        node.outputDer += output.weight * output.dest.inputDer
                    }
                }
            }
        }
    }

    /**
     * Updates the weights of the network using the previously accumulated error
     * derivatives.
     */
    def updateWeights(): Unit = {
        for(currentLayer <- network.drop(1); node <- currentLayer) {
            // Update the node's bias.
            if(node.numAccumulatedDers > 0) {
                node.bias -= learningRate * node.accInputDer / node.numAccumulatedDers
                node.accInputDer = 0.0
                node.numAccumulatedDers = 0.0
            }
            // Update the weights coming into this node.
            for(link <- node.inputLinks) {
                if(!link.isDead) {
                    if(link.numAccumulatedDers > 0) {
                        // Update the weight based on dE/dw.
                        link.weight = link.weight -
                            (learningRate / link.numAccumulatedDers) * link.accErrorDer
                        // Further update the weight based on regularization.
                        val regulDer = if(link.regularization != null) link.regularization.der(link.weight) else 0.0
                        val newLinkWeight = link.weight - (learningRate * regularizationRate) * regulDer
                        if(link.regularization != null &&
                            link.regularization.der(2) == 1 && // regularization == L1
                            link.weight * newLinkWeight < 0) {
                            // The weight crossed 0 due to the regularization term. Set it to 0.
                            link.weight = 0.0
                            link.isDead = true
                        } else {
                            link.weight = newLinkWeight
                        }
                        link.accErrorDer = 0.0
                        link.numAccumulatedDers = 0.0
                    }
                }
            }
        }
    }

    /** Iterates over every node in the network/ */
    def forEachNode(
        ignoreInputs: Boolean,
        accessor: Node => Unit
    ): Unit = {
        if(ignoreInputs) network.drop(1)
        else network
    }.foreach { currentLayer =>
        currentLayer.foreach(node => accessor(node))
    }

    /** Returns the output node in the network. */
    def getOutputNodes: Array[Node] =
        network.last

    /** Reset the network parameters and iteration pointer */
    def reset(onStartup: Boolean = false): Boolean = {
        networkShape = Array(4, 2)
        activation = TANH
        outputActivation = LINEAR
        regularization = null
        inputIds = Array[String]()
        initZero = false
        index = 0
        updateIndex = 0
        batchSize = 10
        learningRate = 0.03
        regularizationRate = 0.0
        config()
    }

    /** Clear current network */
    def clear() = reset(false)

    /** Train one inputs to one targets, moved and Modified from Playground. */
    def trainOne(inputs: Array[Double], targets: Array[Double], errorFunc: ErrorFunction = SQUARE): Unit = {
        forwardProp(inputs)
        backProp(targets, errorFunc)
        if((index - updateIndex + 1) % batchSize == 0) {
            updateIndex = index
            updateWeights()
        }
        index += 1
    }

    /** Predict one inputs */
    def predictOne = forwardProp _

    /** Difference between outputs and targets, moved and Modified from Playground. */
    def LossOne(inputs: Array[Double], targets: Array[Double], errorFunc: ErrorFunction = SQUARE): Double = {
        forwardProp(inputs).zip(targets).map { case (output, target) =>
            errorFunc.error(output, target)
        }.sum
    }

    /** Train all data */
    def train(x: Array[Array[Double]], y: Array[Array[Double]], errorFunc: ErrorFunction = SQUARE, iter: Int = 1, _learningRate: Double = learningRate): Boolean = try {
        learningRate = _learningRate
        val data = x.zip(y)
        for(i <- 0 until iter)
            data.foreach { case (inputs, targets) => trainOne(inputs, targets, errorFunc) }
        true
    } catch { case e: Exception =>
        Console.err.println(e)
        false
    }

    /** Predict all data */
    def predict(data: Array[Array[Double]]): Array[Array[Double]] = data.map(inputs => forwardProp(inputs))
}