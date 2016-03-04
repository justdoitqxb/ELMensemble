package com.sir.model
import org.apache.spark.{Logging, SparkContext} 
import org.apache.spark.annotation.{DeveloperApi, Since}  
import org.apache.spark.mllib.linalg.Vector 
import org.apache.spark.rdd.RDD 
import org.apache.spark.util.Utils

class ELMEnsembleModel {
  
  require(trees.forall(_.algo == algo))
  require(numTrees > 0, "TreeEnsembleModel cannot be created without trees.") 

  private val sumWeights = math.max(treeWeights.sum, 1e-15) 

  /** 
   * Predicts for a single data point using the weighted sum of ensemble predictions. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  private def predictBySumming(features: Vector): Double = { 
    val treePredictions = trees.map(_.predict(features)) 
    blas.ddot(numTrees, treePredictions, 1, treeWeights, 1) 
  } 
 
  /** 
   * Classifies a single data point based on (weighted) majority votes. 
   */ 
  private def predictByVoting(features: Vector): Double = { 
    val votes = mutable.Map.empty[Int, Double] 
    trees.view.zip(treeWeights).foreach { case (tree, weight) => 
      val prediction = tree.predict(features).toInt 
      votes(prediction) = votes.getOrElse(prediction, 0.0) + weight 
    } 
    votes.maxBy(_._2)._1 
  } 

  /** 
   * Predict values for a single data point using the model trained. 
   * 
   * @param features array representing a single data point 
   * @return predicted category from the trained model 
   */ 
  def predict(features: Vector): Double = { 
   (algo, combiningStrategy) match { 
      case (Regression, Sum) => 
        predictBySumming(features) 
      case (Regression, Average) => 
        predictBySumming(features) / sumWeights 
      case (Classification, Sum) => // binary classification 
        val prediction = predictBySumming(features) 
        // TODO: predicted labels are +1 or -1 for GBT. Need a better way to store this info. 
        if (prediction > 0.0) 1.0 else 0.0 
      case (Classification, Vote) => 
        predictByVoting(features) 
      case _ => 
        throw new IllegalArgumentException( 
         "TreeEnsembleModel given unsupported (algo, combiningStrategy) combination: " + 
            s"($algo, $combiningStrategy).") 
     } 
  } 

  /** 
   * Predict values for the given data set. 
   * 
   * @param features RDD representing data points to be predicted 
   * @return RDD[Double] where each entry contains the corresponding prediction 
   */ 
  def predict(features: RDD[Vector]): RDD[Double] = features.map(x => predict(x)) 

  /** 
   * Java-friendly version of [[org.apache.spark.mllib.tree.model.TreeEnsembleModel#predict]]. 
   */ 
  def predict(features: JavaRDD[Vector]): JavaRDD[java.lang.Double] = { 
    predict(features.rdd).toJavaRDD().asInstanceOf[JavaRDD[java.lang.Double]] 
  } 

  /** 
   * Print a summary of the model. 
   */ 
  override def toString: String = { 
    algo match { 
      case Classification => 
        s"TreeEnsembleModel classifier with $numTrees trees\n" 
      case Regression => 
        s"TreeEnsembleModel regressor with $numTrees trees\n" 
      case _ => throw new IllegalArgumentException( 
        s"TreeEnsembleModel given unknown algo parameter: $algo.") 
    } 
  } 

  /** 
   * Print the full model to a string. 
   */ 
  def toDebugString: String = { 
    val header = toString + "\n" 
    header + trees.zipWithIndex.map { case (tree, treeIndex) => 
      s"  Tree $treeIndex:\n" + tree.topNode.subtreeToString(4) 
    }.fold("")(_ + _) 
  } 

  /** 
   * Get number of trees in ensemble. 
   */ 
  def numTrees: Int = trees.length 

}


object ELMEnsembleModel { 
  case class Metadata( 
    algo: String, 
    treeAlgo: String, 
    combiningStrategy: String, 
    treeWeights: Array[Double]) 

  /** 
   * Model data for model import/export. 
   * We have to duplicate NodeData here since Spark SQL does not yet support extracting subfields 
   * of nested fields; once that is possible, we can use something like: 
   *  case class EnsembleNodeData(treeId: Int, node: NodeData), 
   *  where NodeData is from DecisionTreeModel. 
   */ 
  case class EnsembleNodeData(treeId: Int, node: NodeData) 

    def save(sc: SparkContext, path: String, model: TreeEnsembleModel, className: String): Unit = { 
      // Create JSON metadata. 
      implicit val format = DefaultFormats 
      val ensembleMetadata = Metadata(model.algo.toString, model.trees(0).algo.toString, 
      model.combiningStrategy.toString, model.treeWeights) 
      val metadata = compact(render( 
        ("class" -> className) ~ ("version" -> thisFormatVersion) ~ 
          ("metadata" -> Extraction.decompose(ensembleMetadata)))) 
      sc.parallelize(Seq(metadata), 1).saveAsTextFile(Loader.metadataPath(path)) 

      // Create Parquet data. 
      val dataRDD = sc.parallelize(model.trees.zipWithIndex).flatMap { case (tree, treeId) => 
        tree.topNode.subtreeIterator.toSeq.map(node => NodeData(treeId, node)) 
      }.toDF() 
      dataRDD.write.parquet(Loader.dataPath(path)) 
    } 

   /** 
    * Read metadata from the loaded JSON metadata. 
    */ 
   def readMetadata(metadata: JValue): Metadata = { 
     implicit val formats = DefaultFormats 
      (metadata \ "metadata").extract[Metadata] 
   } 
 
   /** 
    * Load trees for an ensemble, and return them in order. 
    * @param path path to load the model from 
    * @param treeAlgo Algorithm for individual trees (which may differ from the ensemble's 
    *                 algorithm). 
    */ 
  def loadTrees( 
    sc: SparkContext, 
    path: String, 
    treeAlgo: String): Array[DecisionTreeModel] = { 
      val datapath = Loader.dataPath(path) 
      val sqlContext = SQLContext.getOrCreate(sc) 
      val nodes = sqlContext.read.parquet(datapath).rdd.map(NodeData.apply) 
      val trees = constructTrees(nodes) 
      trees.map(new DecisionTreeModel(_, Algo.fromString(treeAlgo))) 
    } 
} 