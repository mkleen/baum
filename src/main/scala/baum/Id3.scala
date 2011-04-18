package baum

import scala.math._ 
/*
 * Splits datasets acording to http://en.wikipedia.org/wiki/ID3_algorithm
 */
object Id3 {
		
	def split[T](instances: Iterable[T],attributes: Iterable[Property[T]],target:Property[T]): SplitResult[T] = {
		// calculate a split for each attribute acording to the target
		val splits  = attributes map calculateGain(instances, target) _ 
		// find the one with the highest gain
		val sorted 	= splits.toList sortWith{ _.gain >= _.gain} 
		sorted head
	}
	
	def calculateGain[T](instances: Iterable[T],target:Property[T])(attribute: Property[T]): SplitResult[T] = {
		// split instances where the attribute is present or not
		val (positive, negative) = instances partition attribute._2
		// count tp/fp if the attribiute is present and the target is present
		val (tp, fp)			= count(positive partition target._2)
		// count tn/fn where the attribute is missing and the target is missing
		val (fn, tn)			= count(negative partition target._2)	
		// calculate total information gain 
	    val gain 				= entropy(tp, fp, fn, tn) - informationGain(tp, fp, fn, tn)
		gain match {
			case 0 	=> SplitFailure(attribute, ConfusionMatrix(tp, fp, tn, fn), gain)
			case _	=> SplitSucess(attribute, ConfusionMatrix(tp, fp, tn, fn), gain)	
		}
	}
		
	/*
	 * Shannons entropy http://en.wikipedia.org/wiki/Information_entropy
	 */
	def entropy[T](tp: Double, fp: Double, fn: Double, tn: Double): Double = {
	    val sumPos 	= tp + fn
	    val sumNeg 	= fp + tn
	    val sum 	= sumPos + sumNeg

	    // define 0log0+0log0 = 0
	    if (sum == 0 || sumPos == 0 || sumNeg == 0) return 0

	    val positivePart = -((tp + fn) / sum) * log2((tp + fn) / sum)
	    val negativePart = -((fp + tn) / sum) * log2((fp + tn) / sum)

	    positivePart + negativePart
	  }

	 /*
	  * Information gain http://en.wikipedia.org/wiki/Information_gain
	  */
	 def informationGain[T](tp: Double, fp: Double, fn: Double, tn: Double): Double = {
	    val aFactor = if (tp != 0) -(tp / (tp + fp)) * log2(tp / (tp + fp)) else 0.0
	    val bFactor = if (fp != 0) 	(fp / (tp + fp)) * log2(fp / (tp + fp)) else 0.0
	    val cFactor = if (fn != 0) -(fn / (fn + tn)) * log2(fn / (fn + tn)) else 0.0
	    val dFactor = if (tn != 0) 	(tn / (fn + tn)) * log2(tn / (fn + tn)) else 0.0
		val sum 	= tp + fp + fn + tn

	    ((tp + fp) / sum) * (aFactor - bFactor) + ((fn + tn) / sum) * (cFactor - dFactor)
	  }

	  def log2(x: Double): Double = log(x) / log(2)	
	
	 private def count[T](x:(Iterable[T], Iterable[T])) = (x._1.size, x._2.size)
	
}

