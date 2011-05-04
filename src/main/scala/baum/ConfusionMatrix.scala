package baum

import scala.math._

case class ConfusionMatrix(tp: Int, fp: Int, tn: Int, fn: Int) {	
	   
   /**
	* This method derives the estimated error by counting, using the number of true positive, false positive, 
	* true negatives positive examples in the training set. Given the number of example instances 
	* it calculates which rules lie above a given threshold in 95% of all cases.
	* Estimated errors rate e-hat(e,n,z) = (e + z^2/2n + z * sqrt(e/n - e^2/n + z^2/4n^2))/(1 + z^2/n)
	* it is derived from Page 165 "Estimating error rates" from Ian H. Witten, Eibe Frank - 
	* Data Mining: Practical Machine Learning Tools and Techniques 	
	*/
	def error():Double = {	
	    val zscore 			=  1.6448536283610324 // 0.95, normal distribution inverse cumulative probability
        val all 			=  tp.doubleValue + fp.doubleValue + tn.doubleValue + fn.doubleValue
        val observedError 	=  (fp.doubleValue + fn.doubleValue)/all  

        if (observedError == 0) return 0

        val term2 = (zscore * zscore) / (2.0 * all)
        val term3 = observedError / all - (observedError * observedError) / all + (zscore * zscore) / (4.0 * all * all)
        val term4 = (1.0 + zscore * zscore / all)

        (observedError + term2 + zscore * sqrt(term3)) / term4
	}	
}