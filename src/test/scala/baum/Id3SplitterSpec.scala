package baum

import org.specs._
import org.specs.matcher._

class Id3SplitterSpec extends Specification {

	"""2 Instances are given where the target property is the divisibility by 6."""
	"""None of the Attribute have a true positive so they""" should {
		"return None" in { 
			val instances 	= List(7,  11)
			val attributes	= List(modulo(2), modulo(3), modulo(5)) 
			val target 		= modulo(7)
    		Id3.split(instances, attributes,target) must beLike { case SplitFailure((_,_),_,_) => true } 
		}
	}
	
	"""Integers are given three attributes, being devisibility by 2, 3 and 5.""" 
	"""The target property is divisibility by 6. Six instances are shown. Modulo 3 has """
	"""3 TP and 4 FP, Modulo 2 has 3 TP and 2 FP. Modulo 2"""  should {
		"returned as best split" in { 
			val instances 	= List(6,12,18,9,10,2,15,21,27)
			val attributes 	= List(modulo(2), modulo(3), modulo(5))
			val target 		= modulo(6)
	        Id3.split(instances, attributes,target) must beLike { case SplitSucess((_, Modulo(2)), ConfusionMatrix(3,2,4,0), _ ) => true } 
		}
	}
	
	"The target property divisibility by 6. 10 instances are shown. Modulo 3 has 7 TP, so it" should {
		"return as the best plit" in { 
			val instances 	= List(2, 3, 7, 8, 9, 10, 600, 612, 666, 672)
			val attributes 	= List(modulo(3))
			val target 		= modulo(6)
			Id3.split(instances, attributes,target) must beLike { case SplitSucess((_, Modulo(3)), ConfusionMatrix(4,2,4,0), _ ) => true } 			        
		}
	}
			
	def modulo(value: Int): (String, Int => Boolean) = ("", Modulo(value))	   
	
	case class Modulo(value: Int) extends Function1[Int, Boolean] {
		def apply(x: Int): Boolean = x % value == 0 
	}
}