package baum

import org.specs._
import org.specs.matcher._

class Id3SplitterSpec extends Specification {

	"""2 Instances are given where the target property is the divisibility by 6."""
	"""None of the Attribute have a true positive so they""" should {
		"return None" in { 
			val instances 	= List(7,  11)
			val attributes	= List(Modulo(2), Modulo(3), Modulo(5)) 
			val target 		= Modulo(7)
    		Id3.split(instances, attributes,target) must beLike { case SplitFailure(_,_,_) => true } 
		}
	}
	
	"""Integers are given three attributes, being devisibility by 2, 3 and 5.""" 
	"""The target property is divisibility by 6. Six instances are shown. Modulo 3 has """
	"""3 TP and 4 FP, Modulo 2 has 3 TP and 2 FP. Modulo 2"""  should {
		"returned as best split" in { 
			val instances 	= List(6,12,18,9,10,2,15,21,27)
			val attributes 	= List(Modulo(2), Modulo(3), Modulo(5))
			val target 		= Modulo(6)
	        Id3.split(instances, attributes,target) must beLike { case SplitSucess(Modulo(2), ConfusionMatrix(3,2,4,0), _ ) => true } 
		}
	}
	
	"The target property divisibility by 6. 10 instances are shown. Modulo 3 has 7 TP, so it" should {
		"return as the best plit" in { 
			val instances 	= List(2, 3, 7, 8, 9, 10, 600, 612, 666, 672)
			val attributes 	= List(Modulo(3))
			val target 		= Modulo(6)
			Id3.split(instances, attributes,target) must beLike { case SplitSucess(Modulo(3), ConfusionMatrix(4,2,4,0), _ ) => true } 			        
		}
	}
			
	case class Modulo(div: Int) extends Property[Int] {
		val label = ""
		val value = {x: Int => x % div == 0}
		
		override def hashCode = div.hashCode
		override def equals(that: Any) = that match {
			case other: Modulo	=> other.div == this.div
  			case _				=> false
		}
	}
}