package baum

import scala.annotation.tailrec

sealed trait Id3Tree[T] { 	
	def property(): Property[T]
	
	def eval(t: T, callback: Boolean => Unit): Unit = {
		 @tailrec def rec[T](t: T, tree: Option[Id3Tree[T]]): Unit = {
			tree match {
				case Some(Id3Node(_, property, positive, negative, _))
					=> property.value apply t match {
							case true 	=> rec(t, positive)
							case false	=> rec(t, negative)
						}
				case Some(Id3Leaf(flag,_))
					=>  callback apply flag
				case None 
					=> // ignore  
			}	
		}
		rec(t, Some(this))
	}
}

object Id3Tree {
	
	val threshold = 0.7 // threshold for the maximum error 
	
	def apply[T](instances: Set[T],attributes: Set[Property[T]], target: Property[T]): Id3Tree[T] = {
		make[T](instances, attributes, target)
	}
		
	private def make[T](allInstances: Set[T], allAttributes: Set[Property[T]], target: Property[T]): Id3Tree[T] = {		
		def rec(flag: Boolean, instances: Set[T], attributes: Set[Property[T]]): Id3Tree[T] = 
			Id3.split(instances, attributes, target) match {
		 		case SplitSucess (property, confusionMatrix, _)	
					=>	val (positives, negatives)	= instances partition property.value 
						val newAttributes 			= attributes - property
						val (posNode, negNode)  	= if(confusionMatrix.error < threshold) {(
															Some(rec(true, positives, newAttributes)),
															Some(rec(true, negatives, newAttributes))
											  			)}	else (None, None) 		
						Id3Node[T](
							flag				= flag,
							positive			= posNode, 
							negative			= negNode, 
							property 			= property, 
							confusionMatrix 	= confusionMatrix
						)
		 		case SplitFailure(property, confusionMatrix, _) 
					=>  Id3Leaf[T](flag = flag, property = property)
		 	}
		rec(true, allInstances, allAttributes)
	}
}

/**
 * Unbalanced binary Tree 
 *
**/ 
case class Id3Leaf[T](
	flag: Boolean, 
	property: Property[T]
) extends Id3Tree[T] {
	override def toString() = "Id3Leaf[target:" + property.label + " " + flag + "]"	
}

case class Id3Node[T](
	flag: Boolean, 
	property: Property[T], 
	positive:Option[Id3Tree[T]], 
	negative:Option[Id3Tree[T]], 
	confusionMatrix:ConfusionMatrix
) extends Id3Tree[T] {
	override def toString() = "Id3Node[" + property.label + ", " + confusionMatrix + " positive:" + positive.getOrElse { " "} + " negative:" + negative.getOrElse { " "}  + "]" 
}





