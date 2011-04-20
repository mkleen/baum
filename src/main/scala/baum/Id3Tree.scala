package baum

import scala.annotation.tailrec

sealed trait Id3Tree[T] { 	
	def label(): String
	def value(): T => Boolean
	
	def eval(t: T, callback: Boolean => Unit): Unit = {
		 @tailrec def rec[T](t: T, tree: Option[Id3Tree[T]]): Unit = {
			tree match {
				case Some(Id3Node(_, value, label, positive, negative, _))
					=> value apply t match {
							case true 	=> rec(t, positive)
							case false	=> rec(t, negative)
						}
				case Some(Id3Leaf(flag,_, _))
					=>  callback apply flag
				case None 
					=> // ignore  
			}	
		}
		rec(t, Some(this))
	}
}

object Id3Tree {
	
	val threshold = 0.7
	
	def apply[T](instances: Set[T],attributes: Set[Property[T]], target: Property[T]): Id3Tree[T] = {
		make[T](instances, attributes, target)
	}
		
	private def make[T](allInstances: Set[T], allAttributes: Set[Property[T]], target: Property[T]): Id3Tree[T] = {		
		def rec(flag: Boolean, instances: Set[T], attributes: Set[Property[T]]): Id3Tree[T] = 
			Id3.split(instances, attributes, target) match {
		 		case SplitSucess (value, confusionMatrix, _)	
					=>	val splits			= instances.partition(value._2) 
						val newAttributes 	= attributes - value
						val (pos, neg)  	= if(confusionMatrix.error < threshold) {
							 					(Some(rec(true, splits._1, newAttributes)),Some(rec(true, splits._2, newAttributes))) }
											  else (None, None) 		
						Id3Node[T](
							flag				= flag,
							positive			= pos, 
							negative			= neg, 
							value 				= value._2, 
							confusionMatrix 	= confusionMatrix,
							label 				= value._1	
						)
		 		case SplitFailure(value, confusionMatrix, _) 
					=>  Id3Leaf[T](flag = flag, value = target._2, label = target._1)
		 	}
		rec(true, allInstances, allAttributes)
	}
}

case class Id3Leaf[T](
	flag: Boolean, 
	value: T => Boolean,
	label: String
) extends Id3Tree[T] {
	override def toString() = "Id3Leaf[target:" + label + " " + flag + "]"	
}

case class Id3Node[T](
	flag: Boolean, 
	value: T => Boolean, 
	label: String, 
	positive:Option[Id3Tree[T]], 
	negative:Option[Id3Tree[T]], 
	confusionMatrix:ConfusionMatrix
) extends Id3Tree[T] {
	override def toString() = "Id3Node[" + label + ", " + confusionMatrix + " positive:" + positive.getOrElse { " "} + " negative:" + negative.getOrElse { " "}  + "]" 
}





