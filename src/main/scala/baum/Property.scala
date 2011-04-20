package baum

trait Property[T] {
	val label: String
	val value: T => Boolean
}

case class Attribute[T](
	label: String, 
	value: T => Boolean
	) extends Property[T]
	
case class Target[T](
	label: String, 
	value: T => Boolean
	) extends Property[T]
