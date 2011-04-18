package baum

trait Properties[T] {
	def label(): String
	def value(): T => Boolean
}

