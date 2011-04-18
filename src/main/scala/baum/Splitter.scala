package baum

sealed trait SplitResult[T] {
	def value(): Property[T]
	def confusionMatrix(): ConfusionMatrix
	def gain(): Double
}

case class SplitSucess[T](
	value: Property[T], 
	confusionMatrix: ConfusionMatrix, 
	gain: Double
) extends SplitResult[T]

case class SplitFailure[T](
	value: Property[T], 
	confusionMatrix: ConfusionMatrix,
	gain: Double
)  extends SplitResult[T] 



