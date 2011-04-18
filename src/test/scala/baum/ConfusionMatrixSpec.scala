package baum

import org.specs._
import org.specs.matcher._

class ConfusionMatrixSpec extends Specification {
	
     """Tests the actual error rate.If there are only positive instances"""
     """correctly classified and no fp the error""" should {
	 	"be less than 0.05." in {
			new ConfusionMatrix(100,0,0,0).error  must beLessThan(0.05)	
		} 
	}
}
