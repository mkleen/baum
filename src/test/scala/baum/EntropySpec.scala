package baum

import org.specs._
import org.specs.matcher._

class EntropySpec extends Specification {
	
	"Entropy is 0" in  {
		 Id3.entropy(10, 0, 0, 0) must be equalTo(0D)
	     Id3.entropy(0, 0, 0, 10) must be equalTo(0D)
	     Id3.entropy(0, 10, 0, 10) must be equalTo(0D)
	     Id3.entropy(10, 0, 10, 0) must be equalTo(0D)
	     Id3.entropy(0, 0, 0, 0) must be equalTo(0D)
     }
	"Entropy is 1" in  {
	     Id3.entropy(10, 0, 0, 10) must be equalTo(1D)
	     Id3.entropy(10, 0, 0, 10) must be equalTo(1D)
	     Id3.entropy(0, 5, 5, 0) must be equalTo(1D)
	}	
	"Entropy < 1" in {
		Id3.entropy(11, 0, 0, 10) must beLessThan(1D)
		Id3.entropy(10, 0, 0, 11) must beLessThan(1D)
		Id3.entropy(11, 0, 5, 18) must beLessThan(1D)
	}	 
	"Entropy > 0" in {
		Id3.entropy(11, 0, 0, 10) must beGreaterThan(0D)
		Id3.entropy(10, 0, 0, 11) must beGreaterThan(0D)
		Id3.entropy(11, 0, 5, 18) must beGreaterThan(0D)
	}
}