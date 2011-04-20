package baum

import org.specs._
import org.specs.matcher._

class WeatherExample1 extends Specification {
	// Trainingsdata
	val data = Set(
		Day("a", 	Sunny, 		Hot, 	High, 	false,	false),
		Day("b", 	Sunny, 		Hot, 	High, 	true, 	false),
		Day("c", 	Overcast,	Hot, 	High, 	false,	true),
		Day("d", 	Rainy, 		Mild, 	High, 	false, 	true),
		Day("e", 	Rainy, 		Cool, 	Normal, false, 	true),
		Day("f", 	Rainy, 		Cool, 	Normal, true, 	false),
		Day("g", 	Overcast, 	Cool, 	Normal, true, 	true),	
		Day("h", 	Sunny, 		Mild, 	High, 	false, 	false),
		Day("i", 	Sunny, 		Cool, 	Normal, false, 	true),
		Day("j", 	Rainy, 		Mild, 	Normal, false, 	true),
		Day("k", 	Sunny, 		Mild, 	Normal, true, 	true),	
		Day("l", 	Overcast, 	Mild, 	High, 	true, 	true),
		Day("m", 	Overcast, 	Hot, 	High,  false, 	true),
		Day("n", 	Rainy, 		Mild, 	High, 	true, 	false)	
	)
	
	// define the attributes
	val attributes = Set[Property[Day]](
						Attribute("Outlook: Sunny" , { day: Day => day.outlook == Sunny }), 
						Attribute("Outlook: Overcast", { day: Day => day.outlook == Overcast }), 
						Attribute("Outlook: Rainy", 	{ day: Day => day.outlook == Rainy 	}), 
						Attribute("Temperature: Hot", { day: Day => day.temperature == Hot }), 
						Attribute("Temperature: Mild", { day: Day => day.temperature == Mild }), 
						Attribute("Temperature: Cool", { day: Day => day.temperature == Cool }),
						Attribute("Humidity: High", { day: Day => day.humidity == High 	}),
						Attribute("Humidity: Normal",{ day: Day => day.humidity == Normal }),
						Attribute("Windy", { day: Day => day.windy }) 
					)
	
	// define the targets					
	val target 		= Target("Play", {day: Day => day.play})
	
	val tree = Id3Tree(data, attributes, target) 
	
	val day1 = 	Day("x", 	Overcast, 	Hot, 	High,  false, 	true)
	tree.eval(day1, { x => println("Shall we go out playing? " + x ) } )
	
	val day2 = 	Day("y", 	Rainy, 		Cool, 	Normal, false, 	true)
	tree.eval(day2, { x => println("Shall we go out playing? " + x ) } )
	
	case class Day(id: String, outlook: Outlook, temperature: Temperature, humidity: Humidity, windy: Boolean, play: Boolean) 						
			
	sealed abstract class Outlook
	object Sunny extends Outlook 
	object Overcast extends Outlook
	object Rainy extends Outlook 

	sealed abstract class Temperature
	object Hot extends Temperature
	object Mild extends Temperature
	object Cool extends Temperature

	sealed abstract class Humidity
	object High extends Humidity
	object Normal extends Humidity 
}