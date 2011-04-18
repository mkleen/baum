import sbt._

final class BaumProject(info:ProjectInfo) extends DefaultProject(info) {
	val specs			= "org.scala-tools.testing"		%% "specs"	% "1.6.7.1"	% "test"
}