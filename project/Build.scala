import sbt._
import Keys._

object ClassificationFrameworkBuild extends Build {
    lazy val classificationFramework = Project(id = "classificationFramework", base = file(".")) dependsOn(weka)
    lazy val weka = ProjectRef(id = "weka", base = file("../Weka"))
	
	// this lazy val has to go in a full configuration
	val classifyRun = TaskKey[Unit]("classify", "run the classificaton framework")
	fork in classifyRun := true
    javaOptions in classifyRun += "-Xmx8000m"
}