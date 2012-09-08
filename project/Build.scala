import sbt._
import Keys._

object ClassificationFrameworkBuild extends Build {
    lazy val classificationFramework = Project(id = "classificationFramework", base = file(".")) dependsOn(weka)
    lazy val weka = ProjectRef(id = "weka", base = file("../Weka"))
	
	lazy val classifyRun = TaskKey[Unit]("classify")
}