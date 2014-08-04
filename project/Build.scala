import sbt._
import Keys._

object ClassificationFrameworkBuild extends Build {
    lazy val classificationFramework = Project(id = "classificationFramework", base = file(".")) dependsOn(svmLightJni)
    // lazy val weka = ProjectRef(id = "weka", base = file("../Weka"))
	lazy val svmLightJni = ProjectRef(id = "SvmLightJni", base = file("../SVMLight_JNI"))
	lazy val classifyRun = TaskKey[Unit]("classify")
	lazy val testingRun = TaskKey[Unit]("testing")
}
