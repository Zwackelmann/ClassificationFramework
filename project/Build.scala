import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object ClassificationFrameworkBuild extends Build {
    lazy val classificationFramework = Project(id = "classificationFramework", base = file(".")) dependsOn(weka)
    lazy val weka = ProjectRef(id = "weka", base = file("../Weka"))
	
	javaOptions in run += "-Xmx3000M"
	
	// workaround assembly duplication error for common files
	/*excludedFiles in assembly := {
		(bases: Seq[File]) => bases flatMap {
			base => (base / "META-INF" * "*").get collect {
				case f if f.getName.toLowerCase == "manifest.mf" => f
			}
		}
	}*/

	def ass = assemblySettings ++ Seq(
		mainClass in assembly := Some("testrun.scripts.Testrun3"),
		mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
			//case s: String if(s.length >= 8 && s.substring(0, 8).toLowerCase() == "meta-inf") => MergeStrategy.discard
			case _ => MergeStrategy.discard
			// case x => old(x)
		}}
	)
}