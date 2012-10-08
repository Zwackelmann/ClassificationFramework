name := "Classification Framework"

version := "1.0"

scalaVersion := "2.9.2"

libraryDependencies += "com.cybozu.labs" % "langdetect" % "1.1-20120112"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.20"

libraryDependencies += "postgresql" % "postgresql" % "9.1-901.jdbc4"

libraryDependencies += "org.apache.lucene" % "lucene-snowball" % "3.0.3"

libraryDependencies += "net.sf.json-lib" % "json-lib" % "2.4"

fullRunTask(classifyRun, Test, "script.ApplyFinalClassifier")

fullRunTask(testingRun, Test, "script.Test6")

javaOptions in classifyRun += "-Xmx2000m"

javaOptions in testingRun += "-Xmx2000m"

fork in classifyRun := true

fork in testingRun := true
