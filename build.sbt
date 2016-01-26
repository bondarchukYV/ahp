import AssemblyKeys._

assemblySettings

name := "ahp"

version := "0.9"

scalaVersion := "2.10.4"

scalacOptions += "-feature"

resolvers += "RR release" at "http://maven1.int.retailrocket.ru:8081/artifactory/release/"

resolvers += "RR snapshot" at "http://maven1.int.retailrocket.ru:8081/artifactory/snapshot/"

resolvers += "Maven Central" at "http://central.maven.org/maven2"

libraryDependencies += "ru.retailrocket.spark" %% "multitool" % "0.2-SNAPSHOT"
