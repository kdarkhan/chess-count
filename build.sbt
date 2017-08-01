name := "chess-count"

version := "1.0"

scalaVersion := "2.12.2"


libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.9" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

