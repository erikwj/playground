
name := "ItemPool"

organization := "com.faqtfinding"

version := "0.1-SNAPSHOT"

libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "2.4.2" % "test",
    "org.scalaz" %% "scalaz-core" % "7.1.0",
    "com.typesafe.play" %% "play-json" % "2.3.4")     

scalaVersion := "2.11.2"

//libraryDependencies += "com.faqtfinding" %% "itempool" % "0.1-SNAPSHOT"