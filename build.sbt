name := "scalanotes"

version := "1.0"

scalaVersion := "2.11.0"


lazy val fppinscala = project.settings(
  libraryDependencies ++= Seq(
      "org.specs2" %% "specs2" % "2.3.11" % "test"
  )
)

lazy val neophytes = project
