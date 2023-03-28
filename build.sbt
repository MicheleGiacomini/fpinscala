name := "fpinscala"

ThisBuild / scalaVersion := "3.2.0"

ThisBuild / githubWorkflowBuild := Seq(
  WorkflowStep.Sbt(
    name = Some("Build project"),
    commands = List("test:compile")
  )
)

ThisBuild / scalacOptions ++= List(
  "-feature",
  "-deprecation",
  "-Ykind-projector:underscores",
  "-source:future"
)

ThisBuild / libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M3" % Test
