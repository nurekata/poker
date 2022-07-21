ThisBuild / scalaVersion := "3.1.2"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / githubWorkflowPublishTargetBranches := Seq()

lazy val root = project
   .in(file("."))
   .settings(
      name := "poker",
      version := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq("-source:future"),
      libraryDependencies ++= Seq(
         "org.scalameta" %% "munit" % "0.7.29" % Test,
         "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
      )
   )
