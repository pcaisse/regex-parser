val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "regex-parser",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.4",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
      "org.scalameta" %% "munit-scalacheck" % "0.7.29" % "test"
    )
  )
