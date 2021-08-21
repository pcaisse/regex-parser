val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "regex-parser",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.4",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
