def Scala213 = "2.13.13"

scalaVersion := Scala213
crossScalaVersions := Seq(Scala213, "3.4.1")

scalacOptions ++= {
  if (scalaBinaryVersion.value == "3") {
    Seq(
      "-Ykind-projector"
    )
  } else {
    Nil
  }
}

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "3") {
    Nil
  } else {
    Seq(
      compilerPlugin(
        "org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full
      )
    )
  }
}

scalapropsCoreSettings

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.8"
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalaz" % "0.9.1" % "test"
libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.9.1" % "test"
