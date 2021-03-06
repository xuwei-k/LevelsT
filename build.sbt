def Scala213 = "2.13.6"

scalaVersion := Scala213
crossScalaVersions := Seq(Scala213, "3.1.0-RC1")

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
        "org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full
      )
    )
  }
}

scalapropsCoreSettings

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.5" cross CrossVersion.for3Use2_13
libraryDependencies += "com.github.scalaprops" %% "scalaprops-scalaz" % "0.8.3" % "test"
libraryDependencies += "com.github.scalaprops" %% "scalaprops" % "0.8.3" % "test"
