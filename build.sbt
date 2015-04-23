lazy val root = (project in file(".")).
  settings(
    name 		 := "fopltlmon",
    version      := "0.1",
    scalaVersion := "2.11.5",
    scalacOptions += "-feature",

    assemblyJarName in assembly := "fopltlmon.jar",
    test in assembly 			:= {},

    libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
	resolvers += Resolver.sonatypeRepo("public")
  )