import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.github.xmanu.virtual-classes",
    version := "0.1",
    scalacOptions ++= Seq("-deprecation", "-feature"),
    scalaVersion := "2.11.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      run <<= run in Compile in core
    )
  ) aggregate(macros, core, graph)

  lazy val macros: Project = Project(
    "virtual-classes-macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _)
	  )
  )

  lazy val core: Project = Project(
    "virtual-classes-core",
    file("core"),
    settings = buildSettings ++ Seq(
		libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test")
  ) dependsOn(macros)
  
  lazy val graph: Project = Project(
	  "virtual-classes-graph-example",
	  file("graph-example"),
      settings = buildSettings ++ Seq(
  		libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test",
		libraryDependencies += "org.scalafx" %% "scalafx" % "2.2.60-R9",
		fork := true)
  ) dependsOn(macros)
}
