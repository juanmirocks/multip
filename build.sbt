name := "multip"

mainClass in (Compile, run) := Some("multip.Main")

version := "0.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies  ++= Seq(
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.scalanlp" %% "breeze-natives" % "0.11.2",
  "org.scalanlp" %% "breeze-viz" % "0.11.2",
  //
  "org.clapper" %% "argot" % "1.0.3",
  "org.apache.commons" % "commons-math3" % "3.2",
  //
  "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"
)

resolvers ++= Seq(
  // other resolvers here
  "Scala Tools Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

scalaVersion := "2.11.7"
