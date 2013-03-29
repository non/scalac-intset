import sbt._
import sbt.Keys._

object MyBuild extends Build {
  override lazy val settings = super.settings ++ Seq(
    scalaVersion := "2.10.0",

    scalacOptions ++= Seq(
      "-Xlog-free-terms",
      "-feature",
      "-Yinline-warnings",
      "-deprecation",
      "-optimize",
      "-unchecked"
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.10.0"
    ),

    // raise memory limits here if necessary
    javaOptions in run += "-Xmx6G",

    libraryDependencies ++= Seq(
      "com.google.guava" % "guava" % "r09",
      "com.google.code.java-allocation-instrumenter" % "java-allocation-instrumenter" % "2.0",
      "com.google.code.caliper" % "caliper" % "1.0-SNAPSHOT" from "http://n0d.es/jars/caliper-1.0-SNAPSHOT.jar",
      "com.google.code.gson" % "gson" % "1.7.1"
    ),

    // enable forking in run
    fork in run := true,

    // custom kludge to get caliper to see the right classpath

    // we need to add the runtime classpath as a "-cp" argument to the
    // `javaOptions in run`, otherwise caliper will not see the right classpath
    // and die with a ConfigurationException. unfortunately `javaOptions` is a
    // SettingsKey and `fullClasspath in Runtime` is a TaskKey, so we need to
    // jump through these hoops here in order to feed the result of the latter
    // into the former
    onLoad in Global ~= { previous => state =>
      previous {
        state.get(key) match {
          case None =>
            // get the runtime classpath, turn into a colon-delimited string
            val classPath = Project.runTask(fullClasspath in Runtime in root, state).get._2.toEither.right.get.files.mkString(":")
            // return a state with javaOptionsPatched = true and javaOptions set correctly
            Project.extract(state).append(Seq(javaOptions in (root, run) ++= Seq("-cp", classPath)), state.put(key, true))

          case Some(_) => state // the javaOptions are already patched
        }
      }
    }

    // caliper stuff stolen shamelessly from scala-benchmarking-template
  )

  lazy val root = Project("test", file("."))

  val key = AttributeKey[Boolean]("javaOptionsPatched")

}
