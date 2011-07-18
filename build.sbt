name := "FileSync"

scalaVersion := "2.9.0-1"

resolvers += "Akka Repo" at "http://akka.io/repository"

libraryDependencies += "se.scalablesolutions.akka" % "akka-remote" % "1.1.3"

libraryDependencies += "se.scalablesolutions.akka" % "akka-scalaz" % "1.1.3"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.0-1"

libraryDependencies += "org.scala-tools.testing" %% "scalacheck" % "1.9" % "test"

initialCommands := """
    import com.futurenotfound.filesync._
    import java.io.File
    import scalaz._
    import scalaz.Scalaz._
"""

traceLevel := 0
