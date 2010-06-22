import sbt._

class HashCookerProject(info: ProjectInfo) extends DefaultWebProject(info) {
//  val appengine_sdk = "com.google.appengine" % "appengine-java-sdk" % "1.3.4"
  val scalaz_core = "com.googlecode.scalaz" %% "scalaz-core" % "5.0-M3-SNAPSHOT"
  val scalaz_http = "com.googlecode.scalaz" %% "scalaz-http" % "5.0-M3-SNAPSHOT"
  
  
}
