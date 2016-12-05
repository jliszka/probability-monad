name := "probability-monad" // insert clever name here

scalaVersion := "2.12.0"

crossScalaVersions := Seq("2.10.6", "2.11.8", "2.12.0")

scalacOptions ++= Seq("-Xfatal-warnings", "-deprecation")

version := "1.0.1"

organization := "org.jliszka"

publishMavenStyle := true

publishArtifact in Test := false

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/jliszka/probability-monad</url>
  <licenses>
    <license>
      <name>Apache</name>
      <url>http://www.opensource.org/licenses/Apache-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:jliszka/probability-monad.git</url>
    <connection>scm:git:git@github.com:jliszka/probability-monad.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jliszka</id>
      <name>Jason Liszka</name>
      <url>http://jliszka.github.io</url>
    </developer>
  </developers>)

credentials ++= {
  val sonatype = ("Sonatype Nexus Repository Manager", "oss.sonatype.org")
  def loadMavenCredentials(file: java.io.File) : Seq[Credentials] = {
    xml.XML.loadFile(file) \ "servers" \ "server" map (s => {
      val host = (s \ "id").text
      val realm = if (host == sonatype._2) sonatype._1 else "Unknown"
      Credentials(realm, host, (s \ "username").text, (s \ "password").text)
    })
  }
  val ivyCredentials   = Path.userHome / ".ivy2" / ".credentials"
  val mavenCredentials = Path.userHome / ".m2"   / "settings.xml"
  (ivyCredentials.asFile, mavenCredentials.asFile) match {
    case (ivy, _) if ivy.canRead => Credentials(ivy) :: Nil
    case (_, mvn) if mvn.canRead => loadMavenCredentials(mvn)
    case _ => Nil
  }
}

pgpSecretRing := Path.userHome / ".gnupg" / "secring.gpg"

initialCommands := """
                |import probability_monad._
                |import probability_monad.Distribution._
                |import probability_monad.Examples._""".stripMargin('|')
