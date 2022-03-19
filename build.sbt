lazy val probabilityMonad = (
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    .crossType(CrossType.Pure)
    .in(file("."))
  )
  .settings(

name := "probability-monad",

scalaVersion := crossScalaVersions.value.last,

crossScalaVersions := Seq("2.12.15", "2.13.8"),

scalacOptions ++= Seq("-Xfatal-warnings", "-deprecation"),

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major <= 12 =>
      Seq()
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3")
  }
},

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %%% "scala-collection-compat" % "2.5.0",
  "org.scalatest"          %%% "scalatest"               % "3.2.11" % Test,
  "org.scalactic"          %%% "scalactic"               % "3.2.11" % Test,
),



version := "1.0.4",

organization := "org.jliszka",

publishMavenStyle := true,

Test / publishArtifact := false,

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  Some("releases"  at nexus + "service/local/staging/deploy/maven2")
},

pomIncludeRepository := { _ => false },

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
  </developers>),

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
},

credentials += Credentials(
  "GnuPG Key ID",
  "gpg",
  "66F429E63C9C54A82A217E83410CF5BA60429BD7", // key identifier
  "ignored" // this field is ignored; passwords are supplied by pinentry
),

initialCommands := """
                |import probability_monad._
                |import probability_monad.Distribution._
                |import probability_monad.Examples._""".stripMargin('|')
)
