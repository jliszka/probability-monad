resolvers += Resolver.url("scalasbt", new URL("https://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))  (Resolver.ivyStylePatterns)

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "2.0.2")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.1.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "1.9.0")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.4.4")
