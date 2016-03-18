// --- maven publishing ---

publishMavenStyle := true

publishArtifact in Test := true

pomIncludeRepository := { _ => false }

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  val repo = if (version.value.trim.endsWith("SNAPSHOT"))
    "snapshots" at nexus + "content/repositories/snapshots"
  else
    "releases" at nexus + "service/local/staging/deploy/maven2"
  Some(repo)
}

// do not publish docs for snapshot releases
publishArtifact in (Compile, packageDoc) := !version.value.trim.endsWith("SNAPSHOT")

// `sbt release` should publish signed artifacts
releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomExtra in Global := {
  <developers>
    <developer>
      <id>tiarkrompf</id>
      <name>Tiark Rompf</name>
      <url>https://github.com/tiarkrompf</url>
    </developer>
  </developers>
}


// --- sonatype settings ---

sonatypeProfileName := "org.scala-lang"

credentials ++= (for {
  username <- Option(System.getenv().get("SONATYPE_USER"))
  password <- Option(System.getenv().get("SONATYPE_PASS"))
} yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)).toSeq

// NOTE: sonatypeRelease must be run explicitly, after `sbt release`