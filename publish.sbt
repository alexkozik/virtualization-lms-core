name := "LMS_local"

publishArtifact in Test := true

publishArtifact in (Test, packageDoc) := false

publishTo := {
  val nexus = "http://10.122.85.37:9081/nexus/"
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at (nexus + "content/repositories/snapshots"))
  else
  	  Some("releases" at (nexus + "content/repositories/releases"))
  }