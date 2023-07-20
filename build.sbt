enablePlugins(ScalaJSPlugin)

name := "Circuits Up"
scalaVersion := "3.1.1"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

resolvers += "jitpack" at "https://jitpack.io"

updateOptions := updateOptions.value.withLatestSnapshots(false)

val veautifulVersion = "0.3-M6"

libraryDependencies ++= Seq(
  "com.wbillingsley" %%% "doctacular" % veautifulVersion,
	"com.wbillingsley" %%% "scatter" % veautifulVersion,
  "org.scalatest" %%% "scalatest" % "3.2.9" % "test"
)


val deployScript = taskKey[Unit]("Copies the fullOptJS script to deployscripts/")

// Used by Travis-CI to get the script out from the .gitignored target directory
// Don't run it locally, or you'll find the script gets loaded twice in index.html!
deployScript := {
  val opt = (Compile / fullOptJS).value
  IO.copyFile(opt.data, new java.io.File("deployscripts/compiled.js"))
}