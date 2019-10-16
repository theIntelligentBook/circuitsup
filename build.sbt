enablePlugins(ScalaJSPlugin)

name := "Circuits Up"
scalaVersion := "2.12.10" // or any other Scala version >= 2.10.2

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

resolvers += "jitpack" at "https://jitpack.io"

updateOptions := updateOptions.value.withLatestSnapshots(false)

libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.7",
    "com.github.wbillingsley.veautiful" %%% "veautiful" % "master-SNAPSHOT",
    "com.github.wbillingsley.veautiful" %%% "veautiful-templates" % "master-SNAPSHOT",
	"com.github.wbillingsley.veautiful" %%% "scatter" % "master-SNAPSHOT",
	"com.github.wbillingsley.veautiful" %%% "wren" % "master-SNAPSHOT"
)