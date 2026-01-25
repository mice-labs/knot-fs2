package team.mice

import sbt._

object Plugins {
  object SBTUpdates {
    private val version = "0.4.2"
    val core: ModuleID = "com.timushev.sbt" % "sbt-updates" % version
  }
  object SCoverage {
    private val version = "2.4.4"
    val core: ModuleID = "org.scoverage" % "sbt-scoverage" % version
  }
  object ScalaFmt {
    private val version = "2.0.1"
    val core: ModuleID = "org.scalameta" % "sbt-scalafmt" % version
  }
  object SBT {
    val git            = "com.github.sbt" % "sbt-git" % "2.1.0"
    val pgp = "com.github.sbt" % "sbt-pgp" % "2.3.0"
    val ciRelease = "com.github.sbt" % "sbt-ci-release" % "1.11.2"
  }
  object Jmh {
    private val version = "0.4.8"
    val core: ModuleID = "pl.project13.scala" % "sbt-jmh" % version
  }
}