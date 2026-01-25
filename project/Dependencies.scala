package team.mice

import sbt._

object Dependencies {

  object Cats {
    private val version       = "2.13.0"
    private val effectVersion = "3.6.3"
    val core                  = "org.typelevel" %% "cats-core"        % version
    val effect                = "org.typelevel" %% "cats-effect"      % effectVersion
    val laws                  = "org.typelevel" %% "cats-laws"        % version
    val effectLaws            = "org.typelevel" %% "cats-effect-laws" % effectVersion
  }

  object Knot {
    private val version = "0.0.3"
    val core            = "team.mice" %% "knot-core" % version
  }

  object FS2 {
    private val version = "3.12.2"
    val core            = "co.fs2" %% "fs2-core" % version
    val io              = "co.fs2" %% "fs2-io"   % version
  }

  object Weaver {
    private val version      = "0.11.3"
    val cats: ModuleID       = "org.typelevel" %% "weaver-cats"       % version
    val scalacheck: ModuleID = "org.typelevel" %% "weaver-scalacheck" % version
    val discipline: ModuleID = "org.typelevel" %% "weaver-discipline" % version
  }
}
