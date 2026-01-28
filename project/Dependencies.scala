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

  object Circe {
    val yaml = "io.circe" %% "circe-yaml" % "0.16.1"
  }

  object Knot {
    private val version = "0.0.5"
    val core            = "team.mice" %% "knot-core" % version
  }

  object FS2 {
    private val version = "3.12.2"
    val core            = "co.fs2"    %% "fs2-core"            % version
    val io              = "co.fs2"    %% "fs2-io"              % version
    val dataCirce       = "org.gnieh" %% "fs2-data-json-circe" % "1.12.0"
  }

  object Weaver {
    private val version = "0.11.3"
    val cats            = "org.typelevel" %% "weaver-cats"       % version
    val scalacheck      = "org.typelevel" %% "weaver-scalacheck" % version
    val discipline      = "org.typelevel" %% "weaver-discipline" % version
  }
}
