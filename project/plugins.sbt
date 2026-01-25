import team.mice.Plugins

addSbtPlugin(Plugins.ScalaFmt.core)
addSbtPlugin(Plugins.SCoverage.core)
addSbtPlugin(Plugins.SBTUpdates.core)
addSbtPlugin(Plugins.SBT.git)
addSbtPlugin(Plugins.SBT.pgp)
addSbtPlugin(Plugins.SBT.ciRelease)
addSbtPlugin(Plugins.Jmh.core)
