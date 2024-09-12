package _Self.buildTypes

import jetbrains.buildServer.configs.kotlin.*

object LinuxAll : BuildType({
    templates(LinuxTest)
    name = "Linux (all)"

    params {
        param("branch", "all")
    }
})
