package testbench

import jetbrains.buildServer.configs.kotlin.*

object LinuxAll : BuildType({

    name = "Linux (all)"
    templates(LinuxTestTemplate)

    params {
        param("branch", "all")
    }
})