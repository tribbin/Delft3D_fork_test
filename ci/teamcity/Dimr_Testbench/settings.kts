import jetbrains.buildServer.configs.kotlin.*

import testbench.*

version = "2024.03"

project {
    description = "contact: BlackOps (black-ops@deltares.nl)"

    buildType(LinuxAll)

    buildTypesOrder = arrayListOf(LinuxAll)
}
