# Specify the modules to be included

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dflowfm_configuration_without_interacter.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaq_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dwaves_configuration.cmake)

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/components/dimr_configuration.cmake)

# Additional includes

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/miscellaneous/tools_delft3dfm_configuration.cmake)

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(delft3dfm)
