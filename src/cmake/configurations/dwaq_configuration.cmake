#
# D-WAQ kernel
#=============

if(NOT TARGET waq_definition)
    add_subdirectory(${checkout_src_root}/${waq_definition_module} waq_definition)
endif()

if(NOT TARGET waq_plugin_wasteload)
    add_subdirectory(${checkout_src_root}/${waq_plugin_wasteload_module} waq_plugin_wasteload)
endif()

if(NOT TARGET waq_utils_c)
    add_subdirectory(${checkout_src_root}/${waq_utils_c_module} waq_utils_c)
endif()

if(NOT TARGET waq_utils_f)
    add_subdirectory(${checkout_src_root}/${waq_utils_f_module} waq_utils_f)
endif()

if(NOT TARGET waq_netcdf)
    add_subdirectory(${checkout_src_root}/${waq_netcdf_module} waq_netcdf)
endif()

if(NOT TARGET waq_validation)
    add_subdirectory(${checkout_src_root}/${waq_validation_module} waq_validation)
endif()

if(NOT TARGET waq_process)
    add_subdirectory(${checkout_src_root}/${waq_process_module} waq_process)
endif()

if(NOT TARGET waq_kernel)
    add_subdirectory(${checkout_src_root}/${waq_kernel_module} waq_kernel)
endif()

if(NOT TARGET waq_memory)
    add_subdirectory(${checkout_src_root}/${waq_memory_module} waq_memory)
endif()

if(NOT TARGET waq_io)
    add_subdirectory(${checkout_src_root}/${waq_io_module} waq_io)
endif()

if(NOT TARGET delwaq_lib)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_module} delwaq_lib)
endif()

if(NOT TARGET waq_data)
    add_subdirectory(${checkout_src_root}/${waq_data_module} waq_data)
endif()

if(NOT TARGET delwaq1)
    add_subdirectory(${checkout_src_root}/${delwaq1_module} delwaq1)
endif()

if(NOT TARGET delwaq2)
    add_subdirectory(${checkout_src_root}/${delwaq2_module} delwaq2)
endif()

if(NOT TARGET ftnunit)
        add_subdirectory(${checkout_src_root}/${ftnunit_module} ftnunit)
endif()

if(NOT TARGET delwaq_lib_tests)
    add_subdirectory(${checkout_src_root}/${delwaq_lib_tests_module} delwaq_lib_tests)
endif()

if(NOT TARGET wq_processes)
    add_subdirectory(${checkout_src_root}/${wq_processes_module} wq_processes)
endif()

include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_base.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_kernel.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_dflowfm_online_coupling.cmake)


#
# D-Waq tools
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_tools.cmake)

#
# D-Part kernel
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dpart.cmake)

#
# Third party libraries for D-Waq
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_third_party.cmake)


#
# Utils for D-Waq
#=============
include(${CMAKE_CURRENT_SOURCE_DIR}/configurations/include/dwaq/dwaq_utils.cmake)



#
# Linux installation
#=============
if(UNIX)
    add_subdirectory(${checkout_src_root}/${install_waq_module} install_waq)
endif()

# Project name must be at the end of the configuration: it might get a name when including other configurations and needs to overwrite that
project(dwaq)
