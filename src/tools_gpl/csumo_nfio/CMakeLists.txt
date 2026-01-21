# Set directory which contains all source files
set(src_path src)

# Gather source files
get_fortran_source_files(${src_path} source_files)

# Define library
set(library_name csumo_nfio)
add_library(${library_name} STATIC ${source_files})

# Set dependencies on windows
if (WIN32)
    set(library_dependencies    ec_module
                                deltares_common
                                deltares_common_c
                                gridgeom
                                kdtree_wrapper
                                kdtree2
                                io_netcdf
                                io_netcdf_data
                                netcdff
                                )

    target_link_libraries(${library_name} ${library_dependencies})

    # Set linker properties
    message(STATUS "Setting linker properties in windows")

endif(WIN32)

# Set dependencies on linux
if(UNIX)
    set(library_dependencies    ec_module
                                deltares_common
                                deltares_common_c
                                gridgeom
                                kdtree_wrapper
                                kdtree2
                                io_netcdf
                                io_netcdf_data
                                )

    target_link_libraries(${library_name} ${library_dependencies})


    #target_link_options(${library_name} PRIVATE ${openmp_flag})
    set_property(TARGET ${library_name} PROPERTY LINKER_LANGUAGE Fortran)

endif(UNIX)

# Define how the files should be structured within Visual Studio
source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${library_files})
set_target_properties (${library_name} PROPERTIES FOLDER tools_gpl/csumo_nfio)

install(TARGETS ${library_name} LIBRARY DESTINATION lib
)
