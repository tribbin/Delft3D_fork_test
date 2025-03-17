if(NOT TARGET FBCTools)
    add_subdirectory(${checkout_src_root}/${fbc_module_path} fbc-tools)
    if(UNIX)
        # install
        add_subdirectory(${checkout_src_root}/${install_fbc_module} install_fbc-tools)
    endif()
endif()
