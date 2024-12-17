Moving towards this new structure is a work in progress.

./suites/fm_configuration.cmake
    - For D-Flow FM release.
    - For 'fm-suite' testbenches.

./suites/d3d4_configuration.cmake
    - For Delft3D 4 release.
    - For 'd3d4-suite' testbenches.   

./testbenches/<product>_configuration.cmake
    - For '<product>' pre-merge testbenches.

./components/*
    - The actual separate components.

./miscellaneous/*
    - 'The rest', like tooling and third-party.

./delft3d4_configuration.cmake
    - Currently in use by the 'legacy' Delft3D4 pipelines.

./delft3dfm_configuration.cmake
    - Currently in use by the 'legacy' Delft3D FM pipelines.