#!/usr/bin/env bash
set -eo pipefail
source /opt/bashrc

while [[ $# -gt 0 ]]; do
    case "$1" in
        --generator)
            GENERATOR="$2"
            shift 2
            ;;
        --product)
            PRODUCT="$2"
            shift 2
            ;;
        --build-type)
            BUILD_TYPE="$2"
            shift 2
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo Searching LLVM ...
find / -iname "llvm-symbolizer"

echo Defining MSAN_SYMBOLIZER_PATH ...
export MSAN_SYMBOLIZER_PATH=/opt/intel/oneapi/compiler/2024.2/bin/compiler/llvm-symbolizer

echo Building ...
cmake -S ./src/cmake -G "${GENERATOR}" -D CONFIGURATION_TYPE:STRING="${PRODUCT}" -D CMAKE_BUILD_TYPE="${BUILD_TYPE}" -B "build_${PRODUCT}" -D CMAKE_INSTALL_PREFIX="build_${PRODUCT}/install"
cmake --build "build_${PRODUCT}" --parallel --config "${BUILD_TYPE}"
