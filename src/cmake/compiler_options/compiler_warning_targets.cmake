add_library(all_compiler_warnings INTERFACE)
set(intel_windows_all_warning_flags /stand /warn:all)
set(intel_linux_all_warning_flags -stand "SHELL:-warn all")
set(gcc_all_warning_flags -Wall -pedantic)
target_compile_options(all_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_all_warning_flags},${intel_linux_all_warning_flags}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_all_warning_flags}>"
)

add_library(compiler_warnings_as_errors INTERFACE)
set(intel_windows_warning_error_flag /warn:errors /warn:stderrors)
set(intel_linux_warning_error_flag "SHELL:-warn errors" "SHELL:-warn stderrors")
set(gcc_warning_error_flag -Werror)
target_compile_options(compiler_warnings_as_errors INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_warning_error_flag},${intel_linux_warning_error_flag}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_warning_error_flag}>"
)

add_library(limit_compiler_warnings INTERFACE)
# Disable warning 5462, global name too long. The compiler limit of 90 characters is too restrictive, see https://community.intel.com/t5/Intel-Fortran-Compiler/Many-quot-Global-name-too-long-quot-warnings/td-p/1505843
# Disable warning 5268, allow text longer than 132 characters
set(intel_windows_disabled_warning_flags /Qdiag-disable:5462 /Qdiag-disable:5268)
set(intel_linux_disabled_warning_flags "SHELL:-diag-disable 5462" "SHELL:-diag-disable 5268")
target_compile_options(limit_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_disabled_warning_flags},${intel_linux_disabled_warning_flags}>>"
)

add_library(no_compiler_warnings INTERFACE)
set(intel_windows_no_warning_flags /warn:none)
set(intel_linux_no_warning_flags "SHELL:-warn none")
set(gcc_no_warning_flags -w)
set(msvc_no_warning_flags /w)
target_compile_options(no_compiler_warnings INTERFACE
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_no_warning_flags},${intel_linux_no_warning_flags}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_no_warning_flags}>"
                       "$<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:MSVC>>:${msvc_no_warning_flags}>"
                       "$<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:GNU>>:${gcc_no_warning_flags}>"
                       "$<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_no_warning_flags},${intel_linux_no_warning_flags}>>"
                       "$<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:MSVC>>:${msvc_no_warning_flags}>"
                       "$<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:GNU>>:${gcc_no_warning_flags}>"
                       "$<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_windows_no_warning_flags},${intel_linux_no_warning_flags}>>"
)
