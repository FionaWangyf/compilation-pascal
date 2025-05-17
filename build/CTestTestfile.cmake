# CMake generated Testfile for 
# Source directory: /Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal
# Build directory: /Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_bison "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/bin/BISON_TEST" "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/open_set")
set_tests_properties(test_bison PROPERTIES  _BACKTRACE_TRIPLES "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/CMakeLists.txt;47;add_test;/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/CMakeLists.txt;0;")
add_test(test_ir "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/bin/IR_TEST" "--i" "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/open_set" "-d" "4")
set_tests_properties(test_ir PROPERTIES  _BACKTRACE_TRIPLES "/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/CMakeLists.txt;48;add_test;/Volumes/TSD302/program/Pascal_compiler_v4/compilation-pascal/CMakeLists.txt;0;")
subdirs("test/bison_test")
subdirs("test/ir_test")
