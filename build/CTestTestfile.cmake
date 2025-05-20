# CMake generated Testfile for 
# Source directory: /src
# Build directory: /src/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_bison "/src/bin/BISON_TEST" "/src/test_set")
set_tests_properties(test_bison PROPERTIES  _BACKTRACE_TRIPLES "/src/CMakeLists.txt;46;add_test;/src/CMakeLists.txt;0;")
subdirs("test/bison_test")
