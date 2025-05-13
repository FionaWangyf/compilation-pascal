# CMake generated Testfile for 
# Source directory: /com
# Build directory: /com/build
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(test_bison "/com/bin/BISON_TEST" "/com/open_set")
set_tests_properties(test_bison PROPERTIES  _BACKTRACE_TRIPLES "/com/CMakeLists.txt;47;add_test;/com/CMakeLists.txt;0;")
add_test(test_ir "/com/bin/IR_TEST" "--i" "/com/open_set" "-d" "4")
set_tests_properties(test_ir PROPERTIES  _BACKTRACE_TRIPLES "/com/CMakeLists.txt;48;add_test;/com/CMakeLists.txt;0;")
subdirs("test/bison_test")
subdirs("test/ir_test")
