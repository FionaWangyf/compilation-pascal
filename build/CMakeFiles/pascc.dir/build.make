# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.16

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /com

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /com/build

# Include any dependencies generated for this target.
include CMakeFiles/pascc.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/pascc.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/pascc.dir/flags.make

CMakeFiles/pascc.dir/src/ast/stmt.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/ast/stmt.cpp.o: ../src/ast/stmt.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/pascc.dir/src/ast/stmt.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/ast/stmt.cpp.o -c /com/src/ast/stmt.cpp

CMakeFiles/pascc.dir/src/ast/stmt.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/ast/stmt.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/ast/stmt.cpp > CMakeFiles/pascc.dir/src/ast/stmt.cpp.i

CMakeFiles/pascc.dir/src/ast/stmt.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/ast/stmt.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/ast/stmt.cpp -o CMakeFiles/pascc.dir/src/ast/stmt.cpp.s

CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o: ../src/ast/stmt_test.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Building CXX object CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o -c /com/src/ast/stmt_test.cpp

CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/ast/stmt_test.cpp > CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.i

CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/ast/stmt_test.cpp -o CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.s

CMakeFiles/pascc.dir/src/builder/builder.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/builder/builder.cpp.o: ../src/builder/builder.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_3) "Building CXX object CMakeFiles/pascc.dir/src/builder/builder.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/builder/builder.cpp.o -c /com/src/builder/builder.cpp

CMakeFiles/pascc.dir/src/builder/builder.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/builder/builder.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/builder/builder.cpp > CMakeFiles/pascc.dir/src/builder/builder.cpp.i

CMakeFiles/pascc.dir/src/builder/builder.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/builder/builder.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/builder/builder.cpp -o CMakeFiles/pascc.dir/src/builder/builder.cpp.s

CMakeFiles/pascc.dir/src/common/log/log.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/common/log/log.cpp.o: ../src/common/log/log.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_4) "Building CXX object CMakeFiles/pascc.dir/src/common/log/log.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/common/log/log.cpp.o -c /com/src/common/log/log.cpp

CMakeFiles/pascc.dir/src/common/log/log.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/common/log/log.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/common/log/log.cpp > CMakeFiles/pascc.dir/src/common/log/log.cpp.i

CMakeFiles/pascc.dir/src/common/log/log.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/common/log/log.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/common/log/log.cpp -o CMakeFiles/pascc.dir/src/common/log/log.cpp.s

CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o: ../src/common/setting/settings.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_5) "Building CXX object CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o -c /com/src/common/setting/settings.cpp

CMakeFiles/pascc.dir/src/common/setting/settings.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/common/setting/settings.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/common/setting/settings.cpp > CMakeFiles/pascc.dir/src/common/setting/settings.cpp.i

CMakeFiles/pascc.dir/src/common/setting/settings.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/common/setting/settings.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/common/setting/settings.cpp -o CMakeFiles/pascc.dir/src/common/setting/settings.cpp.s

CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o: ../src/common/thpool/thpool.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_6) "Building CXX object CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o -c /com/src/common/thpool/thpool.cpp

CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/common/thpool/thpool.cpp > CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.i

CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/common/thpool/thpool.cpp -o CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.s

CMakeFiles/pascc.dir/src/ir/ir.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/ir/ir.cpp.o: ../src/ir/ir.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_7) "Building CXX object CMakeFiles/pascc.dir/src/ir/ir.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/ir/ir.cpp.o -c /com/src/ir/ir.cpp

CMakeFiles/pascc.dir/src/ir/ir.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/ir/ir.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/ir/ir.cpp > CMakeFiles/pascc.dir/src/ir/ir.cpp.i

CMakeFiles/pascc.dir/src/ir/ir.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/ir/ir.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/ir/ir.cpp -o CMakeFiles/pascc.dir/src/ir/ir.cpp.s

CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o: ../src/ir/ir_gen.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_8) "Building CXX object CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o -c /com/src/ir/ir_gen.cpp

CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/ir/ir_gen.cpp > CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.i

CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/ir/ir_gen.cpp -o CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.s

CMakeFiles/pascc.dir/src/main.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/main.cpp.o: ../src/main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_9) "Building CXX object CMakeFiles/pascc.dir/src/main.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/main.cpp.o -c /com/src/main.cpp

CMakeFiles/pascc.dir/src/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/main.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/main.cpp > CMakeFiles/pascc.dir/src/main.cpp.i

CMakeFiles/pascc.dir/src/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/main.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/main.cpp -o CMakeFiles/pascc.dir/src/main.cpp.s

CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o: ../src/opt/const_expr.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_10) "Building CXX object CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o -c /com/src/opt/const_expr.cpp

CMakeFiles/pascc.dir/src/opt/const_expr.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/opt/const_expr.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/opt/const_expr.cpp > CMakeFiles/pascc.dir/src/opt/const_expr.cpp.i

CMakeFiles/pascc.dir/src/opt/const_expr.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/opt/const_expr.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/opt/const_expr.cpp -o CMakeFiles/pascc.dir/src/opt/const_expr.cpp.s

CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o: ../src/parser/lex_pascal.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_11) "Building CXX object CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o -c /com/src/parser/lex_pascal.cpp

CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/parser/lex_pascal.cpp > CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.i

CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/parser/lex_pascal.cpp -o CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.s

CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o: ../src/parser/yacc_pascal.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_12) "Building CXX object CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o -c /com/src/parser/yacc_pascal.cpp

CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/parser/yacc_pascal.cpp > CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.i

CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/parser/yacc_pascal.cpp -o CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.s

CMakeFiles/pascc.dir/src/token/token.cpp.o: CMakeFiles/pascc.dir/flags.make
CMakeFiles/pascc.dir/src/token/token.cpp.o: ../src/token/token.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_13) "Building CXX object CMakeFiles/pascc.dir/src/token/token.cpp.o"
	/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/pascc.dir/src/token/token.cpp.o -c /com/src/token/token.cpp

CMakeFiles/pascc.dir/src/token/token.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/pascc.dir/src/token/token.cpp.i"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /com/src/token/token.cpp > CMakeFiles/pascc.dir/src/token/token.cpp.i

CMakeFiles/pascc.dir/src/token/token.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/pascc.dir/src/token/token.cpp.s"
	/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /com/src/token/token.cpp -o CMakeFiles/pascc.dir/src/token/token.cpp.s

# Object files for target pascc
pascc_OBJECTS = \
"CMakeFiles/pascc.dir/src/ast/stmt.cpp.o" \
"CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o" \
"CMakeFiles/pascc.dir/src/builder/builder.cpp.o" \
"CMakeFiles/pascc.dir/src/common/log/log.cpp.o" \
"CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o" \
"CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o" \
"CMakeFiles/pascc.dir/src/ir/ir.cpp.o" \
"CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o" \
"CMakeFiles/pascc.dir/src/main.cpp.o" \
"CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o" \
"CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o" \
"CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o" \
"CMakeFiles/pascc.dir/src/token/token.cpp.o"

# External object files for target pascc
pascc_EXTERNAL_OBJECTS =

../bin/pascc: CMakeFiles/pascc.dir/src/ast/stmt.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/ast/stmt_test.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/builder/builder.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/common/log/log.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/common/setting/settings.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/common/thpool/thpool.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/ir/ir.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/ir/ir_gen.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/main.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/opt/const_expr.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/parser/lex_pascal.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/parser/yacc_pascal.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/src/token/token.cpp.o
../bin/pascc: CMakeFiles/pascc.dir/build.make
../bin/pascc: CMakeFiles/pascc.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/com/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_14) "Linking CXX executable ../bin/pascc"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/pascc.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/pascc.dir/build: ../bin/pascc

.PHONY : CMakeFiles/pascc.dir/build

CMakeFiles/pascc.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/pascc.dir/cmake_clean.cmake
.PHONY : CMakeFiles/pascc.dir/clean

CMakeFiles/pascc.dir/depend:
	cd /com/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /com /com /com/build /com/build /com/build/CMakeFiles/pascc.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/pascc.dir/depend

