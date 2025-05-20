# 适配新的AST打印器的CMakeLists.txt
cmake_minimum_required(VERSION 3.12)
project(PASCC_TEST)
# add_compile_options(-fsanitize=address)
set(CMAKE_CXX_STANDARD 17)

# 包含主项目src目录
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../../src)

# 包含构建出的词法分析器和语法分析器
include_directories(${CMAKE_CURRENT_BINARY_DIR})

set(AST_SOURCE_FILES
    ../../src/ast/ast.cpp
)

set(LOG_SOURCE_FILES
    ../../src/common/log/log.cpp
    ../../src/common/setting/settings.cpp
)

set(YACC_OUTPUTS
    ../../src/parser/lexer.cpp
    ../../src/parser/parser.cpp
)

# 添加AST打印器的头文件
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/ASTPrettyPrinter.hpp
               ${CMAKE_CURRENT_BINARY_DIR}/ASTPrettyPrinter.hpp COPYONLY)

add_executable(BISON_TEST
    main.cpp
    ${YACC_OUTPUTS}
    ${AST_SOURCE_FILES}
    ${LOG_SOURCE_FILES}
)

target_link_libraries(BISON_TEST ${BISON_LIBRARIES} -pthread)