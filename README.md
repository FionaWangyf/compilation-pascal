# compilation-pascal
```
compilation-pascal
├─ 📁.vscode
│  └─ 📄settings.json
├─ 📁bin
├─ 📁build
├─ 📁cmake-build-debug
│  ├─ 📁.cmake
│  │  └─ 📁api
│  │     └─ 📁v1
│  │        └─ 📁query
│  │           ├─ 📄cache-v2
│  │           ├─ 📄cmakeFiles-v1
│  │           ├─ 📄codemodel-v2
│  │           └─ 📄toolchains-v1
│  └─ 📄CMakeCache.txt
├─ 📁error_set
│  ├─ 📄0_program_head.pas
│  ├─ 📄1_const_decl.pas
│  ├─ 📄2_var_decl.pas
│  ├─ 📄3_compound__stmt.pas
│  ├─ 📄4_expression.pas
│  ├─ 📄5_typo_error.pas
│  ├─ 📄6_function_error.pas
│  ├─ 📄7_procedure_error.pas
│  └─ 📄8_multi_error.pas
├─ 📁judge
│  └─ 📄judge.py
├─ 📁open_set
│  ├─ 📄00_main.pas
│  ├─ 📄01_var_defn2.pas
│  ├─ 📄02_var_defn3.pas
│  ├─ 📄03_arr_defn2.pas
│  ├─ 📄04_const_var_defn2.pas
│  ├─ 📄05_const_var_defn3.pas
│  ├─ 📄06_func_defn.pas
│  ├─ 📄07_var_defn_func.pas
│  ├─ 📄08_add2.pas
│  ├─ 📄09_addc.pas
│  ├─ 📄10_sub2.pas
│  ├─ 📄11_subc.pas
│  ├─ 📄12_mul.pas
│  ├─ 📄13_mulc.pas
│  ├─ 📄14_div.pas
│  ├─ 📄15_divc.pas
│  ├─ 📄16_mod.pas
│  ├─ 📄17_rem.pas
│  ├─ 📄18_if_test3.pas
│  ├─ 📄19_if_test4.pas
│  ├─ 📄20_if_test5.pas
│  ├─ 📄21_while_if_test2.pas
│  ├─ 📄22_arr_expr_len.pas
│  ├─ 📄23_op_priority1.pas
│  ├─ 📄24_op_priority2.pas
│  ├─ 📄25_op_priority3.pas
│  ├─ 📄26_op_priority4.in
│  ├─ 📄26_op_priority4.pas
│  ├─ 📄27_op_priority5.pas
│  ├─ 📄28_unary_op.pas
│  ├─ 📄29_unary_op2.pas
│  ├─ 📄30_logi_assign.in
│  ├─ 📄30_logi_assign.pas
│  ├─ 📄31_comment1.pas
│  ├─ 📄32_assign_complex_expr.pas
│  ├─ 📄33_if_complex_expr.pas
│  ├─ 📄34_short_circuit.in
│  ├─ 📄34_short_circuit.pas
│  ├─ 📄35_short_circuit3.pas
│  ├─ 📄36_scope.pas
│  ├─ 📄37_sort_test1.pas
│  ├─ 📄38_sort_test4.pas
│  ├─ 📄39_sort_test6.pas
│  ├─ 📄40_percolation.in
│  ├─ 📄40_percolation.pas
│  ├─ 📄41_big_int_mul.pas
│  ├─ 📄42_color.in
│  ├─ 📄42_color.pas
│  ├─ 📄43_exgcd.pas
│  ├─ 📄44_reverse_output.in
│  ├─ 📄44_reverse_output.pas
│  ├─ 📄45_dijkstra.in
│  ├─ 📄45_dijkstra.pas
│  ├─ 📄46_full_conn.in
│  ├─ 📄46_full_conn.pas
│  ├─ 📄47_hanoi.in
│  ├─ 📄47_hanoi.pas
│  ├─ 📄48_n_queens.in
│  ├─ 📄48_n_queens.pas
│  ├─ 📄49_substr.pas
│  ├─ 📄50_side_effect.pas
│  ├─ 📄51_var_name.pas
│  ├─ 📄52_chaos_token.pas
│  ├─ 📄53_skip_spaces.pas
│  ├─ 📄54_long_array.pas
│  ├─ 📄55_long_array2.pas
│  ├─ 📄56_long_code2.pas
│  ├─ 📄57_many_params.pas
│  ├─ 📄58_many_params2.pas
│  ├─ 📄59_many_globals.pas
│  ├─ 📄60_many_locals.pas
│  ├─ 📄61_many_locals2.pas
│  ├─ 📄62_register_alloc.pas
│  ├─ 📄63_nested_calls.pas
│  ├─ 📄64_nested_loops.pas
│  ├─ 📄65_float.pas
│  ├─ 📄66_matrix_add.pas
│  ├─ 📄67_matrix_sub.pas
│  ├─ 📄68_matrix_mul.pas
│  ├─ 📄69_matrix_tran.pas
│  ├─ 📄93_dct.c
│  └─ 📄93_dct.pas
├─ 📁src
│  ├─ 📁ast
│  │  ├─ 📄stmt.cpp
│  │  ├─ 📄stmt.hpp
│  │  └─ 📄visitor.hpp
│  ├─ 📁code_generator
│  │  ├─ 📄code_generator.cpp
│  │  └─ 📄code_generator.hpp
│  ├─ 📁common
│  │  ├─ 📁exception
│  │  │  └─ 📄exception.hpp
│  │  ├─ 📁log
│  │  │  ├─ 📄log.cpp
│  │  │  └─ 📄log.hpp
│  │  └─ 📁setting
│  │     ├─ 📄settings.cpp
│  │     └─ 📄settings.hpp
│  ├─ 📁parser
│  │  ├─ 📄gen_parser.sh
│  │  ├─ 📄lex_pascal.cpp
│  │  ├─ 📄lex_pascal.hpp
│  │  ├─ 📄lex_pascal.l
│  │  ├─ 📄yacc_pascal.cpp
│  │  ├─ 📄yacc_pascal.hpp
│  │  ├─ 📄yacc_pascal.y
│  │  └─ 📄yacc_pascal.y~
│  ├─ 📁semantic_analyzer
│  │  ├─ 📄semantic_analyzer.cpp
│  │  └─ 📄semantic_analyzer.hpp
│  ├─ 📁token
│  │  ├─ 📄token.cpp
│  │  └─ 📄token.hpp
│  └─ 📄main.cpp
├─ 📁test
│  ├─ 📁bison_test
│  │  ├─ 📄ASTPrettyPrinter.hpp
│  │  ├─ 📄CMakeLists.txt
│  │  ├─ 📄main.cpp
│  │  └─ 📄todo.y
│  ├─ 📁ir_test
│  │  ├─ 📄CMakeLists.txt
│  │  ├─ 📄main.cpp
│  │  ├─ 📄run.sh
│  │  └─ 📄test.py
│  ├─ 📁stmt_test
│  └─ 📁token_test
│     ├─ 📄CMakeLists.txt
│     ├─ 📄lex_pascal.l
│     ├─ 📄lex_pascal_use_for_bison.l
│     └─ 📄main.cpp
├─ 📄.gitignore
├─ 📄CMakeLists.txt
└─ 📄README.md
```