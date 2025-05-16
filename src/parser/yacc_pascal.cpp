/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 2

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "yacc_pascal.y"

// 此处为相关头文件和函数，会添加在生成的代码中

#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <map>
#include <type_traits>

#include "common/log/log.hpp"
#include "common/setting/settings.hpp"
#include "ast/stmt.hpp"
#include "ast/stmt_test.hpp"
#include "yacc_pascal.hpp"
#include "lex_pascal.hpp"

namespace {
    bool hadError = false; // 错误标志

    // 辅助函数：将数值标识符映射到枚举类型
    // 处理运算符类型转换的统一逻辑
    template<typename EnumType, typename MapType>
    EnumType get_operator_type(long long op, const MapType& typeMap) {
        auto it = typeMap.find(op);
        if (it != typeMap.end()) {
            return it->second;
        }
        return static_cast<EnumType>(0); // NULL_TYPE 通常是0
    }

    // 关系运算符映射表
    const std::map<long long, RelExprStmt::RelExprType> relExprTypeMap = {
        {0, RelExprStmt::RelExprType::Equal},
        {1, RelExprStmt::RelExprType::NotEqual},
        {2, RelExprStmt::RelExprType::Less},
        {3, RelExprStmt::RelExprType::LessEqual},
        {4, RelExprStmt::RelExprType::Greater},
        {5, RelExprStmt::RelExprType::GreaterEqual},
        {6, RelExprStmt::RelExprType::In}
    };

    // 加法级运算符映射表
    const std::map<long long, AddExprStmt::AddExprType> addExprTypeMap = {
        {0, AddExprStmt::AddExprType::Plus},
        {1, AddExprStmt::AddExprType::Minus},
        {2, AddExprStmt::AddExprType::Or}
    };

    // 乘法级运算符映射表
    const std::map<long long, MulExprStmt::MulExprType> mulExprTypeMap = {
        {0, MulExprStmt::MulExprType::Mul},
        {1, MulExprStmt::MulExprType::Div},
        {2, MulExprStmt::MulExprType::Mod},
        {3, MulExprStmt::MulExprType::And},
        {4, MulExprStmt::MulExprType::AndThen}
    };

    // 获取各类运算符类型的函数
    RelExprStmt::RelExprType get_rel_expr_type(long long op) {
        return get_operator_type<RelExprStmt::RelExprType>(op, relExprTypeMap);
    }

    AddExprStmt::AddExprType get_add_expr_type(long long op) {
        return get_operator_type<AddExprStmt::AddExprType>(op, addExprTypeMap);
    }

    MulExprStmt::MulExprType get_mul_expr_type(long long op) {
        return get_operator_type<MulExprStmt::MulExprType>(op, mulExprTypeMap);
    }

    // 填充数值节点的统一函数
    template<typename T>
    void fill_number_stmt(std::unique_ptr<NumberStmt>& num_value, T val) {
        num_value->is_signed = true;
        num_value->is_real = std::is_same<T, double>::value;
        num_value->is_char = std::is_same<T, char>::value;
        num_value->is_unsigned = false;

        if constexpr (std::is_same<T, long long>::value) {
            num_value->int_val = val;
        } else if constexpr (std::is_same<T, double>::value) {
            num_value->real_val = val;
        } else if constexpr (std::is_same<T, char>::value) {
            num_value->char_val = val;
            LOG_DEBUG("DEBUG fill_number_stmt -> char_val: %c", val);
        }
    }

    // 节点创建辅助函数
    template<typename T, typename... Args>
    T* create_node(Args&&... args) {
        return new T(std::forward<Args>(args)...);
    }

    // 将一个值添加到列表中的辅助函数
    template<typename T>
    void add_to_list(std::vector<T*>* list, T* item) {
        if (!list) {
            list = new std::vector<T*>();
        }
        list->emplace_back(item);
    }

    // 将列表中的所有项转移到所有者中的辅助函数
    template<typename Owner, typename T>
    void transfer_list_to_owner(Owner* owner, std::vector<T*>* list, std::vector<std::unique_ptr<T>> Owner::* container) {
        if (list) {
            for (auto item : *list) {
                (owner->*container).emplace_back(std::unique_ptr<T>(item));
            }
            delete list;
        }
    }

    // 值节点工厂
    class ValueNodeFactory {
    public:
        static ValueStmt* create_integer(long long val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            fill_number_stmt(value->number, val);
            return value;
        }

        static ValueStmt* create_real(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            double val = atof(str);
            fill_number_stmt(value->number, val);
            value->number->literal = std::string(str);
            return value;
        }

        static ValueStmt* create_char(char val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            fill_number_stmt(value->number, val);
            return value;
        }

        static ValueStmt* create_string(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Str;
            value->str = std::make_unique<StrStmt>();
            value->str->val = std::string(str).substr(1, std::string(str).length() - 2);
            return value;
        }
    };

    // 表达式节点工厂
    class ExprNodeFactory {
    public:
        static ExprStmt* create_from_simple_expr(AddExprStmt* simple_expr) {
            ExprStmt* expr = new ExprStmt();
            expr->rel_expr = std::make_unique<RelExprStmt>();
            RelExprStmt::Term term;
            term.type = RelExprStmt::RelExprType::NULL_TYPE;
            term.add_expr = std::unique_ptr<AddExprStmt>(simple_expr);
            expr->rel_expr->terms.emplace_back(std::move(term));
            return expr;
        }

        static AddExprStmt* create_from_term(MulExprStmt* term) {
            AddExprStmt* add_expr = new AddExprStmt();
            AddExprStmt::Term term_struct;
            term_struct.type = AddExprStmt::AddExprType::NULL_TYPE;
            term_struct.mul_expr = std::unique_ptr<MulExprStmt>(term);
            add_expr->terms.emplace_back(std::move(term_struct));
            return add_expr;
        }

        static MulExprStmt* create_from_factor(UnaryExprStmt* factor) {
            MulExprStmt* mul_expr = new MulExprStmt();
            MulExprStmt::Term term;
            term.type = MulExprStmt::MulExprType::NULL_TYPE;
            term.unary_expr = std::unique_ptr<UnaryExprStmt>(factor);
            mul_expr->terms.emplace_back(std::move(term));
            return mul_expr;
        }
    };

    // 创建一元表达式的函数
    UnaryExprStmt* create_unary_expr(PrimaryExprStmt::PrimaryExprType type) {
        UnaryExprStmt* unary = new UnaryExprStmt();
        unary->primary_expr = std::make_unique<PrimaryExprStmt>();
        unary->primary_expr->type = type;
        return unary;
    }

    // 报告语法错误
    void syntax_error(YYLTYPE *llocp, const char *msg) {
        LOG_ERROR("[Syntax Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column, msg);
        hadError = true;
        exit(1);
    }

    // 获取错误位置的辅助函数
    void get_error_location(const char* code_str, YYLTYPE *llocp, std::string &error_note, std::string &msg, bool have_expected) {
        std::string code(code_str);
        std::istringstream stream(code);
        std::string line;
        int current_line = 1;
        msg += "\t";
        
        while (std::getline(stream, line)) {
            if (current_line == llocp->first_line) {
                // 打印到错误开始之前的部分
                msg += line.substr(0, llocp->first_column);
                // 错误部分使用红色高亮显示
                error_note = line.substr(llocp->first_column, llocp->last_column - llocp->first_column + 1);
                msg += "\033[31m";
                msg += error_note;
                msg += "\033[0m";
                // 打印错误之后的部分
                if (llocp->last_column < line.size()) {
                    msg += line.substr(llocp->last_column + 1);
                }
                // 添加箭头
                msg += "\n\t";
                if (have_expected) {
                    for (int i = 0; i < llocp->first_column; ++i) {
                        msg += " ";
                    }
                    for (int i = llocp->first_column; i <= llocp->last_column; ++i) {
                        msg += "\033[1;37m^\033[0m";
                    }
                } else {
                    for (int i = 0; i < llocp->first_column; ++i) {
                        msg += "\033[1;37m^\033[0m";
                    }
                }
                break; // 已找到错误行，跳出循环
            }
            ++current_line;
        }
    }

    // 日志辅助宏
    #define LOG_RULE(rule) LOG_DEBUG("DEBUG " rule)
    #define LOG_ERROR_RULE(rule) LOG_DEBUG("ERROR " rule)
    #define HANDLE_ERROR(result, cleanup_code) \
        { \
            cleanup_code; \
            LOG_ERROR_RULE(result); \
            yyerrok; \
        }
}

// 枚举类，定义编译器正在处理的语法规则类型
enum class CurrentRule {
    ProgramStruct,     // 整个程序结构，包括程序头和程序体
    ProgramHead,       // 程序头部分，包含程序名和可能的文件参数
    ProgramBody,       // 程序主体，包含常量定义、变量声明、函数/过程声明和主程序语句
    IdList,            // 标识符列表，用于多个变量或参数的声明
    ConstDeclarations, // 常量声明部分
    ConstDeclaration,  // 单个常量的声明
    ConstValue,        // 常量值（可以是数字、字符、字符串等）
    VarDeclarations,   // 变量声明部分
    VarDeclaration,    // 单个或一组变量的声明
    Type,              // 类型定义
    BasicType,         // 基本类型
    PeriodList,        // 数组维度范围列表
    SubprogramDeclarations, // 子程序（函数和过程）声明部分
    Subprogram,        // 单个子程序（函数或过程）的声明
    SubprogramHead,    // 子程序头部，包含名称、参数和返回类型
    FormalParameter,   // 形式参数部分
    ParameterList,     // 参数列表
    Parameter,         // 单个参数或参数组
    VarParameter,      // 引用传递参数
    ValueParameter,    // 值传递参数
    SubprogramBody,    // 子程序体，包含局部声明和语句
    CompoundStatement, // 复合语句块
    StatementList,     // 语句列表，多个语句用分号分隔
    Statement,         // 单个语句（赋值、条件、循环等）
    ProcedureCall,     // 过程调用
    VariableList,      // 变量列表，用于输入/输出语句
    Variable,          // 变量引用，可以是简单变量或数组元素
    IdVarpart,         // 变量引用的附加部分，如数组下标
    ExpressionList,    // 表达式列表，用于函数调用或数组索引
    ArrayIndexExpression, // 数组索引表达式
    BracketExpressionList, // 方括号表达式列表，用于多维数组
    Expression,        // 完整表达式，可能包含关系运算
    SimpleExpression,  // 简单表达式，包含加法级运算符
    Term,              // 项，包含乘法级运算符
    Factor,            // 因子，表达式的基本构建块
};

// 跟踪编译器当前正在处理的语法规则
static CurrentRule current_rule = CurrentRule::ProgramStruct;

void resetErrorFlag() {
    hadError = false; // 重置错误标志，为下一次解析准备
}

int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg);


#line 374 "yacc_pascal.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "yacc_pascal.hpp"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_CONST = 3,                      /* CONST  */
  YYSYMBOL_PROGRAM = 4,                    /* PROGRAM  */
  YYSYMBOL_TYPE = 5,                       /* TYPE  */
  YYSYMBOL_RECORD = 6,                     /* RECORD  */
  YYSYMBOL_ARRAY = 7,                      /* ARRAY  */
  YYSYMBOL_OF = 8,                         /* OF  */
  YYSYMBOL_VAR = 9,                        /* VAR  */
  YYSYMBOL_FUNCTION = 10,                  /* FUNCTION  */
  YYSYMBOL_PROCEDURE = 11,                 /* PROCEDURE  */
  YYSYMBOL_BEGIN_TOKEN = 12,               /* BEGIN_TOKEN  */
  YYSYMBOL_END = 13,                       /* END  */
  YYSYMBOL_IF = 14,                        /* IF  */
  YYSYMBOL_THEN = 15,                      /* THEN  */
  YYSYMBOL_ELSE = 16,                      /* ELSE  */
  YYSYMBOL_CASE = 17,                      /* CASE  */
  YYSYMBOL_WHILE = 18,                     /* WHILE  */
  YYSYMBOL_REPEAT = 19,                    /* REPEAT  */
  YYSYMBOL_UNTIL = 20,                     /* UNTIL  */
  YYSYMBOL_FOR = 21,                       /* FOR  */
  YYSYMBOL_TO = 22,                        /* TO  */
  YYSYMBOL_DOWNTO = 23,                    /* DOWNTO  */
  YYSYMBOL_DO = 24,                        /* DO  */
  YYSYMBOL_READ = 25,                      /* READ  */
  YYSYMBOL_READLN = 26,                    /* READLN  */
  YYSYMBOL_WRITE = 27,                     /* WRITE  */
  YYSYMBOL_WRITELN = 28,                   /* WRITELN  */
  YYSYMBOL_CHAR_KW = 29,                   /* CHAR_KW  */
  YYSYMBOL_INTEGER_KW = 30,                /* INTEGER_KW  */
  YYSYMBOL_REAL_KW = 31,                   /* REAL_KW  */
  YYSYMBOL_BOOLEAN_KW = 32,                /* BOOLEAN_KW  */
  YYSYMBOL_NOT = 33,                       /* NOT  */
  YYSYMBOL_DIV = 34,                       /* DIV  */
  YYSYMBOL_MOD = 35,                       /* MOD  */
  YYSYMBOL_AND = 36,                       /* AND  */
  YYSYMBOL_OR = 37,                        /* OR  */
  YYSYMBOL_NE = 38,                        /* NE  */
  YYSYMBOL_LE = 39,                        /* LE  */
  YYSYMBOL_GE = 40,                        /* GE  */
  YYSYMBOL_ASSIGNOP = 41,                  /* ASSIGNOP  */
  YYSYMBOL_IN = 42,                        /* IN  */
  YYSYMBOL_ORELSE = 43,                    /* ORELSE  */
  YYSYMBOL_ANDTHEN = 44,                   /* ANDTHEN  */
  YYSYMBOL_DOUBLE_DOT = 45,                /* DOUBLE_DOT  */
  YYSYMBOL_BRACE_PAIR = 46,                /* BRACE_PAIR  */
  YYSYMBOL_BREAK = 47,                     /* BREAK  */
  YYSYMBOL_CONTINUE = 48,                  /* CONTINUE  */
  YYSYMBOL_IDENTIFIER = 49,                /* IDENTIFIER  */
  YYSYMBOL_INTEGER = 50,                   /* INTEGER  */
  YYSYMBOL_BOOLEAN = 51,                   /* BOOLEAN  */
  YYSYMBOL_REAL = 52,                      /* REAL  */
  YYSYMBOL_CHAR = 53,                      /* CHAR  */
  YYSYMBOL_STRING = 54,                    /* STRING  */
  YYSYMBOL_55_ = 55,                       /* ';'  */
  YYSYMBOL_56_ = 56,                       /* '.'  */
  YYSYMBOL_57_ = 57,                       /* '('  */
  YYSYMBOL_58_ = 58,                       /* ')'  */
  YYSYMBOL_59_ = 59,                       /* ','  */
  YYSYMBOL_60_ = 60,                       /* '='  */
  YYSYMBOL_61_ = 61,                       /* '+'  */
  YYSYMBOL_62_ = 62,                       /* '-'  */
  YYSYMBOL_63_ = 63,                       /* ':'  */
  YYSYMBOL_64_ = 64,                       /* '['  */
  YYSYMBOL_65_ = 65,                       /* ']'  */
  YYSYMBOL_66_ = 66,                       /* '<'  */
  YYSYMBOL_67_ = 67,                       /* '>'  */
  YYSYMBOL_68_ = 68,                       /* '*'  */
  YYSYMBOL_69_ = 69,                       /* '/'  */
  YYSYMBOL_YYACCEPT = 70,                  /* $accept  */
  YYSYMBOL_programstruct = 71,             /* programstruct  */
  YYSYMBOL_program_head = 72,              /* program_head  */
  YYSYMBOL_program_body = 73,              /* program_body  */
  YYSYMBOL_idlist = 74,                    /* idlist  */
  YYSYMBOL_const_declarations = 75,        /* const_declarations  */
  YYSYMBOL_const_declaration = 76,         /* const_declaration  */
  YYSYMBOL_const_value = 77,               /* const_value  */
  YYSYMBOL_var_declarations = 78,          /* var_declarations  */
  YYSYMBOL_var_declaration = 79,           /* var_declaration  */
  YYSYMBOL_type = 80,                      /* type  */
  YYSYMBOL_basic_type = 81,                /* basic_type  */
  YYSYMBOL_period_list = 82,               /* period_list  */
  YYSYMBOL_subprogram_declarations = 83,   /* subprogram_declarations  */
  YYSYMBOL_subprogram = 84,                /* subprogram  */
  YYSYMBOL_subprogram_head = 85,           /* subprogram_head  */
  YYSYMBOL_formal_parameter = 86,          /* formal_parameter  */
  YYSYMBOL_parameter_list = 87,            /* parameter_list  */
  YYSYMBOL_parameter = 88,                 /* parameter  */
  YYSYMBOL_var_parameter = 89,             /* var_parameter  */
  YYSYMBOL_value_parameter = 90,           /* value_parameter  */
  YYSYMBOL_subprogram_body = 91,           /* subprogram_body  */
  YYSYMBOL_compound_statement = 92,        /* compound_statement  */
  YYSYMBOL_statement_list = 93,            /* statement_list  */
  YYSYMBOL_statement = 94,                 /* statement  */
  YYSYMBOL_variable_list = 95,             /* variable_list  */
  YYSYMBOL_variable = 96,                  /* variable  */
  YYSYMBOL_id_varpart = 97,                /* id_varpart  */
  YYSYMBOL_array_index_expression = 98,    /* array_index_expression  */
  YYSYMBOL_procedure_call = 99,            /* procedure_call  */
  YYSYMBOL_expression_list = 100,          /* expression_list  */
  YYSYMBOL_expression = 101,               /* expression  */
  YYSYMBOL_simple_expression = 102,        /* simple_expression  */
  YYSYMBOL_term = 103,                     /* term  */
  YYSYMBOL_factor = 104,                   /* factor  */
  YYSYMBOL_addop = 105,                    /* addop  */
  YYSYMBOL_relop = 106,                    /* relop  */
  YYSYMBOL_mulop = 107,                    /* mulop  */
  YYSYMBOL_error_recovery = 108            /* error_recovery  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   298

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  70
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  39
/* YYNRULES -- Number of rules.  */
#define YYNRULES  121
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  230

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   309


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      57,    58,    68,    61,    59,    62,    56,    69,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    63,    55,
      66,    60,    67,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    64,     2,    65,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   498,   498,   508,   515,   522,   531,   540,   548,   557,
     594,   630,   638,   650,   655,   679,   688,   697,   704,   713,
     717,   721,   725,   730,   735,   743,   748,   757,   761,   767,
     776,   796,   815,   824,   833,   851,   856,   861,   866,   874,
     884,   896,   900,   916,   928,   947,   966,   972,   981,   985,
     994,   998,  1005,  1013,  1019,  1028,  1036,  1049,  1081,  1090,
    1095,  1117,  1130,  1147,  1151,  1161,  1168,  1173,  1191,  1209,
    1235,  1256,  1271,  1288,  1296,  1307,  1315,  1329,  1337,  1359,
    1363,  1373,  1390,  1398,  1408,  1417,  1439,  1443,  1451,  1461,
    1467,  1480,  1486,  1500,  1506,  1520,  1531,  1545,  1557,  1568,
    1578,  1586,  1607,  1615,  1621,  1631,  1631,  1631,  1633,  1633,
    1633,  1633,  1633,  1633,  1633,  1635,  1635,  1635,  1635,  1635,
    1635,  1638
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  static const char *const yy_sname[] =
  {
  "end of file", "error", "invalid token", "CONST", "PROGRAM", "TYPE",
  "RECORD", "ARRAY", "OF", "VAR", "FUNCTION", "PROCEDURE", "BEGIN_TOKEN",
  "END", "IF", "THEN", "ELSE", "CASE", "WHILE", "REPEAT", "UNTIL", "FOR",
  "TO", "DOWNTO", "DO", "READ", "READLN", "WRITE", "WRITELN", "CHAR_KW",
  "INTEGER_KW", "REAL_KW", "BOOLEAN_KW", "NOT", "DIV", "MOD", "AND", "OR",
  "NE", "LE", "GE", "ASSIGNOP", "IN", "ORELSE", "ANDTHEN", "DOUBLE_DOT",
  "BRACE_PAIR", "BREAK", "CONTINUE", "IDENTIFIER", "INTEGER", "BOOLEAN",
  "REAL", "CHAR", "STRING", "';'", "'.'", "'('", "')'", "','", "'='",
  "'+'", "'-'", "':'", "'['", "']'", "'<'", "'>'", "'*'", "'/'", "$accept",
  "programstruct", "program_head", "program_body", "idlist",
  "const_declarations", "const_declaration", "const_value",
  "var_declarations", "var_declaration", "type", "basic_type",
  "period_list", "subprogram_declarations", "subprogram",
  "subprogram_head", "formal_parameter", "parameter_list", "parameter",
  "var_parameter", "value_parameter", "subprogram_body",
  "compound_statement", "statement_list", "statement", "variable_list",
  "variable", "id_varpart", "array_index_expression", "procedure_call",
  "expression_list", "expression", "simple_expression", "term", "factor",
  "addop", "relop", "mulop", "error_recovery", YY_NULLPTR
  };
  return yy_sname[yysymbol];
}
#endif

#define YYPACT_NINF (-133)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-80)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     107,   -14,     8,    69,    20,   263,  -133,    24,  -133,   267,
      33,     9,    41,    97,   110,    67,    33,    76,  -133,    91,
      65,    98,  -133,    10,  -133,    97,  -133,    70,  -133,   120,
      90,   135,    99,    27,   104,   186,  -133,  -133,   138,    81,
    -133,  -133,  -133,  -133,    -3,    86,  -133,   117,    67,    92,
      67,    11,    16,    15,   137,   154,  -133,   186,  -133,    90,
    -133,  -133,  -133,  -133,    90,    28,   126,  -133,  -133,  -133,
    -133,  -133,  -133,    44,  -133,   162,  -133,   162,   168,   153,
     153,   176,   172,   177,  -133,  -133,    38,  -133,     6,  -133,
     195,  -133,  -133,   110,  -133,  -133,  -133,    92,   187,    92,
      25,   179,  -133,   164,   153,   -25,  -133,  -133,  -133,  -133,
     153,   153,   153,  -133,    95,    39,   -13,  -133,   133,   206,
      17,   153,   153,   153,  -133,  -133,    71,   153,    97,  -133,
    -133,   213,   -22,  -133,    67,    46,    57,  -133,  -133,  -133,
     220,  -133,  -133,   153,   188,  -133,  -133,   164,  -133,  -133,
    -133,  -133,  -133,  -133,  -133,   153,  -133,  -133,  -133,   153,
    -133,  -133,  -133,  -133,  -133,  -133,   153,   164,   153,   208,
     205,   105,  -133,   163,   223,   182,   -21,   193,  -133,  -133,
     223,   259,   230,   231,   274,  -133,   220,    25,  -133,  -133,
     198,  -133,   268,    39,   -13,  -133,  -133,   128,   236,  -133,
     236,  -133,   153,  -133,  -133,   153,  -133,  -133,   241,   220,
    -133,  -133,  -133,   164,   153,  -133,  -133,   223,  -133,   178,
     237,  -133,  -133,   141,   153,  -133,  -133,   164,  -133,  -133
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     8,     7,     1,     0,
       5,     0,     0,    27,    13,     0,     4,     0,   121,     0,
       0,     0,     3,     0,    41,    27,    11,     0,     2,    15,
       0,    14,     0,     0,     0,     0,    41,     6,     0,     0,
      19,    22,    25,    26,     0,     0,    16,     0,    29,     0,
      28,     0,     0,     0,     0,     0,     9,     0,    12,     0,
      20,    23,    21,    24,     0,     0,     0,    38,    35,    36,
      37,    30,    33,     0,    46,    48,    47,    48,     0,     0,
       0,     0,     0,     0,    73,    74,    84,    66,     0,    59,
       0,    65,    42,    13,    10,    18,    17,     0,     0,     0,
      50,     0,    44,    63,     0,    79,    95,    97,    96,    98,
       0,     0,     0,    99,     0,    89,    91,    93,     0,     0,
       0,    86,    86,    86,    78,    58,     0,     0,    27,    43,
      32,     0,     0,    31,     0,     0,     0,    51,    53,    54,
       0,    61,   102,    86,     0,   103,   104,    63,   109,   111,
     113,   114,   108,   110,   112,     0,   107,   105,   106,     0,
     117,   118,   119,   120,   115,   116,     0,    63,     0,     0,
      79,     0,    75,     0,    87,     0,     0,    87,    62,    60,
      64,     0,     0,     0,     0,    55,     0,     0,    49,    45,
       0,   100,    68,    90,    92,    94,    67,     0,     0,    71,
       0,    72,     0,    85,    80,     0,    57,    39,     0,     0,
      56,    52,   101,    63,     0,    77,    76,    88,    81,     0,
       0,    34,    69,     0,     0,    82,    40,    63,    83,    70
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -133,  -133,  -133,   279,    30,   -11,  -133,   -44,   -24,  -133,
      59,  -132,  -133,   255,  -133,  -133,   215,  -133,   106,  -133,
     160,  -133,   -33,  -133,   -96,  -133,   -53,  -133,    72,  -133,
     -97,   -75,   140,   139,   -98,  -133,  -133,  -133,  -133
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     3,     4,    12,   135,    13,    21,    46,    24,    34,
      71,    72,   132,    35,    54,    55,   101,   136,   137,   138,
     139,   129,    87,    88,    89,   171,   113,   124,   218,    91,
     173,   174,   115,   116,   117,   159,   155,   166,    14
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      90,    36,    56,    25,   114,   118,   142,   141,   189,     6,
      19,    32,    74,   145,   146,    95,    78,    76,   169,   125,
      96,   160,   161,   162,    94,   175,   176,    53,   -63,    79,
     179,   163,   143,    80,   134,   144,    81,   183,   202,   123,
      82,     5,    83,   184,   204,    27,   190,    60,   177,    61,
      90,   192,   180,    33,   210,   164,   165,     7,    20,    26,
      75,   126,    84,    85,    86,    77,   170,   172,   195,     8,
     -63,   196,   178,    90,    26,     9,   156,   221,    65,   -79,
      73,    15,   128,    53,   -63,    79,    38,    38,    18,    80,
      49,    97,    81,   197,    90,   122,    82,    22,    83,    66,
     157,   158,   123,    38,   181,    38,    23,    99,     1,   186,
     147,     2,   187,    11,    90,   188,    26,   222,    84,    85,
      86,    67,    68,    69,    70,    30,   -63,   217,    37,    38,
     219,   229,    28,   148,   149,   150,    62,   151,    63,   223,
      40,    59,    41,    42,    43,   215,    29,   216,   206,   219,
     214,    44,    45,    31,    48,   152,   130,   167,   133,    50,
      90,   153,   154,   199,   200,   227,   148,   149,   150,    39,
     151,   148,   149,   150,    90,   151,    53,    64,    79,   148,
     149,   150,    80,   151,    47,    81,   104,    58,   152,    82,
      98,    83,    92,   152,   153,   154,    51,    52,    53,   153,
     154,   152,   105,   106,   107,   108,   109,   153,   154,    93,
     110,    84,    85,    86,   111,   112,   148,   149,   150,   100,
     151,   201,   202,   103,   224,   119,   148,   149,   150,   120,
     151,   148,   149,   150,   121,   151,   127,   131,   152,   205,
     203,   202,   140,   225,   153,   154,   191,   168,   152,    67,
      68,    69,    70,   152,   153,   154,   212,   202,   182,   153,
     154,   148,   149,   150,    10,   151,    11,   198,    16,   123,
      11,    53,   -13,   -13,   -13,   -13,   -13,   -13,   -13,   -13,
     207,   208,   209,   152,   213,   170,   220,   226,    17,   153,
     154,    57,   102,   211,   185,   193,   228,     0,   194
};

static const yytype_int16 yycheck[] =
{
      53,    25,    35,    14,    79,    80,   104,   103,   140,     1,
       1,     1,     1,   111,   112,    59,     1,     1,     1,    13,
      64,    34,    35,    36,    57,   122,   123,    12,    13,    14,
     126,    44,    57,    18,     9,   110,    21,    59,    59,    64,
      25,    55,    27,    65,    65,    15,   143,    50,   123,    52,
     103,   147,   127,    23,   186,    68,    69,    49,    49,    49,
      49,    55,    47,    48,    49,    49,    49,   120,   166,     0,
      55,   167,     1,   126,    49,    55,    37,   209,    48,    41,
      50,    57,    93,    12,    13,    14,    59,    59,    55,    18,
      63,    63,    21,   168,   147,    57,    25,    56,    27,     7,
      61,    62,    64,    59,   128,    59,     9,    63,     1,    63,
      15,     4,    55,     3,   167,    58,    49,   213,    47,    48,
      49,    29,    30,    31,    32,    60,    55,   202,    58,    59,
     205,   227,    56,    38,    39,    40,    50,    42,    52,   214,
      50,    60,    52,    53,    54,   198,    55,   200,   181,   224,
      22,    61,    62,    55,    55,    60,    97,    24,    99,    55,
     213,    66,    67,    58,    59,    24,    38,    39,    40,    49,
      42,    38,    39,    40,   227,    42,    12,    60,    14,    38,
      39,    40,    18,    42,    49,    21,    33,    49,    60,    25,
      64,    27,    55,    60,    66,    67,    10,    11,    12,    66,
      67,    60,    49,    50,    51,    52,    53,    66,    67,    55,
      57,    47,    48,    49,    61,    62,    38,    39,    40,    57,
      42,    58,    59,    55,    46,    49,    38,    39,    40,    57,
      42,    38,    39,    40,    57,    42,    41,    50,    60,    46,
      58,    59,    63,    65,    66,    67,    58,    41,    60,    29,
      30,    31,    32,    60,    66,    67,    58,    59,    45,    66,
      67,    38,    39,    40,     1,    42,     3,    59,     1,    64,
       3,    12,     9,    10,    11,    12,     9,    10,    11,    12,
      50,    50,     8,    60,    16,    49,    45,    50,     9,    66,
      67,    36,    77,   187,   134,   155,   224,    -1,   159
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     4,    71,    72,    55,     1,    49,     0,    55,
       1,     3,    73,    75,   108,    57,     1,    73,    55,     1,
      49,    76,    56,     9,    78,    75,    49,    74,    56,    55,
      60,    55,     1,    74,    79,    83,    78,    58,    59,    49,
      50,    52,    53,    54,    61,    62,    77,    49,    55,    63,
      55,    10,    11,    12,    84,    85,    92,    83,    49,    60,
      50,    52,    50,    52,    60,    74,     7,    29,    30,    31,
      32,    80,    81,    74,     1,    49,     1,    49,     1,    14,
      18,    21,    25,    27,    47,    48,    49,    92,    93,    94,
      96,    99,    55,    55,    92,    77,    77,    63,    64,    63,
      57,    86,    86,    55,    33,    49,    50,    51,    52,    53,
      57,    61,    62,    96,   101,   102,   103,   104,   101,    49,
      57,    57,    57,    64,    97,    13,    55,    41,    75,    91,
      80,    50,    82,    80,     9,    74,    87,    88,    89,    90,
      63,    94,   104,    57,   101,   104,   104,    15,    38,    39,
      40,    42,    60,    66,    67,   106,    37,    61,    62,   105,
      34,    35,    36,    44,    68,    69,   107,    24,    41,     1,
      49,    95,    96,   100,   101,   100,   100,   101,     1,    94,
     101,    78,    45,    59,    65,    90,    63,    55,    58,    81,
     100,    58,    94,   102,   103,   104,    94,   101,    59,    58,
      59,    58,    59,    58,    65,    46,    92,    50,    50,     8,
      81,    88,    58,    16,    22,    96,    96,   101,    98,   101,
      45,    81,    94,   101,    46,    65,    50,    24,    98,    94
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    70,    71,    71,    71,    71,    72,    72,    72,    73,
      73,    74,    74,    75,    75,    75,    76,    76,    76,    77,
      77,    77,    77,    77,    77,    77,    77,    78,    78,    78,
      79,    79,    79,    80,    80,    81,    81,    81,    81,    82,
      82,    83,    83,    84,    85,    85,    85,    85,    86,    86,
      87,    87,    87,    88,    88,    89,    90,    91,    92,    93,
      93,    93,    93,    94,    94,    94,    94,    94,    94,    94,
      94,    94,    94,    94,    94,    95,    95,    95,    96,    97,
      97,    97,    98,    98,    99,    99,   100,   100,   100,   101,
     101,   102,   102,   103,   103,   104,   104,   104,   104,   104,
     104,   104,   104,   104,   104,   105,   105,   105,   106,   106,
     106,   106,   106,   106,   106,   107,   107,   107,   107,   107,
     107,   108
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     4,     4,     3,     3,     5,     2,     2,     4,
       5,     1,     3,     0,     3,     3,     3,     5,     5,     1,
       2,     2,     1,     2,     2,     1,     1,     0,     3,     3,
       3,     5,     5,     1,     6,     1,     1,     1,     1,     3,
       5,     0,     3,     3,     3,     5,     2,     2,     0,     3,
       0,     1,     3,     1,     1,     2,     3,     3,     3,     1,
       3,     3,     3,     0,     3,     1,     1,     4,     4,     6,
       8,     4,     4,     1,     1,     1,     3,     3,     2,     0,
       3,     4,     2,     3,     1,     4,     0,     1,     3,     1,
       3,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       3,     4,     2,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, code_str, program, scanner, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, code_str, program, scanner); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, const char * code_str, ProgramStmt ** program, void * scanner)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (code_str);
  YY_USE (program);
  YY_USE (scanner);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, const char * code_str, ProgramStmt ** program, void * scanner)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, code_str, program, scanner);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, const char * code_str, ProgramStmt ** program, void * scanner)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), code_str, program, scanner);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, code_str, program, scanner); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




/* The kind of the lookahead of this context.  */
static yysymbol_kind_t
yypcontext_token (const yypcontext_t *yyctx) YY_ATTRIBUTE_UNUSED;

static yysymbol_kind_t
yypcontext_token (const yypcontext_t *yyctx)
{
  return yyctx->yytoken;
}

/* The location of the lookahead of this context.  */
static YYLTYPE *
yypcontext_location (const yypcontext_t *yyctx) YY_ATTRIBUTE_UNUSED;

static YYLTYPE *
yypcontext_location (const yypcontext_t *yyctx)
{
  return yyctx->yylloc;
}

/* User defined function to report a syntax error.  */
static int
yyreport_syntax_error (const yypcontext_t *yyctx, const char * code_str, ProgramStmt ** program, void * scanner);

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, const char * code_str, ProgramStmt ** program, void * scanner)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (code_str);
  YY_USE (program);
  YY_USE (scanner);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yykind)
    {
    case YYSYMBOL_IDENTIFIER: /* IDENTIFIER  */
#line 471 "yacc_pascal.y"
            { free(((*yyvaluep).string)); }
#line 1548 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_INTEGER: /* INTEGER  */
#line 470 "yacc_pascal.y"
            {}
#line 1554 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_BOOLEAN: /* BOOLEAN  */
#line 470 "yacc_pascal.y"
            {}
#line 1560 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_REAL: /* REAL  */
#line 471 "yacc_pascal.y"
            { free(((*yyvaluep).real)); }
#line 1566 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_CHAR: /* CHAR  */
#line 470 "yacc_pascal.y"
            {}
#line 1572 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_STRING: /* STRING  */
#line 471 "yacc_pascal.y"
            { free(((*yyvaluep).string)); }
#line 1578 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_programstruct: /* programstruct  */
#line 470 "yacc_pascal.y"
            {}
#line 1584 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_program_head: /* program_head  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).program_head); }
#line 1590 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_program_body: /* program_body  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).program_body); }
#line 1596 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_idlist: /* idlist  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).id_list); }
#line 1602 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_declarations: /* const_declarations  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).const_decls); }
#line 1608 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_declaration: /* const_declaration  */
#line 480 "yacc_pascal.y"
            {
    if(((*yyvaluep).kv_pair_list) != nullptr){
        for(auto pair : *((*yyvaluep).kv_pair_list)){
            delete pair->second;
            delete pair;
        }
        delete ((*yyvaluep).kv_pair_list);
    }
}
#line 1622 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_value: /* const_value  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).value); }
#line 1628 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_declarations: /* var_declarations  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1641 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_declaration: /* var_declaration  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1654 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_type: /* type  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1660 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_basic_type: /* basic_type  */
#line 470 "yacc_pascal.y"
            {}
#line 1666 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_period_list: /* period_list  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).period_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).period_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).period_list);
}
#line 1679 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_declarations: /* subprogram_declarations  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).func_decl_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).func_decl_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).func_decl_list);
}
#line 1692 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram: /* subprogram  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).func_decl); }
#line 1698 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_head: /* subprogram_head  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).func_head); }
#line 1704 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_formal_parameter: /* formal_parameter  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1717 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_parameter_list: /* parameter_list  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1730 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_parameter: /* parameter  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1736 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_parameter: /* var_parameter  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1742 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_value_parameter: /* value_parameter  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1748 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_body: /* subprogram_body  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).func_body); }
#line 1754 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_compound_statement: /* compound_statement  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1767 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_statement_list: /* statement_list  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1780 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_statement: /* statement  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1793 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_variable_list: /* variable_list  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).lval_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).lval_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).lval_list);
}
#line 1806 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_variable: /* variable  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).lval); }
#line 1812 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_id_varpart: /* id_varpart  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).expr_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).expr_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).expr_list);
}
#line 1825 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_array_index_expression: /* array_index_expression  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).expr_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).expr_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).expr_list);
}
#line 1838 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_procedure_call: /* procedure_call  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).func_call_stmt); }
#line 1844 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_expression_list: /* expression_list  */
#line 472 "yacc_pascal.y"
            {
    if(((*yyvaluep).expr_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).expr_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).expr_list);
}
#line 1857 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_expression: /* expression  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).expr); }
#line 1863 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_simple_expression: /* simple_expression  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).add_expr); }
#line 1869 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_term: /* term  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).mul_expr); }
#line 1875 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_factor: /* factor  */
#line 489 "yacc_pascal.y"
            { delete ((*yyvaluep).unary_expr); }
#line 1881 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_addop: /* addop  */
#line 470 "yacc_pascal.y"
            {}
#line 1887 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_relop: /* relop  */
#line 470 "yacc_pascal.y"
            {}
#line 1893 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_mulop: /* mulop  */
#line 470 "yacc_pascal.y"
            {}
#line 1899 "yacc_pascal.cpp"
        break;

      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (const char * code_str, ProgramStmt ** program, void * scanner)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */


/* User initialization code.  */
#line 365 "yacc_pascal.y"
{
    *program = nullptr;
}

#line 1998 "yacc_pascal.cpp"

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, scanner);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* programstruct: program_head ';' program_body '.'  */
#line 499 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProgramStruct;
        ProgramStmt * program_struct = create_node<ProgramStmt>();
        program_struct->head = std::unique_ptr<ProgramHeadStmt>((yyvsp[-3].program_head));
        program_struct->body = std::unique_ptr<ProgramBodyStmt>((yyvsp[-1].program_body));
        LOG_RULE("programstruct -> program_head ';' program_body '.'");
        *program = program_struct;
        (yyval.program_struct) = nullptr; // 防止报错
    }
#line 2219 "yacc_pascal.cpp"
    break;

  case 3: /* programstruct: error ';' program_body '.'  */
#line 509 "yacc_pascal.y"
    {
        *program = create_node<ProgramStmt>();
        delete (yyvsp[-1].program_body);
        (yyval.program_struct) = nullptr;
        LOG_ERROR_RULE("programstruct -> error ';' program_body '.'");
    }
#line 2230 "yacc_pascal.cpp"
    break;

  case 4: /* programstruct: program_head ';' error  */
#line 516 "yacc_pascal.y"
    {
        *program = create_node<ProgramStmt>();
        delete (yyvsp[-2].program_head);
        (yyval.program_struct) = nullptr;
        LOG_ERROR_RULE("programstruct -> program_head ';' error");
    }
#line 2241 "yacc_pascal.cpp"
    break;

  case 5: /* programstruct: error ';' error  */
#line 523 "yacc_pascal.y"
    {
        *program = create_node<ProgramStmt>();
        (yyval.program_struct) = nullptr;
        LOG_ERROR_RULE("programstruct -> error ';' error");
    }
#line 2251 "yacc_pascal.cpp"
    break;

  case 6: /* program_head: PROGRAM IDENTIFIER '(' idlist ')'  */
#line 532 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProgramHead;
        (yyval.program_head) = create_node<ProgramHeadStmt>();
        (yyval.program_head)->id_list = *(yyvsp[-1].id_list);
        delete (yyvsp[-1].id_list);
        free((yyvsp[-3].string));
        LOG_RULE("program_head -> PROGRAM IDENTIFIER '(' idlist ')'");
    }
#line 2264 "yacc_pascal.cpp"
    break;

  case 7: /* program_head: PROGRAM IDENTIFIER  */
#line 541 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProgramHead;
        (yyval.program_head) = create_node<ProgramHeadStmt>();
        (yyval.program_head)->id_list.emplace_back(std::string((yyvsp[0].string)));
        LOG_RULE("program_head -> PROGRAM IDENTIFIER");
        free((yyvsp[0].string));
    }
#line 2276 "yacc_pascal.cpp"
    break;

  case 8: /* program_head: PROGRAM error  */
#line 549 "yacc_pascal.y"
    {
        (yyval.program_head) = nullptr;
        LOG_ERROR_RULE("program_head -> PROGRAM error");
        yyerrok;
    }
#line 2286 "yacc_pascal.cpp"
    break;

  case 9: /* program_body: const_declarations var_declarations subprogram_declarations compound_statement  */
#line 558 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProgramBody;
        ProgramBodyStmt* program_body = create_node<ProgramBodyStmt>();
        
        // 处理常量声明
        if((yyvsp[-3].const_decls) != nullptr) {
            program_body->const_decl = std::unique_ptr<ConstDeclStmt>((yyvsp[-3].const_decls));
        }
        
        // 处理变量声明
        if((yyvsp[-2].var_decls) != nullptr) {
            for(auto var_decl : *(yyvsp[-2].var_decls)) {
                program_body->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(var_decl));
            }
            delete (yyvsp[-2].var_decls);
        }
        
        // 处理子程序声明
        if((yyvsp[-1].func_decl_list) != nullptr) {
            for(auto func_decl : *(yyvsp[-1].func_decl_list)) {
                program_body->func_decl.emplace_back(std::unique_ptr<FuncDeclStmt>(func_decl));
            }
            delete (yyvsp[-1].func_decl_list);
        }
        
        // 处理复合语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                program_body->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete (yyvsp[0].stmt_list);
        }
        
        (yyval.program_body) = program_body;
        LOG_RULE("program_body -> const_declarations var_declarations subprogram_declarations compound_statement");
    }
#line 2327 "yacc_pascal.cpp"
    break;

  case 10: /* program_body: error_recovery const_declarations var_declarations subprogram_declarations compound_statement  */
#line 595 "yacc_pascal.y"
    {
        // 清理分配的资源
        auto cleanup = [](auto* ptr) {
            if (ptr) delete ptr;
        };
        
        cleanup((yyvsp[-3].const_decls));
        
        if((yyvsp[-2].var_decls) != nullptr) {
            for(auto var_decl : *(yyvsp[-2].var_decls)) {
                delete var_decl;
            }
            delete (yyvsp[-2].var_decls);
        }
        
        if((yyvsp[-1].func_decl_list) != nullptr) {
            for(auto func_decl : *(yyvsp[-1].func_decl_list)) {
                delete func_decl;
            }
            delete (yyvsp[-1].func_decl_list);
        }
        
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                delete stmt;
            }
            delete (yyvsp[0].stmt_list);
        }
        
        (yyval.program_body) = nullptr;
        LOG_RULE("program_body -> error_recovery const_declarations var_declarations subprogram_declarations compound_statement");
    }
#line 2364 "yacc_pascal.cpp"
    break;

  case 11: /* idlist: IDENTIFIER  */
#line 631 "yacc_pascal.y"
    {
        current_rule = CurrentRule::IdList;
        (yyval.id_list) = create_node<std::vector<std::string>>();
        (yyval.id_list)->emplace_back(std::string((yyvsp[0].string)));
        LOG_RULE("idlist -> IDENTIFIER");
        free((yyvsp[0].string));
    }
#line 2376 "yacc_pascal.cpp"
    break;

  case 12: /* idlist: idlist ',' IDENTIFIER  */
#line 639 "yacc_pascal.y"
    {
        current_rule = CurrentRule::IdList;
        (yyvsp[-2].id_list)->emplace_back(std::string((yyvsp[0].string)));
        (yyval.id_list) = (yyvsp[-2].id_list);
        LOG_RULE("idlist -> idlist ',' IDENTIFIER");
        free((yyvsp[0].string));
    }
#line 2388 "yacc_pascal.cpp"
    break;

  case 13: /* const_declarations: %empty  */
#line 650 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ConstDeclarations;
        (yyval.const_decls) = nullptr;
        LOG_RULE("const_declarations -> empty");
    }
#line 2398 "yacc_pascal.cpp"
    break;

  case 14: /* const_declarations: CONST const_declaration ';'  */
#line 656 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ConstDeclarations;
        ConstDeclStmt * const_decls = create_node<ConstDeclStmt>();
        
        // 将声明列表中的键值对转移到常量声明对象中
        for(auto kv_pair : *(yyvsp[-1].kv_pair_list)) {
            const_decls->pairs.emplace_back(std::make_pair(kv_pair->first, kv_pair->second));
            delete kv_pair;
        }
        
        delete (yyvsp[-1].kv_pair_list);
        (yyval.const_decls) = const_decls;
        
        // 日志输出声明的常量信息
        for(auto &t: const_decls->pairs) {
            LOG_INFO("Get Const Type:%d, pointer %p", t.second->type, t.second.get());
            if(t.second->str) {
                LOG_INFO("Get string:%s", t.second->str->val.c_str());
            }
        }
        
        LOG_RULE("const_declarations -> CONST const_declaration ';' const_declarations");
    }
#line 2426 "yacc_pascal.cpp"
    break;

  case 15: /* const_declarations: CONST error ';'  */
#line 680 "yacc_pascal.y"
    {
        (yyval.const_decls) = nullptr;
        LOG_ERROR_RULE("const_declarations -> CONST error ;");
        yyerrok;
    }
#line 2436 "yacc_pascal.cpp"
    break;

  case 16: /* const_declaration: IDENTIFIER '=' const_value  */
#line 689 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ConstDeclaration;
        auto const_decls = create_node<std::vector<std::pair<std::string, ValueStmt *>*>>();
        auto kv_pair = create_node<std::pair<std::string, ValueStmt *>>((yyvsp[-2].string), (yyvsp[0].value));
        const_decls->emplace_back(kv_pair);
        free((yyvsp[-2].string));
        (yyval.kv_pair_list) = const_decls;
    }
#line 2449 "yacc_pascal.cpp"
    break;

  case 17: /* const_declaration: const_declaration ';' IDENTIFIER '=' const_value  */
#line 698 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ConstDeclaration;
        (yyvsp[-4].kv_pair_list)->emplace_back(create_node<std::pair<std::string, ValueStmt *>>((yyvsp[-2].string), (yyvsp[0].value)));
        free((yyvsp[-2].string));
        (yyval.kv_pair_list) = (yyvsp[-4].kv_pair_list);
    }
#line 2460 "yacc_pascal.cpp"
    break;

  case 18: /* const_declaration: error ';' IDENTIFIER '=' const_value  */
#line 705 "yacc_pascal.y"
    {
        HANDLE_ERROR("const_declaration -> error ';' IDENTIFIER = const_value", 
                    { free((yyvsp[-2].string)); delete (yyvsp[0].value); });
        (yyval.kv_pair_list) = nullptr;
    }
#line 2470 "yacc_pascal.cpp"
    break;

  case 19: /* const_value: INTEGER  */
#line 714 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_integer((yyvsp[0].number));
    }
#line 2478 "yacc_pascal.cpp"
    break;

  case 20: /* const_value: '+' INTEGER  */
#line 718 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_integer((yyvsp[0].number));
    }
#line 2486 "yacc_pascal.cpp"
    break;

  case 21: /* const_value: '-' INTEGER  */
#line 722 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_integer(-(yyvsp[0].number));
    }
#line 2494 "yacc_pascal.cpp"
    break;

  case 22: /* const_value: REAL  */
#line 726 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_real((yyvsp[0].real));
        free((yyvsp[0].real));
    }
#line 2503 "yacc_pascal.cpp"
    break;

  case 23: /* const_value: '+' REAL  */
#line 731 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_real((yyvsp[0].real));
        free((yyvsp[0].real));
    }
#line 2512 "yacc_pascal.cpp"
    break;

  case 24: /* const_value: '-' REAL  */
#line 736 "yacc_pascal.y"
    {
        ValueStmt* value = ValueNodeFactory::create_real((yyvsp[0].real));
        // 处理负号：将实数值设为负数
        value->number->real_val *= -1;
        free((yyvsp[0].real));
        (yyval.value) = value;
    }
#line 2524 "yacc_pascal.cpp"
    break;

  case 25: /* const_value: CHAR  */
#line 744 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_char((yyvsp[0].charactor));
        LOG_RULE("const_value -> CHAR, value: %c");
    }
#line 2533 "yacc_pascal.cpp"
    break;

  case 26: /* const_value: STRING  */
#line 749 "yacc_pascal.y"
    {
        (yyval.value) = ValueNodeFactory::create_string((yyvsp[0].string));
        free((yyvsp[0].string));
    }
#line 2542 "yacc_pascal.cpp"
    break;

  case 27: /* var_declarations: %empty  */
#line 757 "yacc_pascal.y"
    {
        (yyval.var_decls) = nullptr;
        LOG_RULE("var_declarations -> empty");
    }
#line 2551 "yacc_pascal.cpp"
    break;

  case 28: /* var_declarations: VAR var_declaration ';'  */
#line 762 "yacc_pascal.y"
    {
        current_rule = CurrentRule::VarDeclarations;
        (yyval.var_decls) = (yyvsp[-1].var_decls);
        LOG_RULE("var_declarations -> VAR var_declaration ';'");
    }
#line 2561 "yacc_pascal.cpp"
    break;

  case 29: /* var_declarations: VAR error ';'  */
#line 768 "yacc_pascal.y"
    {
        (yyval.var_decls) = nullptr;
        LOG_ERROR_RULE("var_declarations -> VAR error ;");
        yyerrok;
    }
#line 2571 "yacc_pascal.cpp"
    break;

  case 30: /* var_declaration: idlist ':' type  */
#line 777 "yacc_pascal.y"
    {
        current_rule = CurrentRule::VarDeclaration;
        auto var_decls = create_node<std::vector<VarDeclStmt *>>();
        auto var_decl = create_node<VarDeclStmt>();
        
        // 将标识符列表中的所有标识符复制到变量声明中
        var_decl->id.insert(var_decl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());
        
        // 处理类型信息
        var_decl->basic_type = (yyvsp[0].var_decl)->basic_type;
        var_decl->data_type = (yyvsp[0].var_decl)->data_type;
        var_decl->array_range = std::move((yyvsp[0].var_decl)->array_range);
        
        delete (yyvsp[-2].id_list);
        delete (yyvsp[0].var_decl);
        var_decls->emplace_back(var_decl);
        (yyval.var_decls) = var_decls;
        LOG_RULE("var_declaration -> idlist ':' type");
    }
#line 2595 "yacc_pascal.cpp"
    break;

  case 31: /* var_declaration: var_declaration ';' idlist ':' type  */
#line 797 "yacc_pascal.y"
    {
        current_rule = CurrentRule::VarDeclaration;
        auto var_decl = create_node<VarDeclStmt>();
        
        // 将标识符列表中的所有标识符复制到变量声明中
        var_decl->id.insert(var_decl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());
        
        // 处理类型信息
        var_decl->basic_type = (yyvsp[0].var_decl)->basic_type;
        var_decl->data_type = (yyvsp[0].var_decl)->data_type;
        var_decl->array_range = std::move((yyvsp[0].var_decl)->array_range);
        
        delete (yyvsp[-2].id_list);
        delete (yyvsp[0].var_decl);
        (yyvsp[-4].var_decls)->emplace_back(var_decl);
        (yyval.var_decls) = (yyvsp[-4].var_decls);
        LOG_RULE("var_declaration -> var_declaration ';' idlist ':' type");
    }
#line 2618 "yacc_pascal.cpp"
    break;

  case 32: /* var_declaration: error ';' idlist ':' type  */
#line 816 "yacc_pascal.y"
    {
        HANDLE_ERROR("var_declaration -> error ';' idlist ':' type",
                    { delete (yyvsp[-2].id_list); delete (yyvsp[0].var_decl); });
        (yyval.var_decls) = nullptr;
    }
#line 2628 "yacc_pascal.cpp"
    break;

  case 33: /* type: basic_type  */
#line 825 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Type;
        auto type_stmt = create_node<VarDeclStmt>();
        type_stmt->data_type = DataType::BasicType;
        type_stmt->basic_type = (yyvsp[0].basic_type);
        (yyval.var_decl) = type_stmt;
        LOG_RULE("type -> basic_type");
    }
#line 2641 "yacc_pascal.cpp"
    break;

  case 34: /* type: ARRAY '[' period_list ']' OF basic_type  */
#line 834 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Type;
        auto type_stmt = create_node<VarDeclStmt>();
        type_stmt->data_type = DataType::ArrayType;
        type_stmt->basic_type = (yyvsp[0].basic_type);
        
        // 转移数组范围信息
        for(auto period : *(yyvsp[-3].period_list)) {
            type_stmt->array_range.emplace_back(std::unique_ptr<PeriodStmt>(period));
        }
        
        delete (yyvsp[-3].period_list);
        (yyval.var_decl) = type_stmt;
        LOG_RULE("type -> ARRAY '[' period_list ']' OF basic_type");
    }
#line 2661 "yacc_pascal.cpp"
    break;

  case 35: /* basic_type: INTEGER_KW  */
#line 852 "yacc_pascal.y"
    {
        (yyval.basic_type) = BasicType::INT;
        LOG_RULE("basic_type -> INTEGER_KW");
    }
#line 2670 "yacc_pascal.cpp"
    break;

  case 36: /* basic_type: REAL_KW  */
#line 857 "yacc_pascal.y"
    {
        (yyval.basic_type) = BasicType::REAL;
        LOG_RULE("basic_type -> REAL_KW");
    }
#line 2679 "yacc_pascal.cpp"
    break;

  case 37: /* basic_type: BOOLEAN_KW  */
#line 862 "yacc_pascal.y"
    {
        (yyval.basic_type) = BasicType::BOOLEAN;
        LOG_RULE("basic_type -> BOOLEAN_KW");
    }
#line 2688 "yacc_pascal.cpp"
    break;

  case 38: /* basic_type: CHAR_KW  */
#line 867 "yacc_pascal.y"
    {
        (yyval.basic_type) = BasicType::CHAR;
        LOG_RULE("basic_type -> CHAR_KW");
    }
#line 2697 "yacc_pascal.cpp"
    break;

  case 39: /* period_list: INTEGER DOUBLE_DOT INTEGER  */
#line 875 "yacc_pascal.y"
    {
        auto period_list = create_node<std::vector<PeriodStmt *>>();
        auto period = create_node<PeriodStmt>();
        period->begin = (yyvsp[-2].number);
        period->end = (yyvsp[0].number);
        period_list->emplace_back(period);
        (yyval.period_list) = period_list;
        LOG_RULE("period_list -> INTEGER '..' INTEGER");
    }
#line 2711 "yacc_pascal.cpp"
    break;

  case 40: /* period_list: period_list ',' INTEGER DOUBLE_DOT INTEGER  */
#line 885 "yacc_pascal.y"
    {
        auto period = create_node<PeriodStmt>();
        period->begin = (yyvsp[-2].number);
        period->end = (yyvsp[0].number);
        (yyvsp[-4].period_list)->emplace_back(period);
        (yyval.period_list) = (yyvsp[-4].period_list);
        LOG_RULE("period_list -> period_list ',' INTEGER '..' INTEGER");
    }
#line 2724 "yacc_pascal.cpp"
    break;

  case 41: /* subprogram_declarations: %empty  */
#line 896 "yacc_pascal.y"
    {
        (yyval.func_decl_list) = nullptr;
        LOG_RULE("subprogram_declarations -> empty");
    }
#line 2733 "yacc_pascal.cpp"
    break;

  case 42: /* subprogram_declarations: subprogram_declarations subprogram ';'  */
#line 901 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SubprogramDeclarations;
        if((yyvsp[-2].func_decl_list) == nullptr) {
            auto func_decl_list = create_node<std::vector<FuncDeclStmt *>>();
            func_decl_list->emplace_back((yyvsp[-1].func_decl));    
            (yyval.func_decl_list) = func_decl_list;
        } else {
            (yyvsp[-2].func_decl_list)->emplace_back((yyvsp[-1].func_decl));
            (yyval.func_decl_list) = (yyvsp[-2].func_decl_list);
        }
        LOG_RULE("subprogram_declarations -> subprogram_declarations subprogram ';'");
    }
#line 2750 "yacc_pascal.cpp"
    break;

  case 43: /* subprogram: subprogram_head ';' subprogram_body  */
#line 917 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Subprogram;
        auto subprogram = create_node<FuncDeclStmt>();
        subprogram->header = std::unique_ptr<FuncHeadDeclStmt>((yyvsp[-2].func_head));
        subprogram->body = std::unique_ptr<FuncBodyDeclStmt>((yyvsp[0].func_body));
        (yyval.func_decl) = subprogram;
        LOG_RULE("subprogram -> subprogram_head ';' subprogram_body");
    }
#line 2763 "yacc_pascal.cpp"
    break;

  case 44: /* subprogram_head: PROCEDURE IDENTIFIER formal_parameter  */
#line 929 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SubprogramHead;
        auto sub_head = create_node<FuncHeadDeclStmt>();
        sub_head->func_name = std::string((yyvsp[-1].string));
        sub_head->ret_type = BasicType::VOID;
        
        // 处理形式参数
        if((yyvsp[0].var_decls) != nullptr) {
            for(auto formal_parameter : *(yyvsp[0].var_decls)) {
                sub_head->args.emplace_back(std::unique_ptr<VarDeclStmt>(formal_parameter));
            }
            delete (yyvsp[0].var_decls);
        }
        
        (yyval.func_head) = sub_head;
        free((yyvsp[-1].string));
        LOG_RULE("subprogram_head -> PROGRAM IDENTIFIER formal_parameter");
    }
#line 2786 "yacc_pascal.cpp"
    break;

  case 45: /* subprogram_head: FUNCTION IDENTIFIER formal_parameter ':' basic_type  */
#line 948 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SubprogramHead;
        auto sub_head = create_node<FuncHeadDeclStmt>();
        sub_head->func_name = std::string((yyvsp[-3].string));
        sub_head->ret_type = (yyvsp[0].basic_type);
        
        // 处理形式参数
        if((yyvsp[-2].var_decls) != nullptr) {
            for(auto formal_parameter : *(yyvsp[-2].var_decls)) {
                sub_head->args.emplace_back(std::unique_ptr<VarDeclStmt>(formal_parameter));
            }
            delete (yyvsp[-2].var_decls);
        }
        
        (yyval.func_head) = sub_head;
        free((yyvsp[-3].string));
        LOG_RULE("subprogram_head -> FUNCTION IDENTIFIER formal_parameter ':' basic_type");
    }
#line 2809 "yacc_pascal.cpp"
    break;

  case 46: /* subprogram_head: FUNCTION error  */
#line 967 "yacc_pascal.y"
    {
        (yyval.func_head) = nullptr;
        LOG_ERROR_RULE("subprogram_head -> FUNCTION error");
        yyerrok;
    }
#line 2819 "yacc_pascal.cpp"
    break;

  case 47: /* subprogram_head: PROCEDURE error  */
#line 973 "yacc_pascal.y"
    {
        (yyval.func_head) = nullptr;
        LOG_ERROR_RULE("subprogram_head -> PROCEDURE error");
        yyerrok;
    }
#line 2829 "yacc_pascal.cpp"
    break;

  case 48: /* formal_parameter: %empty  */
#line 981 "yacc_pascal.y"
    {
        (yyval.var_decls) = nullptr;
        LOG_RULE("formal_parameter -> empty");
    }
#line 2838 "yacc_pascal.cpp"
    break;

  case 49: /* formal_parameter: '(' parameter_list ')'  */
#line 986 "yacc_pascal.y"
    {
        current_rule = CurrentRule::FormalParameter;
        (yyval.var_decls) = (yyvsp[-1].var_decls);
        LOG_RULE("formal_parameter -> '(' parameter_list ')'");
    }
#line 2848 "yacc_pascal.cpp"
    break;

  case 50: /* parameter_list: %empty  */
#line 994 "yacc_pascal.y"
    {
        (yyval.var_decls) = nullptr;
        LOG_RULE("parameter_list -> empty");
    }
#line 2857 "yacc_pascal.cpp"
    break;

  case 51: /* parameter_list: parameter  */
#line 999 "yacc_pascal.y"
    {
        auto param_list = create_node<std::vector<VarDeclStmt *>>();
        param_list->emplace_back((yyvsp[0].var_decl));
        (yyval.var_decls) = param_list;
        LOG_RULE("parameter_list -> parameter");
    }
#line 2868 "yacc_pascal.cpp"
    break;

  case 52: /* parameter_list: parameter_list ';' parameter  */
#line 1006 "yacc_pascal.y"
    {
        (yyvsp[-2].var_decls)->emplace_back((yyvsp[0].var_decl));
        (yyval.var_decls) = (yyvsp[-2].var_decls);
        LOG_RULE("parameter_list -> parameter_list ';' parameter");
    }
#line 2878 "yacc_pascal.cpp"
    break;

  case 53: /* parameter: var_parameter  */
#line 1014 "yacc_pascal.y"
    {
        (yyval.var_decl) = (yyvsp[0].var_decl);
        (yyval.var_decl)->is_var = true;
        LOG_RULE("parameter -> var_parameter");
    }
#line 2888 "yacc_pascal.cpp"
    break;

  case 54: /* parameter: value_parameter  */
#line 1020 "yacc_pascal.y"
    {
        (yyval.var_decl) = (yyvsp[0].var_decl);
        (yyval.var_decl)->is_var = false;
        LOG_RULE("parameter -> value_parameter");
    }
#line 2898 "yacc_pascal.cpp"
    break;

  case 55: /* var_parameter: VAR value_parameter  */
#line 1029 "yacc_pascal.y"
    {
        (yyval.var_decl) = (yyvsp[0].var_decl);
        LOG_RULE("var_parameter -> VAR value_parameter");
    }
#line 2907 "yacc_pascal.cpp"
    break;

  case 56: /* value_parameter: idlist ':' basic_type  */
#line 1037 "yacc_pascal.y"
    {
        auto var_decl = create_node<VarDeclStmt>();
        var_decl->id.insert(var_decl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());
        var_decl->data_type = DataType::BasicType;
        var_decl->basic_type = (yyvsp[0].basic_type);
        var_decl->is_var = false;
        delete (yyvsp[-2].id_list);
        (yyval.var_decl) = var_decl;
        LOG_RULE("value_parameter -> idlist ':' basic_type");
    }
#line 2922 "yacc_pascal.cpp"
    break;

  case 57: /* subprogram_body: const_declarations var_declarations compound_statement  */
#line 1050 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SubprogramBody;
        auto func_body = create_node<FuncBodyDeclStmt>();
        
        // 处理常量声明
        if((yyvsp[-2].const_decls) != nullptr) {
            func_body->const_decl = std::unique_ptr<ConstDeclStmt>((yyvsp[-2].const_decls));
        }
        
        // 处理变量声明
        if((yyvsp[-1].var_decls) != nullptr) {
            for(auto var_decl : *(yyvsp[-1].var_decls)) {
                func_body->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(var_decl));
            }
            delete (yyvsp[-1].var_decls);
        }
        
        // 处理复合语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                func_body->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete (yyvsp[0].stmt_list);
        }
        
        (yyval.func_body) = func_body;
        LOG_RULE("subprogram_body -> const_declarations var_declarations compound_statement");
    }
#line 2955 "yacc_pascal.cpp"
    break;

  case 58: /* compound_statement: BEGIN_TOKEN statement_list END  */
#line 1082 "yacc_pascal.y"
    {
        current_rule = CurrentRule::CompoundStatement;
        (yyval.stmt_list) = (yyvsp[-1].stmt_list);
        LOG_RULE("compound_statement -> BEGIN_TOKEN statement_list END");
    }
#line 2965 "yacc_pascal.cpp"
    break;

  case 59: /* statement_list: statement  */
#line 1091 "yacc_pascal.y"
    {
        (yyval.stmt_list) = (yyvsp[0].stmt_list);
        LOG_RULE("statement_list -> statement");
    }
#line 2974 "yacc_pascal.cpp"
    break;

  case 60: /* statement_list: statement_list ';' statement  */
#line 1096 "yacc_pascal.y"
    {
        current_rule = CurrentRule::StatementList;
        
        // 如果有语句要添加，则处理
        if((yyvsp[0].stmt_list) != nullptr) {
            // 将第三个参数中的语句添加到结果列表中
            if((yyvsp[-2].stmt_list) != nullptr) {
                for(auto stmt : *(yyvsp[0].stmt_list)) {
                    (yyvsp[-2].stmt_list)->emplace_back(stmt);
                }
            } else {
                // 如果结果列表为空，则直接使用第三个参数
                (yyvsp[-2].stmt_list) = (yyvsp[0].stmt_list); 
                (yyvsp[0].stmt_list) = nullptr; // 避免被删除
            }
        }
        
        (yyval.stmt_list) = (yyvsp[-2].stmt_list);
        delete (yyvsp[0].stmt_list); // 如果$3已经被处理过，这里实际上删除的是nullptr
        LOG_RULE("statement_list -> statement_list ';' statement");
    }
#line 3000 "yacc_pascal.cpp"
    break;

  case 61: /* statement_list: error ';' statement  */
#line 1118 "yacc_pascal.y"
    {
        HANDLE_ERROR("statement_list -> error ';' statement",
                    {
                        if((yyvsp[0].stmt_list) != nullptr) {
                            for(auto item : *(yyvsp[0].stmt_list)) {
                                delete item;
                            }
                            delete (yyvsp[0].stmt_list);
                        }
                    });
        (yyval.stmt_list) = nullptr;
    }
#line 3017 "yacc_pascal.cpp"
    break;

  case 62: /* statement_list: statement_list ';' error  */
#line 1131 "yacc_pascal.y"
    {
        HANDLE_ERROR("statement_list -> statement_list ';' error",
                    {
                        if((yyvsp[-2].stmt_list) != nullptr) {
                            for(auto item : *(yyvsp[-2].stmt_list)) {
                                delete item;
                            }
                            delete (yyvsp[-2].stmt_list);
                        }
                    });
        (yyval.stmt_list) = nullptr;
    }
#line 3034 "yacc_pascal.cpp"
    break;

  case 63: /* statement: %empty  */
#line 1147 "yacc_pascal.y"
    {
        (yyval.stmt_list) = nullptr;
        LOG_RULE("statement -> empty");
    }
#line 3043 "yacc_pascal.cpp"
    break;

  case 64: /* statement: variable ASSIGNOP expression  */
#line 1152 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto assign_stmt = create_node<AssignStmt>();
        assign_stmt->lval = std::unique_ptr<LValStmt>((yyvsp[-2].lval));
        assign_stmt->expr = std::unique_ptr<ExprStmt>((yyvsp[0].expr));
        stmt_list->emplace_back(assign_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> variable ASSIGNOP expression");
    }
#line 3057 "yacc_pascal.cpp"
    break;

  case 65: /* statement: procedure_call  */
#line 1162 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        stmt_list->emplace_back((yyvsp[0].func_call_stmt));
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> procedure_call");
    }
#line 3068 "yacc_pascal.cpp"
    break;

  case 66: /* statement: compound_statement  */
#line 1169 "yacc_pascal.y"
    {
        (yyval.stmt_list) = (yyvsp[0].stmt_list);
        LOG_RULE("statement -> compound_statement");
    }
#line 3077 "yacc_pascal.cpp"
    break;

  case 67: /* statement: WHILE expression DO statement  */
#line 1174 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto while_stmt = create_node<WhileStmt>();
        while_stmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));
        
        // 处理循环体语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                while_stmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete (yyvsp[0].stmt_list);
        }
        
        stmt_list->emplace_back(while_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> WHILE expression DO statement");
    }
#line 3099 "yacc_pascal.cpp"
    break;

  case 68: /* statement: IF expression THEN statement  */
#line 1192 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto if_stmt = create_node<IfStmt>();
        if_stmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));
        
        // 处理if条件为真时的语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                if_stmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        delete (yyvsp[0].stmt_list);
        stmt_list->emplace_back(if_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> IF expression THEN statement");
    }
#line 3121 "yacc_pascal.cpp"
    break;

  case 69: /* statement: IF expression THEN statement ELSE statement  */
#line 1210 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto if_stmt = create_node<IfStmt>();
        if_stmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-4].expr));
        
        // 处理if条件为真时的语句
        if((yyvsp[-2].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[-2].stmt_list)) {
                if_stmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        // 处理if条件为假时的语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                if_stmt->false_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        stmt_list->emplace_back(if_stmt);
        delete (yyvsp[-2].stmt_list);
        delete (yyvsp[0].stmt_list);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> IF expression THEN statement ELSE statement");
    }
#line 3151 "yacc_pascal.cpp"
    break;

  case 70: /* statement: FOR IDENTIFIER ASSIGNOP expression TO expression DO statement  */
#line 1236 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto for_stmt = create_node<ForStmt>();
        for_stmt->id = std::string((yyvsp[-6].string));
        for_stmt->begin = std::unique_ptr<ExprStmt>((yyvsp[-4].expr));
        for_stmt->end = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));
        
        // 处理循环体语句
        if((yyvsp[0].stmt_list) != nullptr) {
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                for_stmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        stmt_list->emplace_back(for_stmt);
        free((yyvsp[-6].string));
        delete (yyvsp[0].stmt_list);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> FOR IDENTIFIER ASSIGNOP expression TO expression DO statement");
    }
#line 3176 "yacc_pascal.cpp"
    break;

  case 71: /* statement: READ '(' variable_list ')'  */
#line 1257 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto read_stmt = create_node<ReadFuncStmt>();
        
        // 处理变量列表
        for(auto lval : *(yyvsp[-1].lval_list)) {
            read_stmt->lval.emplace_back(std::unique_ptr<LValStmt>(lval));
        }
        
        delete (yyvsp[-1].lval_list);
        stmt_list->emplace_back(read_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> READ '(' variable_list ')'");
    }
#line 3195 "yacc_pascal.cpp"
    break;

  case 72: /* statement: WRITE '(' expression_list ')'  */
#line 1272 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto write_stmt = create_node<WriteFuncStmt>();
        
        // 处理表达式列表
        if((yyvsp[-1].expr_list) != nullptr) {
            for(auto expr : *(yyvsp[-1].expr_list)) {
                write_stmt->expr.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
        }
        
        stmt_list->emplace_back(write_stmt);
        delete (yyvsp[-1].expr_list);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> WRITE '(' expression_list ')'");
    }
#line 3216 "yacc_pascal.cpp"
    break;

  case 73: /* statement: BREAK  */
#line 1289 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto break_stmt = create_node<BreakStmt>();
        stmt_list->emplace_back(break_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> BREAK");
    }
#line 3228 "yacc_pascal.cpp"
    break;

  case 74: /* statement: CONTINUE  */
#line 1297 "yacc_pascal.y"
    {
        auto stmt_list = create_node<std::vector<BaseStmt *>>();
        auto continue_stmt = create_node<ContinueStmt>();
        stmt_list->emplace_back(continue_stmt);
        (yyval.stmt_list) = stmt_list;
        LOG_RULE("statement -> CONTINUE");
    }
#line 3240 "yacc_pascal.cpp"
    break;

  case 75: /* variable_list: variable  */
#line 1308 "yacc_pascal.y"
    {
        current_rule = CurrentRule::VariableList;
        auto lval_list = create_node<std::vector<LValStmt *>>();
        lval_list->emplace_back((yyvsp[0].lval));
        (yyval.lval_list) = lval_list;
        LOG_RULE("variable_list -> variable");
    }
#line 3252 "yacc_pascal.cpp"
    break;

  case 76: /* variable_list: variable_list ',' variable  */
#line 1316 "yacc_pascal.y"
    {
        current_rule = CurrentRule::VariableList;
        if((yyvsp[0].lval) != nullptr) {
            (yyvsp[-2].lval_list)->emplace_back((yyvsp[0].lval));
        } else {
            auto lval_list = create_node<std::vector<LValStmt *>>();
            lval_list->emplace_back((yyvsp[0].lval));
            (yyvsp[-2].lval_list) = lval_list;
        }
        
        (yyval.lval_list) = (yyvsp[-2].lval_list);
        LOG_RULE("variable_list -> variable_list ',' variable");
    }
#line 3270 "yacc_pascal.cpp"
    break;

  case 77: /* variable_list: error ',' variable  */
#line 1330 "yacc_pascal.y"
    {
        HANDLE_ERROR("variable_list -> error ',' variable", { delete (yyvsp[0].lval); });
        (yyval.lval_list) = nullptr;
    }
#line 3279 "yacc_pascal.cpp"
    break;

  case 78: /* variable: IDENTIFIER id_varpart  */
#line 1338 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Variable;
        auto lval = create_node<LValStmt>();
        lval->id = std::string((yyvsp[-1].string));
        
        // 处理变量的数组下标部分
        if((yyvsp[0].expr_list) != nullptr) {
            for(auto expr : *(yyvsp[0].expr_list)) {
                lval->array_index.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete (yyvsp[0].expr_list);
        }
        
        free((yyvsp[-1].string));
        (yyval.lval) = lval;
        LOG_RULE("variable -> IDENTIFIER id_varpart");
    }
#line 3301 "yacc_pascal.cpp"
    break;

  case 79: /* id_varpart: %empty  */
#line 1359 "yacc_pascal.y"
    {
        (yyval.expr_list) = nullptr;
        LOG_RULE("id_varpart -> empty");
    }
#line 3310 "yacc_pascal.cpp"
    break;

  case 80: /* id_varpart: '[' expression_list ']'  */
#line 1364 "yacc_pascal.y"
    {
        current_rule = CurrentRule::IdVarpart;
        if((yyvsp[-1].expr_list) != nullptr) {
            (yyval.expr_list) = (yyvsp[-1].expr_list);    
        } else {
            yyerror(&(yylsp[-1]), "code_str", program, scanner, "数组下标定义出错 请检查是否符合规范");
        }
        LOG_RULE("id_varpart -> '[' expression_list ']'");
    }
#line 3324 "yacc_pascal.cpp"
    break;

  case 81: /* id_varpart: '[' expression BRACE_PAIR array_index_expression  */
#line 1374 "yacc_pascal.y"
    {
        current_rule = CurrentRule::IdVarpart;
        if((yyvsp[0].expr_list) != nullptr) {
            (yyval.expr_list) = (yyvsp[0].expr_list);
            (yyval.expr_list)->emplace_back((yyvsp[-2].expr));
            std::reverse((yyval.expr_list)->begin(), (yyval.expr_list)->end());
        } else {
            // 错误处理
            (yyval.expr_list) = nullptr;
        }
        
        LOG_RULE("id_varpart -> '[' expression BRACE_PAIR array_index_expression");
    }
#line 3342 "yacc_pascal.cpp"
    break;

  case 82: /* array_index_expression: expression ']'  */
#line 1391 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ArrayIndexExpression;
        auto expr_list = create_node<std::vector<ExprStmt *>>();
        expr_list->emplace_back((yyvsp[-1].expr));
        (yyval.expr_list) = expr_list;
        LOG_RULE("array_index_expression -> expression_list");
    }
#line 3354 "yacc_pascal.cpp"
    break;

  case 83: /* array_index_expression: expression BRACE_PAIR array_index_expression  */
#line 1399 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ArrayIndexExpression;
        (yyvsp[0].expr_list)->emplace_back((yyvsp[-2].expr));
        (yyval.expr_list) = (yyvsp[0].expr_list);
        LOG_RULE("array_index_expression -> array_index_expression BRACE_PAIR expression ']'");
    }
#line 3365 "yacc_pascal.cpp"
    break;

  case 84: /* procedure_call: IDENTIFIER  */
#line 1409 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProcedureCall;
        auto proc_call = create_node<FuncCallStmt>();
        proc_call->id = std::string((yyvsp[0].string));
        free((yyvsp[0].string));
        (yyval.func_call_stmt) = proc_call;
        LOG_RULE("procedure_call -> IDENTIFIER");
    }
#line 3378 "yacc_pascal.cpp"
    break;

  case 85: /* procedure_call: IDENTIFIER '(' expression_list ')'  */
#line 1418 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ProcedureCall;
        auto proc_call = create_node<FuncCallStmt>();
        proc_call->id = std::string((yyvsp[-3].string));
        
        // 处理参数表达式列表
        if((yyvsp[-1].expr_list) != nullptr) {
            for(auto expr : *(yyvsp[-1].expr_list)) {
                proc_call->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete (yyvsp[-1].expr_list);
        }
        
        free((yyvsp[-3].string));
        (yyval.func_call_stmt) = proc_call;
        LOG_RULE("procedure_call -> IDENTIFIER '(' expression_list ')'");
    }
#line 3400 "yacc_pascal.cpp"
    break;

  case 86: /* expression_list: %empty  */
#line 1439 "yacc_pascal.y"
    {
        (yyval.expr_list) = nullptr;
        LOG_RULE("expression_list -> empty");
    }
#line 3409 "yacc_pascal.cpp"
    break;

  case 87: /* expression_list: expression  */
#line 1444 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ExpressionList;
        auto expr_list = create_node<std::vector<ExprStmt *>>();
        expr_list->emplace_back((yyvsp[0].expr));
        (yyval.expr_list) = expr_list;
        LOG_RULE("expression_list -> expression");
    }
#line 3421 "yacc_pascal.cpp"
    break;

  case 88: /* expression_list: expression_list ',' expression  */
#line 1452 "yacc_pascal.y"
    {
        current_rule = CurrentRule::ExpressionList;
        (yyvsp[-2].expr_list)->emplace_back((yyvsp[0].expr));
        (yyval.expr_list) = (yyvsp[-2].expr_list);
        LOG_RULE("expression_list -> expression_list ',' expression");
    }
#line 3432 "yacc_pascal.cpp"
    break;

  case 89: /* expression: simple_expression  */
#line 1462 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Expression;
        (yyval.expr) = ExprNodeFactory::create_from_simple_expr((yyvsp[0].add_expr));
        LOG_RULE("expression -> simple_expression");
    }
#line 3442 "yacc_pascal.cpp"
    break;

  case 90: /* expression: expression relop simple_expression  */
#line 1468 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Expression;
        auto expr = (yyvsp[-2].expr);
        RelExprStmt::Term term;
        term.type = get_rel_expr_type((yyvsp[-1].number));
        term.add_expr = std::unique_ptr<AddExprStmt>((yyvsp[0].add_expr));
        expr->rel_expr->terms.emplace_back(std::move(term));
        (yyval.expr) = expr;
        LOG_RULE("expression -> simple_expression relop simple_expression");
    }
#line 3457 "yacc_pascal.cpp"
    break;

  case 91: /* simple_expression: term  */
#line 1481 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SimpleExpression;
        (yyval.add_expr) = ExprNodeFactory::create_from_term((yyvsp[0].mul_expr));
        LOG_RULE("simple_expression -> term");
    }
#line 3467 "yacc_pascal.cpp"
    break;

  case 92: /* simple_expression: simple_expression addop term  */
#line 1487 "yacc_pascal.y"
    {
        current_rule = CurrentRule::SimpleExpression;
        auto add_expr = (yyvsp[-2].add_expr);
        AddExprStmt::Term term;
        term.type = get_add_expr_type((yyvsp[-1].number));
        term.mul_expr = std::unique_ptr<MulExprStmt>((yyvsp[0].mul_expr));
        add_expr->terms.emplace_back(std::move(term));
        (yyval.add_expr) = add_expr;
        LOG_RULE("simple_expression -> simple_expression %lld term\n");
    }
#line 3482 "yacc_pascal.cpp"
    break;

  case 93: /* term: factor  */
#line 1501 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Term;
        (yyval.mul_expr) = ExprNodeFactory::create_from_factor((yyvsp[0].unary_expr));
        LOG_RULE("term -> factor");
    }
#line 3492 "yacc_pascal.cpp"
    break;

  case 94: /* term: term mulop factor  */
#line 1507 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Term;
        auto mul_expr = (yyvsp[-2].mul_expr);
        MulExprStmt::Term term;
        term.type = get_mul_expr_type((yyvsp[-1].number));
        term.unary_expr = std::unique_ptr<UnaryExprStmt>((yyvsp[0].unary_expr));
        mul_expr->terms.emplace_back(std::move(term));
        (yyval.mul_expr) = mul_expr;
        LOG_RULE("term -> term mulop factor");
    }
#line 3507 "yacc_pascal.cpp"
    break;

  case 95: /* factor: INTEGER  */
#line 1521 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unary_expr->primary_expr->value->number = std::make_unique<NumberStmt>();
        fill_number_stmt(unary_expr->primary_expr->value->number, (yyvsp[0].number));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> INTEGER");
    }
#line 3522 "yacc_pascal.cpp"
    break;

  case 96: /* factor: REAL  */
#line 1532 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unary_expr->primary_expr->value->number = std::make_unique<NumberStmt>();
        double val = atof((yyvsp[0].real));
        fill_number_stmt(unary_expr->primary_expr->value->number, val);
        unary_expr->primary_expr->value->number->literal = std::string((yyvsp[0].real));
        free((yyvsp[0].real));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> REAL");
    }
#line 3540 "yacc_pascal.cpp"
    break;

  case 97: /* factor: BOOLEAN  */
#line 1546 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unary_expr->primary_expr->value->number = std::make_unique<NumberStmt>();
        long long int val = (yyvsp[0].boolean) ? 1 : 0;
        fill_number_stmt(unary_expr->primary_expr->value->number, val);
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> BOOLEAN");
    }
#line 3556 "yacc_pascal.cpp"
    break;

  case 98: /* factor: CHAR  */
#line 1558 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unary_expr->primary_expr->value->number = std::make_unique<NumberStmt>();
        fill_number_stmt(unary_expr->primary_expr->value->number, (yyvsp[0].charactor));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> CHAR");
    }
#line 3571 "yacc_pascal.cpp"
    break;

  case 99: /* factor: variable  */
#line 1569 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::LVal;
        unary_expr->primary_expr->value->lval = std::unique_ptr<LValStmt>((yyvsp[0].lval));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> variable");
    }
#line 3585 "yacc_pascal.cpp"
    break;

  case 100: /* factor: '(' expression ')'  */
#line 1579 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Parentheses);
        unary_expr->primary_expr->expr = std::unique_ptr<ExprStmt>((yyvsp[-1].expr));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> '(' expression ')'");
    }
#line 3597 "yacc_pascal.cpp"
    break;

  case 101: /* factor: IDENTIFIER '(' expression_list ')'  */
#line 1587 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = create_unary_expr(PrimaryExprStmt::PrimaryExprType::Value);
        unary_expr->primary_expr->value = std::make_unique<ValueStmt>();
        unary_expr->primary_expr->value->type = ValueStmt::ValueType::FuncCall;
        unary_expr->primary_expr->value->func_call = std::make_unique<FuncCallStmt>();
        unary_expr->primary_expr->value->func_call->id = std::string((yyvsp[-3].string));
        
        // 处理函数参数
        if((yyvsp[-1].expr_list) != nullptr) {
            for(auto expr : *(yyvsp[-1].expr_list)) {
                unary_expr->primary_expr->value->func_call->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete (yyvsp[-1].expr_list);
        }
        
        free((yyvsp[-3].string));
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> IDENTIFIER '(' expression_list ')'");
    }
#line 3622 "yacc_pascal.cpp"
    break;

  case 102: /* factor: NOT factor  */
#line 1608 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = (yyvsp[0].unary_expr);
        unary_expr->types.emplace_back(UnaryExprStmt::UnaryExprType::Not);
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> NOT factor");
    }
#line 3634 "yacc_pascal.cpp"
    break;

  case 103: /* factor: '+' factor  */
#line 1616 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        (yyval.unary_expr) = (yyvsp[0].unary_expr);  // 正号不改变值
        LOG_RULE("factor -> '+' factor");
    }
#line 3644 "yacc_pascal.cpp"
    break;

  case 104: /* factor: '-' factor  */
#line 1622 "yacc_pascal.y"
    {
        current_rule = CurrentRule::Factor;
        auto unary_expr = (yyvsp[0].unary_expr);
        unary_expr->types.emplace_back(UnaryExprStmt::UnaryExprType::Minus);
        (yyval.unary_expr) = unary_expr;
        LOG_RULE("factor -> '-' factor");
    }
#line 3656 "yacc_pascal.cpp"
    break;

  case 105: /* addop: '+'  */
#line 1631 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3662 "yacc_pascal.cpp"
    break;

  case 106: /* addop: '-'  */
#line 1631 "yacc_pascal.y"
                              { (yyval.number) = 1; }
#line 3668 "yacc_pascal.cpp"
    break;

  case 107: /* addop: OR  */
#line 1631 "yacc_pascal.y"
                                               { (yyval.number) = 2; }
#line 3674 "yacc_pascal.cpp"
    break;

  case 108: /* relop: '='  */
#line 1633 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3680 "yacc_pascal.cpp"
    break;

  case 109: /* relop: NE  */
#line 1633 "yacc_pascal.y"
                             { (yyval.number) = 1; }
#line 3686 "yacc_pascal.cpp"
    break;

  case 110: /* relop: '<'  */
#line 1633 "yacc_pascal.y"
                                               { (yyval.number) = 2; }
#line 3692 "yacc_pascal.cpp"
    break;

  case 111: /* relop: LE  */
#line 1633 "yacc_pascal.y"
                                                                { (yyval.number) = 3; }
#line 3698 "yacc_pascal.cpp"
    break;

  case 112: /* relop: '>'  */
#line 1633 "yacc_pascal.y"
                                                                                  { (yyval.number) = 4; }
#line 3704 "yacc_pascal.cpp"
    break;

  case 113: /* relop: GE  */
#line 1633 "yacc_pascal.y"
                                                                                                   { (yyval.number) = 5; }
#line 3710 "yacc_pascal.cpp"
    break;

  case 114: /* relop: IN  */
#line 1633 "yacc_pascal.y"
                                                                                                                    { (yyval.number) = 6; }
#line 3716 "yacc_pascal.cpp"
    break;

  case 115: /* mulop: '*'  */
#line 1635 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3722 "yacc_pascal.cpp"
    break;

  case 116: /* mulop: '/'  */
#line 1635 "yacc_pascal.y"
                              { (yyval.number) = 1; }
#line 3728 "yacc_pascal.cpp"
    break;

  case 117: /* mulop: DIV  */
#line 1635 "yacc_pascal.y"
                                                { (yyval.number) = 1; }
#line 3734 "yacc_pascal.cpp"
    break;

  case 118: /* mulop: MOD  */
#line 1635 "yacc_pascal.y"
                                                                  { (yyval.number) = 2; }
#line 3740 "yacc_pascal.cpp"
    break;

  case 119: /* mulop: AND  */
#line 1635 "yacc_pascal.y"
                                                                                    { (yyval.number) = 3; }
#line 3746 "yacc_pascal.cpp"
    break;

  case 120: /* mulop: ANDTHEN  */
#line 1635 "yacc_pascal.y"
                                                                                                          { (yyval.number) = 4; }
#line 3752 "yacc_pascal.cpp"
    break;

  case 121: /* error_recovery: error ';'  */
#line 1639 "yacc_pascal.y"
    {
        yyerrok;
    }
#line 3760 "yacc_pascal.cpp"
    break;


#line 3764 "yacc_pascal.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        if (yyreport_syntax_error (&yyctx, code_str, program, scanner) == 2)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, code_str, program, scanner);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, code_str, program, scanner);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, code_str, program, scanner, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, code_str, program, scanner);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, code_str, program, scanner);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 1644 "yacc_pascal.y"

// 此处书写相关函数，会添加在生成的代码中
extern void scan_string(const char *str, yyscan_t scanner);

// 相关所需的函数，可能包含一些错误处理函数
int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg)
{
    (void)program;
    (void)scanner;
    (void)msg;
    hadError = true;
    LOG_ERROR("[Unexcepted Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column + 1, msg);
    return 0;
}

static int yyreport_syntax_error(const yypcontext_t *ctx, const char * code_str, ProgramStmt ** program, void * scanner)
{
    hadError = true;
    int res = 0;
    std::ostringstream buf;
    buf << "\033[1;37m" << G_SETTINGS.input_file << ":" << yypcontext_location(ctx)->first_line 
        << ":" << yypcontext_location(ctx)->first_column + 1 << ":\033[0m";
    buf << " \033[1;31m" << "Syntax error:" << "\033[0m";
    bool have_expected = false;
    
    // 报告此处期望的token
    {
        enum { TOKENMAX = 5 };
        yysymbol_kind_t expected[TOKENMAX];
        int n = yypcontext_expected_tokens(ctx, expected, TOKENMAX);
        if (n < 0)
            // 向yyparse传递错误
            res = n;
        else {
            for (int i = 0; i < n; ++i)
                buf << (i == 0 ? " expected" : " or") << " " << yysymbol_name(expected[i]);
            if (n > 0)
                have_expected = true;
        }
    }
    
    // 报告意外的token
    {
        yysymbol_kind_t lookahead = yypcontext_token(ctx);
        if (lookahead != YYSYMBOL_YYEMPTY)
            buf << " before " << yysymbol_name(lookahead);
    }
    
    std::string error_note;
    std::string msg;
    
    get_error_location(code_str, yypcontext_location(ctx), error_note, msg, have_expected);
    if (have_expected)
        buf << " but found \"" << error_note << "\"";
    
    std::cerr << buf.str() << std::endl;
    std::cerr << msg << std::endl;
    
    return res;
}

int code_parse(const char * code_str, ProgramStmt ** program) {
    yyscan_t scanner;
    yylex_init(&scanner);
    scan_string(code_str, scanner);

    int ret = yyparse(code_str, program, scanner);
    
    yylex_destroy(scanner);
    if (hadError) {
        return -1;
    }
    return ret;
}
