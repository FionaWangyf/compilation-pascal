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

#include "common/log/log.hpp"//日志系统
#include "common/setting/settings.hpp"//编译器设置
#include "ast/stmt.hpp"//抽象语法树定义
#include "ast/stmt_test.hpp"//ast测试
#include "yacc_pascal.hpp"//bison生成的头文件
#include "lex_pascal.hpp"//flex生成的头文件

namespace {
    bool syntaxErrorFlag = false; //标记是否发生语法错误
    
    // 日志宏定义
    #define GRAMMAR_TRACE(rule)    LOG_DEBUG("TRACE: " rule)
    #define GRAMMAR_ERROR(rule)    LOG_DEBUG("ERROR: " rule)
    
    // 运算符类型映射表
    const std::map<long long, RelExprStmt::RelExprType> relOperatorMap = {
        {0, RelExprStmt::RelExprType::Equal},
        {1, RelExprStmt::RelExprType::NotEqual},
        {2, RelExprStmt::RelExprType::Less},
        {3, RelExprStmt::RelExprType::LessEqual},
        {4, RelExprStmt::RelExprType::Greater},
        {5, RelExprStmt::RelExprType::GreaterEqual},
        {6, RelExprStmt::RelExprType::In}
    };

    const std::map<long long, AddExprStmt::AddExprType> addOperatorMap = {
        {0, AddExprStmt::AddExprType::Plus},
        {1, AddExprStmt::AddExprType::Minus},
        {2, AddExprStmt::AddExprType::Or}
    };

    const std::map<long long, MulExprStmt::MulExprType> mulOperatorMap = {
        {0, MulExprStmt::MulExprType::Mul},
        {1, MulExprStmt::MulExprType::Div},
        {2, MulExprStmt::MulExprType::Mod},
        {3, MulExprStmt::MulExprType::And},
        {4, MulExprStmt::MulExprType::AndThen}
    };

    // 从映射表中获取运算符枚举类型
    template<typename EnumType, typename MapType>
    EnumType getOperatorKind(long long op, const MapType& typeMap) {
        auto it = typeMap.find(op);
        if (it != typeMap.end()) {
            return it->second;
        }
        return static_cast<EnumType>(0);
    }

    // 特化版本
    //关系运算符 (=, <>, <, <=, >, >=, in)
    RelExprStmt::RelExprType getRelationOperator(long long op) {
        return getOperatorKind<RelExprStmt::RelExprType>(op, relOperatorMap);
    }
    //加法级运算符 (+, -, or)
    AddExprStmt::AddExprType getArithmeticOperator(long long op) {
        return getOperatorKind<AddExprStmt::AddExprType>(op, addOperatorMap);
    }
    //乘法级运算符 (*, /, div, mod, and, andthen)
    MulExprStmt::MulExprType getTermOperator(long long op) {
        return getOperatorKind<MulExprStmt::MulExprType>(op, mulOperatorMap);
    }

    // 填充数值节点函数
    template<typename T>
    void setupNumberNode(std::unique_ptr<NumberStmt>& numNode, T val) {
        numNode->is_signed = true;
        numNode->is_real = std::is_same<T, double>::value;////在编译时检测类型，并相应设置 is_real 和 is_char 标志
        numNode->is_char = std::is_same<T, char>::value;
        numNode->is_unsigned = false;

        //根据类型将值存储到适当的字段中
        if constexpr (std::is_same<T, long long>::value) {//
            numNode->int_val = val;
        } 
        else if constexpr (std::is_same<T, double>::value) {
            numNode->real_val = val;
        } 
        else if constexpr (std::is_same<T, char>::value) {
            numNode->char_val = val;
            LOG_DEBUG("DEBUG setupNumberNode -> char_val: %c", val);
        }
    }

    // 节点创建函数
    template<typename T, typename... Args>
    T* allocateNode(Args&&... args) {
        return new T(std::forward<Args>(args)...);
    }

    // 将列表项目转移到容器函数：用于将一个临时的指针列表转移到所有者对象的智能指针容器中
    //函数将普通指针列表中的每个项目封装到 unique_ptr 中，然后添加到所有者对象的容器中，最后删除原始列表。确保了内存安全和所有权的明确转移。
    template<typename Owner, typename T>
    void moveListToContainer(Owner* owner, std::vector<T*>* list, std::vector<std::unique_ptr<T>> Owner::* container) {
        if (list) {
            for (auto item : *list) {
                (owner->*container).emplace_back(std::unique_ptr<T>(item));
            }
            delete list;
        }
    }

    // 值节点工厂类：用于创建不同类型的值节点对象
    class ValueFactory {
    public:
        //创建整数值节点，返回包含整数值的ValueStmt指针
        static ValueStmt* makeInteger(long long val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            setupNumberNode(value->number, val);
            return value;
        }
        //创建实数值节点，返回包含实数值的ValueStmt指针
        static ValueStmt* makeReal(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            double val = atof(str);//将字符串转换为双精度浮点数
            setupNumberNode(value->number, val);
            value->number->literal = std::string(str);
            return value;
        }
        //创建字符值节点，返回包含字符值的ValueStmt指针
        static ValueStmt* makeChar(char val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            setupNumberNode(value->number, val);
            return value;
        }
        //创建字符串值节点，返回包含字符串值的ValueStmt指针
        static ValueStmt* makeString(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Str;
            value->str = std::make_unique<StrStmt>();
            value->str->val = std::string(str).substr(1, std::string(str).length() - 2);//从输入字符串中提取内容，去掉首尾的引号
            return value;
        }
    };

    // 表达式节点工厂类：负责创建各种层次的表达式节点
    //Factor (因子) → Term (项) → Simple Expression (简单表达式) → Expression (表达式)
    class ExprFactory {
    public:
        //将简单表达式转换为完整表达式节点
        static ExprStmt* createFromSimpleExpr(AddExprStmt* simpleExpr) {
            ExprStmt* expr = new ExprStmt();
            expr->rel_expr = std::make_unique<RelExprStmt>();
            RelExprStmt::Term term;
            term.type = RelExprStmt::RelExprType::NULL_TYPE;
            term.add_expr = std::unique_ptr<AddExprStmt>(simpleExpr);
            expr->rel_expr->terms.emplace_back(std::move(term));
            return expr;
        }
        //将项(term)转换为简单表达式节点
        static AddExprStmt* createFromTerm(MulExprStmt* term) {
            AddExprStmt* addExpr = new AddExprStmt();
            AddExprStmt::Term termStruct;
            termStruct.type = AddExprStmt::AddExprType::NULL_TYPE;
            termStruct.mul_expr = std::unique_ptr<MulExprStmt>(term);
            addExpr->terms.emplace_back(std::move(termStruct));
            return addExpr;
        }
        //将因子(factor)转换为项节点
        static MulExprStmt* createFromFactor(UnaryExprStmt* factor) {
            MulExprStmt* mulExpr = new MulExprStmt();
            MulExprStmt::Term term;
            term.type = MulExprStmt::MulExprType::NULL_TYPE;
            term.unary_expr = std::unique_ptr<UnaryExprStmt>(factor);
            mulExpr->terms.emplace_back(std::move(term));
            return mulExpr;
        }
    };

    // 创建一元表达式的函数
    UnaryExprStmt* makeUnaryExpr(PrimaryExprStmt::PrimaryExprType type) {
        UnaryExprStmt* unary = new UnaryExprStmt();
        unary->primary_expr = std::make_unique<PrimaryExprStmt>();
        unary->primary_expr->type = type;
        return unary;
    }

    // 报告语法错误，输出错误信息并终止编译过程
    void reportSyntaxError(YYLTYPE *llocp, const char *msg) {
        LOG_ERROR("[Syntax Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column, msg);
        syntaxErrorFlag = true;
        exit(1);
    }

    // 获取错误位置函数，生成可视化的错误位置显示
    void locateErrorPosition(const char* code_str, YYLTYPE *llocp, std::string &error_note, std::string &msg, bool have_expected) {
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
}

// 此枚举类，跟踪编译器在语法分析过程中当前所处的上下文它，有助于提供准确的错误诊断，支持特定上下文的代码生成，
enum class ParserContext {
    ProgramStruct,      ///< 整个程序结构，Pascal程序的顶层结构
    ProgramHead,        ///< 程序头部，包括程序名称和参数列表，如 "program Calculator(input, output);"
    ProgramBody,        ///< 程序主体，包括声明部分和执行语句部分
    IdList,             ///< 标识符列表，用于变量声明、参数列表等，如 "x, y, z"
    
    ConstDeclarations,  ///< 常量声明部分，以 "const" 关键字开始的部分
    ConstDeclaration,   ///< 单个常量声明，如 "PI = 3.14159;"
    ConstValue,         ///< 常量值，如整数、实数、字符或字符串字面量
    
    VarDeclarations,    ///< 变量声明部分，以 "var" 关键字开始的部分
    VarDeclaration,     ///< 单个或多个变量的声明，如 "x, y: integer;"
    Type,               ///< 类型说明，如 "integer"、"array[1..10] of real" 等
    BasicType,          ///< 基本类型，如 "integer"、"real"、"boolean"、"char"
    PeriodList,         ///< 数组索引范围列表，如 "1..10, 1..20"
    
    SubprogramDeclarations, ///< 子程序声明部分，包括函数和过程声明
    Subprogram,         ///< 单个子程序（函数或过程）
    SubprogramHead,     ///< 子程序头部，如 "function Max(a, b: integer): integer;"
    FormalParameter,    ///< 形式参数部分，如 "(a, b: integer)"
    ParameterList,      ///< 参数列表，可能包含多组参数
    Parameter,          ///< 单个参数或参数组
    VarParameter,       ///< 变量参数（按引用传递），如 "var a: integer"
    ValueParameter,     ///< 值参数（按值传递），如 "a: integer"
    SubprogramBody,     ///< 子程序体，包括局部声明和语句部分
    
    CompoundStatement,  ///< 复合语句，由 "begin" 和 "end" 包围的语句块
    StatementList,      ///< 语句列表，由分号分隔的多个语句
    Statement,          ///< 单个语句，如赋值、条件、循环等
    ProcedureCall,      ///< 过程调用，如 "WriteLn(x, y);"
    
    VariableList,       ///< 变量列表，用于输入/输出语句等
    Variable,           ///< 单个变量引用，可能包括数组索引
    IdVarpart,          ///< 变量的附加部分，如数组索引 "[i, j]"
    
    ExpressionList,     ///< 表达式列表，如函数调用参数列表
    ArrayIndexExpression, ///< 数组索引表达式，如 "a[i, j+1]" 中的索引部分
    BracketExpressionList, ///< 方括号表达式列表，用于多维数组索引
    
    Expression,         ///< 完整表达式，可包含关系运算符（如 "a > b"）
    SimpleExpression,   ///< 简单表达式，可包含加法级运算符（如 "a + b"）
    Term,               ///< 项，可包含乘法级运算符（如 "a * b"）
    Factor,             ///< 因子，表达式的基本单位（如变量、常量、函数调用等）
};

// 跟踪编译器当前正在处理的语法规则
static ParserContext currentParserContext = ParserContext::ProgramStruct;

void resetSyntaxError() {
    syntaxErrorFlag = false; // 重置错误标志，为下一次解析准备
}
//当语法分析器检测到语法错误时，Bison会自动调用此函数报告错误。
int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg);


#line 371 "yacc_pascal.cpp"

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
  YYSYMBOL_procedure_call = 98,            /* procedure_call  */
  YYSYMBOL_expression_list = 99,           /* expression_list  */
  YYSYMBOL_expression = 100,               /* expression  */
  YYSYMBOL_simple_expression = 101,        /* simple_expression  */
  YYSYMBOL_term = 102,                     /* term  */
  YYSYMBOL_factor = 103,                   /* factor  */
  YYSYMBOL_addop = 104,                    /* addop  */
  YYSYMBOL_relop = 105,                    /* relop  */
  YYSYMBOL_mulop = 106,                    /* mulop  */
  YYSYMBOL_error_recovery = 107            /* error_recovery  */
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
#define YYLAST   276

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  70
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  38
/* YYNRULES -- Number of rules.  */
#define YYNRULES  118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  223

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
       0,   543,   543,   554,   561,   568,   579,   592,   602,   612,
     666,   717,   727,   740,   747,   772,   782,   793,   802,   816,
     821,   826,   831,   837,   843,   853,   859,   869,   875,   882,
     894,   916,   937,   952,   963,   984,   990,   996,  1002,  1012,
    1025,  1041,  1047,  1067,  1083,  1104,  1125,  1132,  1143,  1149,
    1160,  1166,  1175,  1186,  1193,  1203,  1211,  1229,  1261,  1274,
    1281,  1305,  1319,  1338,  1344,  1358,  1367,  1373,  1394,  1416,
    1445,  1469,  1486,  1505,  1516,  1531,  1542,  1554,  1565,  1589,
    1595,  1610,  1620,  1643,  1647,  1656,  1668,  1674,  1690,  1696,
    1712,  1718,  1734,  1746,  1763,  1777,  1790,  1801,  1810,  1832,
    1841,  1847,  1859,  1859,  1859,  1861,  1861,  1861,  1861,  1861,
    1861,  1861,  1863,  1863,  1863,  1863,  1863,  1863,  1867
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
  "variable", "id_varpart", "procedure_call", "expression_list",
  "expression", "simple_expression", "term", "factor", "addop", "relop",
  "mulop", "error_recovery", YY_NULLPTR
  };
  return yy_sname[yysymbol];
}
#endif

#define YYPACT_NINF (-134)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-80)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     102,   -38,     9,    27,    28,   226,  -134,    50,  -134,   238,
      36,    11,   107,   163,   118,   130,    36,   113,  -134,   126,
     143,   156,  -134,    12,  -134,   163,  -134,    84,  -134,   169,
     160,   171,   170,   -23,   173,    34,  -134,  -134,   185,   180,
    -134,  -134,  -134,  -134,   112,   123,  -134,   182,   130,   108,
     130,    22,    29,     8,   188,   189,  -134,    34,  -134,   160,
    -134,  -134,  -134,  -134,   160,    65,   181,  -134,  -134,  -134,
    -134,  -134,  -134,    82,  -134,   194,  -134,   194,   191,   133,
     133,   203,   196,   197,  -134,  -134,    52,  -134,    -2,  -134,
     214,  -134,  -134,   118,  -134,  -134,  -134,   108,   206,   108,
      23,   195,  -134,   153,   133,    44,  -134,  -134,  -134,  -134,
     133,   133,   133,  -134,    -1,   -19,    61,  -134,    93,   216,
      30,   133,   133,   133,  -134,  -134,    63,   133,   163,  -134,
    -134,   215,   -40,  -134,   130,    98,    64,  -134,  -134,  -134,
     167,  -134,  -134,   133,   149,  -134,  -134,   153,  -134,  -134,
    -134,  -134,  -134,  -134,  -134,   133,  -134,  -134,  -134,   133,
    -134,  -134,  -134,  -134,  -134,  -134,   133,   153,   133,   200,
     198,    96,  -134,   134,   166,   165,    21,  -134,  -134,   166,
     249,   213,   217,   256,  -134,   167,    23,  -134,  -134,   172,
    -134,   250,   -19,    61,  -134,  -134,    60,   219,  -134,   219,
    -134,   133,  -134,  -134,  -134,  -134,   220,   167,  -134,  -134,
    -134,   153,   133,  -134,  -134,   166,   221,  -134,  -134,   110,
    -134,   153,  -134
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     0,     0,     0,     8,     7,     1,     0,
       5,     0,     0,    27,    13,     0,     4,     0,   118,     0,
       0,     0,     3,     0,    41,    27,    11,     0,     2,    15,
       0,    14,     0,     0,     0,     0,    41,     6,     0,     0,
      19,    22,    25,    26,     0,     0,    16,     0,    29,     0,
      28,     0,     0,     0,     0,     0,     9,     0,    12,     0,
      20,    23,    21,    24,     0,     0,     0,    38,    35,    36,
      37,    30,    33,     0,    46,    48,    47,    48,     0,     0,
       0,     0,     0,     0,    73,    74,    81,    66,     0,    59,
       0,    65,    42,    13,    10,    18,    17,     0,     0,     0,
      50,     0,    44,    63,     0,    79,    92,    94,    93,    95,
       0,     0,     0,    96,     0,    86,    88,    90,     0,     0,
       0,    83,    83,    83,    78,    58,     0,     0,    27,    43,
      32,     0,     0,    31,     0,     0,     0,    51,    53,    54,
       0,    61,    99,    83,     0,   100,   101,    63,   106,   108,
     110,   111,   105,   107,   109,     0,   104,   102,   103,     0,
     114,   115,   116,   117,   112,   113,     0,    63,     0,     0,
      79,     0,    75,     0,    84,     0,     0,    62,    60,    64,
       0,     0,     0,     0,    55,     0,     0,    49,    45,     0,
      97,    68,    87,    89,    91,    67,     0,     0,    71,     0,
      72,     0,    82,    80,    57,    39,     0,     0,    56,    52,
      98,    63,     0,    77,    76,    85,     0,    34,    69,     0,
      40,    63,    70
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -134,  -134,  -134,   260,    39,    -8,  -134,    92,   -24,  -134,
     120,  -133,  -134,   234,  -134,  -134,   199,  -134,    86,  -134,
     139,  -134,   -33,  -134,   -98,  -134,   -53,  -134,  -134,   -75,
     -76,   119,   116,   -96,  -134,  -134,  -134,  -134
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     3,     4,    12,   135,    13,    21,    46,    24,    34,
      71,    72,   132,    35,    54,    55,   101,   136,   137,   138,
     139,   129,    87,    88,    89,   171,   113,   124,    91,   173,
     174,   115,   116,   117,   159,   155,   166,    14
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      90,    36,    56,   114,   118,   141,    25,   188,   142,    78,
       6,   125,    19,    32,   147,   145,   146,     5,   156,   182,
      53,   -63,    79,    74,    94,   183,    80,     8,   178,    81,
      76,   169,   134,    82,   144,    83,    38,   148,   149,   150,
      49,   151,   157,   158,    51,    52,    53,   175,   176,   191,
      90,   179,   208,   126,    27,    84,    85,    86,     7,   152,
      20,    26,    33,   -63,   177,   153,   154,   172,   189,   195,
     194,    75,    26,    90,   217,    53,   -63,    79,    77,   170,
     201,    80,   212,     9,    81,   128,   203,    65,    82,    73,
      83,    18,   196,   -79,    90,   160,   161,   162,   148,   149,
     150,   143,   151,     1,   180,   163,     2,    15,   123,   122,
      84,    85,    86,   218,    90,    66,   123,   167,   -63,   186,
     152,    11,   187,   222,    38,   215,   153,   154,    97,   164,
     165,   148,   149,   150,   221,   151,   219,    67,    68,    69,
      70,    38,    37,    38,   213,    99,   214,   204,   148,   149,
     150,    95,   151,   152,   198,   199,    96,    38,    90,   153,
     154,   185,    60,    22,    61,    53,   104,    79,    90,    28,
     152,    80,    23,    62,    81,    63,   153,   154,    82,    26,
      83,    29,   105,   106,   107,   108,   109,   148,   149,   150,
     110,   151,   200,   201,   111,   112,    67,    68,    69,    70,
      84,    85,    86,    30,   148,   149,   150,   190,   151,   152,
      40,    31,    41,    42,    43,   153,   154,   130,    39,   133,
      47,    44,    45,   202,   201,    48,   152,    10,    50,    11,
     210,   201,   153,   154,    58,   -13,   -13,   -13,   -13,    16,
      59,    11,    64,    92,    93,    98,   103,   -13,   -13,   -13,
     -13,   100,   119,   120,   121,   127,   131,   168,   140,   197,
     181,    53,   123,   205,   207,   216,   211,   206,   170,    17,
      57,   220,   209,   184,   192,   193,   102
};

static const yytype_uint8 yycheck[] =
{
      53,    25,    35,    79,    80,   103,    14,   140,   104,     1,
       1,    13,     1,     1,    15,   111,   112,    55,    37,    59,
      12,    13,    14,     1,    57,    65,    18,     0,   126,    21,
       1,     1,     9,    25,   110,    27,    59,    38,    39,    40,
      63,    42,    61,    62,    10,    11,    12,   122,   123,   147,
     103,   127,   185,    55,    15,    47,    48,    49,    49,    60,
      49,    49,    23,    55,     1,    66,    67,   120,   143,   167,
     166,    49,    49,   126,   207,    12,    13,    14,    49,    49,
      59,    18,    22,    55,    21,    93,    65,    48,    25,    50,
      27,    55,   168,    41,   147,    34,    35,    36,    38,    39,
      40,    57,    42,     1,   128,    44,     4,    57,    64,    57,
      47,    48,    49,   211,   167,     7,    64,    24,    55,    55,
      60,     3,    58,   221,    59,   201,    66,    67,    63,    68,
      69,    38,    39,    40,    24,    42,   212,    29,    30,    31,
      32,    59,    58,    59,   197,    63,   199,   180,    38,    39,
      40,    59,    42,    60,    58,    59,    64,    59,   211,    66,
      67,    63,    50,    56,    52,    12,    33,    14,   221,    56,
      60,    18,     9,    50,    21,    52,    66,    67,    25,    49,
      27,    55,    49,    50,    51,    52,    53,    38,    39,    40,
      57,    42,    58,    59,    61,    62,    29,    30,    31,    32,
      47,    48,    49,    60,    38,    39,    40,    58,    42,    60,
      50,    55,    52,    53,    54,    66,    67,    97,    49,    99,
      49,    61,    62,    58,    59,    55,    60,     1,    55,     3,
      58,    59,    66,    67,    49,     9,    10,    11,    12,     1,
      60,     3,    60,    55,    55,    64,    55,     9,    10,    11,
      12,    57,    49,    57,    57,    41,    50,    41,    63,    59,
      45,    12,    64,    50,     8,    45,    16,    50,    49,     9,
      36,    50,   186,   134,   155,   159,    77
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     4,    71,    72,    55,     1,    49,     0,    55,
       1,     3,    73,    75,   107,    57,     1,    73,    55,     1,
      49,    76,    56,     9,    78,    75,    49,    74,    56,    55,
      60,    55,     1,    74,    79,    83,    78,    58,    59,    49,
      50,    52,    53,    54,    61,    62,    77,    49,    55,    63,
      55,    10,    11,    12,    84,    85,    92,    83,    49,    60,
      50,    52,    50,    52,    60,    74,     7,    29,    30,    31,
      32,    80,    81,    74,     1,    49,     1,    49,     1,    14,
      18,    21,    25,    27,    47,    48,    49,    92,    93,    94,
      96,    98,    55,    55,    92,    77,    77,    63,    64,    63,
      57,    86,    86,    55,    33,    49,    50,    51,    52,    53,
      57,    61,    62,    96,   100,   101,   102,   103,   100,    49,
      57,    57,    57,    64,    97,    13,    55,    41,    75,    91,
      80,    50,    82,    80,     9,    74,    87,    88,    89,    90,
      63,    94,   103,    57,   100,   103,   103,    15,    38,    39,
      40,    42,    60,    66,    67,   105,    37,    61,    62,   104,
      34,    35,    36,    44,    68,    69,   106,    24,    41,     1,
      49,    95,    96,    99,   100,    99,    99,     1,    94,   100,
      78,    45,    59,    65,    90,    63,    55,    58,    81,    99,
      58,    94,   101,   102,   103,    94,   100,    59,    58,    59,
      58,    59,    58,    65,    92,    50,    50,     8,    81,    88,
      58,    16,    22,    96,    96,   100,    45,    81,    94,   100,
      50,    24,    94
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
      97,    98,    98,    99,    99,    99,   100,   100,   101,   101,
     102,   102,   103,   103,   103,   103,   103,   103,   103,   103,
     103,   103,   104,   104,   104,   105,   105,   105,   105,   105,
     105,   105,   106,   106,   106,   106,   106,   106,   107
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
       3,     1,     4,     0,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     1,     1,     1,     1,     3,     4,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2
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
#line 508 "yacc_pascal.y"
            { free(((*yyvaluep).string)); }
#line 1537 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_INTEGER: /* INTEGER  */
#line 506 "yacc_pascal.y"
            {}
#line 1543 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_BOOLEAN: /* BOOLEAN  */
#line 506 "yacc_pascal.y"
            {}
#line 1549 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_REAL: /* REAL  */
#line 508 "yacc_pascal.y"
            { free(((*yyvaluep).real)); }
#line 1555 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_CHAR: /* CHAR  */
#line 506 "yacc_pascal.y"
            {}
#line 1561 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_STRING: /* STRING  */
#line 508 "yacc_pascal.y"
            { free(((*yyvaluep).string)); }
#line 1567 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_programstruct: /* programstruct  */
#line 506 "yacc_pascal.y"
            {}
#line 1573 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_program_head: /* program_head  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).program_head); }
#line 1579 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_program_body: /* program_body  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).program_body); }
#line 1585 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_idlist: /* idlist  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).id_list); }
#line 1591 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_declarations: /* const_declarations  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).const_decls); }
#line 1597 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_declaration: /* const_declaration  */
#line 520 "yacc_pascal.y"
            {
    if(((*yyvaluep).kv_pair_list) != nullptr){
        for(auto pair : *((*yyvaluep).kv_pair_list)){
            delete pair->second;
            delete pair;
        }
        delete ((*yyvaluep).kv_pair_list);
    }
}
#line 1611 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_const_value: /* const_value  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).value); }
#line 1617 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_declarations: /* var_declarations  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1630 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_declaration: /* var_declaration  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1643 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_type: /* type  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1649 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_basic_type: /* basic_type  */
#line 506 "yacc_pascal.y"
            {}
#line 1655 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_period_list: /* period_list  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).period_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).period_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).period_list);
}
#line 1668 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_declarations: /* subprogram_declarations  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).func_decl_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).func_decl_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).func_decl_list);
}
#line 1681 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram: /* subprogram  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).func_decl); }
#line 1687 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_head: /* subprogram_head  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).func_head); }
#line 1693 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_formal_parameter: /* formal_parameter  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1706 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_parameter_list: /* parameter_list  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).var_decls) != nullptr){
        for(auto kv_pair : *((*yyvaluep).var_decls)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).var_decls);
}
#line 1719 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_parameter: /* parameter  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1725 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_var_parameter: /* var_parameter  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1731 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_value_parameter: /* value_parameter  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).var_decl); }
#line 1737 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_subprogram_body: /* subprogram_body  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).func_body); }
#line 1743 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_compound_statement: /* compound_statement  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1756 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_statement_list: /* statement_list  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1769 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_statement: /* statement  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).stmt_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).stmt_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).stmt_list);
}
#line 1782 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_variable_list: /* variable_list  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).lval_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).lval_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).lval_list);
}
#line 1795 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_variable: /* variable  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).lval); }
#line 1801 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_id_varpart: /* id_varpart  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).expr_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).expr_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).expr_list);
}
#line 1814 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_procedure_call: /* procedure_call  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).func_call_stmt); }
#line 1820 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_expression_list: /* expression_list  */
#line 510 "yacc_pascal.y"
            {
    if(((*yyvaluep).expr_list) != nullptr){
        for(auto kv_pair : *((*yyvaluep).expr_list)){
            delete kv_pair;
        }
    }
    delete ((*yyvaluep).expr_list);
}
#line 1833 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_expression: /* expression  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).expr); }
#line 1839 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_simple_expression: /* simple_expression  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).add_expr); }
#line 1845 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_term: /* term  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).mul_expr); }
#line 1851 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_factor: /* factor  */
#line 530 "yacc_pascal.y"
            { delete ((*yyvaluep).unary_expr); }
#line 1857 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_addop: /* addop  */
#line 506 "yacc_pascal.y"
            {}
#line 1863 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_relop: /* relop  */
#line 506 "yacc_pascal.y"
            {}
#line 1869 "yacc_pascal.cpp"
        break;

    case YYSYMBOL_mulop: /* mulop  */
#line 506 "yacc_pascal.y"
            {}
#line 1875 "yacc_pascal.cpp"
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
#line 363 "yacc_pascal.y"
{
    *program = nullptr;//初始化程序AST根节点为空指针
}

#line 1974 "yacc_pascal.cpp"

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
#line 543 "yacc_pascal.y"
                                           {
        currentParserContext = ParserContext::ProgramStruct;//设置当前解析上下文为程序结构
        ProgramStmt* programStruct = allocateNode<ProgramStmt>();//创建程序结构AST节点
        programStruct->head = std::unique_ptr<ProgramHeadStmt>((yyvsp[-3].program_head));//设置程序头部，使用智能指针管理内存
        programStruct->body = std::unique_ptr<ProgramBodyStmt>((yyvsp[-1].program_body));//设置程序主体，使用智能指针管理内存 
        
        GRAMMAR_TRACE("programstruct -> program_head ';' program_body '.'");
        *program = programStruct;//将构建的AST根节点存储到输出参数中
        (yyval.program_struct) = nullptr; // 防止报错
    }
#line 2196 "yacc_pascal.cpp"
    break;

  case 3: /* programstruct: error ';' program_body '.'  */
#line 554 "yacc_pascal.y"
                                    {
        *program = allocateNode<ProgramStmt>();//创建空的程序结构作为恢复
        delete (yyvsp[-1].program_body);//释放已解析的程序主体
        (yyval.program_struct) = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' program_body '.'");
    }
#line 2207 "yacc_pascal.cpp"
    break;

  case 4: /* programstruct: program_head ';' error  */
#line 561 "yacc_pascal.y"
                                {
        *program = allocateNode<ProgramStmt>();
        delete (yyvsp[-2].program_head);
        (yyval.program_struct) = nullptr;
        GRAMMAR_ERROR("programstruct -> program_head ';' error");
    }
#line 2218 "yacc_pascal.cpp"
    break;

  case 5: /* programstruct: error ';' error  */
#line 568 "yacc_pascal.y"
                         {
        *program = allocateNode<ProgramStmt>();
        (yyval.program_struct) = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' error");
    }
#line 2228 "yacc_pascal.cpp"
    break;

  case 6: /* program_head: PROGRAM IDENTIFIER '(' idlist ')'  */
#line 579 "yacc_pascal.y"
                                         {  // 匹配 "program 标识符 ( 标识符列表 )"
        currentParserContext = ParserContext::ProgramHead;  // 设置当前解析上下文为程序头
        (yyval.program_head) = allocateNode<ProgramHeadStmt>();  // 创建新的程序头AST节点
        (yyval.program_head)->id_list.push_back(std::string((yyvsp[-3].string)));  // 先添加程序名
        (yyval.program_head)->id_list.insert((yyval.program_head)->id_list.end(), (yyvsp[-1].id_list)->begin(), (yyvsp[-1].id_list)->end());  // 再添加参数
        //$$->id_list = *$4; 将标识符列表（参数）复制到程序头节点
        
        delete (yyvsp[-1].id_list);  // 释放标识符列表内存（已复制到AST节点中）
        free((yyvsp[-3].string));   // 释放标识符字符串（程序名称）内存
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER '(' idlist ')'");  // 记录语法跟踪日志
    }
#line 2244 "yacc_pascal.cpp"
    break;

  case 7: /* program_head: PROGRAM IDENTIFIER  */
#line 592 "yacc_pascal.y"
                          {  // 匹配 "program 标识符"
        currentParserContext = ParserContext::ProgramHead;  // 设置当前解析上下文为程序头
        (yyval.program_head) = allocateNode<ProgramHeadStmt>();  // 创建新的程序头AST节点
        (yyval.program_head)->id_list.emplace_back(std::string((yyvsp[0].string)));  // 将程序名称作为参数添加到id_list中
        
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER");  // 记录语法跟踪日志
        free((yyvsp[0].string));  // 释放标识符字符串（程序名称）内存
    }
#line 2257 "yacc_pascal.cpp"
    break;

  case 8: /* program_head: PROGRAM error  */
#line 602 "yacc_pascal.y"
                     {  // 匹配 "program" 后跟语法错误
        (yyval.program_head) = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("program_head -> PROGRAM error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2267 "yacc_pascal.cpp"
    break;

  case 9: /* program_body: const_declarations var_declarations subprogram_declarations compound_statement  */
#line 612 "yacc_pascal.y"
                                                                                      {
        // 设置当前解析上下文为程序体
        currentParserContext = ParserContext::ProgramBody;
        
        // 创建程序体AST节点
        ProgramBodyStmt* programBody = allocateNode<ProgramBodyStmt>();
        
        // 处理常量声明部分
        if((yyvsp[-3].const_decls) != nullptr) {  // 如果有常量声明
            // 将常量声明节点转移到程序体节点中，使用智能指针管理内存
            programBody->const_decl = std::unique_ptr<ConstDeclStmt>((yyvsp[-3].const_decls));
        }
        
        // 处理变量声明部分
        if((yyvsp[-2].var_decls) != nullptr) {  // 如果有变量声明
            // 遍历变量声明列表，将每个声明转移到程序体节点中
            for(auto varDecl : *(yyvsp[-2].var_decls)) {
                // 使用智能指针封装每个变量声明，并添加到程序体的变量声明列表中
                programBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));
            }
            // 释放变量声明列表容器（内部元素已被转移到程序体节点）
            delete (yyvsp[-2].var_decls);
        }
        
        // 处理子程序（函数/过程）声明部分
        if((yyvsp[-1].func_decl_list) != nullptr) {  // 如果有子程序声明
            // 遍历子程序声明列表，将每个声明转移到程序体节点中
            for(auto funcDecl : *(yyvsp[-1].func_decl_list)) {
                // 使用智能指针封装每个函数声明，并添加到程序体的函数声明列表中
                programBody->func_decl.emplace_back(std::unique_ptr<FuncDeclStmt>(funcDecl));
            }
            // 释放子程序声明列表容器
            delete (yyvsp[-1].func_decl_list);
        }
        
        // 处理复合语句部分（主要的执行代码）
        if((yyvsp[0].stmt_list) != nullptr) {  // 如果有复合语句
            // 遍历语句列表，将每个语句转移到程序体节点中
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                // 使用智能指针封装每个语句，并添加到程序体的语句列表中
                programBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            // 释放语句列表容器
            delete (yyvsp[0].stmt_list);
        }
        
        // 设置当前规则的返回值为构建好的程序体节点
        (yyval.program_body) = programBody;
        
        // 记录语法追踪日志
        GRAMMAR_TRACE("program_body -> const_declarations var_declarations subprogram_declarations compound_statement");
    }
#line 2324 "yacc_pascal.cpp"
    break;

  case 10: /* program_body: error_recovery const_declarations var_declarations subprogram_declarations compound_statement  */
#line 666 "yacc_pascal.y"
                                                                                                     {
        // 使用Lambda函数简化清理代码
        auto cleanupPtr = [](auto* ptr) {
            if (ptr) delete ptr;  // 如果指针非空，则删除它指向的对象
        };
        
        // 清理各个部分分配的内存
        cleanupPtr((yyvsp[-3].const_decls));  // 清理常量声明
        
        // 清理变量声明列表
        if((yyvsp[-2].var_decls) != nullptr) {
            // 遍历并删除每个变量声明
            for(auto varDecl : *(yyvsp[-2].var_decls)) {
                delete varDecl;
            }
            // 删除列表容器本身
            delete (yyvsp[-2].var_decls);
        }
        
        // 清理子程序声明列表
        if((yyvsp[-1].func_decl_list) != nullptr) {
            // 遍历并删除每个函数声明
            for(auto funcDecl : *(yyvsp[-1].func_decl_list)) {
                delete funcDecl;
            }
            // 删除列表容器本身
            delete (yyvsp[-1].func_decl_list);
        }
        
        // 清理语句列表
        if((yyvsp[0].stmt_list) != nullptr) {
            // 遍历并删除每个语句
            for(auto stmt : *(yyvsp[0].stmt_list)) {
                delete stmt;
            }
            // 删除列表容器本身
            delete (yyvsp[0].stmt_list);
        }
        
        // 设置返回值为nullptr，表示程序体构建失败
        (yyval.program_body) = nullptr;
        
        // 记录语法追踪日志
        GRAMMAR_TRACE("program_body -> error_recovery const_declarations var_declarations subprogram_declarations compound_statement");
    }
#line 2374 "yacc_pascal.cpp"
    break;

  case 11: /* idlist: IDENTIFIER  */
#line 717 "yacc_pascal.y"
                  {  // 匹配单个标识符，如 "input"
        currentParserContext = ParserContext::IdList;  // 设置当前解析上下文为标识符列表
        (yyval.id_list) = allocateNode<std::vector<std::string>>();  // 创建新的字符串向量
        (yyval.id_list)->emplace_back(std::string((yyvsp[0].string)));  // 将标识符添加到向量中
        
        GRAMMAR_TRACE("idlist -> IDENTIFIER");  // 记录语法跟踪日志
        free((yyvsp[0].string));  // 释放标识符字符串内存（已复制到向量中）
    }
#line 2387 "yacc_pascal.cpp"
    break;

  case 12: /* idlist: idlist ',' IDENTIFIER  */
#line 727 "yacc_pascal.y"
                             {  // 匹配"已存在的标识符列表,新标识符"，如 "input, output"
        currentParserContext = ParserContext::IdList;  // 设置当前解析上下文
        (yyvsp[-2].id_list)->emplace_back(std::string((yyvsp[0].string)));  // 将新标识符添加到已有列表中
        (yyval.id_list) = (yyvsp[-2].id_list);  // 返回更新后的列表
        
        GRAMMAR_TRACE("idlist -> idlist ',' IDENTIFIER");  // 记录语法跟踪日志
        free((yyvsp[0].string));  // 释放新标识符的字符串内存
    }
#line 2400 "yacc_pascal.cpp"
    break;

  case 13: /* const_declarations: %empty  */
#line 740 "yacc_pascal.y"
                 {  // 当程序中没有常量声明时匹配此规则
        currentParserContext = ParserContext::ConstDeclarations;  // 设置当前解析上下文为常量声明
        (yyval.const_decls) = nullptr;  // 返回空指针，表示没有常量声明
        GRAMMAR_TRACE("const_declarations -> empty");  // 记录语法跟踪日志
    }
#line 2410 "yacc_pascal.cpp"
    break;

  case 14: /* const_declarations: CONST const_declaration ';'  */
#line 747 "yacc_pascal.y"
                                   {  // 匹配 "const 常量定义列表;" 形式
        currentParserContext = ParserContext::ConstDeclarations;  // 设置当前解析上下文为常量声明
        ConstDeclStmt* constDecls = allocateNode<ConstDeclStmt>();  // 创建常量声明AST节点
        
        // 将声明列表中的键值对转移到常量声明对象中
        for(auto kvPair : *(yyvsp[-1].kv_pair_list)) {  // 遍历解析得到的常量定义键值对列表
            constDecls->pairs.emplace_back(std::make_pair(kvPair->first, kvPair->second));  // 创建新的键值对并添加到AST节点
            delete kvPair;  // 删除原始键值对对象（内容已被转移）
        }
        
        delete (yyvsp[-1].kv_pair_list);  // 删除原始键值对列表容器
        (yyval.const_decls) = constDecls;  // 设置返回值为构建的常量声明节点
        
        // 日志输出声明的常量信息（用于调试）
        for(auto &t: constDecls->pairs) {  // 遍历所有常量定义
            LOG_INFO("Get Const Type:%d, pointer %p", t.second->type, t.second.get());  // 记录常量类型和指针地址
            if(t.second->str) {  // 如果常量是字符串类型
                LOG_INFO("Get string:%s", t.second->str->val.c_str());  // 记录字符串值
            }
        }
        
        GRAMMAR_TRACE("const_declarations -> CONST const_declaration ';' const_declarations");  // 记录语法跟踪日志
    }
#line 2438 "yacc_pascal.cpp"
    break;

  case 15: /* const_declarations: CONST error ';'  */
#line 772 "yacc_pascal.y"
                       {  // 匹配 "const [错误] ;" 形式
        (yyval.const_decls) = nullptr;  // 设置返回值为空指针，表示常量声明解析失败
        GRAMMAR_ERROR("const_declarations -> CONST error ;");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2448 "yacc_pascal.cpp"
    break;

  case 16: /* const_declaration: IDENTIFIER '=' const_value  */
#line 782 "yacc_pascal.y"
                                  {  // 匹配 "标识符 = 常量值" 形式，如 "PI = 3.14159"
        currentParserContext = ParserContext::ConstDeclaration;  // 设置当前解析上下文为常量声明
        auto constDecls = allocateNode<std::vector<std::pair<std::string, ValueStmt *>*>>();  // 创建存储键值对指针的向量
        auto kvPair = allocateNode<std::pair<std::string, ValueStmt *>>((yyvsp[-2].string), (yyvsp[0].value));  // 创建键值对，存储常量名和值
        
        constDecls->emplace_back(kvPair);  // 将键值对指针添加到向量中
        free((yyvsp[-2].string));  // 释放标识符字符串（已复制到键值对中）
        (yyval.kv_pair_list) = constDecls;  // 设置返回值为构建的常量声明列表
    }
#line 2462 "yacc_pascal.cpp"
    break;

  case 17: /* const_declaration: const_declaration ';' IDENTIFIER '=' const_value  */
#line 793 "yacc_pascal.y"
                                                        {  // 匹配 "已有声明; 标识符 = 常量值" 形式
        currentParserContext = ParserContext::ConstDeclaration;  // 设置当前解析上下文
        (yyvsp[-4].kv_pair_list)->emplace_back(allocateNode<std::pair<std::string, ValueStmt *>>((yyvsp[-2].string), (yyvsp[0].value)));  // 创建新键值对并添加到现有列表
        
        free((yyvsp[-2].string));  // 释放标识符字符串
        (yyval.kv_pair_list) = (yyvsp[-4].kv_pair_list);  // 返回更新后的常量声明列表
    }
#line 2474 "yacc_pascal.cpp"
    break;

  case 18: /* const_declaration: error ';' IDENTIFIER '=' const_value  */
#line 802 "yacc_pascal.y"
                                            {  // 匹配 "[错误]; 标识符 = 常量值" 形式
        free((yyvsp[-2].string));  // 释放标识符字符串
        delete (yyvsp[0].value);  // 删除常量值对象
        (yyval.kv_pair_list) = nullptr;  // 设置返回值为空指针，表示解析失败
        
        GRAMMAR_ERROR("const_declaration -> error ';' IDENTIFIER = const_value");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2487 "yacc_pascal.cpp"
    break;

  case 19: /* const_value: INTEGER  */
#line 816 "yacc_pascal.y"
               {  // 匹配整数，如 "42"
        (yyval.value) = ValueFactory::makeInteger((yyvsp[0].number));  // 使用工厂方法创建整数值对象
    }
#line 2495 "yacc_pascal.cpp"
    break;

  case 20: /* const_value: '+' INTEGER  */
#line 821 "yacc_pascal.y"
                   {  // 匹配带正号的整数，如 "+42"
        (yyval.value) = ValueFactory::makeInteger((yyvsp[0].number));  // 创建整数值对象（正号不改变值）
    }
#line 2503 "yacc_pascal.cpp"
    break;

  case 21: /* const_value: '-' INTEGER  */
#line 826 "yacc_pascal.y"
                   {  // 匹配带负号的整数，如 "-42"
        (yyval.value) = ValueFactory::makeInteger(-(yyvsp[0].number));  // 创建负整数值对象（对值取负）
    }
#line 2511 "yacc_pascal.cpp"
    break;

  case 22: /* const_value: REAL  */
#line 831 "yacc_pascal.y"
            {  // 匹配实数，如 "3.14159"
        (yyval.value) = ValueFactory::makeReal((yyvsp[0].real));  // 创建实数值对象
        free((yyvsp[0].real));  // 释放实数字符串（已在值对象中复制）
    }
#line 2520 "yacc_pascal.cpp"
    break;

  case 23: /* const_value: '+' REAL  */
#line 837 "yacc_pascal.y"
                {  // 匹配带正号的实数，如 "+3.14159"
        (yyval.value) = ValueFactory::makeReal((yyvsp[0].real));  // 创建实数值对象（正号不改变值）
        free((yyvsp[0].real));  // 释放实数字符串
    }
#line 2529 "yacc_pascal.cpp"
    break;

  case 24: /* const_value: '-' REAL  */
#line 843 "yacc_pascal.y"
                {  // 匹配带负号的实数，如 "-3.14159"
        ValueStmt* value = ValueFactory::makeReal((yyvsp[0].real));  // 先创建实数值对象
        // 处理负号：将实数值设为负数
        value->number->real_val *= -1;  // 将实数值取负（直接修改对象中的值）
        
        free((yyvsp[0].real));  // 释放实数字符串
        (yyval.value) = value;  // 返回修改后的值对象
    }
#line 2542 "yacc_pascal.cpp"
    break;

  case 25: /* const_value: CHAR  */
#line 853 "yacc_pascal.y"
            {  // 匹配字符常量，如 "'A'"
        (yyval.value) = ValueFactory::makeChar((yyvsp[0].charactor));  // 创建字符值对象
        GRAMMAR_TRACE("const_value -> CHAR, value: %c");  // 记录语法跟踪日志（注意：缺少参数）
    }
#line 2551 "yacc_pascal.cpp"
    break;

  case 26: /* const_value: STRING  */
#line 859 "yacc_pascal.y"
              {  // 匹配字符串常量，如 "'Hello'"
        (yyval.value) = ValueFactory::makeString((yyvsp[0].string));  // 创建字符串值对象
        free((yyvsp[0].string));  // 释放字符串（已在值对象中复制）
    }
#line 2560 "yacc_pascal.cpp"
    break;

  case 27: /* var_declarations: %empty  */
#line 869 "yacc_pascal.y"
                 {  // 当程序中没有变量声明时匹配此规则
        (yyval.var_decls) = nullptr;  // 返回空指针，表示没有变量声明
        GRAMMAR_TRACE("var_declarations -> empty");  // 记录语法跟踪日志
    }
#line 2569 "yacc_pascal.cpp"
    break;

  case 28: /* var_declarations: VAR var_declaration ';'  */
#line 875 "yacc_pascal.y"
                               {  // 匹配 "var 变量声明列表;" 形式
        currentParserContext = ParserContext::VarDeclarations;  // 设置当前解析上下文为变量声明
        (yyval.var_decls) = (yyvsp[-1].var_decls);  // 直接返回var_declaration的结果（变量声明列表）
        GRAMMAR_TRACE("var_declarations -> VAR var_declaration ';'");  // 记录语法跟踪日志
    }
#line 2579 "yacc_pascal.cpp"
    break;

  case 29: /* var_declarations: VAR error ';'  */
#line 882 "yacc_pascal.y"
                     {  // 匹配 "var [错误] ;" 形式
        (yyval.var_decls) = nullptr;  // 设置返回值为空指针，表示变量声明解析失败
        GRAMMAR_ERROR("var_declarations -> VAR error ;");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2589 "yacc_pascal.cpp"
    break;

  case 30: /* var_declaration: idlist ':' type  */
#line 894 "yacc_pascal.y"
                       {  // 匹配 "标识符列表 : 类型" 形式，如 "x, y : integer"
        currentParserContext = ParserContext::VarDeclaration;  // 设置当前解析上下文为变量声明
        auto varDecls = allocateNode<std::vector<VarDeclStmt *>>();  // 创建变量声明列表容器
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建单个变量声明对象
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());  // 添加所有标识符到变量声明对象
        
        // 处理类型信息
        varDecl->basic_type = (yyvsp[0].var_decl)->basic_type;  // 复制基本类型（integer, real等）
        varDecl->data_type = (yyvsp[0].var_decl)->data_type;    // 复制数据类型（基本类型、数组等）
        varDecl->array_range = std::move((yyvsp[0].var_decl)->array_range);  // 移动数组范围信息（如果是数组类型）
        
        delete (yyvsp[-2].id_list);  // 释放标识符列表
        delete (yyvsp[0].var_decl);  // 释放类型对象
        varDecls->emplace_back(varDecl);  // 将变量声明添加到列表中
        (yyval.var_decls) = varDecls;  // 返回变量声明列表
        
        GRAMMAR_TRACE("var_declaration -> idlist ':' type");  // 记录语法跟踪日志
    }
#line 2614 "yacc_pascal.cpp"
    break;

  case 31: /* var_declaration: var_declaration ';' idlist ':' type  */
#line 916 "yacc_pascal.y"
                                           {  // 匹配 "已有声明; 标识符列表 : 类型" 形式
        currentParserContext = ParserContext::VarDeclaration;  // 设置当前解析上下文
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建新的变量声明对象
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());  // 添加所有标识符到变量声明对象
        
        // 处理类型信息
        varDecl->basic_type = (yyvsp[0].var_decl)->basic_type;  // 复制基本类型
        varDecl->data_type = (yyvsp[0].var_decl)->data_type;    // 复制数据类型
        varDecl->array_range = std::move((yyvsp[0].var_decl)->array_range);  // 移动数组范围信息
        
        delete (yyvsp[-2].id_list);  // 释放标识符列表
        delete (yyvsp[0].var_decl);  // 释放类型对象
        (yyvsp[-4].var_decls)->emplace_back(varDecl);  // 将新变量声明添加到已有列表中
        (yyval.var_decls) = (yyvsp[-4].var_decls);  // 返回更新后的变量声明列表
        
        GRAMMAR_TRACE("var_declaration -> var_declaration ';' idlist ':' type");  // 记录语法跟踪日志
    }
#line 2638 "yacc_pascal.cpp"
    break;

  case 32: /* var_declaration: error ';' idlist ':' type  */
#line 937 "yacc_pascal.y"
                                 {  // 匹配 "[错误]; 标识符列表 : 类型" 形式
        delete (yyvsp[-2].id_list);  // 释放标识符列表
        delete (yyvsp[0].var_decl);  // 释放类型对象
        (yyval.var_decls) = nullptr;  // 设置返回值为空指针，表示解析失败
        
        GRAMMAR_ERROR("var_declaration -> error ';' idlist ':' type");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2651 "yacc_pascal.cpp"
    break;

  case 33: /* type: basic_type  */
#line 952 "yacc_pascal.y"
                  {  // 匹配基本类型，如 "integer"、"real"、"boolean"、"char"
        currentParserContext = ParserContext::Type;  // 设置当前解析上下文为类型
        auto typeStmt = allocateNode<VarDeclStmt>();  // 创建变量声明对象用于存储类型信息
        typeStmt->data_type = DataType::BasicType;  // 设置数据类型为基本类型
        typeStmt->basic_type = (yyvsp[0].basic_type);  // 设置具体的基本类型（integer, real等）
        
        (yyval.var_decl) = typeStmt;  // 返回创建的类型对象
        GRAMMAR_TRACE("type -> basic_type");  // 记录语法跟踪日志
    }
#line 2665 "yacc_pascal.cpp"
    break;

  case 34: /* type: ARRAY '[' period_list ']' OF basic_type  */
#line 963 "yacc_pascal.y"
                                               {  // 匹配 "array [范围列表] of 基本类型"
        currentParserContext = ParserContext::Type;  // 设置当前解析上下文为类型
        auto typeStmt = allocateNode<VarDeclStmt>();  // 创建变量声明对象用于存储类型信息
        typeStmt->data_type = DataType::ArrayType;  // 设置数据类型为数组类型
        typeStmt->basic_type = (yyvsp[0].basic_type);  // 设置数组元素的基本类型
        
        // 转移数组范围信息
        for(auto period : *(yyvsp[-3].period_list)) {  // 遍历范围列表
            typeStmt->array_range.emplace_back(std::unique_ptr<PeriodStmt>(period));  // 将每个范围添加到数组范围容器
        }
        
        delete (yyvsp[-3].period_list);  // 释放原始范围列表容器
        (yyval.var_decl) = typeStmt;  // 返回创建的类型对象
        GRAMMAR_TRACE("type -> ARRAY '[' period_list ']' OF basic_type");  // 记录语法跟踪日志
    }
#line 2685 "yacc_pascal.cpp"
    break;

  case 35: /* basic_type: INTEGER_KW  */
#line 984 "yacc_pascal.y"
                  {  // 匹配 "integer" 关键字
        (yyval.basic_type) = BasicType::INT;  // 返回整数类型枚举值
        GRAMMAR_TRACE("basic_type -> INTEGER_KW");  // 记录语法跟踪日志
    }
#line 2694 "yacc_pascal.cpp"
    break;

  case 36: /* basic_type: REAL_KW  */
#line 990 "yacc_pascal.y"
               {  // 匹配 "real" 关键字
        (yyval.basic_type) = BasicType::REAL;  // 返回实数类型枚举值
        GRAMMAR_TRACE("basic_type -> REAL_KW");  // 记录语法跟踪日志
    }
#line 2703 "yacc_pascal.cpp"
    break;

  case 37: /* basic_type: BOOLEAN_KW  */
#line 996 "yacc_pascal.y"
                  {  // 匹配 "boolean" 关键字
        (yyval.basic_type) = BasicType::BOOLEAN;  // 返回布尔类型枚举值
        GRAMMAR_TRACE("basic_type -> BOOLEAN_KW");  // 记录语法跟踪日志
    }
#line 2712 "yacc_pascal.cpp"
    break;

  case 38: /* basic_type: CHAR_KW  */
#line 1002 "yacc_pascal.y"
               {  // 匹配 "char" 关键字
        (yyval.basic_type) = BasicType::CHAR;  // 返回字符类型枚举值
        GRAMMAR_TRACE("basic_type -> CHAR_KW");  // 记录语法跟踪日志
    }
#line 2721 "yacc_pascal.cpp"
    break;

  case 39: /* period_list: INTEGER DOUBLE_DOT INTEGER  */
#line 1012 "yacc_pascal.y"
                                  {  // 匹配 "整数 .. 整数" 形式，如 "1..10"
        auto periodList = allocateNode<std::vector<PeriodStmt *>>();  // 创建范围列表容器
        auto period = allocateNode<PeriodStmt>();  // 创建单个范围对象
        
        period->begin = (yyvsp[-2].number);  // 设置范围起始值
        period->end = (yyvsp[0].number);    // 设置范围结束值
        periodList->emplace_back(period);  // 将范围对象添加到列表中
        
        (yyval.period_list) = periodList;  // 返回范围列表
        GRAMMAR_TRACE("period_list -> INTEGER '..' INTEGER");  // 记录语法跟踪日志
    }
#line 2737 "yacc_pascal.cpp"
    break;

  case 40: /* period_list: period_list ',' INTEGER DOUBLE_DOT INTEGER  */
#line 1025 "yacc_pascal.y"
                                                  {  // 匹配 "已有范围列表, 整数 .. 整数" 形式
        auto period = allocateNode<PeriodStmt>();  // 创建新的范围对象
        period->begin = (yyvsp[-2].number);  // 设置范围起始值
        period->end = (yyvsp[0].number);    // 设置范围结束值
        
        (yyvsp[-4].period_list)->emplace_back(period);  // 将新范围对象添加到已有列表中
        (yyval.period_list) = (yyvsp[-4].period_list);  // 返回更新后的范围列表
        
        GRAMMAR_TRACE("period_list -> period_list ',' INTEGER '..' INTEGER");  // 记录语法跟踪日志
    }
#line 2752 "yacc_pascal.cpp"
    break;

  case 41: /* subprogram_declarations: %empty  */
#line 1041 "yacc_pascal.y"
                 {  // 当程序中没有子程序声明时匹配此规则
        (yyval.func_decl_list) = nullptr;  // 返回空指针，表示没有子程序声明
        GRAMMAR_TRACE("subprogram_declarations -> empty");  // 记录语法跟踪日志
    }
#line 2761 "yacc_pascal.cpp"
    break;

  case 42: /* subprogram_declarations: subprogram_declarations subprogram ';'  */
#line 1047 "yacc_pascal.y"
                                              {  // 匹配递归的子程序声明列表
        currentParserContext = ParserContext::SubprogramDeclarations;  // 设置当前解析上下文
        
        if ((yyvsp[-2].func_decl_list) == nullptr) {  // 如果这是第一个子程序声明
            auto funcDeclList = allocateNode<std::vector<FuncDeclStmt *>>();  // 创建新的函数声明列表
            funcDeclList->emplace_back((yyvsp[-1].func_decl));  // 添加新的子程序声明    
            (yyval.func_decl_list) = funcDeclList;  // 返回新创建的列表
        } else {  // 如果已经有子程序声明
            (yyvsp[-2].func_decl_list)->emplace_back((yyvsp[-1].func_decl));  // 将新的子程序声明添加到已有列表
            (yyval.func_decl_list) = (yyvsp[-2].func_decl_list);  // 返回更新后的列表
        }
        
        GRAMMAR_TRACE("subprogram_declarations -> subprogram_declarations subprogram ';'");  // 记录语法跟踪日志
    }
#line 2780 "yacc_pascal.cpp"
    break;

  case 43: /* subprogram: subprogram_head ';' subprogram_body  */
#line 1067 "yacc_pascal.y"
                                           {  // 匹配 "子程序头部; 子程序主体" 形式
        currentParserContext = ParserContext::Subprogram;  // 设置当前解析上下文为子程序
        auto subprogram = allocateNode<FuncDeclStmt>();  // 创建函数声明AST节点
        
        subprogram->header = std::unique_ptr<FuncHeadDeclStmt>((yyvsp[-2].func_head));  // 设置子程序头部，使用智能指针管理内存
        subprogram->body = std::unique_ptr<FuncBodyDeclStmt>((yyvsp[0].func_body));  // 设置子程序主体，使用智能指针管理内存
        
        (yyval.func_decl) = subprogram;  // 返回创建的子程序声明节点
        GRAMMAR_TRACE("subprogram -> subprogram_head ';' subprogram_body");  // 记录语法跟踪日志
    }
#line 2795 "yacc_pascal.cpp"
    break;

  case 44: /* subprogram_head: PROCEDURE IDENTIFIER formal_parameter  */
#line 1083 "yacc_pascal.y"
                                             {  // 匹配 "procedure 名称 (参数列表)" 形式
        currentParserContext = ParserContext::SubprogramHead;  // 设置当前解析上下文为子程序头部
        auto subHead = allocateNode<FuncHeadDeclStmt>();  // 创建函数头部声明节点
        
        subHead->func_name = std::string((yyvsp[-1].string));  // 设置函数名称
        subHead->ret_type = BasicType::VOID;  // 设置返回类型为VOID（表示过程）
        
        // 处理形式参数
        if ((yyvsp[0].var_decls) != nullptr) {  // 如果有参数列表
            for (auto formalParameter : *(yyvsp[0].var_decls)) {  // 遍历参数列表
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));  // 将参数添加到函数头部
            }
            delete (yyvsp[0].var_decls);  // 释放原始参数列表容器
        }
        
        (yyval.func_head) = subHead;  // 返回创建的函数头部节点
        free((yyvsp[-1].string));  // 释放函数名称字符串
        GRAMMAR_TRACE("subprogram_head -> PROCEDURE IDENTIFIER formal_parameter");  // 记录语法跟踪日志
    }
#line 2819 "yacc_pascal.cpp"
    break;

  case 45: /* subprogram_head: FUNCTION IDENTIFIER formal_parameter ':' basic_type  */
#line 1104 "yacc_pascal.y"
                                                           {  // 匹配 "function 名称 (参数列表) : 类型" 形式
        currentParserContext = ParserContext::SubprogramHead;  // 设置当前解析上下文为子程序头部
        auto subHead = allocateNode<FuncHeadDeclStmt>();  // 创建函数头部声明节点
        
        subHead->func_name = std::string((yyvsp[-3].string));  // 设置函数名称
        subHead->ret_type = (yyvsp[0].basic_type);  // 设置返回类型（由basic_type规则提供）
        
        // 处理形式参数
        if ((yyvsp[-2].var_decls) != nullptr) {  // 如果有参数列表
            for (auto formalParameter : *(yyvsp[-2].var_decls)) {  // 遍历参数列表
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));  // 将参数添加到函数头部
            }
            delete (yyvsp[-2].var_decls);  // 释放原始参数列表容器
        }
        
        (yyval.func_head) = subHead;  // 返回创建的函数头部节点
        free((yyvsp[-3].string));  // 释放函数名称字符串
        GRAMMAR_TRACE("subprogram_head -> FUNCTION IDENTIFIER formal_parameter ':' basic_type");  // 记录语法跟踪日志
    }
#line 2843 "yacc_pascal.cpp"
    break;

  case 46: /* subprogram_head: FUNCTION error  */
#line 1125 "yacc_pascal.y"
                      {  // 匹配 "function" 后跟语法错误
        (yyval.func_head) = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("subprogram_head -> FUNCTION error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2853 "yacc_pascal.cpp"
    break;

  case 47: /* subprogram_head: PROCEDURE error  */
#line 1132 "yacc_pascal.y"
                       {  // 匹配 "procedure" 后跟语法错误
        (yyval.func_head) = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("subprogram_head -> PROCEDURE error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
#line 2863 "yacc_pascal.cpp"
    break;

  case 48: /* formal_parameter: %empty  */
#line 1143 "yacc_pascal.y"
                   {  // 匹配空规则，表示没有参数
        (yyval.var_decls) = nullptr;  // 返回空指针，表示没有参数列表
        GRAMMAR_TRACE("formal_parameter -> empty");  // 记录语法跟踪日志
    }
#line 2872 "yacc_pascal.cpp"
    break;

  case 49: /* formal_parameter: '(' parameter_list ')'  */
#line 1149 "yacc_pascal.y"
                              {  // 匹配 "(参数列表)" 形式
        currentParserContext = ParserContext::FormalParameter;  // 设置当前解析上下文为形式参数
        (yyval.var_decls) = (yyvsp[-1].var_decls);  // 直接返回parameter_list的结果
        GRAMMAR_TRACE("formal_parameter -> '(' parameter_list ')'");  // 记录语法跟踪日志
    }
#line 2882 "yacc_pascal.cpp"
    break;

  case 50: /* parameter_list: %empty  */
#line 1160 "yacc_pascal.y"
                   {  // 匹配空规则，表示空的参数列表
        (yyval.var_decls) = nullptr;  // 返回空指针，表示没有参数
        GRAMMAR_TRACE("parameter_list -> empty");  // 记录语法跟踪日志
    }
#line 2891 "yacc_pascal.cpp"
    break;

  case 51: /* parameter_list: parameter  */
#line 1166 "yacc_pascal.y"
                 {  // 匹配单个参数，如 "x: integer" 或 "var a: real"
        auto paramList = allocateNode<std::vector<VarDeclStmt *>>();  // 创建参数列表容器
        paramList->emplace_back((yyvsp[0].var_decl));  // 将参数添加到列表中
        
        (yyval.var_decls) = paramList;  // 返回参数列表
        GRAMMAR_TRACE("parameter_list -> parameter");  // 记录语法跟踪日志
    }
#line 2903 "yacc_pascal.cpp"
    break;

  case 52: /* parameter_list: parameter_list ';' parameter  */
#line 1175 "yacc_pascal.y"
                                    {  // 匹配 "已有参数列表; 新参数" 形式
        (yyvsp[-2].var_decls)->emplace_back((yyvsp[0].var_decl));  // 将新参数添加到已有列表中
        (yyval.var_decls) = (yyvsp[-2].var_decls);  // 返回更新后的列表
        
        GRAMMAR_TRACE("parameter_list -> parameter_list ';' parameter");  // 记录语法跟踪日志
    }
#line 2914 "yacc_pascal.cpp"
    break;

  case 53: /* parameter: var_parameter  */
#line 1186 "yacc_pascal.y"
                     {// 匹配变量参数，如"var x: integer"
        (yyval.var_decl) = (yyvsp[0].var_decl);// 将 var_parameter 的结果作为 parameter 的结果返回
        (yyval.var_decl)->is_var = true;// 标记这是一个变量参数（即通过引用传递）
        
        GRAMMAR_TRACE("parameter -> var_parameter"); // 记录语法跟踪信息到日志
    }
#line 2925 "yacc_pascal.cpp"
    break;

  case 54: /* parameter: value_parameter  */
#line 1193 "yacc_pascal.y"
                       {// 匹配值参数，如"x: integer"
        (yyval.var_decl) = (yyvsp[0].var_decl);// 将 value_parameter 的结果作为 parameter 的结果返回
        (yyval.var_decl)->is_var = false;// 标记这是一个值参数（即通过值传递）
        
        GRAMMAR_TRACE("parameter -> value_parameter"); // 记录语法跟踪信息到日志
    }
#line 2936 "yacc_pascal.cpp"
    break;

  case 55: /* var_parameter: VAR value_parameter  */
#line 1203 "yacc_pascal.y"
                           {  // 匹配以VAR关键字开头的参数定义
        (yyval.var_decl) = (yyvsp[0].var_decl);  // 将value_parameter创建的节点作为当前规则的结果
        GRAMMAR_TRACE("var_parameter -> VAR value_parameter");  // 记录语法规则应用的跟踪日志
    }
#line 2945 "yacc_pascal.cpp"
    break;

  case 56: /* value_parameter: idlist ':' basic_type  */
#line 1211 "yacc_pascal.y"
                             {  // 匹配"标识符列表:类型"形式的参数定义
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建新的变量声明节点
        
        varDecl->id.insert(varDecl->id.end(), (yyvsp[-2].id_list)->begin(), (yyvsp[-2].id_list)->end());  // 将标识符列表中的所有标识符添加到变量声明中
        varDecl->data_type = DataType::BasicType;  // 设置数据类型为基本类型
        varDecl->basic_type = (yyvsp[0].basic_type);  // 设置具体的基本类型（integer、real等）
        varDecl->is_var = false;  // 初始设置为值参数（非var参数）
        
        delete (yyvsp[-2].id_list);  // 释放标识符列表的内存（已复制到变量声明节点）
        (yyval.var_decl) = varDecl;  // 返回创建的变量声明节点
        
        GRAMMAR_TRACE("value_parameter -> idlist ':' basic_type");  // 记录语法规则应用的跟踪日志
    }
#line 2963 "yacc_pascal.cpp"
    break;

  case 57: /* subprogram_body: const_declarations var_declarations compound_statement  */
#line 1229 "yacc_pascal.y"
                                                             {  // 匹配常量声明、变量声明和复合语句组成的子程序体
       currentParserContext = ParserContext::SubprogramBody;  // 设置当前解析上下文为子程序体
       auto funcBody = allocateNode<FuncBodyDeclStmt>();  // 创建函数体声明节点
       
       // 处理常量声明
       if ((yyvsp[-2].const_decls) != nullptr) {  // 如果存在常量声明部分
           funcBody->const_decl = std::unique_ptr<ConstDeclStmt>((yyvsp[-2].const_decls));  // 将常量声明添加到函数体，使用智能指针管理内存
       }
       
       // 处理变量声明
       if ((yyvsp[-1].var_decls) != nullptr) {  // 如果存在变量声明部分
           for (auto varDecl : *(yyvsp[-1].var_decls)) {  // 遍历所有变量声明
               funcBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));  // 将每个变量声明添加到函数体
           }
           delete (yyvsp[-1].var_decls);  // 释放变量声明列表容器（内容已转移到函数体节点中）
       }
       
       // 处理复合语句
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果存在复合语句部分
           for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历所有语句
               funcBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到函数体
           }
           delete (yyvsp[0].stmt_list);  // 释放语句列表容器（内容已转移到函数体节点中）
       }
       
       (yyval.func_body) = funcBody;  // 返回构建完成的函数体节点
       GRAMMAR_TRACE("subprogram_body -> const_declarations var_declarations compound_statement");  // 记录语法解析跟踪信息
   }
#line 2996 "yacc_pascal.cpp"
    break;

  case 58: /* compound_statement: BEGIN_TOKEN statement_list END  */
#line 1261 "yacc_pascal.y"
                                     {  // 匹配"BEGIN 语句列表 END"形式的语法结构
       currentParserContext = ParserContext::CompoundStatement;  // 设置当前解析上下文为复合语句
       (yyval.stmt_list) = (yyvsp[-1].stmt_list);  // 直接使用statement_list返回的语句列表作为复合语句的结果
                 // 这里没有创建新的AST节点，而是复用了语句列表节点
       
       GRAMMAR_TRACE("compound_statement -> BEGIN_TOKEN statement_list END");  // 记录语法解析跟踪信息
   }
#line 3008 "yacc_pascal.cpp"
    break;

  case 59: /* statement_list: statement  */
#line 1274 "yacc_pascal.y"
                {  // 匹配单个语句
       (yyval.stmt_list) = (yyvsp[0].stmt_list);  // 直接返回statement规则的结果作为语句列表
                 // 如果statement返回nullptr（空语句），则语句列表也为空
       GRAMMAR_TRACE("statement_list -> statement");  // 记录语法解析跟踪信息
   }
#line 3018 "yacc_pascal.cpp"
    break;

  case 60: /* statement_list: statement_list ';' statement  */
#line 1281 "yacc_pascal.y"
                                   {  // 匹配"已有语句列表;新语句"形式
       currentParserContext = ParserContext::StatementList;  // 设置当前解析上下文为语句列表
       
       // 如果有语句要添加，则处理
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果新语句不为空
           // 将第三个参数中的语句添加到结果列表中
           if ((yyvsp[-2].stmt_list) != nullptr) {  // 如果原有语句列表不为空
               for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历新语句列表中的所有语句
                   (yyvsp[-2].stmt_list)->emplace_back(stmt);  // 将新语句添加到原有列表末尾
               }
           } else {  // 如果原有语句列表为空
               // 如果结果列表为空，则直接使用第三个参数
               (yyvsp[-2].stmt_list) = (yyvsp[0].stmt_list);  // 使用新语句列表作为结果
               (yyvsp[0].stmt_list) = nullptr;  // 避免新语句列表被删除，因为它现在是结果
           }
       }
       
       (yyval.stmt_list) = (yyvsp[-2].stmt_list);  // 返回更新后的语句列表
       delete (yyvsp[0].stmt_list);  // 如果$3已经被处理过，这里实际上删除的是nullptr
       
       GRAMMAR_TRACE("statement_list -> statement_list ';' statement");  // 记录语法解析跟踪信息
   }
#line 3045 "yacc_pascal.cpp"
    break;

  case 61: /* statement_list: error ';' statement  */
#line 1305 "yacc_pascal.y"
                          {  // 匹配"[错误];语句"形式
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果后面的语句不为空
           for (auto item : *(yyvsp[0].stmt_list)) {  // 遍历语句列表中的所有语句
               delete item;  // 删除每个语句节点
           }
           delete (yyvsp[0].stmt_list);  // 删除语句列表容器
       }
       
       (yyval.stmt_list) = nullptr;  // 返回空指针，表示语法错误导致解析失败
       GRAMMAR_ERROR("statement_list -> error ';' statement");  // 记录语法错误信息
       yyerrok;  // 告诉Bison错误已恢复，可以继续解析
   }
#line 3062 "yacc_pascal.cpp"
    break;

  case 62: /* statement_list: statement_list ';' error  */
#line 1319 "yacc_pascal.y"
                               {  // 匹配"语句列表;[错误]"形式
       if ((yyvsp[-2].stmt_list) != nullptr) {  // 如果前面的语句列表不为空
           for (auto item : *(yyvsp[-2].stmt_list)) {  // 遍历语句列表中的所有语句
               delete item;  // 删除每个语句节点
           }
           delete (yyvsp[-2].stmt_list);  // 删除语句列表容器
       }
       
       (yyval.stmt_list) = nullptr;  // 返回空指针，表示语法错误导致解析失败
       GRAMMAR_ERROR("statement_list -> statement_list ';' error");  // 记录语法错误信息
       yyerrok;  // 告诉Bison错误已恢复，可以继续解析
   }
#line 3079 "yacc_pascal.cpp"
    break;

  case 63: /* statement: %empty  */
#line 1338 "yacc_pascal.y"
                {  // 匹配空语句，如连续两个分号之间没有代码
       (yyval.stmt_list) = nullptr;  // 返回空指针表示没有语句
       GRAMMAR_TRACE("statement -> empty");  // 记录语法解析跟踪信息
   }
#line 3088 "yacc_pascal.cpp"
    break;

  case 64: /* statement: variable ASSIGNOP expression  */
#line 1344 "yacc_pascal.y"
                                   {  // 匹配"变量 := 表达式"形式，如"x := y + 1"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto assignStmt = allocateNode<AssignStmt>();  // 创建赋值语句节点
       
       assignStmt->lval = std::unique_ptr<LValStmt>((yyvsp[-2].lval));  // 设置左值（赋值目标）
       assignStmt->expr = std::unique_ptr<ExprStmt>((yyvsp[0].expr));  // 设置右值表达式
       
       stmtList->emplace_back(assignStmt);  // 将赋值语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含赋值语句的列表
       
       GRAMMAR_TRACE("statement -> variable ASSIGNOP expression");  // 记录语法解析跟踪信息
   }
#line 3105 "yacc_pascal.cpp"
    break;

  case 65: /* statement: procedure_call  */
#line 1358 "yacc_pascal.y"
                     {  // 匹配过程调用，如"WriteLn(x)"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       stmtList->emplace_back((yyvsp[0].func_call_stmt));  // 将过程调用添加到语句列表
       
       (yyval.stmt_list) = stmtList;  // 返回包含过程调用的列表
       GRAMMAR_TRACE("statement -> procedure_call");  // 记录语法解析跟踪信息
   }
#line 3117 "yacc_pascal.cpp"
    break;

  case 66: /* statement: compound_statement  */
#line 1367 "yacc_pascal.y"
                         {  // 匹配begin-end块，如"begin x:=1; y:=2 end"
       (yyval.stmt_list) = (yyvsp[0].stmt_list);  // 直接返回复合语句规则的结果
       GRAMMAR_TRACE("statement -> compound_statement");  // 记录语法解析跟踪信息
   }
#line 3126 "yacc_pascal.cpp"
    break;

  case 67: /* statement: WHILE expression DO statement  */
#line 1373 "yacc_pascal.y"
                                    {  // 匹配"while 条件表达式 do 循环体"形式
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto whileStmt = allocateNode<WhileStmt>();  // 创建while语句节点
       
       whileStmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));  // 设置循环条件表达式
       
       // 处理循环体语句
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果循环体不为空
           for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历循环体中的所有语句
               whileStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到while语句的循环体
           }
           delete (yyvsp[0].stmt_list);  // 释放语句列表容器（内容已转移）
       }
       
       stmtList->emplace_back(whileStmt);  // 将while语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含while语句的列表
       
       GRAMMAR_TRACE("statement -> WHILE expression DO statement");  // 记录语法解析跟踪信息
   }
#line 3150 "yacc_pascal.cpp"
    break;

  case 68: /* statement: IF expression THEN statement  */
#line 1394 "yacc_pascal.y"
                                              {  // 匹配"if 条件 then 语句"形式
                                                 // %prec THEN 指定规则6的优先级等于THEN终结符的优先级，用于解决else悬挂问题
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto ifStmt = allocateNode<IfStmt>();  // 创建if语句节点
       
       ifStmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));  // 设置条件表达式
       
       // 处理if条件为真时的语句
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果then分支不为空
           for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历then分支中的所有语句
               ifStmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到if语句的true分支
           }
       }
       
       delete (yyvsp[0].stmt_list);  // 释放语句列表容器（内容已转移）
       stmtList->emplace_back(ifStmt);  // 将if语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含if语句的列表
       
       GRAMMAR_TRACE("statement -> IF expression THEN statement");  // 记录语法解析跟踪信息
   }
#line 3175 "yacc_pascal.cpp"
    break;

  case 69: /* statement: IF expression THEN statement ELSE statement  */
#line 1416 "yacc_pascal.y"
                                                  {  // 匹配"if 条件 then 语句1 else 语句2"形式
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto ifStmt = allocateNode<IfStmt>();  // 创建if语句节点
       
       ifStmt->expr = std::unique_ptr<ExprStmt>((yyvsp[-4].expr));  // 设置条件表达式
       
       // 处理if条件为真时的语句
       if ((yyvsp[-2].stmt_list) != nullptr) {  // 如果then分支不为空
           for (auto stmt : *(yyvsp[-2].stmt_list)) {  // 遍历then分支中的所有语句
               ifStmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到if语句的true分支
           }
       }
       
       // 处理if条件为假时的语句
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果else分支不为空
           for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历else分支中的所有语句
               ifStmt->false_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到if语句的false分支
           }
       }
       
       stmtList->emplace_back(ifStmt);  // 将if语句添加到语句列表
       delete (yyvsp[-2].stmt_list);  // 释放then分支语句列表容器（内容已转移）
       delete (yyvsp[0].stmt_list);  // 释放else分支语句列表容器（内容已转移）
       
       (yyval.stmt_list) = stmtList;  // 返回包含if-else语句的列表
       GRAMMAR_TRACE("statement -> IF expression THEN statement ELSE statement");  // 记录语法解析跟踪信息
   }
#line 3207 "yacc_pascal.cpp"
    break;

  case 70: /* statement: FOR IDENTIFIER ASSIGNOP expression TO expression DO statement  */
#line 1445 "yacc_pascal.y"
                                                                    {  // 匹配"for 变量:=初值 to 终值 do 循环体"形式
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto forStmt = allocateNode<ForStmt>();  // 创建for语句节点
       
       forStmt->id = std::string((yyvsp[-6].string));  // 设置循环变量名
       forStmt->begin = std::unique_ptr<ExprStmt>((yyvsp[-4].expr));  // 设置循环初始值表达式
       forStmt->end = std::unique_ptr<ExprStmt>((yyvsp[-2].expr));  // 设置循环终止值表达式
       
       // 处理循环体语句
       if ((yyvsp[0].stmt_list) != nullptr) {  // 如果循环体不为空
           for (auto stmt : *(yyvsp[0].stmt_list)) {  // 遍历循环体中的所有语句
               forStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到for语句的循环体
           }
       }
       
       stmtList->emplace_back(forStmt);  // 将for语句添加到语句列表
       free((yyvsp[-6].string));  // 释放循环变量名字符串
       delete (yyvsp[0].stmt_list);  // 释放循环体语句列表容器（内容已转移）
       
       (yyval.stmt_list) = stmtList;  // 返回包含for语句的列表
       GRAMMAR_TRACE("statement -> FOR IDENTIFIER ASSIGNOP expression TO expression DO statement");  // 记录语法解析跟踪信息
   }
#line 3234 "yacc_pascal.cpp"
    break;

  case 71: /* statement: READ '(' variable_list ')'  */
#line 1469 "yacc_pascal.y"
                                 {  // 匹配"read(变量列表)"形式，如"read(x, y, z)"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto readStmt = allocateNode<ReadFuncStmt>();  // 创建读取语句节点
       
       // 处理变量列表
       for (auto lval : *(yyvsp[-1].lval_list)) {  // 遍历变量列表中的所有变量
           readStmt->lval.emplace_back(std::unique_ptr<LValStmt>(lval));  // 将每个变量添加到读取语句的变量列表
       }
       
       delete (yyvsp[-1].lval_list);  // 释放变量列表容器（内容已转移）
       stmtList->emplace_back(readStmt);  // 将读取语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含读取语句的列表
       
       GRAMMAR_TRACE("statement -> READ '(' variable_list ')'");  // 记录语法解析跟踪信息
   }
#line 3254 "yacc_pascal.cpp"
    break;

  case 72: /* statement: WRITE '(' expression_list ')'  */
#line 1486 "yacc_pascal.y"
                                    {  // 匹配"write(表达式列表)"形式，如"write(x+y, 'hello')"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto writeStmt = allocateNode<WriteFuncStmt>();  // 创建写入语句节点
       
       // 处理表达式列表
       if ((yyvsp[-1].expr_list) != nullptr) {  // 如果表达式列表不为空
           for (auto expr : *(yyvsp[-1].expr_list)) {  // 遍历表达式列表中的所有表达式
               writeStmt->expr.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 将每个表达式添加到写入语句的表达式列表
           }
       }
       
       stmtList->emplace_back(writeStmt);  // 将写入语句添加到语句列表
       delete (yyvsp[-1].expr_list);  // 释放表达式列表容器（内容已转移）
       (yyval.stmt_list) = stmtList;  // 返回包含写入语句的列表
       
       GRAMMAR_TRACE("statement -> WRITE '(' expression_list ')'");  // 记录语法解析跟踪信息
   }
#line 3276 "yacc_pascal.cpp"
    break;

  case 73: /* statement: BREAK  */
#line 1505 "yacc_pascal.y"
            {  // 匹配break关键字
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto breakStmt = allocateNode<BreakStmt>();  // 创建break语句节点
       
       stmtList->emplace_back(breakStmt);  // 将break语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含break语句的列表
       
       GRAMMAR_TRACE("statement -> BREAK");  // 记录语法解析跟踪信息
   }
#line 3290 "yacc_pascal.cpp"
    break;

  case 74: /* statement: CONTINUE  */
#line 1516 "yacc_pascal.y"
               {  // 匹配continue关键字
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto continueStmt = allocateNode<ContinueStmt>();  // 创建continue语句节点
       
       stmtList->emplace_back(continueStmt);  // 将continue语句添加到语句列表
       (yyval.stmt_list) = stmtList;  // 返回包含continue语句的列表
       
       GRAMMAR_TRACE("statement -> CONTINUE");  // 记录语法解析跟踪信息
   }
#line 3304 "yacc_pascal.cpp"
    break;

  case 75: /* variable_list: variable  */
#line 1531 "yacc_pascal.y"
               {  // 匹配单个变量引用，如"x"或"arr[i]"
       currentParserContext = ParserContext::VariableList;  // 设置当前解析上下文为变量列表
       auto lvalList = allocateNode<std::vector<LValStmt *>>();  // 创建左值列表容器，变量列表中的元素必须是可赋值的目标（左值）
       
       lvalList->emplace_back((yyvsp[0].lval));  // 将变量添加到左值列表中
       (yyval.lval_list) = lvalList;  // 返回包含单个变量的左值列表
       
       GRAMMAR_TRACE("variable_list -> variable");  // 记录语法解析跟踪信息
   }
#line 3318 "yacc_pascal.cpp"
    break;

  case 76: /* variable_list: variable_list ',' variable  */
#line 1542 "yacc_pascal.y"
                                 {  // 匹配"已有变量列表,新变量"形式，如"x, y, z"
       currentParserContext = ParserContext::VariableList;  // 设置当前解析上下文为变量列表
       
       if ((yyvsp[0].lval) != nullptr) {  // 如果新变量不为空
           (yyvsp[-2].lval_list)->emplace_back((yyvsp[0].lval));  // 将新变量添加到已有列表末尾
       } 
       
       (yyval.lval_list) = (yyvsp[-2].lval_list);  // 返回更新后的变量列表
       GRAMMAR_TRACE("variable_list -> variable_list ',' variable");  // 记录语法解析跟踪信息
   }
#line 3333 "yacc_pascal.cpp"
    break;

  case 77: /* variable_list: error ',' variable  */
#line 1554 "yacc_pascal.y"
                         {  // 匹配"[错误],变量"形式
       delete (yyvsp[0].lval);  // 释放新变量的内存
       (yyval.lval_list) = nullptr;  // 返回空指针，表示语法错误导致解析失败
       
       GRAMMAR_ERROR("variable_list -> error ',' variable");  // 记录语法错误信息
   }
#line 3344 "yacc_pascal.cpp"
    break;

  case 78: /* variable: IDENTIFIER id_varpart  */
#line 1565 "yacc_pascal.y"
                            {  // 匹配"标识符 可选的数组下标"形式，如"x"或"array[i, j+1]"
       currentParserContext = ParserContext::Variable;  // 设置当前解析上下文为变量
       auto lval = allocateNode<LValStmt>();  // 创建左值语句节点
       
       lval->id = std::string((yyvsp[-1].string));  // 设置变量的标识符名称
       
       // 处理变量的数组下标部分
       if ((yyvsp[0].expr_list) != nullptr) {  // 如果有数组下标部分
           for (auto expr : *(yyvsp[0].expr_list)) {  // 遍历所有下标表达式
               lval->array_index.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 将每个下标表达式添加到左值的数组索引列表
           }
           delete (yyvsp[0].expr_list);  // 释放下标表达式列表容器（内容已转移）
       }
       
       free((yyvsp[-1].string));  // 释放标识符字符串（已复制到节点中）
       (yyval.lval) = lval;  // 返回构建的左值节点
       
       GRAMMAR_TRACE("variable -> IDENTIFIER id_varpart");  // 记录语法解析跟踪信息
   }
#line 3368 "yacc_pascal.cpp"
    break;

  case 79: /* id_varpart: %empty  */
#line 1589 "yacc_pascal.y"
                 {  // 匹配没有数组下标的情况，如简单变量"x"
        (yyval.expr_list) = nullptr;  // 返回空指针，表示没有数组下标
        GRAMMAR_TRACE("id_varpart -> empty");  // 记录语法解析跟踪信息
    }
#line 3377 "yacc_pascal.cpp"
    break;

  case 80: /* id_varpart: '[' expression_list ']'  */
#line 1595 "yacc_pascal.y"
                               {  // 匹配"[表达式列表]"形式，如"[i]"或"[i, j]"
        currentParserContext = ParserContext::IdVarpart;  // 设置当前解析上下文为变量附加部分
        
        if ((yyvsp[-1].expr_list) != nullptr) {  // 如果表达式列表不为空
            (yyval.expr_list) = (yyvsp[-1].expr_list);  // 直接使用expression_list返回的表达式列表作为结果    
        } else {  // 如果表达式列表为空（语法错误）
            yyerror(&(yylsp[-1]), "code_str", program, scanner, "数组下标定义出错 请检查是否符合规范");  // 报告错误：数组下标定义有误
        }
        
        GRAMMAR_TRACE("id_varpart -> '[' expression_list ']'");  // 记录语法解析跟踪信息
    }
#line 3393 "yacc_pascal.cpp"
    break;

  case 81: /* procedure_call: IDENTIFIER  */
#line 1610 "yacc_pascal.y"
                  {
        currentParserContext = ParserContext::ProcedureCall;
        auto procCall = allocateNode<FuncCallStmt>();
        
        procCall->id = std::string((yyvsp[0].string));
        free((yyvsp[0].string));
        (yyval.func_call_stmt) = procCall;
        
        GRAMMAR_TRACE("procedure_call -> IDENTIFIER");
    }
#line 3408 "yacc_pascal.cpp"
    break;

  case 82: /* procedure_call: IDENTIFIER '(' expression_list ')'  */
#line 1620 "yacc_pascal.y"
                                          {
        currentParserContext = ParserContext::ProcedureCall;
        auto procCall = allocateNode<FuncCallStmt>();
        
        procCall->id = std::string((yyvsp[-3].string));
        
        // 处理参数表达式列表
        if ((yyvsp[-1].expr_list) != nullptr) {
            for (auto expr : *(yyvsp[-1].expr_list)) {
                procCall->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete (yyvsp[-1].expr_list);
        }
        
        free((yyvsp[-3].string));
        (yyval.func_call_stmt) = procCall;
        
        GRAMMAR_TRACE("procedure_call -> IDENTIFIER '(' expression_list ')'");
    }
#line 3432 "yacc_pascal.cpp"
    break;

  case 83: /* expression_list: %empty  */
#line 1643 "yacc_pascal.y"
                 {
        (yyval.expr_list) = nullptr;
        GRAMMAR_TRACE("expression_list -> empty");
    }
#line 3441 "yacc_pascal.cpp"
    break;

  case 84: /* expression_list: expression  */
#line 1647 "yacc_pascal.y"
                  {
        currentParserContext = ParserContext::ExpressionList;
        auto exprList = allocateNode<std::vector<ExprStmt *>>();
        
        exprList->emplace_back((yyvsp[0].expr));
        (yyval.expr_list) = exprList;
        
        GRAMMAR_TRACE("expression_list -> expression");
    }
#line 3455 "yacc_pascal.cpp"
    break;

  case 85: /* expression_list: expression_list ',' expression  */
#line 1656 "yacc_pascal.y"
                                      {
        currentParserContext = ParserContext::ExpressionList;
        
        (yyvsp[-2].expr_list)->emplace_back((yyvsp[0].expr));
        (yyval.expr_list) = (yyvsp[-2].expr_list);
        
        GRAMMAR_TRACE("expression_list -> expression_list ',' expression");
    }
#line 3468 "yacc_pascal.cpp"
    break;

  case 86: /* expression: simple_expression  */
#line 1668 "yacc_pascal.y"
                         {
        currentParserContext = ParserContext::Expression;
        (yyval.expr) = ExprFactory::createFromSimpleExpr((yyvsp[0].add_expr));
        
        GRAMMAR_TRACE("expression -> simple_expression");
    }
#line 3479 "yacc_pascal.cpp"
    break;

  case 87: /* expression: expression relop simple_expression  */
#line 1674 "yacc_pascal.y"
                                          {
        currentParserContext = ParserContext::Expression;
        auto expr = (yyvsp[-2].expr);
        RelExprStmt::Term term;
        
        term.type = getRelationOperator((yyvsp[-1].number));
        term.add_expr = std::unique_ptr<AddExprStmt>((yyvsp[0].add_expr));
        expr->rel_expr->terms.emplace_back(std::move(term));
        
        (yyval.expr) = expr;
        GRAMMAR_TRACE("expression -> simple_expression relop simple_expression");
    }
#line 3496 "yacc_pascal.cpp"
    break;

  case 88: /* simple_expression: term  */
#line 1690 "yacc_pascal.y"
            {
        currentParserContext = ParserContext::SimpleExpression;
        (yyval.add_expr) = ExprFactory::createFromTerm((yyvsp[0].mul_expr));
        
        GRAMMAR_TRACE("simple_expression -> term");
    }
#line 3507 "yacc_pascal.cpp"
    break;

  case 89: /* simple_expression: simple_expression addop term  */
#line 1696 "yacc_pascal.y"
                                    {
        currentParserContext = ParserContext::SimpleExpression;
        auto addExpr = (yyvsp[-2].add_expr);
        AddExprStmt::Term term;
        
        term.type = getArithmeticOperator((yyvsp[-1].number));
        term.mul_expr = std::unique_ptr<MulExprStmt>((yyvsp[0].mul_expr));
        addExpr->terms.emplace_back(std::move(term));
        
        (yyval.add_expr) = addExpr;
        GRAMMAR_TRACE("simple_expression -> simple_expression %lld term\n");
    }
#line 3524 "yacc_pascal.cpp"
    break;

  case 90: /* term: factor  */
#line 1712 "yacc_pascal.y"
              {
        currentParserContext = ParserContext::Term;
        (yyval.mul_expr) = ExprFactory::createFromFactor((yyvsp[0].unary_expr));
        
        GRAMMAR_TRACE("term -> factor");
    }
#line 3535 "yacc_pascal.cpp"
    break;

  case 91: /* term: term mulop factor  */
#line 1718 "yacc_pascal.y"
                         {
        currentParserContext = ParserContext::Term;
        auto mulExpr = (yyvsp[-2].mul_expr);
        MulExprStmt::Term term;
        
        term.type = getTermOperator((yyvsp[-1].number));
        term.unary_expr = std::unique_ptr<UnaryExprStmt>((yyvsp[0].unary_expr));
        mulExpr->terms.emplace_back(std::move(term));
        
        (yyval.mul_expr) = mulExpr;
        GRAMMAR_TRACE("term -> term mulop factor");
    }
#line 3552 "yacc_pascal.cpp"
    break;

  case 92: /* factor: INTEGER  */
#line 1734 "yacc_pascal.y"
               {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        setupNumberNode(unaryExpr->primary_expr->value->number, (yyvsp[0].number));
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> INTEGER");
    }
#line 3569 "yacc_pascal.cpp"
    break;

  case 93: /* factor: REAL  */
#line 1746 "yacc_pascal.y"
            {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        double val = atof((yyvsp[0].real));
        setupNumberNode(unaryExpr->primary_expr->value->number, val);
        unaryExpr->primary_expr->value->number->literal = std::string((yyvsp[0].real));
        
        free((yyvsp[0].real));
        (yyval.unary_expr) = unaryExpr;
        
        GRAMMAR_TRACE("factor -> REAL");
    }
#line 3591 "yacc_pascal.cpp"
    break;

  case 94: /* factor: BOOLEAN  */
#line 1763 "yacc_pascal.y"
               {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        long long int val = (yyvsp[0].boolean) ? 1 : 0;
        setupNumberNode(unaryExpr->primary_expr->value->number, val);
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> BOOLEAN");
    }
#line 3610 "yacc_pascal.cpp"
    break;

  case 95: /* factor: CHAR  */
#line 1777 "yacc_pascal.y"
            {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        setupNumberNode(unaryExpr->primary_expr->value->number, (yyvsp[0].charactor));
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> CHAR");
    }
#line 3628 "yacc_pascal.cpp"
    break;

  case 96: /* factor: variable  */
#line 1790 "yacc_pascal.y"
                {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::LVal;
        unaryExpr->primary_expr->value->lval = std::unique_ptr<LValStmt>((yyvsp[0].lval));
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> variable");
    }
#line 3644 "yacc_pascal.cpp"
    break;

  case 97: /* factor: '(' expression ')'  */
#line 1801 "yacc_pascal.y"
                          {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Parentheses);
        
        unaryExpr->primary_expr->expr = std::unique_ptr<ExprStmt>((yyvsp[-1].expr));
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> '(' expression ')'");
    }
#line 3658 "yacc_pascal.cpp"
    break;

  case 98: /* factor: IDENTIFIER '(' expression_list ')'  */
#line 1810 "yacc_pascal.y"
                                          {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::FuncCall;
        unaryExpr->primary_expr->value->func_call = std::make_unique<FuncCallStmt>();
        unaryExpr->primary_expr->value->func_call->id = std::string((yyvsp[-3].string));
        
        // 处理函数参数
        if ((yyvsp[-1].expr_list) != nullptr) {
            for (auto expr : *(yyvsp[-1].expr_list)) {
                unaryExpr->primary_expr->value->func_call->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete (yyvsp[-1].expr_list);
        }
        
        free((yyvsp[-3].string));
        (yyval.unary_expr) = unaryExpr;
        
        GRAMMAR_TRACE("factor -> IDENTIFIER '(' expression_list ')'");
    }
#line 3685 "yacc_pascal.cpp"
    break;

  case 99: /* factor: NOT factor  */
#line 1832 "yacc_pascal.y"
                  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = (yyvsp[0].unary_expr);
        
        unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Not);
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> NOT factor");
    }
#line 3699 "yacc_pascal.cpp"
    break;

  case 100: /* factor: '+' factor  */
#line 1841 "yacc_pascal.y"
                  {
        currentParserContext = ParserContext::Factor;
        (yyval.unary_expr) = (yyvsp[0].unary_expr);  // 正号不改变值
        
        GRAMMAR_TRACE("factor -> '+' factor");
    }
#line 3710 "yacc_pascal.cpp"
    break;

  case 101: /* factor: '-' factor  */
#line 1847 "yacc_pascal.y"
                  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = (yyvsp[0].unary_expr);
        
        unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Minus);
        
        (yyval.unary_expr) = unaryExpr;
        GRAMMAR_TRACE("factor -> '-' factor");
    }
#line 3724 "yacc_pascal.cpp"
    break;

  case 102: /* addop: '+'  */
#line 1859 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3730 "yacc_pascal.cpp"
    break;

  case 103: /* addop: '-'  */
#line 1859 "yacc_pascal.y"
                              { (yyval.number) = 1; }
#line 3736 "yacc_pascal.cpp"
    break;

  case 104: /* addop: OR  */
#line 1859 "yacc_pascal.y"
                                               { (yyval.number) = 2; }
#line 3742 "yacc_pascal.cpp"
    break;

  case 105: /* relop: '='  */
#line 1861 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3748 "yacc_pascal.cpp"
    break;

  case 106: /* relop: NE  */
#line 1861 "yacc_pascal.y"
                             { (yyval.number) = 1; }
#line 3754 "yacc_pascal.cpp"
    break;

  case 107: /* relop: '<'  */
#line 1861 "yacc_pascal.y"
                                               { (yyval.number) = 2; }
#line 3760 "yacc_pascal.cpp"
    break;

  case 108: /* relop: LE  */
#line 1861 "yacc_pascal.y"
                                                                { (yyval.number) = 3; }
#line 3766 "yacc_pascal.cpp"
    break;

  case 109: /* relop: '>'  */
#line 1861 "yacc_pascal.y"
                                                                                  { (yyval.number) = 4; }
#line 3772 "yacc_pascal.cpp"
    break;

  case 110: /* relop: GE  */
#line 1861 "yacc_pascal.y"
                                                                                                   { (yyval.number) = 5; }
#line 3778 "yacc_pascal.cpp"
    break;

  case 111: /* relop: IN  */
#line 1861 "yacc_pascal.y"
                                                                                                                    { (yyval.number) = 6; }
#line 3784 "yacc_pascal.cpp"
    break;

  case 112: /* mulop: '*'  */
#line 1863 "yacc_pascal.y"
            { (yyval.number) = 0; }
#line 3790 "yacc_pascal.cpp"
    break;

  case 113: /* mulop: '/'  */
#line 1863 "yacc_pascal.y"
                              { (yyval.number) = 1; }
#line 3796 "yacc_pascal.cpp"
    break;

  case 114: /* mulop: DIV  */
#line 1863 "yacc_pascal.y"
                                                { (yyval.number) = 1; }
#line 3802 "yacc_pascal.cpp"
    break;

  case 115: /* mulop: MOD  */
#line 1863 "yacc_pascal.y"
                                                                  { (yyval.number) = 2; }
#line 3808 "yacc_pascal.cpp"
    break;

  case 116: /* mulop: AND  */
#line 1863 "yacc_pascal.y"
                                                                                    { (yyval.number) = 3; }
#line 3814 "yacc_pascal.cpp"
    break;

  case 117: /* mulop: ANDTHEN  */
#line 1863 "yacc_pascal.y"
                                                                                                          { (yyval.number) = 4; }
#line 3820 "yacc_pascal.cpp"
    break;

  case 118: /* error_recovery: error ';'  */
#line 1867 "yacc_pascal.y"
                 {
        yyerrok;
    }
#line 3828 "yacc_pascal.cpp"
    break;


#line 3832 "yacc_pascal.cpp"

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

#line 1872 "yacc_pascal.y"

// 此处书写相关函数，会添加在生成的代码中
extern void scan_string(const char *str, yyscan_t scanner);

// 相关所需的函数，可能包含一些错误处理函数
int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg)
{
    (void)program;
    (void)scanner;
    (void)msg;
    syntaxErrorFlag = true;
    LOG_ERROR("[Syntax Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column + 1, msg);
    return 0;
}

static int yyreport_syntax_error(const yypcontext_t *ctx, const char * code_str, ProgramStmt ** program, void * scanner)
{
    syntaxErrorFlag = true;
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
        
        if (n < 0) {
            // 向yyparse传递错误
            res = n;
        } else {
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
    
    locateErrorPosition(code_str, yypcontext_location(ctx), error_note, msg, have_expected);
    
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
    if (syntaxErrorFlag) {
        return -1;
    }
    return ret;
}
