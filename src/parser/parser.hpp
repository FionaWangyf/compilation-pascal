/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    CONST = 258,                   /* CONST  */
    PROGRAM = 259,                 /* PROGRAM  */
    TYPE = 260,                    /* TYPE  */
    RECORD = 261,                  /* RECORD  */
    ARRAY = 262,                   /* ARRAY  */
    OF = 263,                      /* OF  */
    VAR = 264,                     /* VAR  */
    FUNCTION = 265,                /* FUNCTION  */
    PROCEDURE = 266,               /* PROCEDURE  */
    BEGIN_TOKEN = 267,             /* BEGIN_TOKEN  */
    END = 268,                     /* END  */
    IF = 269,                      /* IF  */
    THEN = 270,                    /* THEN  */
    ELSE = 271,                    /* ELSE  */
    CASE = 272,                    /* CASE  */
    WHILE = 273,                   /* WHILE  */
    REPEAT = 274,                  /* REPEAT  */
    UNTIL = 275,                   /* UNTIL  */
    FOR = 276,                     /* FOR  */
    TO = 277,                      /* TO  */
    DOWNTO = 278,                  /* DOWNTO  */
    DO = 279,                      /* DO  */
    READ = 280,                    /* READ  */
    READLN = 281,                  /* READLN  */
    WRITE = 282,                   /* WRITE  */
    WRITELN = 283,                 /* WRITELN  */
    CHAR_KW = 284,                 /* CHAR_KW  */
    INTEGER_KW = 285,              /* INTEGER_KW  */
    REAL_KW = 286,                 /* REAL_KW  */
    BOOLEAN_KW = 287,              /* BOOLEAN_KW  */
    NOT = 288,                     /* NOT  */
    DIV = 289,                     /* DIV  */
    MOD = 290,                     /* MOD  */
    AND = 291,                     /* AND  */
    OR = 292,                      /* OR  */
    NE = 293,                      /* NE  */
    LE = 294,                      /* LE  */
    GE = 295,                      /* GE  */
    ASSIGNOP = 296,                /* ASSIGNOP  */
    IN = 297,                      /* IN  */
    ORELSE = 298,                  /* ORELSE  */
    ANDTHEN = 299,                 /* ANDTHEN  */
    DOUBLE_DOT = 300,              /* DOUBLE_DOT  */
    BREAK = 301,                   /* BREAK  */
    CONTINUE = 302,                /* CONTINUE  */
    IDENTIFIER = 303,              /* IDENTIFIER  */
    INTEGER = 304,                 /* INTEGER  */
    BOOLEAN = 305,                 /* BOOLEAN  */
    REAL = 306,                    /* REAL  */
    CHAR = 307,                    /* CHAR  */
    STRING = 308                   /* STRING  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 398 "parser.y"

    /* 程序结构相关类型 */
    ProgramNode *                                   program_struct;   /* 整个程序的AST节点 */
    ProgramHeadNode *                               program_head;     /* 程序头部AST节点 */
    ProgramBodyNode *                               program_body;     /* 程序主体AST节点 */
    std::vector<std::string> *                      id_list;          /* 标识符列表，用于程序参数、变量声明等 */
    
    /* 常量声明相关类型 */
    ConstDeclNode *                                 const_decls;      /* 常量声明AST节点 */
    std::pair<std::string, ValueNode *> *           kv_pair;          /* 常量名称-值对 */
    std::vector<std::pair<std::string, ValueNode *>*> * kv_pair_list; /* 常量名称-值对列表 */
    ValueNode *                                     value;            /* 值AST节点，表示常量值 */
    
    /* 变量声明相关类型 */
    std::vector<VarDeclNode *> *                    var_decls;        /* 变量声明列表 */
    VarDeclNode *                                   var_decl;         /* 单个变量声明AST节点 */
    DataType                                        var_type;         /* 变量数据类型枚举 */
    BasicType                                       basic_type;       /* 基本类型枚举（int, real等） */
    
    /* 数组范围相关类型 */
    std::vector<PeriodNode *> *                     period_list;      /* 数组索引范围列表 */
    PeriodNode *                                    period;           /* 单个数组索引范围 */
    
    /* 函数/过程声明相关类型 */
    std::vector<FuncDeclNode *> *                   func_decl_list;   /* 函数声明列表 */
    FuncDeclNode *                                  func_decl;        /* 单个函数声明AST节点 */
    FuncHeadDeclNode *                              func_head;        /* 函数头部AST节点 */
    FuncBodyDeclNode *                              func_body;        /* 函数主体AST节点 */
    
    /* 语句相关类型 */
    std::vector<BaseNode *> *                       stmt_list;        /* 语句列表 */
    AssignmentNode *                                    assign_stmt;      /* 赋值语句AST节点 */
    IfNode *                                        if_stmt;          /* 条件语句AST节点 */
    ForNode *                                       for_stmt;         /* For循环语句AST节点 */
    ReadFuncNode *                                  read_stmt;        /* 读取语句AST节点 */
    WriteFuncNode *                                 write_stmt;       /* 写入语句AST节点 */
    FuncCallNode *                                  func_call_stmt;   /* 函数调用语句AST节点 */
    std::vector<LValueNode *> *                       lval_list;        /* 左值表达式列表（用于变量引用） */
    LValueNode *                                      lval;             /* 单个左值表达式AST节点 */
    BaseNode *                                      stmt;             /* 基本语句AST节点 */

    /* 表达式相关类型 */
    std::vector<ExprNode *> *                       expr_list;        /* 表达式列表 */
    ExprNode *                                      expr;             /* 完整表达式AST节点 */
    RelExprNode *                                   rel_expr;         /* 关系表达式AST节点 */
    AddExprNode *                                   add_expr;         /* 加法表达式AST节点 */
    MulExprNode *                                   mul_expr;         /* 乘法表达式AST节点 */
    UnaryExprNode *                                 unary_expr;       /* 一元表达式AST节点 */
    PrimaryExprNode *                               primary_expr;     /* 基本表达式AST节点 */

    /* 控制流相关类型 */
    BreakNode *                                     break_stmt;       /* Break语句AST节点 */
    ContinueNode *                                  continue_stmt;    /* Continue语句AST节点 */

    /* 基本数据类型 */
    char *                                          string;           /* 字符串，用于标识符和字符串字面量 */
    long long                                       number;           /* 整数值 */
    bool                                            boolean;          /* 布尔值 */
    char *                                          real;             /* 实数值的字符串表示 */
    char                                            charactor;        /* 字符值 */
    int                                             token;            /* 令牌值，用于某些特殊情况 */

#line 180 "parser.hpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif




int yyparse (const char * code_str, ProgramNode ** program, void * scanner);


#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
