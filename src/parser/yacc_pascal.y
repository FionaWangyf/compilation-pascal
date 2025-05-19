%{
// 此处为相关头文件和函数，会添加在生成的代码中

#include <memory>
#include <sstream>
#include <string>
#include <utility>
#include <vector>
#include <map>
#include <type_traits>
#include <iostream>
#include "common/log/log.hpp"//日志系统
#include "common/setting/settings.hpp"//编译器设置
#include "ast/stmt.hpp"//抽象语法树定义
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

%}


// 定义Token
%token 
    CONST       /* 常量声明关键字 'const' */
    PROGRAM     /* 程序声明关键字 'program' */
    TYPE        /* 类型声明关键字 'type' */
    RECORD      /* 记录类型关键字 'record' */
    ARRAY       /* 数组类型关键字 'array' */
    OF          /* 类型定义中使用的关键字 'of' */
    VAR         /* 变量声明关键字 'var' */
    FUNCTION    /* 函数声明关键字 'function' */
    PROCEDURE   /* 过程声明关键字 'procedure' */
    BEGIN_TOKEN /* 复合语句开始关键字 'begin'（重命名为避免与C关键字冲突） */
    END         /* 复合语句结束关键字 'end' */
    IF          /* 条件语句关键字 'if' */
    THEN        /* 条件语句关键字 'then' */
    ELSE        /* 条件语句关键字 'else' */
    CASE        /* 分支语句关键字 'case' */
    WHILE       /* 循环语句关键字 'while' */
    REPEAT      /* 循环语句关键字 'repeat' */
    UNTIL       /* 循环语句关键字 'until' */
    FOR         /* 循环语句关键字 'for' */
    TO          /* for循环增量方向关键字 'to' */
    DOWNTO      /* for循环减量方向关键字 'downto' */
    DO          /* 循环语句关键字 'do' */
    READ        /* 输入关键字 'read' */
    READLN      /* 行输入关键字 'readln' */
    WRITE       /* 输出关键字 'write' */
    WRITELN     /* 行输出关键字 'writeln' */
    CHAR_KW     /* 字符类型关键字 'char' */
    INTEGER_KW  /* 整数类型关键字 'integer' */
    REAL_KW     /* 实数类型关键字 'real' */
    BOOLEAN_KW  /* 布尔类型关键字 'boolean' */
    NOT         /* 逻辑非运算符 'not' */
    DIV         /* 整数除法运算符 'div' */
    MOD         /* 取模运算符 'mod' */
    AND         /* 逻辑与运算符 'and' */
    OR          /* 逻辑或运算符 'or' */
    NE          /* 不等于运算符 '<>' */
    LE          /* 小于等于运算符 '<=' */
    GE          /* 大于等于运算符 '>=' */
    ASSIGNOP    /* 赋值运算符 ':=' */
    IN          /* 集合成员测试运算符 'in' */
    ORELSE      /* 短路逻辑或运算符 'orelse'（Pascal扩展） */
    ANDTHEN     /* 短路逻辑与运算符 'andthen'（Pascal扩展） */
    DOUBLE_DOT  /* 范围运算符 '..' */
    BREAK       /* 跳出循环关键字 'break'（Pascal扩展） */
    CONTINUE    /* 继续循环关键字 'continue'（Pascal扩展） */


%define api.pure full
%define parse.error custom
%locations
%define parse.trace
%lex-param { yyscan_t scanner }
%parse-param { const char * code_str }
%parse-param { ProgramStmt ** program}
%parse-param { void * scanner }

// 定义初始动作
%initial-action 
{
    *program = nullptr;//初始化程序AST根节点为空指针
};


//定义了在语法分析过程中可以使用的所有数据类型，Bison使用这个联合体作为YYSTYPE（语义值类型），在解析器和词法分析器间传递值
//由于C++的限制，联合体成员只能是POD（Plain Old Data）类型，因此所有复杂类型都使用指针表示。
%union {
    /* 程序结构相关类型 */
    ProgramStmt *                                   program_struct;   /* 整个程序的AST节点 */
    ProgramHeadStmt *                               program_head;     /* 程序头部AST节点 */
    ProgramBodyStmt *                               program_body;     /* 程序主体AST节点 */
    std::vector<std::string> *                      id_list;          /* 标识符列表，用于程序参数、变量声明等 */
    
    /* 常量声明相关类型 */
    ConstDeclStmt *                                 const_decls;      /* 常量声明AST节点 */
    std::pair<std::string, ValueStmt *> *           kv_pair;          /* 常量名称-值对 */
    std::vector<std::pair<std::string, ValueStmt *>*> * kv_pair_list; /* 常量名称-值对列表 */
    ValueStmt *                                     value;            /* 值AST节点，表示常量值 */
    
    /* 变量声明相关类型 */
    std::vector<VarDeclStmt *> *                    var_decls;        /* 变量声明列表 */
    VarDeclStmt *                                   var_decl;         /* 单个变量声明AST节点 */
    DataType                                        var_type;         /* 变量数据类型枚举 */
    BasicType                                       basic_type;       /* 基本类型枚举（int, real等） */
    
    /* 数组范围相关类型 */
    std::vector<PeriodStmt *> *                     period_list;      /* 数组索引范围列表 */
    PeriodStmt *                                    period;           /* 单个数组索引范围 */
    
    /* 函数/过程声明相关类型 */
    std::vector<FuncDeclStmt *> *                   func_decl_list;   /* 函数声明列表 */
    FuncDeclStmt *                                  func_decl;        /* 单个函数声明AST节点 */
    FuncHeadDeclStmt *                              func_head;        /* 函数头部AST节点 */
    FuncBodyDeclStmt *                              func_body;        /* 函数主体AST节点 */
    
    /* 语句相关类型 */
    std::vector<BaseStmt *> *                       stmt_list;        /* 语句列表 */
    AssignStmt *                                    assign_stmt;      /* 赋值语句AST节点 */
    IfStmt *                                        if_stmt;          /* 条件语句AST节点 */
    ForStmt *                                       for_stmt;         /* For循环语句AST节点 */
    ReadFuncStmt *                                  read_stmt;        /* 读取语句AST节点 */
    WriteFuncStmt *                                 write_stmt;       /* 写入语句AST节点 */
    FuncCallStmt *                                  func_call_stmt;   /* 函数调用语句AST节点 */
    std::vector<LValStmt *> *                       lval_list;        /* 左值表达式列表（用于变量引用） */
    LValStmt *                                      lval;             /* 单个左值表达式AST节点 */
    BaseStmt *                                      stmt;             /* 基本语句AST节点 */

    /* 表达式相关类型 */
    std::vector<ExprStmt *> *                       expr_list;        /* 表达式列表 */
    ExprStmt *                                      expr;             /* 完整表达式AST节点 */
    RelExprStmt *                                   rel_expr;         /* 关系表达式AST节点 */
    AddExprStmt *                                   add_expr;         /* 加法表达式AST节点 */
    MulExprStmt *                                   mul_expr;         /* 乘法表达式AST节点 */
    UnaryExprStmt *                                 unary_expr;       /* 一元表达式AST节点 */
    PrimaryExprStmt *                               primary_expr;     /* 基本表达式AST节点 */

    /* 控制流相关类型 */
    BreakStmt *                                     break_stmt;       /* Break语句AST节点 */
    ContinueStmt *                                  continue_stmt;    /* Continue语句AST节点 */

    /* 基本数据类型 */
    char *                                          string;           /* 字符串，用于标识符和字符串字面量 */
    long long                                       number;           /* 整数值 */
    bool                                            boolean;          /* 布尔值 */
    char *                                          real;             /* 实数值的字符串表示 */
    char                                            charactor;        /* 字符值 */
    int                                             token;            /* 令牌值，用于某些特殊情况 */
}

//类型关联
%token <string> IDENTIFIER     /* 将IDENTIFIER的值类型关联为union.string（char*） */
%token <number> INTEGER        /* 将INTEGER的值类型关联为union.number（long long） */
%token <boolean> BOOLEAN       /* 将BOOLEAN的值类型关联为union.boolean（bool） */
%token <real> REAL             /* 将REAL的值类型关联为union.real（char*） */
%token <charactor> CHAR        /* 将CHAR的值类型关联为union.charactor（char） */
%token <string> STRING         /* 将STRING的值类型关联为union.string（char*） */

// 非终结符关联
/**
 * 非终结符类型关联
 * 
 * 这些声明将语法规则中的非终结符与联合体中的特定成员类型关联起来。
 * 语法分析器使用这些关联来确定在规则归约时如何处理和传递值。
 */

/* 运算符相关非终结符 */
%type <number> relop               /* 关系运算符，如 =, <>, <, <=, >, >= */
%type <number> addop               /* 加法运算符，如 +, -, or */
%type <number> mulop               /* 乘法运算符，如 *, /, div, mod, and */

/* 程序结构相关非终结符 */
%type <program_struct> programstruct   /* 整个程序结构 */
%type <program_head> program_head      /* 程序头部，包含程序名和参数 */
%type <program_body> program_body      /* 程序主体，包含声明和语句 */
%type <id_list> idlist                 /* 标识符列表，用于参数和声明 */

/* 常量声明相关非终结符 */
%type <const_decls> const_declarations /* 常量声明部分 */
%type <kv_pair_list> const_declaration /* 单个常量声明 */
%type <value> const_value              /* 常量值 */

/* 变量声明相关非终结符 */
%type <var_decls> var_declarations     /* 变量声明部分 */
%type <var_decls> var_declaration      /* 单个变量声明 */
%type <var_decl> type                  /* 类型定义 */
%type <basic_type> basic_type          /* 基本类型，如integer, real, boolean */
%type <period_list> period_list        /* 数组索引范围列表 */

/* 子程序声明相关非终结符 */
%type <func_decl_list> subprogram_declarations /* 子程序声明部分 */
%type <func_decl> subprogram            /* 单个子程序（函数或过程） */
%type <func_head> subprogram_head       /* 子程序头部 */
%type <var_decls> parameter_list        /* 参数列表 */
%type <var_decls> formal_parameter      /* 形式参数 */
%type <var_decl> parameter              /* 单个参数 */
%type <var_decl> var_parameter          /* 变量参数（通过引用传递） */
%type <var_decl> value_parameter        /* 值参数（通过值传递） */
%type <func_body> subprogram_body       /* 子程序主体 */

/* 语句相关非终结符 */
%type <stmt_list> compound_statement    /* 复合语句（begin...end块） */
%type <stmt_list> statement_list        /* 语句列表 */
%type <stmt_list> statement             /* 单个语句 */
%type <func_call_stmt> procedure_call   /* 过程调用 */
%type <stmt_list> else_part
%type <lval_list> variable_list         /* 变量列表 */
%type <lval> variable                   /* 单个变量引用 */

/* 表达式相关非终结符 */
%type <expr_list> id_varpart            /* 变量的附加部分（如数组索引） */
%type <expr_list> expression_list       /* 表达式列表 */
%type <expr> expression                 /* 完整表达式 */
%type <add_expr> simple_expression      /* 简单表达式（含加法级运算） */
%type <mul_expr> term                   /* 项（含乘法级运算） */
%type <unary_expr> factor               /* 因子（表达式的基本单位） */




// 析构器
//告诉Bison如何释放不再需要的语法符号值的内存，在错误恢复或语法歧义解析中，某些创建的值可能会被丢弃，析构器确保这些值的内存被正确释放，防止内存泄漏
/* 无需析构的基本类型 */
%destructor {} <program_struct> <boolean> <number> <charactor> <basic_type>
/* 需要使用free()释放的字符串类型 */
%destructor { free($$); } IDENTIFIER <string> <real>
/* 包含指针列表的复杂类型 */
%destructor {
    if($$ != nullptr){
        for(auto kv_pair : *$$){
            delete kv_pair;
        }
    }
    delete $$;
} <var_decls> <period_list> <func_decl_list> <stmt_list> <lval_list> <expr_list>

/* 包含键值对列表的特殊类型 */
%destructor {
    if($$ != nullptr){
        for(auto pair : *$$){
            delete pair->second;
            delete pair;
        }
        delete $$;
    }
} <kv_pair_list>
/* 默认析构器：适用于所有其他类型 */
%destructor { delete $$; } <*>
//不允许这些符号直接相邻或嵌套
//解决“悬空else”问题（类似于右结合）
%nonassoc THEN //较低优先级
%nonassoc ELSE //较高优先级
%expect 1

%%
// 语法规则定义部分

//程序结构语法规则
//定义Pascal程序的最顶层结构：程序头部 + 分号 + 程序主体 + 句点，包含正常语法规则和三种错误恢复规则
programstruct 
    //正常语法规则-完整的程序结构
    : program_head  ';'  program_body '.'  {
        currentParserContext = ParserContext::ProgramStruct;//设置当前解析上下文为程序结构
        ProgramStmt* programStruct = allocateNode<ProgramStmt>();//创建程序结构AST节点
        programStruct->head = std::unique_ptr<ProgramHeadStmt>($1);//设置程序头部，使用智能指针管理内存
        programStruct->body = std::unique_ptr<ProgramBodyStmt>($3);//设置程序主体，使用智能指针管理内存 
        
        GRAMMAR_TRACE("programstruct -> program_head ';' program_body '.'");
        *program = programStruct;//将构建的AST根节点存储到输出参数中
        $$ = nullptr; // 防止报错
    }
    //错误恢复规则1-程序头部有错误
    | error  ';'  program_body '.'  {
        *program = allocateNode<ProgramStmt>();//创建空的程序结构作为恢复
        delete $3;//释放已解析的程序主体
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' program_body '.'");
    }
    //错误恢复规则2-程序主体有错误
    | program_head  ';'  error  {
        *program = allocateNode<ProgramStmt>();
        delete $1;
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> program_head ';' error");
    }
    //错误恢复规则3：程序头部和主体都有错误
    | error  ';'  error  {
        *program = allocateNode<ProgramStmt>();
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' error");
    }
    ;

//程序头部语法规则
// 定义Pascal程序的头部结构，包括程序名称和参数列表
program_head 
    // 规则1：完整形式，带参数列表的程序头 - program Name(param1, param2, ...)
    : PROGRAM IDENTIFIER '(' idlist ')'  {  // 匹配 "program 标识符 ( 标识符列表 )"
        currentParserContext = ParserContext::ProgramHead;  // 设置当前解析上下文为程序头
        $$ = allocateNode<ProgramHeadStmt>();  // 创建新的程序头AST节点
        $$->id_list.push_back(std::string($2));  // 先添加程序名
        $$->id_list.insert($$->id_list.end(), $4->begin(), $4->end());  // 再添加参数
        //$$->id_list = *$4; 将标识符列表（参数）复制到程序头节点
        
        delete $4;  // 释放标识符列表内存（已复制到AST节点中）
        free($2);   // 释放标识符字符串（程序名称）内存
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER '(' idlist ')'");  // 记录语法跟踪日志
    }
    
    // 规则2：简化形式，不带参数列表的程序头 - program Name
    | PROGRAM IDENTIFIER  {  // 匹配 "program 标识符"
        currentParserContext = ParserContext::ProgramHead;  // 设置当前解析上下文为程序头
        $$ = allocateNode<ProgramHeadStmt>();  // 创建新的程序头AST节点
        $$->id_list.emplace_back(std::string($2));  // 将程序名称作为参数添加到id_list中
        
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER");  // 记录语法跟踪日志
        free($2);  // 释放标识符字符串（程序名称）内存
    }
    
    // 规则3：错误恢复，处理PROGRAM关键字后的语法错误
    | PROGRAM error  {  // 匹配 "program" 后跟语法错误
        $$ = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("program_head -> PROGRAM error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;
    
// 定义Pascal程序的主体部分，由常量声明、变量声明、子程序声明和执行语句组成
program_body 
    // 规则1：正常的程序体结构
    : const_declarations var_declarations subprogram_declarations compound_statement  {
        // 设置当前解析上下文为程序体
        currentParserContext = ParserContext::ProgramBody;
        
        // 创建程序体AST节点
        ProgramBodyStmt* programBody = allocateNode<ProgramBodyStmt>();
        
        // 处理常量声明部分
        if($1 != nullptr) {  // 如果有常量声明
            // 将常量声明节点转移到程序体节点中，使用智能指针管理内存
            programBody->const_decl = std::unique_ptr<ConstDeclStmt>($1);
        }
        
        // 处理变量声明部分
        if($2 != nullptr) {  // 如果有变量声明
            // 遍历变量声明列表，将每个声明转移到程序体节点中
            for(auto varDecl : *$2) {
                // 使用智能指针封装每个变量声明，并添加到程序体的变量声明列表中
                programBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));
            }
            // 释放变量声明列表容器（内部元素已被转移到程序体节点）
            delete $2;
        }
        
        // 处理子程序（函数/过程）声明部分
        if($3 != nullptr) {  // 如果有子程序声明
            // 遍历子程序声明列表，将每个声明转移到程序体节点中
            for(auto funcDecl : *$3) {
                // 使用智能指针封装每个函数声明，并添加到程序体的函数声明列表中
                programBody->func_decl.emplace_back(std::unique_ptr<FuncDeclStmt>(funcDecl));
            }
            // 释放子程序声明列表容器
            delete $3;
        }
        
        // 处理复合语句部分（主要的执行代码）
        if($4 != nullptr) {  // 如果有复合语句
            // 遍历语句列表，将每个语句转移到程序体节点中
            for(auto stmt : *$4) {
                // 使用智能指针封装每个语句，并添加到程序体的语句列表中
                programBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            // 释放语句列表容器
            delete $4;
        }
        
        // 设置当前规则的返回值为构建好的程序体节点
        $$ = programBody;
        
        // 记录语法追踪日志
        GRAMMAR_TRACE("program_body -> const_declarations var_declarations subprogram_declarations compound_statement");
    }
    
    // 规则2：错误恢复后的程序体结构
    | error_recovery const_declarations var_declarations subprogram_declarations compound_statement  {
        // 使用Lambda函数简化清理代码
        auto cleanupPtr = [](auto* ptr) {
            if (ptr) delete ptr;  // 如果指针非空，则删除它指向的对象
        };
        
        // 清理各个部分分配的内存
        cleanupPtr($2);  // 清理常量声明
        
        // 清理变量声明列表
        if($3 != nullptr) {
            // 遍历并删除每个变量声明
            for(auto varDecl : *$3) {
                delete varDecl;
            }
            // 删除列表容器本身
            delete $3;
        }
        
        // 清理子程序声明列表
        if($4 != nullptr) {
            // 遍历并删除每个函数声明
            for(auto funcDecl : *$4) {
                delete funcDecl;
            }
            // 删除列表容器本身
            delete $4;
        }
        
        // 清理语句列表
        if($5 != nullptr) {
            // 遍历并删除每个语句
            for(auto stmt : *$5) {
                delete stmt;
            }
            // 删除列表容器本身
            delete $5;
        }
        
        // 设置返回值为nullptr，表示程序体构建失败
        $$ = nullptr;
        
        // 记录语法追踪日志
        GRAMMAR_TRACE("program_body -> error_recovery const_declarations var_declarations subprogram_declarations compound_statement");
    }
    ;

// 标识符列表语法规则
// 用于定义逗号分隔的标识符序列，如参数列表、变量声明等
idlist 
    // 规则1：单个标识符
    : IDENTIFIER  {  // 匹配单个标识符，如 "input"
        currentParserContext = ParserContext::IdList;  // 设置当前解析上下文为标识符列表
        $$ = allocateNode<std::vector<std::string>>();  // 创建新的字符串向量
        $$->emplace_back(std::string($1));  // 将标识符添加到向量中
        
        GRAMMAR_TRACE("idlist -> IDENTIFIER");  // 记录语法跟踪日志
        free($1);  // 释放标识符字符串内存（已复制到向量中）
    }
    
    // 规则2：已有标识符列表后添加新标识符
    | idlist ',' IDENTIFIER  {  // 匹配"已存在的标识符列表,新标识符"，如 "input, output"
        currentParserContext = ParserContext::IdList;  // 设置当前解析上下文
        $1->emplace_back(std::string($3));  // 将新标识符添加到已有列表中
        $$ = $1;  // 返回更新后的列表
        
        GRAMMAR_TRACE("idlist -> idlist ',' IDENTIFIER");  // 记录语法跟踪日志
        free($3);  // 释放新标识符的字符串内存
    }
    ;

//常量声明，定义Pascal程序中常量声明部分的语法规则
const_declarations 
    // 规则1：空规则，匹配没有常量声明的情况
    : /*empty*/  {  // 当程序中没有常量声明时匹配此规则
        currentParserContext = ParserContext::ConstDeclarations;  // 设置当前解析上下文为常量声明
        $$ = nullptr;  // 返回空指针，表示没有常量声明
        GRAMMAR_TRACE("const_declarations -> empty");  // 记录语法跟踪日志
    }
    
    // 规则2：正常的常量声明，以CONST关键字开始
    | CONST const_declaration ';'  {  // 匹配 "const 常量定义列表;" 形式
        currentParserContext = ParserContext::ConstDeclarations;  // 设置当前解析上下文为常量声明
        ConstDeclStmt* constDecls = allocateNode<ConstDeclStmt>();  // 创建常量声明AST节点
        
        // 将声明列表中的键值对转移到常量声明对象中
        for(auto kvPair : *$2) {  // 遍历解析得到的常量定义键值对列表
            constDecls->pairs.emplace_back(std::make_pair(kvPair->first, kvPair->second));  // 创建新的键值对并添加到AST节点
            delete kvPair;  // 删除原始键值对对象（内容已被转移）
        }
        
        delete $2;  // 删除原始键值对列表容器
        $$ = constDecls;  // 设置返回值为构建的常量声明节点
        
        // 日志输出声明的常量信息（用于调试）
        for(auto &t: constDecls->pairs) {  // 遍历所有常量定义
            LOG_INFO("Get Const Type:%d, pointer %p", t.second->type, t.second.get());  // 记录常量类型和指针地址
            if(t.second->str) {  // 如果常量是字符串类型
                LOG_INFO("Get string:%s", t.second->str->val.c_str());  // 记录字符串值
            }
        }
        
        GRAMMAR_TRACE("const_declarations -> CONST const_declaration ';' const_declarations");  // 记录语法跟踪日志
    }
    
    // 规则3：错误恢复，处理CONST关键字后的语法错误
    | CONST error ';'  {  // 匹配 "const [错误] ;" 形式
        $$ = nullptr;  // 设置返回值为空指针，表示常量声明解析失败
        GRAMMAR_ERROR("const_declarations -> CONST error ;");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;

// 定义Pascal程序中常量定义列表的语法规则，处理一个或多个常量定义
const_declaration 
    // 规则1：单个常量定义 - 标识符 = 常量值
    : IDENTIFIER '=' const_value  {  // 匹配 "标识符 = 常量值" 形式，如 "PI = 3.14159"
        currentParserContext = ParserContext::ConstDeclaration;  // 设置当前解析上下文为常量声明
        auto constDecls = allocateNode<std::vector<std::pair<std::string, ValueStmt *>*>>();  // 创建存储键值对指针的向量
        auto kvPair = allocateNode<std::pair<std::string, ValueStmt *>>($1, $3);  // 创建键值对，存储常量名和值
        
        constDecls->emplace_back(kvPair);  // 将键值对指针添加到向量中
        free($1);  // 释放标识符字符串（已复制到键值对中）
        $$ = constDecls;  // 设置返回值为构建的常量声明列表
    }
    
    // 规则2：在已有常量声明列表后添加新的常量定义
    | const_declaration ';' IDENTIFIER '=' const_value  {  // 匹配 "已有声明; 标识符 = 常量值" 形式
        currentParserContext = ParserContext::ConstDeclaration;  // 设置当前解析上下文
        $1->emplace_back(allocateNode<std::pair<std::string, ValueStmt *>>($3, $5));  // 创建新键值对并添加到现有列表
        
        free($3);  // 释放标识符字符串
        $$ = $1;  // 返回更新后的常量声明列表
    }
    
    // 规则3：错误恢复，处理声明中的语法错误
    | error ';' IDENTIFIER '=' const_value  {  // 匹配 "[错误]; 标识符 = 常量值" 形式
        free($3);  // 释放标识符字符串
        delete $5;  // 删除常量值对象
        $$ = nullptr;  // 设置返回值为空指针，表示解析失败
        
        GRAMMAR_ERROR("const_declaration -> error ';' IDENTIFIER = const_value");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;

// 常量值
// 定义Pascal程序中常量值的语法规则，处理各种类型的常量字面量
const_value
    // 规则1：整数常量
    : INTEGER  {  // 匹配整数，如 "42"
        $$ = ValueFactory::makeInteger($1);  // 使用工厂方法创建整数值对象
    }
    
    // 规则2：带正号的整数常量
    | '+' INTEGER  {  // 匹配带正号的整数，如 "+42"
        $$ = ValueFactory::makeInteger($2);  // 创建整数值对象（正号不改变值）
    }
    
    // 规则3：带负号的整数常量
    | '-' INTEGER  {  // 匹配带负号的整数，如 "-42"
        $$ = ValueFactory::makeInteger(-$2);  // 创建负整数值对象（对值取负）
    }
    
    // 规则4：实数常量
    | REAL  {  // 匹配实数，如 "3.14159"
        $$ = ValueFactory::makeReal($1);  // 创建实数值对象
        free($1);  // 释放实数字符串（已在值对象中复制）
    }
    
    // 规则5：带正号的实数常量
    | '+' REAL  {  // 匹配带正号的实数，如 "+3.14159"
        $$ = ValueFactory::makeReal($2);  // 创建实数值对象（正号不改变值）
        free($2);  // 释放实数字符串
    }
    
    // 规则6：带负号的实数常量
    | '-' REAL  {  // 匹配带负号的实数，如 "-3.14159"
        ValueStmt* value = ValueFactory::makeReal($2);  // 先创建实数值对象
        // 处理负号：将实数值设为负数
        value->number->real_val *= -1;  // 将实数值取负（直接修改对象中的值）
        
        free($2);  // 释放实数字符串
        $$ = value;  // 返回修改后的值对象
    }
    
    // 规则7：字符常量
    | CHAR  {  // 匹配字符常量，如 "'A'"
        $$ = ValueFactory::makeChar($1);  // 创建字符值对象
        GRAMMAR_TRACE("const_value -> CHAR, value: %c");  // 记录语法跟踪日志（注意：缺少参数）
    }
    
    // 规则8：字符串常量
    | STRING  {  // 匹配字符串常量，如 "'Hello'"
        $$ = ValueFactory::makeString($1);  // 创建字符串值对象
        free($1);  // 释放字符串（已在值对象中复制）
    }
    ;

// 变量声明
// 定义Pascal程序中变量声明部分的语法规则
var_declarations 
    // 规则1：空规则，匹配没有变量声明的情况
    : /*empty*/  {  // 当程序中没有变量声明时匹配此规则
        $$ = nullptr;  // 返回空指针，表示没有变量声明
        GRAMMAR_TRACE("var_declarations -> empty");  // 记录语法跟踪日志
    }
    
    // 规则2：正常的变量声明，以VAR关键字开始
    | VAR var_declaration ';'  {  // 匹配 "var 变量声明列表;" 形式
        currentParserContext = ParserContext::VarDeclarations;  // 设置当前解析上下文为变量声明
        $$ = $2;  // 直接返回var_declaration的结果（变量声明列表）
        GRAMMAR_TRACE("var_declarations -> VAR var_declaration ';'");  // 记录语法跟踪日志
    }
    
    // 规则3：错误恢复，处理VAR关键字后的语法错误
    | VAR error ';'  {  // 匹配 "var [错误] ;" 形式
        $$ = nullptr;  // 设置返回值为空指针，表示变量声明解析失败
        GRAMMAR_ERROR("var_declarations -> VAR error ;");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;


// 变量声明列表
// 定义Pascal程序中变量声明列表的语法规则，处理一个或多个变量声明
var_declaration 
    // 规则1：单个变量声明 - 标识符列表:类型
    : idlist ':' type  {  // 匹配 "标识符列表 : 类型" 形式，如 "x, y : integer"
        currentParserContext = ParserContext::VarDeclaration;  // 设置当前解析上下文为变量声明
        auto varDecls = allocateNode<std::vector<VarDeclStmt *>>();  // 创建变量声明列表容器
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建单个变量声明对象
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), $1->begin(), $1->end());  // 添加所有标识符到变量声明对象
        
        // 处理类型信息
        varDecl->basic_type = $3->basic_type;  // 复制基本类型（integer, real等）
        varDecl->data_type = $3->data_type;    // 复制数据类型（基本类型、数组等）
        varDecl->array_range = std::move($3->array_range);  // 移动数组范围信息（如果是数组类型）
        
        delete $1;  // 释放标识符列表
        delete $3;  // 释放类型对象
        varDecls->emplace_back(varDecl);  // 将变量声明添加到列表中
        $$ = varDecls;  // 返回变量声明列表
        
        GRAMMAR_TRACE("var_declaration -> idlist ':' type");  // 记录语法跟踪日志
    }
    
    // 规则2：在已有变量声明列表后添加新的变量声明
    | var_declaration ';' idlist ':' type  {  // 匹配 "已有声明; 标识符列表 : 类型" 形式
        currentParserContext = ParserContext::VarDeclaration;  // 设置当前解析上下文
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建新的变量声明对象
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), $3->begin(), $3->end());  // 添加所有标识符到变量声明对象
        
        // 处理类型信息
        varDecl->basic_type = $5->basic_type;  // 复制基本类型
        varDecl->data_type = $5->data_type;    // 复制数据类型
        varDecl->array_range = std::move($5->array_range);  // 移动数组范围信息
        
        delete $3;  // 释放标识符列表
        delete $5;  // 释放类型对象
        $1->emplace_back(varDecl);  // 将新变量声明添加到已有列表中
        $$ = $1;  // 返回更新后的变量声明列表
        
        GRAMMAR_TRACE("var_declaration -> var_declaration ';' idlist ':' type");  // 记录语法跟踪日志
    }
    
    // 规则3：错误恢复，处理声明中的语法错误
    | error ';' idlist ':' type  {  // 匹配 "[错误]; 标识符列表 : 类型" 形式
        delete $3;  // 释放标识符列表
        delete $5;  // 释放类型对象
        $$ = nullptr;  // 设置返回值为空指针，表示解析失败
        
        GRAMMAR_ERROR("var_declaration -> error ';' idlist ':' type");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;


// 类型定义
// 定义Pascal程序中类型的语法规则，支持基本类型和数组类型
type 
    // 规则1：基本类型
    : basic_type  {  // 匹配基本类型，如 "integer"、"real"、"boolean"、"char"
        currentParserContext = ParserContext::Type;  // 设置当前解析上下文为类型
        auto typeStmt = allocateNode<VarDeclStmt>();  // 创建变量声明对象用于存储类型信息
        typeStmt->data_type = DataType::BasicType;  // 设置数据类型为基本类型
        typeStmt->basic_type = $1;  // 设置具体的基本类型（integer, real等）
        
        $$ = typeStmt;  // 返回创建的类型对象
        GRAMMAR_TRACE("type -> basic_type");  // 记录语法跟踪日志
    }
    
    // 规则2：数组类型
    | ARRAY '[' period_list ']' OF basic_type  {  // 匹配 "array [范围列表] of 基本类型"
        currentParserContext = ParserContext::Type;  // 设置当前解析上下文为类型
        auto typeStmt = allocateNode<VarDeclStmt>();  // 创建变量声明对象用于存储类型信息
        typeStmt->data_type = DataType::ArrayType;  // 设置数据类型为数组类型
        typeStmt->basic_type = $6;  // 设置数组元素的基本类型
        
        // 转移数组范围信息
        for(auto period : *$3) {  // 遍历范围列表
            typeStmt->array_range.emplace_back(std::unique_ptr<PeriodStmt>(period));  // 将每个范围添加到数组范围容器
        }
        
        delete $3;  // 释放原始范围列表容器
        $$ = typeStmt;  // 返回创建的类型对象
        GRAMMAR_TRACE("type -> ARRAY '[' period_list ']' OF basic_type");  // 记录语法跟踪日志
    }
    ;

// 基本类型
// 定义Pascal程序中基本数据类型的语法规则
basic_type
    // 规则1：整数类型
    : INTEGER_KW  {  // 匹配 "integer" 关键字
        $$ = BasicType::INT;  // 返回整数类型枚举值
        GRAMMAR_TRACE("basic_type -> INTEGER_KW");  // 记录语法跟踪日志
    }
    
    // 规则2：实数类型
    | REAL_KW  {  // 匹配 "real" 关键字
        $$ = BasicType::REAL;  // 返回实数类型枚举值
        GRAMMAR_TRACE("basic_type -> REAL_KW");  // 记录语法跟踪日志
    }
    
    // 规则3：布尔类型
    | BOOLEAN_KW  {  // 匹配 "boolean" 关键字
        $$ = BasicType::BOOLEAN;  // 返回布尔类型枚举值
        GRAMMAR_TRACE("basic_type -> BOOLEAN_KW");  // 记录语法跟踪日志
    }
    
    // 规则4：字符类型
    | CHAR_KW  {  // 匹配 "char" 关键字
        $$ = BasicType::CHAR;  // 返回字符类型枚举值
        GRAMMAR_TRACE("basic_type -> CHAR_KW");  // 记录语法跟踪日志
    }
    ;

// 数组索引范围
// 定义Pascal数组声明中索引范围的语法规则，支持单维和多维数组
period_list
    // 规则1：单个索引范围
    : INTEGER DOUBLE_DOT INTEGER  {  // 匹配 "整数 .. 整数" 形式，如 "1..10"
        auto periodList = allocateNode<std::vector<PeriodStmt *>>();  // 创建范围列表容器
        auto period = allocateNode<PeriodStmt>();  // 创建单个范围对象
        
        period->begin = $1;  // 设置范围起始值
        period->end = $3;    // 设置范围结束值
        periodList->emplace_back(period);  // 将范围对象添加到列表中
        
        $$ = periodList;  // 返回范围列表
        GRAMMAR_TRACE("period_list -> INTEGER '..' INTEGER");  // 记录语法跟踪日志
    }
    
    // 规则2：添加新的索引范围（用于多维数组）
    | period_list ',' INTEGER DOUBLE_DOT INTEGER  {  // 匹配 "已有范围列表, 整数 .. 整数" 形式
        auto period = allocateNode<PeriodStmt>();  // 创建新的范围对象
        period->begin = $3;  // 设置范围起始值
        period->end = $5;    // 设置范围结束值
        
        $1->emplace_back(period);  // 将新范围对象添加到已有列表中
        $$ = $1;  // 返回更新后的范围列表
        
        GRAMMAR_TRACE("period_list -> period_list ',' INTEGER '..' INTEGER");  // 记录语法跟踪日志
    }
    ;

// 函数和声明声明列表
// 定义Pascal程序中子程序（函数和过程）声明部分的语法规则
subprogram_declarations 
    // 规则1：空规则，匹配没有子程序声明的情况
    : /*empty*/  {  // 当程序中没有子程序声明时匹配此规则
        $$ = nullptr;  // 返回空指针，表示没有子程序声明
        GRAMMAR_TRACE("subprogram_declarations -> empty");  // 记录语法跟踪日志
    }
    
    // 规则2：添加子程序声明
    | subprogram_declarations subprogram ';'  {  // 匹配递归的子程序声明列表
        currentParserContext = ParserContext::SubprogramDeclarations;  // 设置当前解析上下文
        
        if ($1 == nullptr) {  // 如果这是第一个子程序声明
            auto funcDeclList = allocateNode<std::vector<FuncDeclStmt *>>();  // 创建新的函数声明列表
            funcDeclList->emplace_back($2);  // 添加新的子程序声明    
            $$ = funcDeclList;  // 返回新创建的列表
        } else {  // 如果已经有子程序声明
            $1->emplace_back($2);  // 将新的子程序声明添加到已有列表
            $$ = $1;  // 返回更新后的列表
        }
        
        GRAMMAR_TRACE("subprogram_declarations -> subprogram_declarations subprogram ';'");  // 记录语法跟踪日志
    }
    ;

// 子程序
// 定义Pascal程序中单个子程序（函数或过程）的语法规则
subprogram 
    // 子程序由头部、分号和主体组成
    : subprogram_head ';' subprogram_body  {  // 匹配 "子程序头部; 子程序主体" 形式
        currentParserContext = ParserContext::Subprogram;  // 设置当前解析上下文为子程序
        auto subprogram = allocateNode<FuncDeclStmt>();  // 创建函数声明AST节点
        
        subprogram->header = std::unique_ptr<FuncHeadDeclStmt>($1);  // 设置子程序头部，使用智能指针管理内存
        subprogram->body = std::unique_ptr<FuncBodyDeclStmt>($3);  // 设置子程序主体，使用智能指针管理内存
        
        $$ = subprogram;  // 返回创建的子程序声明节点
        GRAMMAR_TRACE("subprogram -> subprogram_head ';' subprogram_body");  // 记录语法跟踪日志
    }
    ;

// 函数或声明头部
// 定义Pascal程序中子程序（函数或过程）头部的语法规则
subprogram_head
    // 规则1：过程头部 - procedure 标识符 (参数列表)
    : PROCEDURE IDENTIFIER formal_parameter  {  // 匹配 "procedure 名称 (参数列表)" 形式
        currentParserContext = ParserContext::SubprogramHead;  // 设置当前解析上下文为子程序头部
        auto subHead = allocateNode<FuncHeadDeclStmt>();  // 创建函数头部声明节点
        
        subHead->func_name = std::string($2);  // 设置函数名称
        subHead->ret_type = BasicType::VOID;  // 设置返回类型为VOID（表示过程）
        
        // 处理形式参数
        if ($3 != nullptr) {  // 如果有参数列表
            for (auto formalParameter : *$3) {  // 遍历参数列表
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));  // 将参数添加到函数头部
            }
            delete $3;  // 释放原始参数列表容器
        }
        
        $$ = subHead;  // 返回创建的函数头部节点
        free($2);  // 释放函数名称字符串
        GRAMMAR_TRACE("subprogram_head -> PROCEDURE IDENTIFIER formal_parameter");  // 记录语法跟踪日志
    }
    
    // 规则2：函数头部 - function 标识符 (参数列表) : 返回类型
    | FUNCTION IDENTIFIER formal_parameter ':' basic_type  {  // 匹配 "function 名称 (参数列表) : 类型" 形式
        currentParserContext = ParserContext::SubprogramHead;  // 设置当前解析上下文为子程序头部
        auto subHead = allocateNode<FuncHeadDeclStmt>();  // 创建函数头部声明节点
        
        subHead->func_name = std::string($2);  // 设置函数名称
        subHead->ret_type = $5;  // 设置返回类型（由basic_type规则提供）
        
        // 处理形式参数
        if ($3 != nullptr) {  // 如果有参数列表
            for (auto formalParameter : *$3) {  // 遍历参数列表
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));  // 将参数添加到函数头部
            }
            delete $3;  // 释放原始参数列表容器
        }
        
        $$ = subHead;  // 返回创建的函数头部节点
        free($2);  // 释放函数名称字符串
        GRAMMAR_TRACE("subprogram_head -> FUNCTION IDENTIFIER formal_parameter ':' basic_type");  // 记录语法跟踪日志
    }
    
    // 规则3：函数头部错误恢复
    | FUNCTION error  {  // 匹配 "function" 后跟语法错误
        $$ = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("subprogram_head -> FUNCTION error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    
    // 规则4：过程头部错误恢复
    | PROCEDURE error  {  // 匹配 "procedure" 后跟语法错误
        $$ = nullptr;  // 设置返回值为空指针，表示解析失败
        GRAMMAR_ERROR("subprogram_head -> PROCEDURE error");  // 记录语法错误日志
        yyerrok;  // 告诉Bison错误已恢复，可以继续解析
    }
    ;

// 形式参数
// 定义Pascal程序中子程序（函数和过程）的形式参数部分的语法规则
formal_parameter 
    // 规则1：空参数 - 没有参数列表
    : /* empty */  {  // 匹配空规则，表示没有参数
        $$ = nullptr;  // 返回空指针，表示没有参数列表
        GRAMMAR_TRACE("formal_parameter -> empty");  // 记录语法跟踪日志
    }
    
    // 规则2：带参数列表 - (参数列表)
    | '(' parameter_list ')'  {  // 匹配 "(参数列表)" 形式
        currentParserContext = ParserContext::FormalParameter;  // 设置当前解析上下文为形式参数
        $$ = $2;  // 直接返回parameter_list的结果
        GRAMMAR_TRACE("formal_parameter -> '(' parameter_list ')'");  // 记录语法跟踪日志
    }
    ;

// 参数列表
// 定义Pascal程序中子程序（函数和过程）参数列表的语法规则
parameter_list
    // 规则1：空参数列表
    : /* empty */  {  // 匹配空规则，表示空的参数列表
        $$ = nullptr;  // 返回空指针，表示没有参数
        GRAMMAR_TRACE("parameter_list -> empty");  // 记录语法跟踪日志
    }
    
    // 规则2：单个参数
    | parameter  {  // 匹配单个参数，如 "x: integer" 或 "var a: real"
        auto paramList = allocateNode<std::vector<VarDeclStmt *>>();  // 创建参数列表容器
        paramList->emplace_back($1);  // 将参数添加到列表中
        
        $$ = paramList;  // 返回参数列表
        GRAMMAR_TRACE("parameter_list -> parameter");  // 记录语法跟踪日志
    }
    
    // 规则3：多个参数组（分号分隔）
    | parameter_list ';' parameter  {  // 匹配 "已有参数列表; 新参数" 形式
        $1->emplace_back($3);  // 将新参数添加到已有列表中
        $$ = $1;  // 返回更新后的列表
        
        GRAMMAR_TRACE("parameter_list -> parameter_list ';' parameter");  // 记录语法跟踪日志
    }
    ;

// 函数/过程的参数
parameter
    // 规则1：变量参数（使用var关键字，按引用传递）
    : var_parameter  {// 匹配变量参数，如"var x: integer"
        $$ = $1;// 将 var_parameter 的结果作为 parameter 的结果返回
        $$->is_var = true;// 标记这是一个变量参数（即通过引用传递）
        
        GRAMMAR_TRACE("parameter -> var_parameter"); // 记录语法跟踪信息到日志
    }
    // 规则2：值参数（不使用var关键字，按值传递）
    | value_parameter  {// 匹配值参数，如"x: integer"
        $$ = $1;// 将 value_parameter 的结果作为 parameter 的结果返回
        $$->is_var = false;// 标记这是一个值参数（即通过值传递）
        
        GRAMMAR_TRACE("parameter -> value_parameter"); // 记录语法跟踪信息到日志
    }
    ;

// 变量参数
var_parameter
    : VAR value_parameter  {  // 匹配以VAR关键字开头的参数定义
        $$ = $2;  // 将value_parameter创建的节点作为当前规则的结果
        GRAMMAR_TRACE("var_parameter -> VAR value_parameter");  // 记录语法规则应用的跟踪日志
    }
    ;

// 值参数
value_parameter
    : idlist ':' basic_type  {  // 匹配"标识符列表:类型"形式的参数定义
        auto varDecl = allocateNode<VarDeclStmt>();  // 创建新的变量声明节点
        
        varDecl->id.insert(varDecl->id.end(), $1->begin(), $1->end());  // 将标识符列表中的所有标识符添加到变量声明中
        varDecl->data_type = DataType::BasicType;  // 设置数据类型为基本类型
        varDecl->basic_type = $3;  // 设置具体的基本类型（integer、real等）
        varDecl->is_var = false;  // 初始设置为值参数（非var参数）
        
        delete $1;  // 释放标识符列表的内存（已复制到变量声明节点）
        $$ = varDecl;  // 返回创建的变量声明节点
        
        GRAMMAR_TRACE("value_parameter -> idlist ':' basic_type");  // 记录语法规则应用的跟踪日志
    }
    ;

// 子程序体 - 定义函数或过程的主体部分，包括常量声明、变量声明和执行语句块
// 这部分对应Pascal中函数/过程声明中begin-end之间的实现部分
subprogram_body 
   : const_declarations var_declarations compound_statement  {  // 匹配常量声明、变量声明和复合语句组成的子程序体
       currentParserContext = ParserContext::SubprogramBody;  // 设置当前解析上下文为子程序体
       auto funcBody = allocateNode<FuncBodyDeclStmt>();  // 创建函数体声明节点
       
       // 处理常量声明
       if ($1 != nullptr) {  // 如果存在常量声明部分
           funcBody->const_decl = std::unique_ptr<ConstDeclStmt>($1);  // 将常量声明添加到函数体，使用智能指针管理内存
       }
       
       // 处理变量声明
       if ($2 != nullptr) {  // 如果存在变量声明部分
           for (auto varDecl : *$2) {  // 遍历所有变量声明
               funcBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));  // 将每个变量声明添加到函数体
           }
           delete $2;  // 释放变量声明列表容器（内容已转移到函数体节点中）
       }
       
       // 处理复合语句
       if ($3 != nullptr) {  // 如果存在复合语句部分
           for (auto stmt : *$3) {  // 遍历所有语句
               funcBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到函数体
           }
           delete $3;  // 释放语句列表容器（内容已转移到函数体节点中）
       }
       
       $$ = funcBody;  // 返回构建完成的函数体节点
       GRAMMAR_TRACE("subprogram_body -> const_declarations var_declarations compound_statement");  // 记录语法解析跟踪信息
   }
   ;

// 复合语句
compound_statement 
   : BEGIN_TOKEN statement_list END  {  // 匹配"BEGIN 语句列表 END"形式的语法结构
       currentParserContext = ParserContext::CompoundStatement;  // 设置当前解析上下文为复合语句
       $$ = $2;  // 直接使用statement_list返回的语句列表作为复合语句的结果
                 // 这里没有创建新的AST节点，而是复用了语句列表节点
       
       GRAMMAR_TRACE("compound_statement -> BEGIN_TOKEN statement_list END");  // 记录语法解析跟踪信息
   }
   ;

// 语句列表 - 定义Pascal中由分号分隔的一系列语句
// 语句列表是构成复合语句的核心部分，用于顺序执行多个语句
statement_list 
   // 规则1：单个语句
   : statement  {  // 匹配单个语句
       $$ = $1;  // 直接返回statement规则的结果作为语句列表
                 // 如果statement返回nullptr（空语句），则语句列表也为空
       GRAMMAR_TRACE("statement_list -> statement");  // 记录语法解析跟踪信息
   }
   
   // 规则2：已有语句列表后添加新语句
   | statement_list ';' statement  {  // 匹配"已有语句列表;新语句"形式
       currentParserContext = ParserContext::StatementList;  // 设置当前解析上下文为语句列表
       
       // 如果有语句要添加，则处理
       if ($3 != nullptr) {  // 如果新语句不为空
           // 将第三个参数中的语句添加到结果列表中
           if ($1 != nullptr) {  // 如果原有语句列表不为空
               for (auto stmt : *$3) {  // 遍历新语句列表中的所有语句
                   $1->emplace_back(stmt);  // 将新语句添加到原有列表末尾
               }
           } else {  // 如果原有语句列表为空
               // 如果结果列表为空，则直接使用第三个参数
               $1 = $3;  // 使用新语句列表作为结果
               $3 = nullptr;  // 避免新语句列表被删除，因为它现在是结果
           }
       }
       
       $$ = $1;  // 返回更新后的语句列表
       delete $3;  // 如果$3已经被处理过，这里实际上删除的是nullptr
       
       GRAMMAR_TRACE("statement_list -> statement_list ';' statement");  // 记录语法解析跟踪信息
   }
   
   // 规则3：错误恢复 - 处理语句列表开始部分的错误
   | error ';' statement  {  // 匹配"[错误];语句"形式
       if ($3 != nullptr) {  // 如果后面的语句不为空
           for (auto item : *$3) {  // 遍历语句列表中的所有语句
               delete item;  // 删除每个语句节点
           }
           delete $3;  // 删除语句列表容器
       }
       
       $$ = nullptr;  // 返回空指针，表示语法错误导致解析失败
       GRAMMAR_ERROR("statement_list -> error ';' statement");  // 记录语法错误信息
       yyerrok;  // 告诉Bison错误已恢复，可以继续解析
   }
   
   // 规则4：错误恢复 - 处理语句列表末尾部分的错误
   | statement_list ';' error  {  // 匹配"语句列表;[错误]"形式
       if ($1 != nullptr) {  // 如果前面的语句列表不为空
           for (auto item : *$1) {  // 遍历语句列表中的所有语句
               delete item;  // 删除每个语句节点
           }
           delete $1;  // 删除语句列表容器
       }
       
       $$ = nullptr;  // 返回空指针，表示语法错误导致解析失败
       GRAMMAR_ERROR("statement_list -> statement_list ';' error");  // 记录语法错误信息
       yyerrok;  // 告诉Bison错误已恢复，可以继续解析
   }
   ;

// 语句 - 定义Pascal语言中的各种语句类型及其语法结构
// 包括空语句、赋值语句、过程调用、复合语句、条件语句、循环语句、IO语句和控制转移语句
// 每种语句类型都创建对应的AST节点，用于后续代码生成和优化
statement 
   // 规则1：空语句
   : /*empty*/  {  // 匹配空语句，如连续两个分号之间没有代码
       $$ = nullptr;  // 返回空指针表示没有语句
       GRAMMAR_TRACE("statement -> empty");  // 记录语法解析跟踪信息
   }
   
   // 规则2：赋值语句
   | variable ASSIGNOP expression  {  // 匹配"变量 := 表达式"形式，如"x := y + 1"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto assignStmt = allocateNode<AssignStmt>();  // 创建赋值语句节点
       
       assignStmt->lval = std::unique_ptr<LValStmt>($1);  // 设置左值（赋值目标）
       assignStmt->expr = std::unique_ptr<ExprStmt>($3);  // 设置右值表达式
       
       stmtList->emplace_back(assignStmt);  // 将赋值语句添加到语句列表
       $$ = stmtList;  // 返回包含赋值语句的列表
       
       GRAMMAR_TRACE("statement -> variable ASSIGNOP expression");  // 记录语法解析跟踪信息
   }
   
   // 规则3：过程调用语句
   | procedure_call  {  // 匹配过程调用，如"WriteLn(x)"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       stmtList->emplace_back($1);  // 将过程调用添加到语句列表
       
       $$ = stmtList;  // 返回包含过程调用的列表
       GRAMMAR_TRACE("statement -> procedure_call");  // 记录语法解析跟踪信息
   }
   
   // 规则4：复合语句（语句块）
   | compound_statement  {  // 匹配begin-end块，如"begin x:=1; y:=2 end"
       $$ = $1;  // 直接返回复合语句规则的结果
       GRAMMAR_TRACE("statement -> compound_statement");  // 记录语法解析跟踪信息
   }
   
   // 规则5：while循环语句
   | WHILE expression DO statement  {  // 匹配"while 条件表达式 do 循环体"形式
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto whileStmt = allocateNode<WhileStmt>();  // 创建while语句节点
       
       whileStmt->expr = std::unique_ptr<ExprStmt>($2);  // 设置循环条件表达式
       
       // 处理循环体语句
       if ($4 != nullptr) {  // 如果循环体不为空
           for (auto stmt : *$4) {  // 遍历循环体中的所有语句
               whileStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到while语句的循环体
           }
           delete $4;  // 释放语句列表容器（内容已转移）
       }
       
       stmtList->emplace_back(whileStmt);  // 将while语句添加到语句列表
       $$ = stmtList;  // 返回包含while语句的列表
       
       GRAMMAR_TRACE("statement -> WHILE expression DO statement");  // 记录语法解析跟踪信息
   }
   
    // 规则6：if语句（带可选的else部分）
    | IF expression THEN statement else_part {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
        auto ifStmt = allocateNode<IfStmt>();  // 创建if语句节点
        
        ifStmt->expr = std::unique_ptr<ExprStmt>($2);  // 设置条件表达式
        
        // 处理if条件为真时的语句
        if ($4 != nullptr) {  // 如果then分支不为空
            for (auto stmt : *$4) {  // 遍历then分支中的所有语句
                ifStmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到if语句的true分支
            }
        }
        
        // 处理if条件为假时的语句（else部分）
        if ($5 != nullptr) {  // 如果else分支不为空
            for (auto stmt : *$5) {  // 遍历else分支中的所有语句
                ifStmt->false_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到if语句的false分支
            }
        }
        
        stmtList->emplace_back(ifStmt);  // 将if语句添加到语句列表
        delete $4;  // 释放then分支语句列表容器（内容已转移）
        delete $5;  // 释放else分支语句列表容器（内容已转移）
        
        $$ = stmtList;  // 返回包含if语句的列表
        GRAMMAR_TRACE("statement -> IF expression THEN statement else_part");  // 记录语法解析跟踪信息
    }
   
   // 规则8：for循环语句
   | FOR IDENTIFIER ASSIGNOP expression TO expression DO statement  {  // 匹配"for 变量:=初值 to 终值 do 循环体"形式
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto forStmt = allocateNode<ForStmt>();  // 创建for语句节点
       
       forStmt->id = std::string($2);  // 设置循环变量名
       forStmt->begin = std::unique_ptr<ExprStmt>($4);  // 设置循环初始值表达式
       forStmt->end = std::unique_ptr<ExprStmt>($6);  // 设置循环终止值表达式
       
       // 处理循环体语句
       if ($8 != nullptr) {  // 如果循环体不为空
           for (auto stmt : *$8) {  // 遍历循环体中的所有语句
               forStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));  // 将每个语句添加到for语句的循环体
           }
       }
       
       stmtList->emplace_back(forStmt);  // 将for语句添加到语句列表
       free($2);  // 释放循环变量名字符串
       delete $8;  // 释放循环体语句列表容器（内容已转移）
       
       $$ = stmtList;  // 返回包含for语句的列表
       GRAMMAR_TRACE("statement -> FOR IDENTIFIER ASSIGNOP expression TO expression DO statement");  // 记录语法解析跟踪信息
   }
   
   // 规则9：读取语句
   | READ '(' variable_list ')'  {  // 匹配"read(变量列表)"形式，如"read(x, y, z)"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto readStmt = allocateNode<ReadFuncStmt>();  // 创建读取语句节点
       
       // 处理变量列表
       for (auto lval : *$3) {  // 遍历变量列表中的所有变量
           readStmt->lval.emplace_back(std::unique_ptr<LValStmt>(lval));  // 将每个变量添加到读取语句的变量列表
       }
       
       delete $3;  // 释放变量列表容器（内容已转移）
       stmtList->emplace_back(readStmt);  // 将读取语句添加到语句列表
       $$ = stmtList;  // 返回包含读取语句的列表
       
       GRAMMAR_TRACE("statement -> READ '(' variable_list ')'");  // 记录语法解析跟踪信息
   }
   
   // 规则10：写入语句
   | WRITE '(' expression_list ')'  {  // 匹配"write(表达式列表)"形式，如"write(x+y, 'hello')"
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto writeStmt = allocateNode<WriteFuncStmt>();  // 创建写入语句节点
       
       // 处理表达式列表
       if ($3 != nullptr) {  // 如果表达式列表不为空
           for (auto expr : *$3) {  // 遍历表达式列表中的所有表达式
               writeStmt->expr.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 将每个表达式添加到写入语句的表达式列表
           }
       }
       
       stmtList->emplace_back(writeStmt);  // 将写入语句添加到语句列表
       delete $3;  // 释放表达式列表容器（内容已转移）
       $$ = stmtList;  // 返回包含写入语句的列表
       
       GRAMMAR_TRACE("statement -> WRITE '(' expression_list ')'");  // 记录语法解析跟踪信息
   }
   
   // 规则11：break语句
   | BREAK  {  // 匹配break关键字
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto breakStmt = allocateNode<BreakStmt>();  // 创建break语句节点
       
       stmtList->emplace_back(breakStmt);  // 将break语句添加到语句列表
       $$ = stmtList;  // 返回包含break语句的列表
       
       GRAMMAR_TRACE("statement -> BREAK");  // 记录语法解析跟踪信息
   }
   
   // 规则12：continue语句
   | CONTINUE  {  // 匹配continue关键字
       auto stmtList = allocateNode<std::vector<BaseStmt *>>();  // 创建语句列表容器
       auto continueStmt = allocateNode<ContinueStmt>();  // 创建continue语句节点
       
       stmtList->emplace_back(continueStmt);  // 将continue语句添加到语句列表
       $$ = stmtList;  // 返回包含continue语句的列表
       
       GRAMMAR_TRACE("statement -> CONTINUE");  // 记录语法解析跟踪信息
   }
   ;

// 变量列表 - 定义Pascal中由逗号分隔的变量引用序列
// 主要用于read/readln语句中的输入目标，也可用于其他需要多个变量的上下文
variable_list 
   // 规则1：单个变量
   : variable  {  // 匹配单个变量引用，如"x"或"arr[i]"
       currentParserContext = ParserContext::VariableList;  // 设置当前解析上下文为变量列表
       auto lvalList = allocateNode<std::vector<LValStmt *>>();  // 创建左值列表容器，变量列表中的元素必须是可赋值的目标（左值）
       
       lvalList->emplace_back($1);  // 将变量添加到左值列表中
       $$ = lvalList;  // 返回包含单个变量的左值列表
       
       GRAMMAR_TRACE("variable_list -> variable");  // 记录语法解析跟踪信息
   }
   
   // 规则2：已有变量列表后添加新变量
   | variable_list ',' variable  {  // 匹配"已有变量列表,新变量"形式，如"x, y, z"
       currentParserContext = ParserContext::VariableList;  // 设置当前解析上下文为变量列表
       
       if ($3 != nullptr) {  // 如果新变量不为空
           $1->emplace_back($3);  // 将新变量添加到已有列表末尾
       } 
       
       $$ = $1;  // 返回更新后的变量列表
       GRAMMAR_TRACE("variable_list -> variable_list ',' variable");  // 记录语法解析跟踪信息
   }
   
   // 规则3：错误恢复 - 处理变量列表开始部分的错误
   | error ',' variable  {  // 匹配"[错误],变量"形式
       delete $3;  // 释放新变量的内存
       $$ = nullptr;  // 返回空指针，表示语法错误导致解析失败
       
       GRAMMAR_ERROR("variable_list -> error ',' variable");  // 记录语法错误信息
   }
   ;

// 变量可以是简单标识符或带有数组下标的形式
// 这是构建左值表达式的基础，用于赋值语句左侧和IO语句的参数
variable 
   : IDENTIFIER id_varpart  {  // 匹配"标识符 可选的数组下标"形式，如"x"或"array[i, j+1]"
       currentParserContext = ParserContext::Variable;  // 设置当前解析上下文为变量
       auto lval = allocateNode<LValStmt>();  // 创建左值语句节点
       
       lval->id = std::string($1);  // 设置变量的标识符名称
       
       // 处理变量的数组下标部分
       if ($2 != nullptr) {  // 如果有数组下标部分
           for (auto expr : *$2) {  // 遍历所有下标表达式
               lval->array_index.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 将每个下标表达式添加到左值的数组索引列表
           }
           delete $2;  // 释放下标表达式列表容器（内容已转移）
       }
       
       free($1);  // 释放标识符字符串（已复制到节点中）
       $$ = lval;  // 返回构建的左值节点
       
       GRAMMAR_TRACE("variable -> IDENTIFIER id_varpart");  // 记录语法解析跟踪信息
   }
   ;

// 定义Pascal中变量引用时可选的数组索引部分，处理不同形式的数组下标表示法，包括单维数组、多维数组
// 4.4 变量附加部分（数组下标）- 定义Pascal中变量引用时可选的数组索引部分
// 简化版：只处理标准形式的数组下标
id_varpart 
    // 规则1：空规则 - 处理没有数组下标的普通变量
    : /*empty*/  {  // 匹配没有数组下标的情况，如简单变量"x"
        $$ = nullptr;  // 返回空指针，表示没有数组下标
        GRAMMAR_TRACE("id_varpart -> empty");  // 记录语法解析跟踪信息
    }
    
    // 规则2：标准数组下标 - 方括号内的表达式列表
    | '[' expression_list ']'  {  // 匹配"[表达式列表]"形式，如"[i]"或"[i, j]"
        currentParserContext = ParserContext::IdVarpart;  // 设置当前解析上下文为变量附加部分
        
        if ($2 != nullptr) {  // 如果表达式列表不为空
            $$ = $2;  // 直接使用expression_list返回的表达式列表作为结果    
        } else {  // 如果表达式列表为空（语法错误）
            yyerror(&@2, "code_str", program, scanner, "数组下标定义出错 请检查是否符合规范");  // 报告错误：数组下标定义有误
        }
        
        GRAMMAR_TRACE("id_varpart -> '[' expression_list ']'");  // 记录语法解析跟踪信息
    }
    ;


// 过程调用 - 定义Pascal中过程调用的语法结构
// 支持两种形式：不带参数的简单调用和带参数列表的调用
// 生成函数调用语句的AST节点，用于后续代码生成
procedure_call 
   // 规则1：不带参数的过程调用
   : IDENTIFIER  {  // 匹配单个标识符，如"WriteLn"或"Initialize"
       currentParserContext = ParserContext::ProcedureCall;  // 设置当前解析上下文为过程调用
       auto procCall = allocateNode<FuncCallStmt>();  // 创建函数调用语句节点
       
       procCall->id = std::string($1);  // 设置过程名称
       free($1);  // 释放词法分析器分配的标识符字符串（已复制到节点中）
       $$ = procCall;  // 返回创建的函数调用节点
       
       GRAMMAR_TRACE("procedure_call -> IDENTIFIER");  // 记录语法解析跟踪信息
   }
   
   // 规则2：带参数的过程调用
   | IDENTIFIER '(' expression_list ')'  {  // 匹配"标识符(参数列表)"形式，如"WriteLn(x, y+1)"
       currentParserContext = ParserContext::ProcedureCall;  // 设置当前解析上下文为过程调用
       auto procCall = allocateNode<FuncCallStmt>();  // 创建函数调用语句节点
       
       procCall->id = std::string($1);  // 设置过程名称
       
       // 处理参数表达式列表
       if ($3 != nullptr) {  // 如果参数列表不为空
           for (auto expr : *$3) {  // 遍历所有参数表达式
               procCall->args.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 将每个参数表达式添加到函数调用的参数列表
           }
           delete $3;  // 释放表达式列表容器（内容已转移到函数调用节点）
       }
       
       free($1);  // 释放词法分析器分配的标识符字符串
       $$ = procCall;  // 返回创建的函数调用节点
       
       GRAMMAR_TRACE("procedure_call -> IDENTIFIER '(' expression_list ')'");  // 记录语法解析跟踪信息
   }
   ;

// else部分 - 定义可选的else分支
else_part
    // 规则1：空（无else分支）
    : /*empty*/ {
        $$ = nullptr;  // 返回空指针表示没有else分支
        GRAMMAR_TRACE("else_part -> empty");  // 记录语法解析跟踪信息
    }
    
    // 规则2：else分支
    | ELSE statement {
        $$ = $2;  // 直接返回else分支中的语句列表
        GRAMMAR_TRACE("else_part -> ELSE statement");  // 记录语法解析跟踪信息
    }
    ;

// 表达式列表 - 定义Pascal中由逗号分隔的表达式序列
// 用于函数/过程调用的参数列表、数组索引和其他需要多个表达式的上下文
// 支持空列表、单个表达式和多个表达式，构建表示表达式序列的数据结构
expression_list 
   // 规则1：空表达式列表
   : /*empty*/  {  // 匹配空表达式列表，如"()"或"[]"中没有内容
       $$ = nullptr;  // 返回空指针，表示空列表
       GRAMMAR_TRACE("expression_list -> empty");  // 记录语法解析跟踪信息
   }
   
   // 规则2：单个表达式
   | expression  {  // 匹配单个表达式，如"(x)"或"[i]"
       currentParserContext = ParserContext::ExpressionList;  // 设置当前解析上下文为表达式列表
       auto exprList = allocateNode<std::vector<ExprStmt *>>();  // 创建表达式列表容器
       
       exprList->emplace_back($1);  // 将表达式添加到列表中
       $$ = exprList;  // 返回包含单个表达式的列表
       
       GRAMMAR_TRACE("expression_list -> expression");  // 记录语法解析跟踪信息
   }
   
   // 规则3：已有表达式列表后添加新表达式
   | expression_list ',' expression  {  // 匹配"已有表达式列表,新表达式"形式，如"x, y, z"
       currentParserContext = ParserContext::ExpressionList;  // 设置当前解析上下文为表达式列表
       
       $1->emplace_back($3);  // 将新表达式添加到已有列表末尾
       $$ = $1;  // 返回更新后的表达式列表
       
       GRAMMAR_TRACE("expression_list -> expression_list ',' expression");  // 记录语法解析跟踪信息
   }
   ;

// 表达式 - 定义Pascal语言中的表达式语法结构
// 处理简单表达式和关系表达式（使用关系运算符如=, <>, <, <=, >, >=, in连接的表达式）
// 生成表达式的AST节点，支持条件判断、赋值和其他需要表达式的上下文
expression 
   // 规则1：简单表达式
   : simple_expression  {  // 匹配不包含关系运算符的表达式，如"x+y"或"a*b-c"
       currentParserContext = ParserContext::Expression;  // 设置当前解析上下文为表达式
       $$ = ExprFactory::createFromSimpleExpr($1);  // 使用工厂方法将简单表达式转换为完整表达式
                                                    // 创建包含简单表达式的表达式节点结构
       
       GRAMMAR_TRACE("expression -> simple_expression");  // 记录语法解析跟踪信息
   }
   
   // 规则2：关系表达式
   | expression relop simple_expression  {  // 匹配"表达式 关系运算符 简单表达式"形式，如"a > b"或"x = y+1"
       currentParserContext = ParserContext::Expression;  // 设置当前解析上下文为表达式
       auto expr = $1;  // 获取左侧已解析的表达式
       RelExprStmt::Term term;  // 创建新的关系表达式项结构
       
       term.type = getRelationOperator($2);  // 设置关系运算符类型（通过将语法分析器的token转换为枚举值）
       term.add_expr = std::unique_ptr<AddExprStmt>($3);  // 设置右侧的简单表达式，使用智能指针管理内存
       expr->rel_expr->terms.emplace_back(std::move(term));  // 将新的关系表达式项添加到表达式节点中
                                                             // 使用std::move转移所有权
       
       $$ = expr;  // 返回更新后的表达式节点作为规则结果
       GRAMMAR_TRACE("expression -> simple_expression relop simple_expression");  // 记录语法解析跟踪信息
                                                                                 // 注意：跟踪信息不准确，应为"expression relop simple_expression"
   }
   ;

// 简单表达式 - 定义Pascal中简单表达式的语法结构
// 处理由加法级运算符(+, -, or)连接的项(term)序列
// 构建简单表达式的AST节点，作为完整表达式的组成部分
simple_expression 
   // 规则1：单个项
   : term  {  // 匹配单个项，如"a*b"或"x"
       currentParserContext = ParserContext::SimpleExpression;  // 设置当前解析上下文为简单表达式
       $$ = ExprFactory::createFromTerm($1);  // 使用工厂方法将项转换为简单表达式
                                             // 创建包含单个项的简单表达式节点
       
       GRAMMAR_TRACE("simple_expression -> term");  // 记录语法解析跟踪信息
   }
   
   // 规则2：加法表达式
   | simple_expression addop term  {  // 匹配"简单表达式 加法运算符 项"形式，如"a+b"或"x-y*z"
       currentParserContext = ParserContext::SimpleExpression;  // 设置当前解析上下文为简单表达式
       auto addExpr = $1;  // 获取左侧已解析的简单表达式
       AddExprStmt::Term term;  // 创建新的加法表达式项
       
       term.type = getArithmeticOperator($2);  // 设置加法运算符类型（如Plus、Minus、Or）
       term.mul_expr = std::unique_ptr<MulExprStmt>($3);  // 将右侧的项转换为智能指针并存储
       addExpr->terms.emplace_back(std::move(term));  // 将新的加法表达式项添加到简单表达式节点
                                                      // 使用std::move转移所有权
       
       $$ = addExpr;  // 返回更新后的简单表达式节点
       GRAMMAR_TRACE("simple_expression -> simple_expression %lld term\n");  // 记录语法解析跟踪信息
                                                                             // 注意：跟踪信息中有格式化问题，应使用运算符符号而非%lld
   }
   ;

// 项 - 定义Pascal语言中项(term)的语法结构
// 处理由乘法级运算符(*, /, div, mod, and, andthen)连接的因子(factor)序列
// 构建项的AST节点，作为简单表达式的组成部分，实现乘法运算优先级高于加法运算
term 
   // 规则1：单个因子
   : factor  {  // 匹配单个因子，如"x"或"(a+b)"
       currentParserContext = ParserContext::Term;  // 设置当前解析上下文为项
       $$ = ExprFactory::createFromFactor($1);  // 使用工厂方法将因子转换为项
                                               // 创建包含单个因子的项节点
       
       GRAMMAR_TRACE("term -> factor");  // 记录语法解析跟踪信息
   }
   
   // 规则2：乘法表达式
   | term mulop factor  {  // 匹配"项 乘法运算符 因子"形式，如"a*b"或"x/y"
       currentParserContext = ParserContext::Term;  // 设置当前解析上下文为项
       auto mulExpr = $1;  // 获取左侧已解析的项
       MulExprStmt::Term term;  // 创建新的乘法表达式项结构
       
       term.type = getTermOperator($2);  // 设置乘法运算符类型（如Mul、Div、Mod、And、AndThen）
       term.unary_expr = std::unique_ptr<UnaryExprStmt>($3);  // 将右侧的因子转换为智能指针并存储
       mulExpr->terms.emplace_back(std::move(term));  // 将新的乘法表达式项添加到项节点
                                                      // 使用std::move转移所有权
       
       $$ = mulExpr;  // 返回更新后的项节点
       GRAMMAR_TRACE("term -> term mulop factor");  // 记录语法解析跟踪信息
   }
   ;

// 因子 - 定义Pascal语言中因子(factor)的语法结构
// 因子是表达式的基本构建单元，包括常量、变量、函数调用、括号表达式和一元运算
// 处理各种类型的基本值和带一元运算符(+, -, not)的表达式，构建表达式层次结构的底层
factor 
   // 规则1：整数常量
   : INTEGER  {  // 匹配整数字面量，如"42"或"1024"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;  // 设置值类型为数字
       unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();  // 创建数字节点
       setupNumberNode(unaryExpr->primary_expr->value->number, $1);  // 设置数字节点的值和类型信息
       
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       GRAMMAR_TRACE("factor -> INTEGER");  // 记录语法解析跟踪信息
   }
   
   // 规则2：实数常量
   | REAL  {  // 匹配实数字面量，如"3.14"或"2.71828"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;  // 设置值类型为数字
       unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();  // 创建数字节点
       
       double val = atof($1);  // 将字符串转换为双精度浮点数
       setupNumberNode(unaryExpr->primary_expr->value->number, val);  // 设置数字节点的值和类型信息
       unaryExpr->primary_expr->value->number->literal = std::string($1);  // 保存原始字面量字符串
       
       free($1);  // 释放词法分析器分配的字符串内存
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       
       GRAMMAR_TRACE("factor -> REAL");  // 记录语法解析跟踪信息
   }
   
   // 规则3：布尔常量
   | BOOLEAN  {  // 匹配布尔字面量，如"true"或"false"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;  // 设置值类型为数字
       unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();  // 创建数字节点
       
       long long int val = $1 ? 1 : 0;  // 将布尔值转换为整数（true=1, false=0）
       setupNumberNode(unaryExpr->primary_expr->value->number, val);  // 设置数字节点的值和类型信息
       
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       GRAMMAR_TRACE("factor -> BOOLEAN");  // 记录语法解析跟踪信息
   }
   
   // 规则4：字符常量
   | CHAR  {  // 匹配字符字面量，如"'A'"或"'z'"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;  // 设置值类型为数字
       unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();  // 创建数字节点
       
       setupNumberNode(unaryExpr->primary_expr->value->number, $1);  // 设置数字节点的值和类型信息
       
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       GRAMMAR_TRACE("factor -> CHAR");  // 记录语法解析跟踪信息
   }
   
   // 规则5：变量引用
   | variable  {  // 匹配变量引用，如"x"或"array[i]"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::LVal;  // 设置值类型为左值
       unaryExpr->primary_expr->value->lval = std::unique_ptr<LValStmt>($1);  // 设置左值节点
       
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       GRAMMAR_TRACE("factor -> variable");  // 记录语法解析跟踪信息
   }
   
   // 规则6：括号表达式
   | '(' expression ')'  {  // 匹配括号表达式，如"(a+b)"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Parentheses);  // 创建一元表达式节点，类型为括号表达式
       
       unaryExpr->primary_expr->expr = std::unique_ptr<ExprStmt>($2);  // 设置括号内的表达式
       
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       GRAMMAR_TRACE("factor -> '(' expression ')'");  // 记录语法解析跟踪信息
   }
   
   // 规则7：函数调用
   | IDENTIFIER '(' expression_list ')'  {  // 匹配函数调用，如"Sqrt(x)"或"Max(a, b)"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);  // 创建一元表达式节点，类型为值
       
       unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();  // 创建值语句节点
       unaryExpr->primary_expr->value->type = ValueStmt::ValueType::FuncCall;  // 设置值类型为函数调用
       unaryExpr->primary_expr->value->func_call = std::make_unique<FuncCallStmt>();  // 创建函数调用节点
       unaryExpr->primary_expr->value->func_call->id = std::string($1);  // 设置函数名
       
       // 处理函数参数
       if ($3 != nullptr) {  // 如果有参数列表
           for (auto expr : *$3) {  // 遍历所有参数表达式
               unaryExpr->primary_expr->value->func_call->args.emplace_back(std::unique_ptr<ExprStmt>(expr));  // 添加参数
           }
           delete $3;  // 释放参数列表容器
       }
       
       free($1);  // 释放函数名字符串
       $$ = unaryExpr;  // 返回创建的一元表达式节点
       
       GRAMMAR_TRACE("factor -> IDENTIFIER '(' expression_list ')'");  // 记录语法解析跟踪信息
   }
   
   // 规则8：逻辑非运算
   | NOT factor  {  // 匹配逻辑非表达式，如"not flag"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = $2;  // 获取操作数（已解析的因子）
       
       unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Not);  // 添加逻辑非运算符类型
       
       $$ = unaryExpr;  // 返回更新后的一元表达式节点
       GRAMMAR_TRACE("factor -> NOT factor");  // 记录语法解析跟踪信息
   }
   
   // 规则9：正号运算
   | '+' factor  {  // 匹配带正号的因子，如"+5"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       $$ = $2;  // 正号不改变值，直接使用操作数（因正号不影响值）
       
       GRAMMAR_TRACE("factor -> '+' factor");  // 记录语法解析跟踪信息
   }
   
   // 规则10：负号运算
   | '-' factor  {  // 匹配带负号的因子，如"-x"或"-5"
       currentParserContext = ParserContext::Factor;  // 设置当前解析上下文为因子
       auto unaryExpr = $2;  // 获取操作数（已解析的因子）
       
       unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Minus);  // 添加负号运算符类型
       
       $$ = unaryExpr;  // 返回更新后的一元表达式节点
       GRAMMAR_TRACE("factor -> '-' factor");  // 记录语法解析跟踪信息
   }
   ;

// 运算符定义 - 将Pascal语言中的各类运算符映射为数值常量
// 通过数值表示不同的运算符类型，简化语法规则和运算符处理
// 加法级运算符定义 (+, -, or)
addop 
   : '+' { $$ = 0; }  // 加号运算符映射为0
   | '-' { $$ = 1; }  // 减号运算符映射为1
   | OR  { $$ = 2; }  // 逻辑或运算符映射为2
   ;

// 关系运算符定义 (=, <>, <, <=, >, >=, in)
relop 
   : '=' { $$ = 0; }  // 等于运算符映射为0
   | NE  { $$ = 1; }  // 不等于运算符(<>)映射为1
   | '<' { $$ = 2; }  // 小于运算符映射为2
   | LE  { $$ = 3; }  // 小于等于运算符(<=)映射为3
   | '>' { $$ = 4; }  // 大于运算符映射为4
   | GE  { $$ = 5; }  // 大于等于运算符(>=)映射为5
   | IN  { $$ = 6; }  // 集合成员测试运算符(in)映射为6
   ;

// 乘法级运算符定义 (*, /, div, mod, and, andthen)
mulop 
   : '*'     { $$ = 0; }  // 乘法运算符映射为0
   | '/'     { $$ = 1; }  // 浮点除法运算符映射为1
   | DIV     { $$ = 1; }  // 整数除法运算符也映射为1（与浮点除法共用编号）
   | MOD     { $$ = 2; }  // 取模运算符映射为2
   | AND     { $$ = 3; }  // 逻辑与运算符映射为3
   | ANDTHEN { $$ = 4; }  // 短路逻辑与运算符映射为4
   ;

// 错误恢复
// 跳过错误部分直到分号，允许编译器识别并报告多个错误而不是在第一个错误处停止
error_recovery 
    : error ';'  {// 匹配从错误标记到分号的任何内容
        yyerrok;
    }
    ;

%%
// 语法分析辅助函数 - 提供Pascal编译器中的错误处理和语法分析功能
// 包含错误报告、详细错误信息格式化、源代码位置定位和语法分析入口函数

// 声明外部函数，用于将输入字符串加载到词法分析器中进行处理
extern void scan_string(const char *str, yyscan_t scanner);

// 基本错误处理函数 - 由Bison在遇到语法错误时自动调用
int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg)
{
   (void)program;  // 未使用的参数，使用void转换避免编译警告
   (void)scanner;  // 未使用的参数，使用void转换避免编译警告
   (void)msg;      // 未使用的参数，使用void转换避免编译警告
   
   syntaxErrorFlag = true;  // 设置全局错误标志，表示发生了语法错误
   LOG_ERROR("[Syntax Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column + 1, msg);  // 记录错误信息到日志
   return 0;  // 返回0表示错误已处理但不终止解析
}

// 详细语法错误报告函数 - 为用户提供更直观的错误信息
static int yyreport_syntax_error(const yypcontext_t *ctx, const char * code_str, ProgramStmt ** program, void * scanner)
{
   syntaxErrorFlag = true;  // 设置全局错误标志
   int res = 0;  // 初始化返回值
   std::ostringstream buf;  // 创建字符串流用于构建错误消息
   
   // 构建错误位置信息（高亮显示文件名和行号）
   buf << "\033[1;37m" << G_SETTINGS.input_file << ":" << yypcontext_location(ctx)->first_line 
       << ":" << yypcontext_location(ctx)->first_column + 1 << ":\033[0m";
   buf << " \033[1;31m" << "Syntax error:" << "\033[0m";  // 添加红色的"Syntax error:"标记
   bool have_expected = false;  // 标记是否有期望的token信息
   
   // 报告此处期望的token（最多显示5个）
   {
       enum { TOKENMAX = 5 };  // 最多显示5个期望的token
       yysymbol_kind_t expected[TOKENMAX];  // 存储期望的token类型
       int n = yypcontext_expected_tokens(ctx, expected, TOKENMAX);  // 获取期望的token
       
       if (n < 0) {  // 如果获取失败
           // 向yyparse传递错误
           res = n;  // 设置返回值表示错误
       } else {  // 成功获取期望的token
           for (int i = 0; i < n; ++i)  // 遍历所有期望的token
               buf << (i == 0 ? " expected" : " or") << " " << yysymbol_name(expected[i]);  // 格式化显示
           
           if (n > 0)  // 如果有期望的token
               have_expected = true;  // 设置标志
       }
   }
   
   // 报告意外的token（实际遇到的token）
   {
       yysymbol_kind_t lookahead = yypcontext_token(ctx);  // 获取当前查看的token
       if (lookahead != YYSYMBOL_YYEMPTY)  // 如果不是空token
           buf << " before " << yysymbol_name(lookahead);  // 添加到错误信息中
   }
   
   std::string error_note;  // 存储错误部分的文本
   std::string msg;  // 存储完整的错误消息
   
   // 调用辅助函数定位错误在源代码中的位置，并生成可视化的错误显示
   locateErrorPosition(code_str, yypcontext_location(ctx), error_note, msg, have_expected);
   
   // 如果有期望的token，添加"但发现了xxx"的信息
   if (have_expected)
       buf << " but found \"" << error_note << "\"";
   
   // 输出错误信息
   std::cerr << buf.str() << std::endl;  // 输出基本错误信息
   std::cerr << msg << std::endl;  // 输出错误位置的可视化显示
   
   return res;  // 返回结果
}

// 主解析函数 - 语法分析的入口点，初始化词法分析器并执行解析
int code_parse(const char * code_str, ProgramStmt ** program) {
   yyscan_t scanner;  // 词法分析器状态
   yylex_init(&scanner);  // 初始化词法分析器
   scan_string(code_str, scanner);  // 加载源代码字符串到词法分析器

   int ret = yyparse(code_str, program, scanner);  // 执行语法分析，构建AST
   
   yylex_destroy(scanner);  // 销毁词法分析器，释放资源
   if (syntaxErrorFlag) {  // 检查是否发生了语法错误
       return -1;  // 如果有错误，返回-1
   }
   return ret;  // 否则返回yyparse的结果（通常成功为0）
}