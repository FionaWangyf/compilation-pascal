%{
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
    bool syntaxErrorFlag = false; 
    
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

    // 获取运算符类型的函数 - 重命名函数
    template<typename EnumType, typename MapType>
    EnumType getOperatorKind(long long op, const MapType& typeMap) {
        auto it = typeMap.find(op);
        if (it != typeMap.end()) {
            return it->second;
        }
        return static_cast<EnumType>(0);
    }

    // 特化版本 - 重命名函数
    RelExprStmt::RelExprType getRelationOperator(long long op) {
        return getOperatorKind<RelExprStmt::RelExprType>(op, relOperatorMap);
    }

    AddExprStmt::AddExprType getArithmeticOperator(long long op) {
        return getOperatorKind<AddExprStmt::AddExprType>(op, addOperatorMap);
    }

    MulExprStmt::MulExprType getTermOperator(long long op) {
        return getOperatorKind<MulExprStmt::MulExprType>(op, mulOperatorMap);
    }

    // 填充数值节点函数 - 重命名函数
    template<typename T>
    void setupNumberNode(std::unique_ptr<NumberStmt>& numNode, T val) {
        numNode->is_signed = true;
        numNode->is_real = std::is_same<T, double>::value;
        numNode->is_char = std::is_same<T, char>::value;
        numNode->is_unsigned = false;

        if constexpr (std::is_same<T, long long>::value) {
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

    // 节点创建函数 - 重命名函数
    template<typename T, typename... Args>
    T* allocateNode(Args&&... args) {
        return new T(std::forward<Args>(args)...);
    }

    // 将列表项目转移到容器函数 - 重命名函数
    template<typename Owner, typename T>
    void moveListToContainer(Owner* owner, std::vector<T*>* list, std::vector<std::unique_ptr<T>> Owner::* container) {
        if (list) {
            for (auto item : *list) {
                (owner->*container).emplace_back(std::unique_ptr<T>(item));
            }
            delete list;
        }
    }

    // 值节点工厂类 - 重命名类和方法
    class ValueFactory {
    public:
        static ValueStmt* makeInteger(long long val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            setupNumberNode(value->number, val);
            return value;
        }

        static ValueStmt* makeReal(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            double val = atof(str);
            setupNumberNode(value->number, val);
            value->number->literal = std::string(str);
            return value;
        }

        static ValueStmt* makeChar(char val) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Number;
            value->number = std::make_unique<NumberStmt>();
            setupNumberNode(value->number, val);
            return value;
        }

        static ValueStmt* makeString(const char* str) {
            ValueStmt* value = new ValueStmt();
            value->type = ValueStmt::ValueType::Str;
            value->str = std::make_unique<StrStmt>();
            value->str->val = std::string(str).substr(1, std::string(str).length() - 2);
            return value;
        }
    };

    // 表达式节点工厂类 - 重命名类和方法
    class ExprFactory {
    public:
        static ExprStmt* createFromSimpleExpr(AddExprStmt* simpleExpr) {
            ExprStmt* expr = new ExprStmt();
            expr->rel_expr = std::make_unique<RelExprStmt>();
            RelExprStmt::Term term;
            term.type = RelExprStmt::RelExprType::NULL_TYPE;
            term.add_expr = std::unique_ptr<AddExprStmt>(simpleExpr);
            expr->rel_expr->terms.emplace_back(std::move(term));
            return expr;
        }

        static AddExprStmt* createFromTerm(MulExprStmt* term) {
            AddExprStmt* addExpr = new AddExprStmt();
            AddExprStmt::Term termStruct;
            termStruct.type = AddExprStmt::AddExprType::NULL_TYPE;
            termStruct.mul_expr = std::unique_ptr<MulExprStmt>(term);
            addExpr->terms.emplace_back(std::move(termStruct));
            return addExpr;
        }

        static MulExprStmt* createFromFactor(UnaryExprStmt* factor) {
            MulExprStmt* mulExpr = new MulExprStmt();
            MulExprStmt::Term term;
            term.type = MulExprStmt::MulExprType::NULL_TYPE;
            term.unary_expr = std::unique_ptr<UnaryExprStmt>(factor);
            mulExpr->terms.emplace_back(std::move(term));
            return mulExpr;
        }
    };

    // 创建一元表达式的函数 - 重命名函数
    UnaryExprStmt* makeUnaryExpr(PrimaryExprStmt::PrimaryExprType type) {
        UnaryExprStmt* unary = new UnaryExprStmt();
        unary->primary_expr = std::make_unique<PrimaryExprStmt>();
        unary->primary_expr->type = type;
        return unary;
    }

    // 报告语法错误 - 重命名函数
    void reportSyntaxError(YYLTYPE *llocp, const char *msg) {
        LOG_ERROR("[Syntax Error] at line %d, column %d: %s", llocp->first_line, llocp->first_column, msg);
        syntaxErrorFlag = true;
        exit(1);
    }

    // 获取错误位置函数 - 重命名函数
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

// 枚举类，定义编译器正在处理的语法规则类型
enum class ParserContext {
    ProgramStruct,     
    ProgramHead,      
    ProgramBody,       
    IdList,            
    ConstDeclarations, 
    ConstDeclaration,  
    ConstValue,        
    VarDeclarations,   
    VarDeclaration,    
    Type,              
    BasicType,         
    PeriodList,        
    SubprogramDeclarations, 
    Subprogram,        
    SubprogramHead,    
    FormalParameter,   
    ParameterList,     
    Parameter,         
    VarParameter,      
    ValueParameter,    
    SubprogramBody,    
    CompoundStatement, 
    StatementList,     
    Statement,         
    ProcedureCall,     
    VariableList,      
    Variable,          
    IdVarpart,         
    ExpressionList,    
    ArrayIndexExpression, 
    BracketExpressionList, 
    Expression,        
    SimpleExpression,  
    Term,              
    Factor,            
};

// 跟踪编译器当前正在处理的语法规则 - 重命名变量
static ParserContext currentParserContext = ParserContext::ProgramStruct;

void resetSyntaxError() {
    syntaxErrorFlag = false; // 重置错误标志，为下一次解析准备
}

int yyerror(YYLTYPE *llocp, const char *code_str, ProgramStmt ** program, yyscan_t scanner, const char *msg);

%}


// 定义Token
%token 
    CONST
    PROGRAM
    TYPE
    RECORD
    ARRAY
    OF
    VAR
    FUNCTION
    PROCEDURE
    BEGIN_TOKEN
    END
    IF
    THEN
    ELSE
    CASE
    WHILE
    REPEAT
    UNTIL
    FOR
    TO
    DOWNTO
    DO
    READ
    READLN
    WRITE
    WRITELN
    CHAR_KW
    INTEGER_KW
    REAL_KW
    BOOLEAN_KW
    NOT
    DIV
    MOD
    AND
    OR
    NE
    LE
    GE
    ASSIGNOP
    IN
    ORELSE
    ANDTHEN
    DOUBLE_DOT
    BRACE_PAIR
    BREAK
    CONTINUE

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
    *program = nullptr;
};


/** union 中定义各种数据类型，真实生成的代码也是union类型，所以不能有非POD类型的数据 **/
%union {
    ProgramStmt *                                   program_struct;
    ProgramHeadStmt *                               program_head;
    ProgramBodyStmt *                               program_body;
    std::vector<std::string> *                      id_list;
    ConstDeclStmt *                                 const_decls;
    std::pair<std::string, ValueStmt *> *            kv_pair;
    std::vector<std::pair<std::string, ValueStmt *>*> * kv_pair_list;
    ValueStmt *                                    value;
    std::vector<VarDeclStmt *> *                    var_decls;
    VarDeclStmt *                                   var_decl;
    DataType                                        var_type;
    BasicType                                       basic_type;
    std::vector<PeriodStmt *> *                     period_list;
    PeriodStmt *                                    period;
    std::vector<FuncDeclStmt *> *                   func_decl_list;
    FuncDeclStmt *                                  func_decl;
    FuncHeadDeclStmt *                              func_head;
    FuncBodyDeclStmt *                              func_body;
    std::vector<BaseStmt *> *                       stmt_list;
    AssignStmt *                                    assign_stmt;
    IfStmt *                                        if_stmt;
    ForStmt *                                       for_stmt;
    ReadFuncStmt *                                      read_stmt;
    WriteFuncStmt *                                     write_stmt;
    FuncCallStmt *                                  func_call_stmt;
    std::vector<LValStmt *> *                       lval_list;
    LValStmt *                                      lval;
    BaseStmt *                                      stmt;

    std::vector<ExprStmt *> *                       expr_list;
    ExprStmt *                                      expr;
    RelExprStmt *                                   rel_expr;
    AddExprStmt *                                   add_expr;
    MulExprStmt *                                   mul_expr;
    UnaryExprStmt *                                 unary_expr;
    PrimaryExprStmt *                               primary_expr;

    BreakStmt *                                     break_stmt;
    ContinueStmt *                                  continue_stmt;

    char *                                          string;
    long long                                       number;
    bool                                            boolean;
    char *                                          real;
    char                                           charactor;
    int                                            token;
}


%token <string> IDENTIFIER
%token <number> INTEGER
%token <boolean> BOOLEAN
%token <real> REAL
%token <charactor> CHAR
%token <string> STRING



// 下面定义非终结符
%type <number> relop
%type <number> addop
%type <number> mulop
%type <program_struct>      programstruct
%type <program_head>        program_head
%type <program_body>        program_body
%type <id_list>                   idlist
%type <const_decls>         const_declarations
%type <kv_pair_list>             const_declaration
%type <value>               const_value
%type <var_decls>           var_declarations
%type <var_decls>           var_declaration
%type <var_decl>            type
%type <basic_type>          basic_type
%type <period_list>         period_list
%type <func_decl_list>      subprogram_declarations
%type <func_decl>           subprogram
%type <func_head>           subprogram_head
%type <var_decls>           parameter_list
%type <var_decls>           formal_parameter
%type <var_decl>            parameter
%type <var_decl>            var_parameter
%type <var_decl>            value_parameter
%type <func_body>           subprogram_body
%type <stmt_list>           compound_statement
%type <stmt_list>           statement_list
%type <stmt_list>           statement
%type <func_call_stmt>      procedure_call
%type <lval_list>           variable_list
%type <lval>                variable
%type <expr_list>           id_varpart
%type <expr_list>           expression_list
%type <expr_list>           array_index_expression
%type <expr>                expression   
%type <add_expr>            simple_expression
%type <mul_expr>            term
%type <unary_expr>          factor

// 对丢弃的符号进行析构
%destructor {} <program_struct> <boolean> <number> <charactor> <basic_type>
%destructor { free($$); } IDENTIFIER <string> <real>
%destructor {
    if($$ != nullptr){
        for(auto kv_pair : *$$){
            delete kv_pair;
        }
    }
    delete $$;
} <var_decls> <period_list> <func_decl_list> <stmt_list> <lval_list> <expr_list>
%destructor {
    if($$ != nullptr){
        for(auto pair : *$$){
            delete pair->second;
            delete pair;
        }
        delete $$;
    }
} <kv_pair_list>
%destructor { delete $$; } <*>
// THEN 和 ELSE 为右结合
%nonassoc THEN
%nonassoc ELSE

%%
// 语法规则定义部分 - 改变缩进和排版风格

// 1.1 程序结构
programstruct 
    : program_head  ';'  program_body '.'  {
        currentParserContext = ParserContext::ProgramStruct;
        ProgramStmt* programStruct = allocateNode<ProgramStmt>();
        programStruct->head = std::unique_ptr<ProgramHeadStmt>($1);
        programStruct->body = std::unique_ptr<ProgramBodyStmt>($3);
        
        GRAMMAR_TRACE("programstruct -> program_head ';' program_body '.'");
        *program = programStruct;
        $$ = nullptr; // 防止报错
    }
    | error  ';'  program_body '.'  {
        *program = allocateNode<ProgramStmt>();
        delete $3;
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' program_body '.'");
    }
    | program_head  ';'  error  {
        *program = allocateNode<ProgramStmt>();
        delete $1;
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> program_head ';' error");
    }
    | error  ';'  error  {
        *program = allocateNode<ProgramStmt>();
        $$ = nullptr;
        GRAMMAR_ERROR("programstruct -> error ';' error");
    }
    ;

// 1.2 程序头
program_head 
    : PROGRAM IDENTIFIER '(' idlist ')'  {
        currentParserContext = ParserContext::ProgramHead;
        $$ = allocateNode<ProgramHeadStmt>();
        $$->id_list = *$4;
        
        delete $4;
        free($2);
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER '(' idlist ')'");
    }
    | PROGRAM IDENTIFIER  {
        currentParserContext = ParserContext::ProgramHead;
        $$ = allocateNode<ProgramHeadStmt>();
        $$->id_list.emplace_back(std::string($2));
        
        GRAMMAR_TRACE("program_head -> PROGRAM IDENTIFIER");
        free($2);
    }
    | PROGRAM error  {
        $$ = nullptr;
        GRAMMAR_ERROR("program_head -> PROGRAM error");
        yyerrok;
    }
    ;

// 1.3 程序体
program_body 
    : const_declarations var_declarations subprogram_declarations compound_statement  {
        currentParserContext = ParserContext::ProgramBody;
        ProgramBodyStmt* programBody = allocateNode<ProgramBodyStmt>();
        
        // 处理常量声明
        if($1 != nullptr) {
            programBody->const_decl = std::unique_ptr<ConstDeclStmt>($1);
        }
        
        // 处理变量声明
        if($2 != nullptr) {
            for(auto varDecl : *$2) {
                programBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));
            }
            delete $2;
        }
        
        // 处理子程序声明
        if($3 != nullptr) {
            for(auto funcDecl : *$3) {
                programBody->func_decl.emplace_back(std::unique_ptr<FuncDeclStmt>(funcDecl));
            }
            delete $3;
        }
        
        // 处理复合语句
        if($4 != nullptr) {
            for(auto stmt : *$4) {
                programBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete $4;
        }
        
        $$ = programBody;
        GRAMMAR_TRACE("program_body -> const_declarations var_declarations subprogram_declarations compound_statement");
    }
    | error_recovery const_declarations var_declarations subprogram_declarations compound_statement  {
        // 清理分配的资源
        auto cleanupPtr = [](auto* ptr) {
            if (ptr) delete ptr;
        };
        
        cleanupPtr($2);
        
        if($3 != nullptr) {
            for(auto varDecl : *$3) {
                delete varDecl;
            }
            delete $3;
        }
        
        if($4 != nullptr) {
            for(auto funcDecl : *$4) {
                delete funcDecl;
            }
            delete $4;
        }
        
        if($5 != nullptr) {
            for(auto stmt : *$5) {
                delete stmt;
            }
            delete $5;
        }
        
        $$ = nullptr;
        GRAMMAR_TRACE("program_body -> error_recovery const_declarations var_declarations subprogram_declarations compound_statement");
    }
    ;

// 1.4 标识符列表
idlist 
    : IDENTIFIER  {
        currentParserContext = ParserContext::IdList;
        $$ = allocateNode<std::vector<std::string>>();
        $$->emplace_back(std::string($1));
        
        GRAMMAR_TRACE("idlist -> IDENTIFIER");
        free($1);
    }
    | idlist ',' IDENTIFIER  {
        currentParserContext = ParserContext::IdList;
        $1->emplace_back(std::string($3));
        $$ = $1;
        
        GRAMMAR_TRACE("idlist -> idlist ',' IDENTIFIER");
        free($3);
    }
    ;

// 1.5 常量声明
const_declarations 
    : /*empty*/  {
        currentParserContext = ParserContext::ConstDeclarations;
        $$ = nullptr;
        GRAMMAR_TRACE("const_declarations -> empty");
    }
    | CONST const_declaration ';'  {
        currentParserContext = ParserContext::ConstDeclarations;
        ConstDeclStmt* constDecls = allocateNode<ConstDeclStmt>();
        
        // 将声明列表中的键值对转移到常量声明对象中
        for(auto kvPair : *$2) {
            constDecls->pairs.emplace_back(std::make_pair(kvPair->first, kvPair->second));
            delete kvPair;
        }
        
        delete $2;
        $$ = constDecls;
        
        // 日志输出声明的常量信息
        for(auto &t: constDecls->pairs) {
            LOG_INFO("Get Const Type:%d, pointer %p", t.second->type, t.second.get());
            if(t.second->str) {
                LOG_INFO("Get string:%s", t.second->str->val.c_str());
            }
        }
        
        GRAMMAR_TRACE("const_declarations -> CONST const_declaration ';' const_declarations");
    }
    | CONST error ';'  {
        $$ = nullptr;
        GRAMMAR_ERROR("const_declarations -> CONST error ;");
        yyerrok;
    }
    ;

// 1.6 常量声明列表
const_declaration 
    : IDENTIFIER '=' const_value  {
        currentParserContext = ParserContext::ConstDeclaration;
        auto constDecls = allocateNode<std::vector<std::pair<std::string, ValueStmt *>*>>();
        auto kvPair = allocateNode<std::pair<std::string, ValueStmt *>>($1, $3);
        
        constDecls->emplace_back(kvPair);
        free($1);
        $$ = constDecls;
    }
    | const_declaration ';' IDENTIFIER '=' const_value  {
        currentParserContext = ParserContext::ConstDeclaration;
        $1->emplace_back(allocateNode<std::pair<std::string, ValueStmt *>>($3, $5));
        
        free($3);
        $$ = $1;
    }
    | error ';' IDENTIFIER '=' const_value  {
        free($3);
        delete $5;
        $$ = nullptr;
        
        GRAMMAR_ERROR("const_declaration -> error ';' IDENTIFIER = const_value");
        yyerrok;
    }
    ;

// 1.7 常量值
const_value
    : INTEGER  {
        $$ = ValueFactory::makeInteger($1);
    }
    | '+' INTEGER  {
        $$ = ValueFactory::makeInteger($2);
    }
    | '-' INTEGER  {
        $$ = ValueFactory::makeInteger(-$2);
    }
    | REAL  {
        $$ = ValueFactory::makeReal($1);
        free($1);
    }
    | '+' REAL  {
        $$ = ValueFactory::makeReal($2);
        free($2);
    }
    | '-' REAL  {
        ValueStmt* value = ValueFactory::makeReal($2);
        // 处理负号：将实数值设为负数
        value->number->real_val *= -1;
        
        free($2);
        $$ = value;
    }
    | CHAR  {
        $$ = ValueFactory::makeChar($1);
        GRAMMAR_TRACE("const_value -> CHAR, value: %c");
    }
    | STRING  {
        $$ = ValueFactory::makeString($1);
        free($1);
    }
    ;

// 2.1 变量声明
var_declarations 
    : /*empty*/  {
        $$ = nullptr;
        GRAMMAR_TRACE("var_declarations -> empty");
    }
    | VAR var_declaration ';'  {
        currentParserContext = ParserContext::VarDeclarations;
        $$ = $2;
        GRAMMAR_TRACE("var_declarations -> VAR var_declaration ';'");
    }
    | VAR error ';'  {
        $$ = nullptr;
        GRAMMAR_ERROR("var_declarations -> VAR error ;");
        yyerrok;
    }
    ;

// 2.2 变量声明列表
var_declaration 
    : idlist ':' type  {
        currentParserContext = ParserContext::VarDeclaration;
        auto varDecls = allocateNode<std::vector<VarDeclStmt *>>();
        auto varDecl = allocateNode<VarDeclStmt>();
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), $1->begin(), $1->end());
        
        // 处理类型信息
        varDecl->basic_type = $3->basic_type;
        varDecl->data_type = $3->data_type;
        varDecl->array_range = std::move($3->array_range);
        
        delete $1;
        delete $3;
        varDecls->emplace_back(varDecl);
        $$ = varDecls;
        
        GRAMMAR_TRACE("var_declaration -> idlist ':' type");
    }
    | var_declaration ';' idlist ':' type  {
        currentParserContext = ParserContext::VarDeclaration;
        auto varDecl = allocateNode<VarDeclStmt>();
        
        // 将标识符列表中的所有标识符复制到变量声明中
        varDecl->id.insert(varDecl->id.end(), $3->begin(), $3->end());
        
        // 处理类型信息
        varDecl->basic_type = $5->basic_type;
        varDecl->data_type = $5->data_type;
        varDecl->array_range = std::move($5->array_range);
        
        delete $3;
        delete $5;
        $1->emplace_back(varDecl);
        $$ = $1;
        
        GRAMMAR_TRACE("var_declaration -> var_declaration ';' idlist ':' type");
    }
    | error ';' idlist ':' type  {
        delete $3;
        delete $5;
        $$ = nullptr;
        
        GRAMMAR_ERROR("var_declaration -> error ';' idlist ':' type");
        yyerrok;
    }
    ;

// 2.3 类型定义
type 
    : basic_type  {
        currentParserContext = ParserContext::Type;
        auto typeStmt = allocateNode<VarDeclStmt>();
        typeStmt->data_type = DataType::BasicType;
        typeStmt->basic_type = $1;
        
        $$ = typeStmt;
        GRAMMAR_TRACE("type -> basic_type");
    }
    | ARRAY '[' period_list ']' OF basic_type  {
        currentParserContext = ParserContext::Type;
        auto typeStmt = allocateNode<VarDeclStmt>();
        typeStmt->data_type = DataType::ArrayType;
        typeStmt->basic_type = $6;
        
        // 转移数组范围信息
        for(auto period : *$3) {
            typeStmt->array_range.emplace_back(std::unique_ptr<PeriodStmt>(period));
        }
        
        delete $3;
        $$ = typeStmt;
        GRAMMAR_TRACE("type -> ARRAY '[' period_list ']' OF basic_type");
    }
    ;

// 2.4 基本类型
basic_type
    : INTEGER_KW  {
        $$ = BasicType::INT;
        GRAMMAR_TRACE("basic_type -> INTEGER_KW");
    }
    | REAL_KW  {
        $$ = BasicType::REAL;
        GRAMMAR_TRACE("basic_type -> REAL_KW");
    }
    | BOOLEAN_KW  {
        $$ = BasicType::BOOLEAN;
        GRAMMAR_TRACE("basic_type -> BOOLEAN_KW");
    }
    | CHAR_KW  {
        $$ = BasicType::CHAR;
        GRAMMAR_TRACE("basic_type -> CHAR_KW");
    }
    ;

// 2.5 周期列表（数组索引范围）
period_list
    : INTEGER DOUBLE_DOT INTEGER  {
        auto periodList = allocateNode<std::vector<PeriodStmt *>>();
        auto period = allocateNode<PeriodStmt>();
        
        period->begin = $1;
        period->end = $3;
        periodList->emplace_back(period);
        
        $$ = periodList;
        GRAMMAR_TRACE("period_list -> INTEGER '..' INTEGER");
    }
    | period_list ',' INTEGER DOUBLE_DOT INTEGER  {
        auto period = allocateNode<PeriodStmt>();
        period->begin = $3;
        period->end = $5;
        
        $1->emplace_back(period);
        $$ = $1;
        
        GRAMMAR_TRACE("period_list -> period_list ',' INTEGER '..' INTEGER");
    }
    ;

// 2.6 子程序声明列表
subprogram_declarations 
    : /*empty*/  {
        $$ = nullptr;
        GRAMMAR_TRACE("subprogram_declarations -> empty");
    }
    | subprogram_declarations subprogram ';'  {
        currentParserContext = ParserContext::SubprogramDeclarations;
        
        if ($1 == nullptr) {
            auto funcDeclList = allocateNode<std::vector<FuncDeclStmt *>>();
            funcDeclList->emplace_back($2);    
            $$ = funcDeclList;
        } else {
            $1->emplace_back($2);
            $$ = $1;
        }
        
        GRAMMAR_TRACE("subprogram_declarations -> subprogram_declarations subprogram ';'");
    }
    ;

// 2.7 子程序
subprogram 
    : subprogram_head ';' subprogram_body  {
        currentParserContext = ParserContext::Subprogram;
        auto subprogram = allocateNode<FuncDeclStmt>();
        
        subprogram->header = std::unique_ptr<FuncHeadDeclStmt>($1);
        subprogram->body = std::unique_ptr<FuncBodyDeclStmt>($3);
        
        $$ = subprogram;
        GRAMMAR_TRACE("subprogram -> subprogram_head ';' subprogram_body");
    }
    ;

// 2.8 子程序头
subprogram_head
    : PROCEDURE IDENTIFIER formal_parameter  {
        currentParserContext = ParserContext::SubprogramHead;
        auto subHead = allocateNode<FuncHeadDeclStmt>();
        
        subHead->func_name = std::string($2);
        subHead->ret_type = BasicType::VOID;
        
        // 处理形式参数
        if ($3 != nullptr) {
            for (auto formalParameter : *$3) {
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));
            }
            delete $3;
        }
        
        $$ = subHead;
        free($2);
        GRAMMAR_TRACE("subprogram_head -> PROGRAM IDENTIFIER formal_parameter");
    }
    | FUNCTION IDENTIFIER formal_parameter ':' basic_type  {
        currentParserContext = ParserContext::SubprogramHead;
        auto subHead = allocateNode<FuncHeadDeclStmt>();
        
        subHead->func_name = std::string($2);
        subHead->ret_type = $5;
        
        // 处理形式参数
        if ($3 != nullptr) {
            for (auto formalParameter : *$3) {
                subHead->args.emplace_back(std::unique_ptr<VarDeclStmt>(formalParameter));
            }
            delete $3;
        }
        
        $$ = subHead;
        free($2);
        GRAMMAR_TRACE("subprogram_head -> FUNCTION IDENTIFIER formal_parameter ':' basic_type");
    }
    | FUNCTION error  {
        $$ = nullptr;
        GRAMMAR_ERROR("subprogram_head -> FUNCTION error");
        yyerrok;
    }
    | PROCEDURE error  {
        $$ = nullptr;
        GRAMMAR_ERROR("subprogram_head -> PROCEDURE error");
        yyerrok;
    }
    ;

// 2.9 形式参数
formal_parameter 
    : /* empty */  {
        $$ = nullptr;
        GRAMMAR_TRACE("formal_parameter -> empty");
    }
    | '(' parameter_list ')'  {
        currentParserContext = ParserContext::FormalParameter;
        $$ = $2;
        GRAMMAR_TRACE("formal_parameter -> '(' parameter_list ')'");
    }
    ;

// 2.10 参数列表
parameter_list
    : /* empty */  {
        $$ = nullptr;
        GRAMMAR_TRACE("parameter_list -> empty");
    }
    | parameter  {
        auto paramList = allocateNode<std::vector<VarDeclStmt *>>();
        paramList->emplace_back($1);
        
        $$ = paramList;
        GRAMMAR_TRACE("parameter_list -> parameter");
    }
    | parameter_list ';' parameter  {
        $1->emplace_back($3);
        $$ = $1;
        
        GRAMMAR_TRACE("parameter_list -> parameter_list ';' parameter");
    }
    ;

// 3.1 参数
parameter
    : var_parameter  {
        $$ = $1;
        $$->is_var = true;
        
        GRAMMAR_TRACE("parameter -> var_parameter");
    }
    | value_parameter  {
        $$ = $1;
        $$->is_var = false;
        
        GRAMMAR_TRACE("parameter -> value_parameter");
    }
    ;

// 3.2 变量参数
var_parameter
    : VAR value_parameter  {
        $$ = $2;
        GRAMMAR_TRACE("var_parameter -> VAR value_parameter");
    }
    ;

// 3.3 值参数
value_parameter
    : idlist ':' basic_type  {
        auto varDecl = allocateNode<VarDeclStmt>();
        
        varDecl->id.insert(varDecl->id.end(), $1->begin(), $1->end());
        varDecl->data_type = DataType::BasicType;
        varDecl->basic_type = $3;
        varDecl->is_var = false;
        
        delete $1;
        $$ = varDecl;
        
        GRAMMAR_TRACE("value_parameter -> idlist ':' basic_type");
    }
    ;

// 3.4 子程序体
subprogram_body 
    : const_declarations var_declarations compound_statement  {
        currentParserContext = ParserContext::SubprogramBody;
        auto funcBody = allocateNode<FuncBodyDeclStmt>();
        
        // 处理常量声明
        if ($1 != nullptr) {
            funcBody->const_decl = std::unique_ptr<ConstDeclStmt>($1);
        }
        
        // 处理变量声明
        if ($2 != nullptr) {
            for (auto varDecl : *$2) {
                funcBody->var_decl.emplace_back(std::unique_ptr<VarDeclStmt>(varDecl));
            }
            delete $2;
        }
        
        // 处理复合语句
        if ($3 != nullptr) {
            for (auto stmt : *$3) {
                funcBody->comp_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete $3;
        }
        
        $$ = funcBody;
        GRAMMAR_TRACE("subprogram_body -> const_declarations var_declarations compound_statement");
    }
    ;

// 3.5 复合语句
compound_statement 
    : BEGIN_TOKEN statement_list END  {
        currentParserContext = ParserContext::CompoundStatement;
        $$ = $2;
        
        GRAMMAR_TRACE("compound_statement -> BEGIN_TOKEN statement_list END");
    }
    ;

// 3.6 语句列表
statement_list 
    : statement  {
        $$ = $1;
        GRAMMAR_TRACE("statement_list -> statement");
    }
    | statement_list ';' statement  {
        currentParserContext = ParserContext::StatementList;
        
        // 如果有语句要添加，则处理
        if ($3 != nullptr) {
            // 将第三个参数中的语句添加到结果列表中
            if ($1 != nullptr) {
                for (auto stmt : *$3) {
                    $1->emplace_back(stmt);
                }
            } else {
                // 如果结果列表为空，则直接使用第三个参数
                $1 = $3; 
                $3 = nullptr; // 避免被删除
            }
        }
        
        $$ = $1;
        delete $3; // 如果$3已经被处理过，这里实际上删除的是nullptr
        
        GRAMMAR_TRACE("statement_list -> statement_list ';' statement");
    }
    | error ';' statement  {
        if ($3 != nullptr) {
            for (auto item : *$3) {
                delete item;
            }
            delete $3;
        }
        
        $$ = nullptr;
        GRAMMAR_ERROR("statement_list -> error ';' statement");
        yyerrok;
    }
    | statement_list ';' error  {
        if ($1 != nullptr) {
            for (auto item : *$1) {
                delete item;
            }
            delete $1;
        }
        
        $$ = nullptr;
        GRAMMAR_ERROR("statement_list -> statement_list ';' error");
        yyerrok;
    }
    ;

// 4.1 语句
statement 
    : /*empty*/  {
        $$ = nullptr;
        GRAMMAR_TRACE("statement -> empty");
    }
    | variable ASSIGNOP expression  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto assignStmt = allocateNode<AssignStmt>();
        
        assignStmt->lval = std::unique_ptr<LValStmt>($1);
        assignStmt->expr = std::unique_ptr<ExprStmt>($3);
        
        stmtList->emplace_back(assignStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> variable ASSIGNOP expression");
    }
    | procedure_call  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        stmtList->emplace_back($1);
        
        $$ = stmtList;
        GRAMMAR_TRACE("statement -> procedure_call");
    }
    | compound_statement  {
        $$ = $1;
        GRAMMAR_TRACE("statement -> compound_statement");
    }
    | WHILE expression DO statement  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto whileStmt = allocateNode<WhileStmt>();
        
        whileStmt->expr = std::unique_ptr<ExprStmt>($2);
        
        // 处理循环体语句
        if ($4 != nullptr) {
            for (auto stmt : *$4) {
                whileStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
            delete $4;
        }
        
        stmtList->emplace_back(whileStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> WHILE expression DO statement");
    }
    | IF expression THEN statement %prec THEN  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto ifStmt = allocateNode<IfStmt>();
        
        ifStmt->expr = std::unique_ptr<ExprStmt>($2);
        
        // 处理if条件为真时的语句
        if ($4 != nullptr) {
            for (auto stmt : *$4) {
                ifStmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        delete $4;
        stmtList->emplace_back(ifStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> IF expression THEN statement");
    }
    | IF expression THEN statement ELSE statement  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto ifStmt = allocateNode<IfStmt>();
        
        ifStmt->expr = std::unique_ptr<ExprStmt>($2);
        
        // 处理if条件为真时的语句
        if ($4 != nullptr) {
            for (auto stmt : *$4) {
                ifStmt->true_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        // 处理if条件为假时的语句
        if ($6 != nullptr) {
            for (auto stmt : *$6) {
                ifStmt->false_stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        stmtList->emplace_back(ifStmt);
        delete $4;
        delete $6;
        
        $$ = stmtList;
        GRAMMAR_TRACE("statement -> IF expression THEN statement ELSE statement");
    }
    | FOR IDENTIFIER ASSIGNOP expression TO expression DO statement  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto forStmt = allocateNode<ForStmt>();
        
        forStmt->id = std::string($2);
        forStmt->begin = std::unique_ptr<ExprStmt>($4);
        forStmt->end = std::unique_ptr<ExprStmt>($6);
        
        // 处理循环体语句
        if ($8 != nullptr) {
            for (auto stmt : *$8) {
                forStmt->stmt.emplace_back(std::unique_ptr<BaseStmt>(stmt));
            }
        }
        
        stmtList->emplace_back(forStmt);
        free($2);
        delete $8;
        
        $$ = stmtList;
        GRAMMAR_TRACE("statement -> FOR IDENTIFIER ASSIGNOP expression TO expression DO statement");
    }
    | READ '(' variable_list ')'  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto readStmt = allocateNode<ReadFuncStmt>();
        
        // 处理变量列表
        for (auto lval : *$3) {
            readStmt->lval.emplace_back(std::unique_ptr<LValStmt>(lval));
        }
        
        delete $3;
        stmtList->emplace_back(readStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> READ '(' variable_list ')'");
    }
    | WRITE '(' expression_list ')'  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto writeStmt = allocateNode<WriteFuncStmt>();
        
        // 处理表达式列表
        if ($3 != nullptr) {
            for (auto expr : *$3) {
                writeStmt->expr.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
        }
        
        stmtList->emplace_back(writeStmt);
        delete $3;
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> WRITE '(' expression_list ')'");
    }
    | BREAK  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto breakStmt = allocateNode<BreakStmt>();
        
        stmtList->emplace_back(breakStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> BREAK");
    }
    | CONTINUE  {
        auto stmtList = allocateNode<std::vector<BaseStmt *>>();
        auto continueStmt = allocateNode<ContinueStmt>();
        
        stmtList->emplace_back(continueStmt);
        $$ = stmtList;
        
        GRAMMAR_TRACE("statement -> CONTINUE");
    }
    ;

// 4.2 变量列表
variable_list 
    : variable  {
        currentParserContext = ParserContext::VariableList;
        auto lvalList = allocateNode<std::vector<LValStmt *>>();
        
        lvalList->emplace_back($1);
        $$ = lvalList;
        
        GRAMMAR_TRACE("variable_list -> variable");
    }
    | variable_list ',' variable  {
        currentParserContext = ParserContext::VariableList;
        
        if ($3 != nullptr) {
            $1->emplace_back($3);
        } else {
            auto lvalList = allocateNode<std::vector<LValStmt *>>();
            lvalList->emplace_back($3);
            $1 = lvalList;
        }
        
        $$ = $1;
        GRAMMAR_TRACE("variable_list -> variable_list ',' variable");
    }
    | error ',' variable  {
        delete $3;
        $$ = nullptr;
        
        GRAMMAR_ERROR("variable_list -> error ',' variable");
    }
    ;

// 4.3 变量
variable 
    : IDENTIFIER id_varpart  {
        currentParserContext = ParserContext::Variable;
        auto lval = allocateNode<LValStmt>();
        
        lval->id = std::string($1);
        
        // 处理变量的数组下标部分
        if ($2 != nullptr) {
            for (auto expr : *$2) {
                lval->array_index.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete $2;
        }
        
        free($1);
        $$ = lval;
        
        GRAMMAR_TRACE("variable -> IDENTIFIER id_varpart");
    }
    ;

// 4.4 变量附加部分（数组下标）
id_varpart 
    : /*empty*/  {
        $$ = nullptr;
        GRAMMAR_TRACE("id_varpart -> empty");
    }
    | '[' expression_list ']'  {
        currentParserContext = ParserContext::IdVarpart;
        
        if ($2 != nullptr) {
            $$ = $2;    
        } else {
            yyerror(&@2, "code_str", program, scanner, "数组下标定义出错 请检查是否符合规范");
        }
        
        GRAMMAR_TRACE("id_varpart -> '[' expression_list ']'");
    }
    | '[' expression BRACE_PAIR array_index_expression  {
        currentParserContext = ParserContext::IdVarpart;
        
        if ($4 != nullptr) {
            $$ = $4;
            $$->emplace_back($2);
            std::reverse($$->begin(), $$->end());
        } else {
            // 错误处理
            $$ = nullptr;
        }
        
        GRAMMAR_TRACE("id_varpart -> '[' expression BRACE_PAIR array_index_expression");
    }
    ;

// 4.5 数组索引表达式
array_index_expression
    : expression ']'  {
        currentParserContext = ParserContext::ArrayIndexExpression;
        auto exprList = allocateNode<std::vector<ExprStmt *>>();
        
        exprList->emplace_back($1);
        $$ = exprList;
        
        GRAMMAR_TRACE("array_index_expression -> expression_list");
    }
    | expression BRACE_PAIR array_index_expression  {
        currentParserContext = ParserContext::ArrayIndexExpression;
        
        $3->emplace_back($1);
        $$ = $3;
        
        GRAMMAR_TRACE("array_index_expression -> array_index_expression BRACE_PAIR expression ']'");
    }
    ;

// 5.1 过程调用
procedure_call 
    : IDENTIFIER  {
        currentParserContext = ParserContext::ProcedureCall;
        auto procCall = allocateNode<FuncCallStmt>();
        
        procCall->id = std::string($1);
        free($1);
        $$ = procCall;
        
        GRAMMAR_TRACE("procedure_call -> IDENTIFIER");
    }
    | IDENTIFIER '(' expression_list ')'  {
        currentParserContext = ParserContext::ProcedureCall;
        auto procCall = allocateNode<FuncCallStmt>();
        
        procCall->id = std::string($1);
        
        // 处理参数表达式列表
        if ($3 != nullptr) {
            for (auto expr : *$3) {
                procCall->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete $3;
        }
        
        free($1);
        $$ = procCall;
        
        GRAMMAR_TRACE("procedure_call -> IDENTIFIER '(' expression_list ')'");
    }
    ;

// 5.3 表达式列表
expression_list 
    : /*empty*/  {
        $$ = nullptr;
        GRAMMAR_TRACE("expression_list -> empty");
    }
    | expression  {
        currentParserContext = ParserContext::ExpressionList;
        auto exprList = allocateNode<std::vector<ExprStmt *>>();
        
        exprList->emplace_back($1);
        $$ = exprList;
        
        GRAMMAR_TRACE("expression_list -> expression");
    }
    | expression_list ',' expression  {
        currentParserContext = ParserContext::ExpressionList;
        
        $1->emplace_back($3);
        $$ = $1;
        
        GRAMMAR_TRACE("expression_list -> expression_list ',' expression");
    }
    ;

// 5.4 表达式
expression 
    : simple_expression  {
        currentParserContext = ParserContext::Expression;
        $$ = ExprFactory::createFromSimpleExpr($1);
        
        GRAMMAR_TRACE("expression -> simple_expression");
    }
    | expression relop simple_expression  {
        currentParserContext = ParserContext::Expression;
        auto expr = $1;
        RelExprStmt::Term term;
        
        term.type = getRelationOperator($2);
        term.add_expr = std::unique_ptr<AddExprStmt>($3);
        expr->rel_expr->terms.emplace_back(std::move(term));
        
        $$ = expr;
        GRAMMAR_TRACE("expression -> simple_expression relop simple_expression");
    }
    ;

// 5.5 简单表达式
simple_expression 
    : term  {
        currentParserContext = ParserContext::SimpleExpression;
        $$ = ExprFactory::createFromTerm($1);
        
        GRAMMAR_TRACE("simple_expression -> term");
    }
    | simple_expression addop term  {
        currentParserContext = ParserContext::SimpleExpression;
        auto addExpr = $1;
        AddExprStmt::Term term;
        
        term.type = getArithmeticOperator($2);
        term.mul_expr = std::unique_ptr<MulExprStmt>($3);
        addExpr->terms.emplace_back(std::move(term));
        
        $$ = addExpr;
        GRAMMAR_TRACE("simple_expression -> simple_expression %lld term\n");
    }
    ;

// 5.6 项
term 
    : factor  {
        currentParserContext = ParserContext::Term;
        $$ = ExprFactory::createFromFactor($1);
        
        GRAMMAR_TRACE("term -> factor");
    }
    | term mulop factor  {
        currentParserContext = ParserContext::Term;
        auto mulExpr = $1;
        MulExprStmt::Term term;
        
        term.type = getTermOperator($2);
        term.unary_expr = std::unique_ptr<UnaryExprStmt>($3);
        mulExpr->terms.emplace_back(std::move(term));
        
        $$ = mulExpr;
        GRAMMAR_TRACE("term -> term mulop factor");
    }
    ;

// 5.7 因子
factor 
    : INTEGER  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        setupNumberNode(unaryExpr->primary_expr->value->number, $1);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> INTEGER");
    }
    | REAL  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        double val = atof($1);
        setupNumberNode(unaryExpr->primary_expr->value->number, val);
        unaryExpr->primary_expr->value->number->literal = std::string($1);
        
        free($1);
        $$ = unaryExpr;
        
        GRAMMAR_TRACE("factor -> REAL");
    }
    | BOOLEAN  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        long long int val = $1 ? 1 : 0;
        setupNumberNode(unaryExpr->primary_expr->value->number, val);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> BOOLEAN");
    }
    | CHAR  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::Number;
        unaryExpr->primary_expr->value->number = std::make_unique<NumberStmt>();
        
        setupNumberNode(unaryExpr->primary_expr->value->number, $1);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> CHAR");
    }
    | variable  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::LVal;
        unaryExpr->primary_expr->value->lval = std::unique_ptr<LValStmt>($1);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> variable");
    }
    | '(' expression ')'  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Parentheses);
        
        unaryExpr->primary_expr->expr = std::unique_ptr<ExprStmt>($2);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> '(' expression ')'");
    }
    | IDENTIFIER '(' expression_list ')'  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = makeUnaryExpr(PrimaryExprStmt::PrimaryExprType::Value);
        
        unaryExpr->primary_expr->value = std::make_unique<ValueStmt>();
        unaryExpr->primary_expr->value->type = ValueStmt::ValueType::FuncCall;
        unaryExpr->primary_expr->value->func_call = std::make_unique<FuncCallStmt>();
        unaryExpr->primary_expr->value->func_call->id = std::string($1);
        
        // 处理函数参数
        if ($3 != nullptr) {
            for (auto expr : *$3) {
                unaryExpr->primary_expr->value->func_call->args.emplace_back(std::unique_ptr<ExprStmt>(expr));
            }
            delete $3;
        }
        
        free($1);
        $$ = unaryExpr;
        
        GRAMMAR_TRACE("factor -> IDENTIFIER '(' expression_list ')'");
    }
    | NOT factor  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = $2;
        
        unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Not);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> NOT factor");
    }
    | '+' factor  {
        currentParserContext = ParserContext::Factor;
        $$ = $2;  // 正号不改变值
        
        GRAMMAR_TRACE("factor -> '+' factor");
    }
    | '-' factor  {
        currentParserContext = ParserContext::Factor;
        auto unaryExpr = $2;
        
        unaryExpr->types.emplace_back(UnaryExprStmt::UnaryExprType::Minus);
        
        $$ = unaryExpr;
        GRAMMAR_TRACE("factor -> '-' factor");
    }
    ;

// 运算符定义
addop : '+' { $$ = 0; } | '-' { $$ = 1; } | OR { $$ = 2; }

relop : '=' { $$ = 0; } | NE { $$ = 1; } | '<' { $$ = 2; } | LE { $$ = 3; } | '>' { $$ = 4; } | GE { $$ = 5; } | IN { $$ = 6; }

mulop : '*' { $$ = 0; } | '/' { $$ = 1; } | DIV { $$ = 1; } | MOD { $$ = 2; } | AND { $$ = 3; } | ANDTHEN { $$ = 4; } 

// 错误恢复
error_recovery 
    : error ';'  {
        yyerrok;
    }
    ;

%%
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