#pragma once

#include <algorithm>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <optional>

// 前向声明
class StmtVisitor;

// AST节点类型前向声明
class FuncCallStmt;
class LValStmt;
class StrStmt;
class NumberStmt;
class ValueStmt;
class PrimaryExprStmt;
class UnaryExprStmt;
class MulExprStmt;
class AddExprStmt;
class RelExprStmt;
class FuncHeadDeclStmt;
class FuncBodyDeclStmt;

/**
 * @brief 抽象语法树(AST)的基类节点
 */
class BaseStmt {
public:
    /**
     * @brief 用于接受visitor模式的访问
     * @param visitor 访问者对象
     */
    virtual void accept(StmtVisitor& visitor) = 0;
    
    // 使用默认构造和虚析构
    BaseStmt() = default;
    virtual ~BaseStmt() = default;
    
    // 禁用复制
    BaseStmt(const BaseStmt&) = delete;
    BaseStmt& operator=(const BaseStmt&) = delete;
};

/********************************** 表达式节点 ***********************************/
/**
 * Pascal运算符优先级(从高到低):
 * 1. ~, not
 * 2. *, /, div, mod, and, &
 * 3. |, !, +, -, or
 * 4. =, <>, <, <=, >, >=, in
 * 5. or else, and then
 */

/**
 * @brief 表达式语句(如: a + b * c)
 */
class ExprStmt : public BaseStmt {
public:
    std::unique_ptr<RelExprStmt> rel_expr;
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 关系表达式(如: a < b < c)
 */
class RelExprStmt : public BaseStmt {
public:
    // 关系操作符类型
    enum class RelExprType {
        NULL_TYPE,    // 占位初始值
        Equal,        // =
        NotEqual,     // <>
        Less,         // <
        LessEqual,    // <=
        Greater,      // >
        GreaterEqual, // >=
        In,           // in
    };
    
    // 关系表达式项
    struct Term {
        RelExprType type;                     // 操作类型
        std::unique_ptr<AddExprStmt> add_expr; // 加法表达式
    };
    
    std::vector<Term> terms;  // 关系表达式列表
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 加法表达式(如: a + b - c)
 */
class AddExprStmt : public BaseStmt {
public:
    // 加法操作符类型
    enum class AddExprType {
        NULL_TYPE, 
        Plus,  // +
        Minus, // -
        Or,    // or
    };
    
    // 加法表达式项
    struct Term {
        AddExprType type;
        std::unique_ptr<MulExprStmt> mul_expr;
    };
    
    std::vector<Term> terms;  // 加法表达式列表
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 乘法表达式(如: a * b / c)
 */
class MulExprStmt : public BaseStmt {
public:
    // 乘法操作符类型
    enum class MulExprType {
        NULL_TYPE,
        Mul,     // *
        Div,     // /
        Mod,     // mod
        And,     // and
        AndThen, // and then
    }; 
    
    // 乘法表达式项
    struct Term {
        MulExprType type;
        std::unique_ptr<UnaryExprStmt> unary_expr;
    };
    
    std::vector<Term> terms;  // 乘法表达式列表
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 一元表达式(如: -a)
 */
class UnaryExprStmt : public BaseStmt {
public:
    // 一元操作符类型
    enum class UnaryExprType {
        NULL_TYPE, 
        Not,   // not
        Minus, // -
    };
    
    std::vector<UnaryExprType> types;  // 一元操作符链
    std::unique_ptr<PrimaryExprStmt> primary_expr;  // 基本表达式
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 基本表达式(表达式的最小完整单元)
 */
class PrimaryExprStmt : public BaseStmt {
public:
    // 基本表达式类型
    enum class PrimaryExprType {
        NULL_TYPE,
        Value,       // 值
        Parentheses, // 括号内的表达式(a + b)
    };
    
    PrimaryExprType type = PrimaryExprType::NULL_TYPE;
    std::unique_ptr<ValueStmt> value;  // 值表达式
    std::unique_ptr<ExprStmt> expr;    // 括号内表达式
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 值表达式
 */
class ValueStmt : public BaseStmt {
public:
    // 值类型
    enum class ValueType {
        NULL_TYPE,
        Number,   // 立即数
        Str,      // 字符串
        LVal,     // 左值
        FuncCall, // 函数调用
    };
    
    ValueType type = ValueType::NULL_TYPE;
    std::unique_ptr<NumberStmt> number;      // 数值
    std::unique_ptr<StrStmt> str;            // 字符串
    std::unique_ptr<LValStmt> lval;          // 左值
    std::unique_ptr<FuncCallStmt> func_call; // 函数调用
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 数字表达式(如: 42)
 */
class NumberStmt : public BaseStmt {
public:
    bool is_real = false;     // 是否为实数(浮点型)
    bool is_signed = true;    // 是否为有符号数
    bool is_char = false;     // 是否为字符
    bool is_unsigned = false; // 是否为无符号数
    
    // 使用union存储不同类型的值
    union {
        long long int_val;
        double real_val;
        unsigned long long uint_val;
        char char_val;
    };
    
    std::string literal;  // 字面量
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 字符串表达式(如: 'hello world')
 */
class StrStmt : public BaseStmt {
public:
    std::string val;
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 左值表达式(如: 变量名x, 数组名arr[i+1])
 */
class LValStmt : public BaseStmt {
public:
    std::string id;  // 标识符名称
    std::vector<std::unique_ptr<ExprStmt>> array_index;  // 多维数组下标
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 函数调用表达式(如: func(a, b+c))
 */
class FuncCallStmt : public BaseStmt {
public:
    std::string id;  // 函数名
    std::vector<std::unique_ptr<ExprStmt>> args;  // 函数参数列表
    
    void accept(StmtVisitor& visitor) override;
};

/********************************** 声明语句 ***********************************/

/**
 * @brief Pascal基本数据类型
 */
enum class BasicType {
    VOID,
    INT,
    REAL,
    CHAR,
    BOOLEAN,
};

/**
 * @brief 变量数据类型种类
 */
enum class DataType {
    NULL_TYPE,  // 未初始化或未知类型
    BasicType,  // 基本类型
    ArrayType,  // 数组类型
};

/**
 * @brief 范围表达式(用于数组声明)
 */
class PeriodStmt : public BaseStmt {
public:
    int begin;
    int end;
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 常量声明语句(对应Pascal中的const部分)
 */
class ConstDeclStmt : public BaseStmt {
public:
    using KvPair = std::pair<std::string, std::unique_ptr<ValueStmt>>;  // 键值对(标识符-值)
    std::vector<KvPair> pairs;
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 变量声明语句(对应Pascal中的变量声明部分)
 */
class VarDeclStmt : public BaseStmt {
public:
    std::vector<std::string> id;  // 变量名数组(支持一条语句声明多个同类型变量)
    DataType data_type = DataType::NULL_TYPE;  // 顶层类型(基本类型或数组类型)
    BasicType basic_type = BasicType::VOID;    // 具体的基本类型(INT, REAL等)
    int type_size = 0;                         // 类型占用的内存大小
    bool is_var = false;                       // 是否按引用传递(Pascal中的var关键字)
    std::vector<std::unique_ptr<PeriodStmt>> array_range;  // 数组各维度的范围
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 完整函数声明
 */
class FuncDeclStmt : public BaseStmt {
public:
    std::unique_ptr<FuncHeadDeclStmt> header;  // 函数头部
    std::unique_ptr<FuncBodyDeclStmt> body;    // 函数体
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 函数头部声明(对应Pascal中的function/procedure声明部分)
 */
class FuncHeadDeclStmt : public BaseStmt {
public:
    std::string func_name;  // 函数名
    BasicType ret_type = BasicType::VOID;  // 返回值类型(过程为void)
    std::vector<std::unique_ptr<VarDeclStmt>> args;  // 函数参数
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 函数体声明(对应Pascal中的function/procedure实现部分)
 */
class FuncBodyDeclStmt : public BaseStmt {
public:
    std::unique_ptr<ConstDeclStmt> const_decl;  // 常量声明块
    std::vector<std::unique_ptr<VarDeclStmt>> var_decl;  // 局部变量声明块
    std::vector<std::unique_ptr<BaseStmt>> comp_stmt;    // 复合语句块(代码段)
    
    void accept(StmtVisitor& visitor) override;
};

/********************************** 功能语句 ***********************************/

/**
 * @brief 赋值语句(如: a := expr)
 */
class AssignStmt : public BaseStmt {
public:
    bool is_lval_func = false;  // 标记左值是否为函数名
    std::unique_ptr<LValStmt> lval;  // 左值
    std::unique_ptr<ExprStmt> expr;  // 右值表达式
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 分支语句(if-then-else)
 */
class IfStmt : public BaseStmt {
public:
    std::unique_ptr<ExprStmt> expr;  // 条件表达式
    std::vector<std::unique_ptr<BaseStmt>> true_stmt;   // if语句块
    std::vector<std::unique_ptr<BaseStmt>> false_stmt;  // else语句块
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief for循环语句
 */
class ForStmt : public BaseStmt {
public:
    std::string id;  // 循环变量名称(只能自增)
    std::unique_ptr<ExprStmt> begin;  // 起始值表达式
    std::unique_ptr<ExprStmt> end;    // 结束值表达式
    std::vector<std::unique_ptr<BaseStmt>> stmt;  // 循环体语句
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief while循环语句
 */
class WhileStmt : public BaseStmt {
public:
    std::unique_ptr<ExprStmt> expr;  // 循环条件表达式
    std::vector<std::unique_ptr<BaseStmt>> stmt;  // 循环体语句
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 输入函数(read/readln)
 */
class ReadFuncStmt : public BaseStmt {
public:
    std::vector<std::unique_ptr<LValStmt>> lval;  // 接收输入的变量列表
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 输出函数(write/writeln)
 */
class WriteFuncStmt : public BaseStmt {
public:
    std::vector<std::unique_ptr<ExprStmt>> expr;  // 输出的表达式列表
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief break语句
 */
class BreakStmt : public BaseStmt {
public:
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief continue语句
 */
class ContinueStmt : public BaseStmt {
public:
    void accept(StmtVisitor& visitor) override;
};

/********************************** 主程序 ***********************************/

/**
 * @brief 程序头部声明(对应Pascal中的program声明部分)
 */
class ProgramHeadStmt : public BaseStmt {
public:
    std::vector<std::string> id_list;  // 程序名和可能的外部文件参数
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 程序体声明(对应Pascal中的program实现部分)
 */
class ProgramBodyStmt : public BaseStmt {
public:
    std::unique_ptr<ConstDeclStmt> const_decl;  // 常量声明部分
    std::vector<std::unique_ptr<VarDeclStmt>> var_decl;  // 变量声明部分
    std::vector<std::unique_ptr<FuncDeclStmt>> func_decl;  // 函数/过程声明部分
    std::vector<std::unique_ptr<BaseStmt>> comp_stmt;  // 主程序语句部分(begin...end)
    
    void accept(StmtVisitor& visitor) override;
};

/**
 * @brief 整个程序
 */
class ProgramStmt : public BaseStmt {
public:
    std::unique_ptr<ProgramHeadStmt> head;  // 程序头部
    std::unique_ptr<ProgramBodyStmt> body;  // 程序体
    
    void accept(StmtVisitor& visitor) override;
};