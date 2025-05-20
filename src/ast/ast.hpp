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
class FuncCallNode;
class LValueNode;
class StringNode;
class NumberNode;
class ValueNode;
class PrimaryExprNode;
class UnaryExprNode;
class MulExprNode;
class AddExprNode;
class RelExprNode;
class FuncHeadDeclNode;
class FuncBodyDeclNode;

//抽象语法树(AST)的基类节点
class BaseNode {
public:
    // 访问者模式的接受方法
    virtual void accept(StmtVisitor& visitor) = 0;
    
    // 使用默认构造和虚析构
    BaseNode() = default;
    virtual ~BaseNode() = default;
    
    // 禁用复制
    BaseNode(const BaseNode&) = delete;
    BaseNode& operator=(const BaseNode&) = delete;
};


//--------------------------表达式节点--------------------------

// ExprNode 表示一个完整的表达式语句，如：a + b * c 或 x < 5
// 是表达式层次中最顶层的节点类型
class ExprNode : public BaseNode {
public:
    std::unique_ptr<RelExprNode> rel_expr;

    void accept(StmtVisitor& visitor) override;
};

// RelExprNode 表示关系表达式，处理比较操作，如: a < b, x >= y, flag = true 等
// 是运算符优先级中最低的一层
class RelExprNode : public BaseNode {
public:
    // 枚举定义所有可能的关系操作符类型
    enum class RelExprType {
        NULL_TYPE,    // 占位初始值，表示未设置状态
        Equal,        // = 等于
        NotEqual,     // <> 不等于
        Less,         // < 小于
        LessEqual,    // <= 小于等于
        Greater,      // > 大于
        GreaterEqual, // >= 大于等于
        In,           // in 包含关系(Pascal特有)
    };
    
    // Term结构表示一个关系表达式项，由操作符类型和右侧表达式组成
    struct Term {
        RelExprType type;                      // 关系操作符类型
        std::unique_ptr<AddExprNode> add_expr; // 关系右侧的加法表达式
    };
    
    // 存储一系列关系表达式项的vector
    std::vector<Term> terms;
    
    void accept(StmtVisitor& visitor) override;
};

// AddExprNode 表示加法级别的表达式，处理加法、减法和逻辑OR操作，如: a + b - c, x or y
class AddExprNode : public BaseNode {
public:
    // 枚举定义加法级别的操作符类型
    enum class AddExprType {
        NULL_TYPE,    // 未设置状态
        Plus,         // + 加法
        Minus,        // - 减法
        Or,           // or 逻辑或
    };
    
    // Term结构表示一个加法表达式项，由操作符类型和右侧表达式组成
    struct Term {
        AddExprType type;                      // 操作符类型
        std::unique_ptr<MulExprNode> mul_expr; // 指向乘法表达式的指针
    };
    
    // 存储加法表达式序列
    std::vector<Term> terms;
    
    void accept(StmtVisitor& visitor) override;
};

// MulExprNode 表示乘法级别的表达式，处理乘法、除法、取模和逻辑AND操作，如: a * b / c, x and y
class MulExprNode : public BaseNode {
public:
    // 枚举定义乘法级别的操作符类型
    enum class MulExprType {
        NULL_TYPE,   // 未设置状态
        Mul,         // * 乘法
        Div,         // / 除法
        Mod,         // mod 取模运算(Pascal语法)
        And,         // and 逻辑与(Pascal语法)
        AndThen,     // and then 短路逻辑与(Pascal扩展语法)
    }; 
    
    // Term结构表示一个乘法表达式项，由操作符类型和右侧的一元表达式组成
    struct Term {
        MulExprType type;                         // 操作符类型
        std::unique_ptr<UnaryExprNode> unary_expr; // 指向一元表达式的指针
    };
    
    // 存储乘法表达式序列
    std::vector<Term> terms;

    void accept(StmtVisitor& visitor) override;
};

// UnaryExprNode 表示一元操作表达式，处理负号和逻辑非操作，如: -a, not flag, -(a+b)
class UnaryExprNode : public BaseNode {
public:
    // 枚举定义一元操作符类型
    enum class UnaryExprType {
        NULL_TYPE,   // 未设置状态
        Not,         // not 逻辑非(Pascal语法)
        Minus,       // - 负号
    };
    
    // 存储一系列一元操作符
    std::vector<UnaryExprType> types;
    
    // 指向一元操作符作用的基本表达式，这是操作符链最终应用到的表达式
    std::unique_ptr<PrimaryExprNode> primary_expr;
    
    void accept(StmtVisitor& visitor) override;
};

// PrimaryExprNode 表示基本表达式，是表达式递归结构中的原子单元，可以是简单值或括号内的复杂表达式
class PrimaryExprNode : public BaseNode {
public:
    // 枚举定义基本表达式的可能类型
    enum class PrimaryExprType {
        NULL_TYPE,    // 未设置状态
        Value,        // 值(变量、常量、字面量等)
        Parentheses,  // 括号内的表达式，如(a + b)
    };
    
    // 表达式类型，默认为NULL_TYPE
    PrimaryExprType type = PrimaryExprType::NULL_TYPE;
    
    // 如果类型是Value，指针指向具体的值节点(可能是数字、字符串、变量等)
    std::unique_ptr<ValueNode> value;
    
    // 如果类型是Parentheses，指向括号内的完整表达式
    std::unique_ptr<ExprNode> expr;
 
    void accept(StmtVisitor& visitor) override;
};


// ValueNode 表示一个具体的值，是表达式树中可能出现的实际数据节点，包括数字、字符串、变量引用和函数调用
class ValueNode : public BaseNode {
public:
    // 枚举定义值的可能类型
    enum class ValueType {
        NULL_TYPE,   // 未设置状态
        Number,      // 数字字面量，如42, 3.14
        Str,         // 字符串字面量，如'hello'
        LVal,        // 左值(变量引用)，如x, array[i]
        FuncCall,    // 函数调用，如max(a, b)
    };
    
    // 值的具体类型，默认为NULL_TYPE
    ValueType type = ValueType::NULL_TYPE;
    
    // 以下四个指针中只有一个会被设置，取决于type的值
    
    // 如果type是Number，这个指针被设置
    std::unique_ptr<NumberNode> number;
    
    // 如果type是Str，这个指针被设置
    std::unique_ptr<StringNode> str;
    
    // 如果type是LVal，这个指针被设置
    std::unique_ptr<LValueNode> lval;
    
    // 如果type是FuncCall，这个指针被设置
    std::unique_ptr<FuncCallNode> func_call;
    
    // 访问者模式的accept实现
    void accept(StmtVisitor& visitor) override;
};

// NumberNode 表示数字字面量，支持多种数值类型：整数、浮点数、字符和无符号数
class NumberNode : public BaseNode {
public:
    // 类型标志，用于指示数字的精确类型
    bool is_real = false;     // true表示浮点数，false表示整数
    bool is_signed = true;    // true表示有符号数，false表示无符号数
    bool is_char = false;     // true表示字符值，false表示数字值
    bool is_unsigned = false; // true表示明确的无符号类型
    
    union {
        long long int_val;           // 用于有符号整数
        double real_val;             // 用于浮点数
        unsigned long long uint_val; // 用于无符号整数
        char char_val;               // 用于字符值
    };
    
    // 原始字面量字符串表示，保留源代码中的确切表示形式
    std::string literal;
    
    void accept(StmtVisitor& visitor) override;
};

// StringNode 表示字符串字面量，在Pascal中，字符串通常用单引号表示，如'hello world'
class StringNode : public BaseNode {
public:
    // 字符串的实际值，不包含引号
    std::string val;

    void accept(StmtVisitor& visitor) override;
};

// LValueNode 表示左值表达式，左值是可以出现在赋值语句左侧的表达式，包括变量名和数组元素引用
class LValueNode : public BaseNode {
public:
    // 标识符名称，变量或数组的名称
    std::string id;
    
    // 数组索引表达式列表，用于多维数组访问
    // 如果是简单变量引用，这个vector为空
    // 如果是数组访问，每个元素对应一个维度的索引表达式
    std::vector<std::unique_ptr<ExprNode>> array_index;
    
    // 访问者模式的accept实现
    void accept(StmtVisitor& visitor) override;
};

// FuncCallNode 表示函数调用表达式，用于表示调用函数或过程并传递参数的语法
class FuncCallNode : public BaseNode {
public:
    // 函数或过程的名称(标识符)
    // 在Pascal中，函数返回值，而过程不返回值
    std::string id;
    
    // 函数调用的参数列表，每个参数都是一个完整的表达式，可以有任意复杂度
    std::vector<std::unique_ptr<ExprNode>> args;
    
    void accept(StmtVisitor& visitor) override;
};

//--------------------------声明语句--------------------------

// BasicType 枚举定义了Pascal语言支持的基本数据类型，这些是最基础的类型，不能再细分
enum class BasicType {
    VOID,     // 空类型
    INT,      // 整数类型
    REAL,     // 实数(浮点数)类型
    CHAR,     // 字符类型
    BOOLEAN,  // 布尔类型(true/false)
};

// DataType 枚举定义了变量可能的顶层类型种类，区分基本类型和复合类型(如数组)
enum class DataType {
    NULL_TYPE,  // 未初始化或未知类型
    BasicType,  // 基本类型(整数、实数、字符、布尔等)
    ArrayType,  // 数组类型(元素可以是基本类型或嵌套数组)
};

// PeriodNode 表示一个范围表达式，在Pascal中用于定义数组的索引范围
class PeriodNode : public BaseNode {
public:
    // 范围的起始值
    int begin;
    
    // 范围的结束值
    int end;

    void accept(StmtVisitor& visitor) override;
};

// ConstDeclNode 表示常量声明区域，对应Pascal中的const部分
class ConstDeclNode : public BaseNode {
public:
    // 定义键值对类型，用于存储常量名和其对应的值，第一个元素是常量名(字符串)，第二个元素是常量值(ValueNode指针)
    using KvPair = std::pair<std::string, std::unique_ptr<ValueNode>>;
    
    // 存储所有常量定义的vector，每个元素是一个常量名-值对
    std::vector<KvPair> pairs;
    
    // 访问者模式的accept实现
    void accept(StmtVisitor& visitor) override;
};

// VarDeclNode 表示变量声明语句，对应Pascal中的var部分中的单条变量声明
class VarDeclNode : public BaseNode {
public:
    // 变量名列表，支持一条语句声明多个同类型变量
    std::vector<std::string> id;
    
    // 变量的顶层类型种类(基本类型或数组类型)
    DataType data_type = DataType::NULL_TYPE;
    
    // 变量的基本类型
    // 如果data_type是BasicType，则直接使用此类型
    // 如果data_type是ArrayType，则此类型表示数组元素的类型
    BasicType basic_type = BasicType::VOID;
    
    // 类型占用的内存大小(以字节为单位)，用于内存分配和偏移计算
    int type_size = 0;
    
    // 标记变量是否为引用参数，在Pascal中，var关键字表示参数按引用传递
    bool is_var = false;
    
    // 数组各维度的范围
    std::vector<std::unique_ptr<PeriodNode>> array_range;

    void accept(StmtVisitor& visitor) override;
};

// FuncDeclNode 表示一个完整的函数或过程声明，包含函数头部和函数体
class FuncDeclNode : public BaseNode {
public:
    // 函数头部，包含函数名、返回类型和参数列表
    std::unique_ptr<FuncHeadDeclNode> header;
    
    // 函数体，包含局部声明和实现代码
    std::unique_ptr<FuncBodyDeclNode> body;

    void accept(StmtVisitor& visitor) override;
};

// FuncHeadDeclNode 表示函数头部声明，包括名称、参数和返回类型
class FuncHeadDeclNode : public BaseNode {
public:
    // 函数或过程的名称
    std::string func_name;
    
    // 返回值类型
    // 如果是function，则为具体类型(INT, REAL等)
    // 如果是procedure，则为VOID
    BasicType ret_type = BasicType::VOID;
    
    // 函数参数列表，每个参数都是一个变量声明
    std::vector<std::unique_ptr<VarDeclNode>> args;
    
    // 访问者模式的accept实现
    void accept(StmtVisitor& visitor) override;
};

// FuncBodyDeclNode 表示函数体声明，包含局部常量、变量声明和函数实现代码
class FuncBodyDeclNode : public BaseNode {
public:
    // 局部常量声明块
    std::unique_ptr<ConstDeclNode> const_decl;
    
    // 局部变量声明块
    std::vector<std::unique_ptr<VarDeclNode>> var_decl;
    
    // 函数实现代码
    std::vector<std::unique_ptr<BaseNode>> comp_stmt;
    
    void accept(StmtVisitor& visitor) override;
};

//------------------------------语句节点-------------------------------

// AssignmentNode 表示赋值语句，用于将表达式的结果赋给变量或数组元素
class AssignmentNode : public BaseNode {
public:
    // 标记左值是否为函数名
    bool is_lval_func = false;
    
    // 左值，表示被赋值的目标，可以是变量名或数组元素
    std::unique_ptr<LValueNode> lval;
    
    // 右值表达式，计算后的结果将被赋给左值，可以是任意复杂度的表达式
    std::unique_ptr<ExprNode> expr;
    
    void accept(StmtVisitor& visitor) override;
};

// IfNode 表示条件分支语句，实现"if-then-else"控制结构
class IfNode : public BaseNode {
public:
    // 条件表达式，决定执行哪个分支
    std::unique_ptr<ExprNode> expr;
    
    // if语句块(条件为真时执行)
    std::vector<std::unique_ptr<BaseNode>> true_stmt;
    
    // else语句块(条件为假时执行)
    std::vector<std::unique_ptr<BaseNode>> false_stmt;

    void accept(StmtVisitor& visitor) override;
};

// ForNode 表示for循环语句，在Pascal中，for循环控制变量自动递增或递减

class ForNode : public BaseNode {
public:
    // 循环控制变量的名称
    std::string id;
    
    // 循环起始值表达式
    std::unique_ptr<ExprNode> begin;
    
    // 循环结束值表达式
    std::unique_ptr<ExprNode> end;
    
    // 循环体语句列表
    std::vector<std::unique_ptr<BaseNode>> stmt;

    void accept(StmtVisitor& visitor) override;
};

// WhileNode 表示while循环语句，实现基于条件的循环执行
class WhileNode : public BaseNode {
public:
    // 循环条件表达式
    std::unique_ptr<ExprNode> expr;
    
    // 循环体语句列表
    std::vector<std::unique_ptr<BaseNode>> stmt;

    void accept(StmtVisitor& visitor) override;
};

// ReadFuncNode 表示输入语句，对应Pascal中的read或readln函数调用
class ReadFuncNode : public BaseNode {
public:
    // 接收输入的变量列表，每个变量必须是可赋值的左值(变量或数组元素)
    std::vector<std::unique_ptr<LValueNode>> lval;

    void accept(StmtVisitor& visitor) override;
};

// WriteFuncNode 表示输出语句，对应Pascal中的write或writeln函数调用
class WriteFuncNode : public BaseNode {
public:
    // 输出的表达式列表，每个表达式的值会被转换为字符串并输出
    std::vector<std::unique_ptr<ExprNode>> expr;
    
    void accept(StmtVisitor& visitor) override;
};

// BreakNode 表示break语句，用于立即退出当前循环
class BreakNode : public BaseNode {
public:
    void accept(StmtVisitor& visitor) override;
};

// ContinueNode 表示continue语句，用于跳过循环体中剩余的语句，直接进入下一次迭代
class ContinueNode : public BaseNode {
public:

    void accept(StmtVisitor& visitor) override;
};

//------------------------------程序节点-------------------------------

// ProgramHeadNode 表示程序头部声明
class ProgramHeadNode : public BaseNode {
public:
    // 程序名和可能的外部文件参数列表
    // 第一个元素是程序名
    // 后续元素是程序可能使用的外部文件名(如input, output)
    std::vector<std::string> id_list;

    void accept(StmtVisitor& visitor) override;
};

// ProgramBodyNode 表示程序体声明，包含常量定义、变量声明、函数/过程声明和主程序代码
class ProgramBodyNode : public BaseNode {
public:
    // 常量声明部分
    std::unique_ptr<ConstDeclNode> const_decl;
    
    // 变量声明部分
    std::vector<std::unique_ptr<VarDeclNode>> var_decl;
    
    // 函数/过程声明部分
    std::vector<std::unique_ptr<FuncDeclNode>> func_decl;
    
    // 主程序语句部分
    std::vector<std::unique_ptr<BaseNode>> comp_stmt;

    void accept(StmtVisitor& visitor) override;
};

// ProgramNode 表示整个Pascal程序，这是AST的根节点，包含完整的程序结构
// 一个Pascal程序由头部声明和程序体两部分组成
class ProgramNode : public BaseNode {
public:
    // 程序头部，包含程序名和文件参数
    // 对应program语句
    std::unique_ptr<ProgramHeadNode> head;
    
    // 程序体，包含常量、变量、函数声明和主程序代码
    // 对应program语句后的所有内容
    std::unique_ptr<ProgramBodyNode> body;

    void accept(StmtVisitor& visitor) override;
};