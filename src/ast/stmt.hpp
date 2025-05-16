#pragma once

#include <algorithm>
#include <memory>
#include <string>
#include <utility>
#include <vector>



class StmtVisitor;

/**
 * @brief stmt的基类, stmt即为AST的节点
 */
class BaseStmt {
public:
    /**
     * @brief 用于接受visitor的访问，继承visitor类即可实现对stmt的访问
     * @param visitor 
     */
    virtual void accept(StmtVisitor &visitor) = 0;
    BaseStmt() = default;
    virtual ~BaseStmt() = default;
};

/********************************表达式*******************************************/
// 在Pascal中，运算符被分为五个优先等级，从高到低分别是
// ~, not
// *, /, div, mod, and, &
// |, !, +, -, or
// =, <>, <, <=, >, >=, in
// or else, and then

class FuncCallStmt; // 函数调用
class LValStmt; // 左值
class StrStmt; // 字符串
class NumberStmt; // 数字
class ValueStmt; // 值的stmt
class PrimaryExprStmt; // 优先级最高的表达式，即括号内的表达式
class UnaryExprStmt; // 一元表达式
class MulExprStmt; // 乘法以及与乘法同级别的运算
class AddExprStmt; // 加法以及与加法同级别的运算
class RelExprStmt; // 关系运算
class FuncHeadDeclStmt;
class FuncBodyDeclStmt;

//表达式语句，如：a + b * c
class ExprStmt : public BaseStmt {
public:
    std::unique_ptr<RelExprStmt> rel_expr;
    void accept(StmtVisitor &visitor) override;
};

//关系表达式，如：a < b < c
class RelExprStmt : public BaseStmt {
public:
    enum class RelExprType {//该层级表达式支持的操作符类型
        NULL_TYPE,//占位初始值+错误检测
        Equal, // =
        NotEqual, // <>
        Less, // <
        LessEqual, // <=
        Greater, // >
        GreaterEqual, // >=
        In, // in
    };
    struct Term {
        RelExprType type;//操作类型
        std::unique_ptr<AddExprStmt> add_expr;//add_expr
    };
    std::vector<Term> terms;
    void accept(StmtVisitor &visitor) override;
};

//加法表达式：a + b - c
class AddExprStmt : public BaseStmt {
public:
    enum class AddExprType {
        NULL_TYPE, 
        Plus, // +
        Minus, // -
        Or, // or
    };
    struct Term {
        AddExprType type;
        std::unique_ptr<MulExprStmt> mul_expr;
    };
    std::vector<Term> terms;
    void accept(StmtVisitor &visitor) override;
};

//乘法表达式：a * b / c
class MulExprStmt : public BaseStmt {
public:
    enum class MulExprType {
        NULL_TYPE,
        Mul, // *
        Div, // /
        Mod, // mod
        And, // and
        AndThen, // and then
    }; 
    struct Term {
        MulExprType type;
        std::unique_ptr<UnaryExprStmt> unary_expr;
    };
    std::vector<Term> terms;
    void accept(StmtVisitor &visitor) override;
};

//一元表达式：-a
class UnaryExprStmt : public BaseStmt {
public:
    enum class UnaryExprType {
        NULL_TYPE, 
        Not, // not
        Minus, // -
    };
    std::vector<UnaryExprType> types;
    std::unique_ptr<PrimaryExprStmt> primary_expr;
    void accept(StmtVisitor &visitor) override;
};
//基本表达式：表示表达式的最小完整单元
class PrimaryExprStmt : public BaseStmt {
public:
    enum class PrimaryExprType {
        NULL_TYPE,
        Value, // 值
        Parentheses, // 括号内的表达式(a + b)
    };
    PrimaryExprType type;
    std::unique_ptr<ValueStmt> value;
    std::unique_ptr<ExprStmt> expr;
    void accept(StmtVisitor &visitor) override;
};
//值表达式
class ValueStmt : public BaseStmt {
public:
    enum class ValueType {
        NULL_TYPE,
        Number, // 立即数
        Str, // 字符串
        LVal, // 左值
        FuncCall, // 函数调用
    };
    ValueType type;
    std::unique_ptr<NumberStmt> number;
    std::unique_ptr<StrStmt> str;
    std::unique_ptr<LValStmt> lval;
    std::unique_ptr<FuncCallStmt> func_call;
    void accept(StmtVisitor &visitor) override;
};
//数字表达式：42
class NumberStmt : public BaseStmt {
public:
    bool is_real; // 是否为实数(浮点型)
    bool is_signed; // 是否为有符号数
    bool is_char; // 是否为字符
    bool is_unsigned; // 是否为无符号数
    union {
        long long int_val;
        double real_val;
        unsigned long long uint_val;
        char char_val;
    };
    std::string literal; // 字面量
    void accept(StmtVisitor &visitor) override;
};

//字符串表达式：'hello world'
class StrStmt : public BaseStmt {
public:
    std::string val;
    void accept(StmtVisitor &visitor) override;
};

//左值表达式：变量名x，数组名arr[i+1]
class LValStmt : public BaseStmt {
public:
    std::string id; //存储标识符名称a
    std::vector<std::unique_ptr<ExprStmt>> array_index; // 多维数组的下标 i+1
    void accept(StmtVisitor &visitor) override;
};

//函数调用表达式：函数名(参数)
class FuncCallStmt : public BaseStmt {
public:
    std::string id;//函数名
    std::vector<std::unique_ptr<ExprStmt>> args; // 函数的参数
    void accept(StmtVisitor &visitor) override;
};

/********************************表达式*******************************************/

/*******************************声明语句******************************************/

//Pascal语言支持的基本数据类型
enum class BasicType {
    VOID,
    INT,
    REAL,
    CHAR,
    BOOLEAN,
};
//区分变量的数据类型种类
enum class DataType {
    NULL_TYPE,//未初始化或未知类型
    BasicType,
    ArrayType,
};
//范围表达式，用于数组声明
class PeriodStmt : public BaseStmt {
public:
    int begin;
    int end;
    void accept(StmtVisitor &visitor) override;
};

//常量声明语句，对应Pascal中的const部分
class ConstDeclStmt : public BaseStmt {
public:
    typedef std::pair<std::string, std::unique_ptr<ValueStmt>> KvPair;//表示键值对(标识符-值)
    std::vector<KvPair> pairs;
    void accept(StmtVisitor &visitor) override;
};

//变量声明语句，对应Pascal中的变量声明部分
class VarDeclStmt : public BaseStmt {
public:
    std::vector<std::string> id; //  变量名数组，允许在一条语句中声明多个同类型变量
    DataType data_type; // 顶层类型(基本类型或数组类型)
    BasicType basic_type; // 具体的基本类型(INT, REAL等)
    int type_size; //  类型占用的内存大小
    bool is_var; // 标记参数是否按引用传递(对应Pascal中的var关键字)
    std::vector<std::unique_ptr<PeriodStmt>> array_range; //  数组各维度的范围表达式
    void accept(StmtVisitor &visitor) override;
 };

//函数/过程声明

//完整函数声明
class FuncDeclStmt : public BaseStmt {
public:
    std::unique_ptr<FuncHeadDeclStmt> header; // 函数的头部
    std::unique_ptr<FuncBodyDeclStmt> body; // 函数体
    void accept(StmtVisitor &visitor) override;
};

//函数头部声明，对应Pascal中的function/procedure声明部分
class FuncHeadDeclStmt : public BaseStmt {
public:
    std::string func_name; // 函数名
    BasicType ret_type; //返回值的类型，过程设置为void
    std::vector<std::unique_ptr<VarDeclStmt>> args; // 函数的参数
    void accept(StmtVisitor &visitor) override;
};

//函数体声明，对应Pascal中的function/procedure的实现部分
class FuncBodyDeclStmt : public BaseStmt {
public:
    std::unique_ptr<ConstDeclStmt> const_decl;// 常量声明块
    std::vector<std::unique_ptr<VarDeclStmt>> var_decl; // 局部变量声明块
    std::vector<std::unique_ptr<BaseStmt>> comp_stmt;// 复合声明块，即代码段
    void accept(StmtVisitor &visitor) override;
};

/*******************************声明语句******************************************/

/*******************************功能语句******************************************/
//控制流和功能性语句
//赋值语句
class AssignStmt : public BaseStmt {
public:
    bool is_lval_func; //标记左值是否为函数名
    std::unique_ptr<LValStmt> lval; //左值
    std::unique_ptr<ExprStmt> expr; //右值-表达式
    void accept(StmtVisitor &visitor) override;
};

//分支语句if
class IfStmt : public BaseStmt {
public:
    std::unique_ptr<ExprStmt> expr; // 条件表达式，决定执行哪个分支
    std::vector<std::unique_ptr<BaseStmt>> true_stmt; // 条件为真时执行的语句块（if语句块）
    std::vector<std::unique_ptr<BaseStmt>> false_stmt; // 条件为假时执行的语句块（else语句块）
    void accept(StmtVisitor &visitor) override;
};

//for循环语句
class ForStmt : public BaseStmt {
public:
    std::string id; // 循环变量名称,只能自增
    std::unique_ptr<ExprStmt> begin; // 起始值表达式
    std::unique_ptr<ExprStmt> end; // 结束值表达式
    std::vector<std::unique_ptr<BaseStmt>> stmt; // 循环体语句
    void accept(StmtVisitor &visitor) override;
};

//while循环语句
class WhileStmt : public BaseStmt {
public:
    std::unique_ptr<ExprStmt> expr; // while条件表达式
    std::vector<std::unique_ptr<BaseStmt>> stmt; // while语句块
    void accept(StmtVisitor &visitor) override;
};


//输入函数
class ReadFuncStmt : public BaseStmt {
public:
    std::vector<std::unique_ptr<LValStmt>> lval; // 接收输入的变量列表
    void accept(StmtVisitor &visitor) override;
};

//输出函数
class WriteFuncStmt : public BaseStmt {
public:
    std::vector<std::unique_ptr<ExprStmt>> expr; // 输出的表达式列表
    void accept(StmtVisitor &visitor) override;
};

//break语句
class BreakStmt : public BaseStmt {
public:
    void accept(StmtVisitor &visitor) override;
};
//continue语句
class ContinueStmt : public BaseStmt {
public:
    void accept(StmtVisitor &visitor) override;
};


/*******************************功能语句******************************************/

/*******************************主函数******************************************/

//程序头部声明，对应Pascal中的program声明部分
class ProgramHeadStmt : public BaseStmt {
public:
    std::vector<std::string> id_list;//程序名和可能的外部文件参数
    void accept(StmtVisitor &visitor) override;
};

//程序体声明，对应Pascal中的program的实现部分
class ProgramBodyStmt : public BaseStmt {
public:
    std::unique_ptr<ConstDeclStmt> const_decl;// 常量声明部分
    std::vector<std::unique_ptr<VarDeclStmt>> var_decl;// 局部变量声明部分
    std::vector<std::unique_ptr<FuncDeclStmt>> func_decl;//函数和过程声明部分
    std::vector<std::unique_ptr<BaseStmt>> comp_stmt;//主程序语句部分（begin……end）
    void accept(StmtVisitor &visitor) override;
};


//整个程序
class ProgramStmt : public BaseStmt {
public:
    std::unique_ptr<ProgramHeadStmt> head;//指向程序头部的智能指针
    std::unique_ptr<ProgramBodyStmt> body;//指向程序体的智能指针
    void accept(StmtVisitor &visitor) override;
};

