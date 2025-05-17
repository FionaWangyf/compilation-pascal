/**
 * @file token.hpp
 * @brief 定义Token类及相关数据结构
 */

#ifndef TOKEN_HPP
#define TOKEN_HPP

#include <string>

/**
 * @brief Token类型枚举，对应Pascal语言的各种词法单元
 */
enum TokenType {
    // 特殊标记
    ERROR = 0,          // 错误token
    TOKENEOF,           // 文件结束

    // 关键字
    PROGRAM,            // program
    CONST,              // const
    TYPE,               // type
    RECORD,             // record
    ARRAY,              // array
    OF,                 // of
    VAR,                // var
    FUNCTION,           // function
    PROCEDURE,          // procedure
    BEGIN_TOKEN,        // begin
    END,                // end
    IF,                 // if
    THEN,               // then
    ELSE,               // else
    CASE,               // case
    WHILE,              // while
    REPEAT,             // repeat
    UNTIL,              // until
    FOR,                // for
    TO,                 // to
    DOWNTO,             // downto
    DO,                 // do
    READ,               // read
    READLN,             // readln
    BREAK,              // break
    CONTINUE,           // continue
    WRITE,              // write
    WRITELN,            // writeln
    
    // 类型关键字
    CHAR_KW,            // char
    INTEGER_KW,         // integer
    REAL_KW,            // real
    BOOLEAN_KW,         // boolean
    
    // 常量和标识符
    BOOLEAN,            // true/false
    INTEGER,            // 整数常量
    REAL,               // 实数常量
    CHAR,               // 字符常量
    STRING,             // 字符串常量
    IDENTIFIER,         // 标识符
    
    // 操作符
    NOT,                // not
    DIV,                // div
    MOD,                // mod
    AND,                // and
    OR,                 // or
    NE,                 // <>
    LE,                 // <=
    GE,                 // >=
    ASSIGNOP,           // :=
    IN,                 // in
    ORELSE,             // or else
    ANDTHEN,            // and then
    
    // 特殊符号
    BRACE_PAIR,         // ][
    DOUBLE_DOT,         // ..
    
    // 单字符操作符和分隔符（ASCII值）
    // +, -, *, /, (, ), [, ], =, <, >, ;, :, ., ,, etc.
    
    // 分类token类型（用于to_string方法）
    KW,                 // 关键字类别
    OPERATOR,           // 操作符类别
    SEPARATOR,          // 分隔符类别
    ANNO                // 注释类别
};

/**
 * @brief Token类，用于表示词法分析的结果
 */
class Token {
private:
    int token_type;          // token类型
    std::string token_value; // token值
    int lineno;              // 行号
    int colno;               // 列号

public:
    /**
     * @brief 默认构造函数
     */
    Token();
    
    /**
     * @brief 析构函数
     */
    ~Token();
    
    /**
     * @brief 装载Token信息
     * @param token_type token的类型
     * @param token_value token的值
     * @param lineno token所在行号
     * @param colno token所在列号
     */
    void fill(int token_type, std::string token_value, int lineno, int colno);
    
    /**
     * @brief 获取token的类型
     * @return token的类型
     */
    int get_token_type();
    
    /**
     * @brief 获取token所在行号
     * @return token所在行号
     */
    int get_lineno();
    
    /**
     * @brief 获取token所在列号
     * @return token所在列号
     */
    int get_colno();
    
    /**
     * @brief 获取token的string类型值
     * @return token的string类型值
     */
    std::string get_string();
    
    /**
     * @brief 获取token的int类型值
     * @return token的int类型值
     */
    long long get_int();
    
    /**
     * @brief 获取token的double类型值
     * @return token的double类型值
     */
    double get_real();
    
    /**
     * @brief 获取token的bool类型值
     * @return token的bool类型值
     */
    bool get_bool();
    
    /**
     * @brief 获取token的char类型值
     * @return token的char类型值
     */
    char get_char();
    
    /**
     * @brief 获取token的类型的字符串表示
     * @return token的类型的字符串表示
     */
    std::string get_type();
    
    /**
     * @brief 将token转换为字符串表示
     * @return token的字符串表示
     */
    std::string to_string();
};

#endif // TOKEN_HPP