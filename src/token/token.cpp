/**
 * @file token.cpp
 * @brief Token类的实现
 */

#include "token.hpp"
#include <string>
#include <limits>

/*
 * @brief Token类默认构造函数
 */
Token::Token() : token_type(ERROR), token_value(""), lineno(0), colno(0) {}

/*
 * @brief 装载Token信息
 * @param token_type token的类型
 * @param token_value token的值
 * @param lineno token所在行号
 * @param colno token所在列号
 */
void Token::fill(int token_type, std::string token_value, int lineno, int colno) {
    this->token_type = token_type;
    this->token_value = token_value;
    this->lineno = lineno;
    this->colno = colno;
}

/*
 * @brief 析构函数
 */
Token::~Token() {}

/*
 * @brief 获取token的类型
 * @return token的类型
 */
int Token::get_token_type() {
    return this->token_type;
}

/*
 * @brief 获取token所在行号
 * @return token所在行号
 */
int Token::get_lineno() {
    return this->lineno;
}

/*
 * @brief 获取token所在列号
 * @return token所在列号
 */
int Token::get_colno() {
    return this->colno;
}

/*
 * @brief 获取token的string类型值
 * @return token的string类型值
 */
std::string Token::get_string() {
    return this->token_value;
}

/*
 * @brief 获取token的int类型值
 * @return token的int类型值
 */
long long Token::get_int() {
    try {
        return std::stoll(this->token_value);
    } catch (...) {
        return 0;
    }
}

/*
 * @brief 获取token的double类型值
 * @return token的double类型值
 */
double Token::get_real() {
    try {
        return std::stod(this->token_value);
    } catch (...) {
        return 0.0;
    }
}

/*
 * @brief 获取token的bool类型值
 * @return token的bool类型值
 */
bool Token::get_bool() {
    return this->token_value == "true";
}

/*
 * @brief 获取token的char类型值
 * @return token的char类型值
 */
char Token::get_char() {
    if (this->token_value.length() >= 3) {
        // 处理转义字符
        if (this->token_value[1] == '\\') {
            switch (this->token_value[2]) {
                case 'n': return '\n';
                case 't': return '\t';
                case 'r': return '\r';
                case '\\': return '\\';
                case '\'': return '\'';
                case '\"': return '\"';
                default: return this->token_value[2];
            }
        }
        return this->token_value[1];
    }
    return '\0';
}

/*
 * @brief 获取token的类型的字符串表示
 * @return token的类型的字符串表示
 */
std::string Token::get_type() {
    // 关键字
    if (token_type >= PROGRAM && token_type <= BOOLEAN_KW) {
        return "KW";
    }
    
    // 基于具体token类型
    switch (token_type) {
        case ERROR:
            return "ERROR";
        case TOKENEOF:
            return "TOKENEOF";
        case IDENTIFIER:
            return "ID";
        case INTEGER:
            return "INT";
        case REAL:
            return "REAL";
        case CHAR:
            return "CHAR";
        case STRING:
            return "STRING";
        case BOOLEAN:
            return "BOOL";
        case ANNO:
            return "ANNO";
            
        // 操作符
        case NOT:
        case DIV:
        case MOD:
        case AND:
        case OR:
        case NE:
        case LE:
        case GE:
        case ASSIGNOP:
        case IN:
        case ORELSE:
        case ANDTHEN:
        case BRACE_PAIR:
        case DOUBLE_DOT:
            return "OPERATOR";
            
        default:
            // ASCII值操作符和分隔符
            if ((token_type >= 33 && token_type <= 47) ||
                (token_type >= 58 && token_type <= 64) ||
                (token_type >= 91 && token_type <= 96) ||
                (token_type >= 123 && token_type <= 126)) {
                // 标点符号和操作符
                if (token_type == ';' || token_type == ':' || token_type == '.' || 
                    token_type == ',' || token_type == '(' || token_type == ')' ||
                    token_type == '[' || token_type == ']') {
                    return "SEPARATOR";
                }
                return "OPERATOR";
            }
            return "ERROR";
    }
}

/*
 * @brief 将token转换为字符串表示
 * @return token的字符串表示
 */
std::string Token::to_string() {
    switch (token_type) {
        case BOOLEAN:
            return "[Token] " + get_type() + " " + (get_bool() ? "true" : "false") + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        case CHAR:
            return "[Token] " + get_type() + " " + get_char() + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        case STRING:
            // 处理字符串引号
            if (this->token_value.length() >= 2) {
                // 去除引号
                std::string unquoted_string = this->token_value.substr(1, this->token_value.length() - 2);
                return "[Token] " + get_type() + " " + unquoted_string + " " + 
                       "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            } else {
                return "[Token] " + get_type() + " " + this->token_value + " " + 
                       "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            }
            
        case INTEGER:
            return "[Token] " + get_type() + " " + std::to_string(get_int()) + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        case REAL:
            return "[Token] " + get_type() + " " + std::to_string(get_real()) + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        case IDENTIFIER:
        case KW:
        case OPERATOR:
        case SEPARATOR:
        case ANNO:
            return "[Token] " + get_type() + " " + get_string() + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        case TOKENEOF:
        case ERROR:
            return "[Token] " + get_type() + " " + 
                   "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            
        default:
            // 处理所有其他token类型
            if (get_type() == "KW" || get_type() == "OPERATOR" || get_type() == "SEPARATOR") {
                return "[Token] " + get_type() + " " + get_string() + " " + 
                       "[" + std::to_string(lineno) + "," + std::to_string(colno) + "]";
            }
            return "[Token] DEFAULT TOKENTYPE line : >>> " + std::to_string(lineno) + 
                   " " + "col : >>> " + std::to_string(colno);
    }
}