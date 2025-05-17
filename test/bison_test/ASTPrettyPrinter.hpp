#pragma once

#include "ast/stmt.hpp"
#include <iostream>
#include <string>
#include <sstream>

/**
 * @brief 用于格式化输出AST的类
 */
class ASTPrettyPrinter {
private:
    int indent_level = 0;
    const int INDENT_SIZE = 2;
    
    std::string getIndent() const {
        return std::string(indent_level * INDENT_SIZE, ' ');
    }
    
    void increaseIndent() {
        indent_level++;
    }
    
    void decreaseIndent() {
        if (indent_level > 0) indent_level--;
    }

public:
    ASTPrettyPrinter() = default;
    
    // 打印整个程序AST
    std::string printProgram(const ProgramStmt* program) {
        if (!program) return "NULL Program";
        
        std::stringstream ss;
        ss << "===== Pascal-S AST Test Program =====" << std::endl;
        ss << "AST for program Test:" << std::endl;
        
        // 程序名
        if (program->head && !program->head->id_list.empty()) {
            ss << "Program: " << program->head->id_list[0] << std::endl;
        }
        
        // 程序体
        if (program->body) {
            ss << "Block:" << std::endl;
            increaseIndent();
            
            // 声明部分
            bool hasDeclarations = (program->body->const_decl && !program->body->const_decl->pairs.empty()) || 
                                  !program->body->var_decl.empty() || 
                                  !program->body->func_decl.empty();
            
            if (hasDeclarations) {
                ss << getIndent() << "Declarations:" << std::endl;
                increaseIndent();
                
                // 常量声明
                if (program->body->const_decl && !program->body->const_decl->pairs.empty()) {
                    printConstDeclarations(ss, program->body->const_decl.get());
                }
                
                // 变量声明
                for (const auto& varDecl : program->body->var_decl) {
                    printVarDeclaration(ss, varDecl.get());
                }
                
                // 函数/过程声明
                for (const auto& funcDecl : program->body->func_decl) {
                    printFunctionDeclaration(ss, funcDecl.get());
                }
                
                decreaseIndent();
            }
            
            // 语句部分
            if (!program->body->comp_stmt.empty()) {
                ss << getIndent() << "CompoundStatement:" << std::endl;
                increaseIndent();
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
                
                for (const auto& stmt : program->body->comp_stmt) {
                    printStatement(ss, stmt.get());
                }
                
                decreaseIndent();
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        ss << "===== Test Completed =====" << std::endl;
        return ss.str();
    }
    
    // 打印常量声明
    void printConstDeclarations(std::stringstream& ss, const ConstDeclStmt* constDecl) {
        for (const auto& pair : constDecl->pairs) {
            ss << getIndent() << "ConstDeclaration: " << pair.first << std::endl;
            increaseIndent();
            
            // 获取常量的值
            std::string valueStr = "Unknown";
            if (pair.second) {
                if (pair.second->type == ValueStmt::ValueType::Number && pair.second->number) {
                    if (pair.second->number->is_real) {
                        valueStr = "REAL " + std::to_string(pair.second->number->real_val);
                    } else if (pair.second->number->is_char) {
                        valueStr = "CHAR '" + std::string(1, pair.second->number->char_val) + "'";
                    } else {
                        valueStr = "INTEGER " + std::to_string(pair.second->number->int_val);
                    }
                } else if (pair.second->type == ValueStmt::ValueType::Str && pair.second->str) {
                    valueStr = "STRING '" + pair.second->str->val + "'";
                }
            }
            
            ss << getIndent() << "Value: " << valueStr << std::endl;
            decreaseIndent();
        }
    }
    
    // 打印变量声明
    void printVarDeclaration(std::stringstream& ss, const VarDeclStmt* varDecl) {
        if (!varDecl) return;
        
        for (const auto& id : varDecl->id) {
            ss << getIndent() << "VarDeclaration: " << id << std::endl;
            increaseIndent();
            
            std::string typeStr;
            if (varDecl->data_type == DataType::BasicType) {
                switch (varDecl->basic_type) {
                    case BasicType::INT: typeStr = "integer"; break;
                    case BasicType::REAL: typeStr = "real"; break;
                    case BasicType::CHAR: typeStr = "char"; break;
                    case BasicType::BOOLEAN: typeStr = "boolean"; break;
                    default: typeStr = "unknown"; break;
                }
                ss << getIndent() << "SimpleType: " << typeStr << std::endl;
            } else if (varDecl->data_type == DataType::ArrayType) {
                ss << getIndent() << "ArrayType:" << std::endl;
                increaseIndent();
                
                ss << getIndent() << "Dimensions:" << std::endl;
                increaseIndent();
                
                for (const auto& range : varDecl->array_range) {
                    ss << getIndent() << "Range: " << range->begin << ".." << range->end << std::endl;
                }
                
                decreaseIndent();
                
                // 元素类型
                switch (varDecl->basic_type) {
                    case BasicType::INT: typeStr = "integer"; break;
                    case BasicType::REAL: typeStr = "real"; break;
                    case BasicType::CHAR: typeStr = "char"; break;
                    case BasicType::BOOLEAN: typeStr = "boolean"; break;
                    default: typeStr = "unknown"; break;
                }
                ss << getIndent() << "ElementType: " << typeStr << std::endl;
                
                decreaseIndent();
            }
            
            decreaseIndent();
        }
    }
    
    // 打印函数声明
    void printFunctionDeclaration(std::stringstream& ss, const FuncDeclStmt* funcDecl) {
        if (!funcDecl || !funcDecl->header) return;
        
        bool isFunction = (funcDecl->header->ret_type != BasicType::VOID);
        
        ss << getIndent() << (isFunction ? "Function" : "Procedure") << ": " << funcDecl->header->func_name << std::endl;
        increaseIndent();
        
        // 参数
        if (!funcDecl->header->args.empty()) {
            ss << getIndent() << "Parameters:" << std::endl;
            increaseIndent();
            
            for (const auto& arg : funcDecl->header->args) {
                printVarDeclaration(ss, arg.get());
            }
            
            decreaseIndent();
        }
        
        // 返回类型（只对函数）
        if (isFunction) {
            std::string typeStr;
            switch (funcDecl->header->ret_type) {
                case BasicType::INT: typeStr = "integer"; break;
                case BasicType::REAL: typeStr = "real"; break;
                case BasicType::CHAR: typeStr = "char"; break;
                case BasicType::BOOLEAN: typeStr = "boolean"; break;
                default: typeStr = "unknown"; break;
            }
            ss << getIndent() << "ReturnType: " << typeStr << std::endl;
        }
        
        // 函数体
        if (funcDecl->body) {
            ss << getIndent() << "Body:" << std::endl;
            increaseIndent();
            
            // 局部声明
            bool hasLocalDecls = (funcDecl->body->const_decl && !funcDecl->body->const_decl->pairs.empty()) || 
                               !funcDecl->body->var_decl.empty();
            
            if (hasLocalDecls) {
                ss << getIndent() << "LocalDeclarations:" << std::endl;
                increaseIndent();
                
                // 常量声明
                if (funcDecl->body->const_decl && !funcDecl->body->const_decl->pairs.empty()) {
                    printConstDeclarations(ss, funcDecl->body->const_decl.get());
                }
                
                // 变量声明
                for (const auto& varDecl : funcDecl->body->var_decl) {
                    printVarDeclaration(ss, varDecl.get());
                }
                
                decreaseIndent();
            }
            
            // 函数语句
            if (!funcDecl->body->comp_stmt.empty()) {
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
                
                for (const auto& stmt : funcDecl->body->comp_stmt) {
                    printStatement(ss, stmt.get());
                }
                
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        decreaseIndent();
    }

    // 打印语句
    void printStatement(std::stringstream& ss, const BaseStmt* stmt) {
        if (!stmt) return;
        
        // 根据语句类型调用相应的打印函数
        if (const AssignStmt* assignStmt = dynamic_cast<const AssignStmt*>(stmt)) {
            printAssignStatement(ss, assignStmt);
        } else if (const IfStmt* ifStmt = dynamic_cast<const IfStmt*>(stmt)) {
            printIfStatement(ss, ifStmt);
        } else if (const WhileStmt* whileStmt = dynamic_cast<const WhileStmt*>(stmt)) {
            printWhileStatement(ss, whileStmt);
        } else if (const ForStmt* forStmt = dynamic_cast<const ForStmt*>(stmt)) {
            printForStatement(ss, forStmt);
        } else if (const ReadFuncStmt* readStmt = dynamic_cast<const ReadFuncStmt*>(stmt)) {
            printReadStatement(ss, readStmt);
        } else if (const WriteFuncStmt* writeStmt = dynamic_cast<const WriteFuncStmt*>(stmt)) {
            printWriteStatement(ss, writeStmt);
        } else if (const FuncCallStmt* funcCallStmt = dynamic_cast<const FuncCallStmt*>(stmt)) {
            printFunctionCallStatement(ss, funcCallStmt);
        } else if (const BreakStmt* breakStmt = dynamic_cast<const BreakStmt*>(stmt)) {
            ss << getIndent() << "BreakStatement" << std::endl;
        } else if (const ContinueStmt* continueStmt = dynamic_cast<const ContinueStmt*>(stmt)) {
            ss << getIndent() << "ContinueStatement" << std::endl;
        } else {
            ss << getIndent() << "UnknownStatement" << std::endl;
        }
    }
    
    // 打印赋值语句
    void printAssignStatement(std::stringstream& ss, const AssignStmt* assignStmt) {
        if (!assignStmt) return;
        
        ss << getIndent() << "Assignment:" << std::endl;
        increaseIndent();
        
        // 目标（左值）
        ss << getIndent() << "Target:" << std::endl;
        increaseIndent();
        
        if (assignStmt->lval) {
            ss << getIndent() << "Identifier: " << assignStmt->lval->id << std::endl;
            
            // 数组索引
            if (!assignStmt->lval->array_index.empty()) {
                for (const auto& index : assignStmt->lval->array_index) {
                    ss << getIndent() << "Index:" << std::endl;
                    increaseIndent();
                    printExpression(ss, index.get());
                    decreaseIndent();
                }
            }
        }
        
        decreaseIndent();
        
        // 表达式（右值）
        ss << getIndent() << "Expression:" << std::endl;
        increaseIndent();
        printExpression(ss, assignStmt->expr.get());
        decreaseIndent();
        
        decreaseIndent();
    }
    
    // 打印if语句
    void printIfStatement(std::stringstream& ss, const IfStmt* ifStmt) {
        if (!ifStmt) return;
        
        ss << getIndent() << "IfStatement:" << std::endl;
        increaseIndent();
        
        // 条件
        ss << getIndent() << "Condition:" << std::endl;
        increaseIndent();
        printExpression(ss, ifStmt->expr.get());
        decreaseIndent();
        
        // Then部分
        if (!ifStmt->true_stmt.empty()) {
            ss << getIndent() << "Then:" << std::endl;
            increaseIndent();
            
            if (ifStmt->true_stmt.size() > 1) {
                ss << getIndent() << "CompoundStatement:" << std::endl;
                increaseIndent();
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
            }
            
            for (const auto& stmt : ifStmt->true_stmt) {
                printStatement(ss, stmt.get());
            }
            
            if (ifStmt->true_stmt.size() > 1) {
                decreaseIndent();
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        // Else部分
        if (!ifStmt->false_stmt.empty()) {
            ss << getIndent() << "Else:" << std::endl;
            increaseIndent();
            
            if (ifStmt->false_stmt.size() > 1) {
                ss << getIndent() << "CompoundStatement:" << std::endl;
                increaseIndent();
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
            }
            
            for (const auto& stmt : ifStmt->false_stmt) {
                printStatement(ss, stmt.get());
            }
            
            if (ifStmt->false_stmt.size() > 1) {
                decreaseIndent();
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        decreaseIndent();
    }
    
    // 打印while语句
    void printWhileStatement(std::stringstream& ss, const WhileStmt* whileStmt) {
        if (!whileStmt) return;
        
        ss << getIndent() << "WhileStatement:" << std::endl;
        increaseIndent();
        
        // 条件
        ss << getIndent() << "Condition:" << std::endl;
        increaseIndent();
        printExpression(ss, whileStmt->expr.get());
        decreaseIndent();
        
        // 循环体
        if (!whileStmt->stmt.empty()) {
            ss << getIndent() << "Body:" << std::endl;
            increaseIndent();
            
            if (whileStmt->stmt.size() > 1) {
                ss << getIndent() << "CompoundStatement:" << std::endl;
                increaseIndent();
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
            }
            
            for (const auto& stmt : whileStmt->stmt) {
                printStatement(ss, stmt.get());
            }
            
            if (whileStmt->stmt.size() > 1) {
                decreaseIndent();
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        decreaseIndent();
    }
    
    // 打印for语句
    void printForStatement(std::stringstream& ss, const ForStmt* forStmt) {
        if (!forStmt) return;
        
        ss << getIndent() << "ForStatement:" << std::endl;
        increaseIndent();
        
        // 循环变量
        ss << getIndent() << "LoopVariable: " << forStmt->id << std::endl;
        
        // 起始值
        ss << getIndent() << "InitialValue:" << std::endl;
        increaseIndent();
        printExpression(ss, forStmt->begin.get());
        decreaseIndent();
        
        // 结束值
        ss << getIndent() << "FinalValue:" << std::endl;
        increaseIndent();
        printExpression(ss, forStmt->end.get());
        decreaseIndent();
        
        // 循环体
        if (!forStmt->stmt.empty()) {
            ss << getIndent() << "Body:" << std::endl;
            increaseIndent();
            
            if (forStmt->stmt.size() > 1) {
                ss << getIndent() << "CompoundStatement:" << std::endl;
                increaseIndent();
                ss << getIndent() << "Statements:" << std::endl;
                increaseIndent();
            }
            
            for (const auto& stmt : forStmt->stmt) {
                printStatement(ss, stmt.get());
            }
            
            if (forStmt->stmt.size() > 1) {
                decreaseIndent();
                decreaseIndent();
            }
            
            decreaseIndent();
        }
        
        decreaseIndent();
    }
    
    // 打印read语句
    void printReadStatement(std::stringstream& ss, const ReadFuncStmt* readStmt) {
        if (!readStmt) return;
        
        ss << getIndent() << "ReadStatement:" << std::endl;
        increaseIndent();
        
        // 变量列表
        ss << getIndent() << "Variables:" << std::endl;
        increaseIndent();
        
        for (const auto& var : readStmt->lval) {
            if (var) {
                ss << getIndent() << "Identifier: " << var->id << std::endl;
                
                // 数组索引
                if (!var->array_index.empty()) {
                    for (const auto& index : var->array_index) {
                        ss << getIndent() << "Index:" << std::endl;
                        increaseIndent();
                        printExpression(ss, index.get());
                        decreaseIndent();
                    }
                }
            }
        }
        
        decreaseIndent();
        decreaseIndent();
    }
    
    // 打印write语句
    void printWriteStatement(std::stringstream& ss, const WriteFuncStmt* writeStmt) {
        if (!writeStmt) return;
        
        ss << getIndent() << "WriteStatement:" << std::endl;
        increaseIndent();
        
        // 表达式列表
        ss << getIndent() << "Expressions:" << std::endl;
        increaseIndent();
        
        for (const auto& expr : writeStmt->expr) {
            printExpression(ss, expr.get());
        }
        
        decreaseIndent();
        decreaseIndent();
    }
    
    // 打印函数调用语句
    void printFunctionCallStatement(std::stringstream& ss, const FuncCallStmt* funcCallStmt) {
        if (!funcCallStmt) return;
        
        ss << getIndent() << "FunctionCall: " << funcCallStmt->id << std::endl;
        increaseIndent();
        
        // 参数列表
        if (!funcCallStmt->args.empty()) {
            ss << getIndent() << "Arguments:" << std::endl;
            increaseIndent();
            
            for (const auto& arg : funcCallStmt->args) {
                printExpression(ss, arg.get());
            }
            
            decreaseIndent();
        }
        
        decreaseIndent();
    }
    
    // 打印表达式
    void printExpression(std::stringstream& ss, const ExprStmt* exprStmt) {
        if (!exprStmt || !exprStmt->rel_expr) return;
        
        const RelExprStmt* relExpr = exprStmt->rel_expr.get();
        
        // 单项关系表达式
        if (relExpr->terms.size() == 1) {
            printAddExpression(ss, relExpr->terms[0].add_expr.get());
            return;
        }
        
        // 双项或多项关系表达式
        std::string opStr;
        switch (relExpr->terms[1].type) {
            case RelExprStmt::RelExprType::Equal: opStr = "="; break;
            case RelExprStmt::RelExprType::NotEqual: opStr = "<>"; break;
            case RelExprStmt::RelExprType::Less: opStr = "<"; break;
            case RelExprStmt::RelExprType::LessEqual: opStr = "<="; break;
            case RelExprStmt::RelExprType::Greater: opStr = ">"; break;
            case RelExprStmt::RelExprType::GreaterEqual: opStr = ">="; break;
            case RelExprStmt::RelExprType::In: opStr = "in"; break;
            default: opStr = "?"; break;
        }
        
        ss << getIndent() << "BinaryExpr: " << opStr << std::endl;
        increaseIndent();
        
        printAddExpression(ss, relExpr->terms[0].add_expr.get());
        printAddExpression(ss, relExpr->terms[1].add_expr.get());
        
        decreaseIndent();
    }
    
    // 打印加法表达式
    void printAddExpression(std::stringstream& ss, const AddExprStmt* addExpr) {
        if (!addExpr) return;
        
        // 单项加法表达式
        if (addExpr->terms.size() == 1) {
            printMulExpression(ss, addExpr->terms[0].mul_expr.get());
            return;
        }
        
        // 双项或多项加法表达式
        std::string opStr;
        switch (addExpr->terms[1].type) {
            case AddExprStmt::AddExprType::Plus: opStr = "+"; break;
            case AddExprStmt::AddExprType::Minus: opStr = "-"; break;
            case AddExprStmt::AddExprType::Or: opStr = "or"; break;
            default: opStr = "?"; break;
        }
        
        ss << getIndent() << "BinaryExpr: " << opStr << std::endl;
        increaseIndent();
        
        printMulExpression(ss, addExpr->terms[0].mul_expr.get());
        printMulExpression(ss, addExpr->terms[1].mul_expr.get());
        
        decreaseIndent();
    }
    
    // 打印乘法表达式
    void printMulExpression(std::stringstream& ss, const MulExprStmt* mulExpr) {
        if (!mulExpr) return;
        
        // 单项乘法表达式
        if (mulExpr->terms.size() == 1) {
            printUnaryExpression(ss, mulExpr->terms[0].unary_expr.get());
            return;
        }
        
        // 双项或多项乘法表达式
        std::string opStr;
        switch (mulExpr->terms[1].type) {
            case MulExprStmt::MulExprType::Mul: opStr = "*"; break;
            case MulExprStmt::MulExprType::Div: opStr = "/"; break;
            case MulExprStmt::MulExprType::Mod: opStr = "mod"; break;
            case MulExprStmt::MulExprType::And: opStr = "and"; break;
            case MulExprStmt::MulExprType::AndThen: opStr = "and then"; break;
            default: opStr = "?"; break;
        }
        
        ss << getIndent() << "BinaryExpr: " << opStr << std::endl;
        increaseIndent();
        
        printUnaryExpression(ss, mulExpr->terms[0].unary_expr.get());
        printUnaryExpression(ss, mulExpr->terms[1].unary_expr.get());
        
        decreaseIndent();
    }
    
    // 打印一元表达式
    void printUnaryExpression(std::stringstream& ss, const UnaryExprStmt* unaryExpr) {
        if (!unaryExpr || !unaryExpr->primary_expr) return;
        
        // 没有一元操作符
        if (unaryExpr->types.empty() || unaryExpr->types[0] == UnaryExprStmt::UnaryExprType::NULL_TYPE) {
            printPrimaryExpression(ss, unaryExpr->primary_expr.get());
            return;
        }
        
        // 有一元操作符
        std::string opStr;
        switch (unaryExpr->types[0]) {
            case UnaryExprStmt::UnaryExprType::Not: opStr = "not"; break;
            case UnaryExprStmt::UnaryExprType::Minus: opStr = "-"; break;
            default: opStr = "?"; break;
        }
        
        ss << getIndent() << "UnaryExpr: " << opStr << std::endl;
        increaseIndent();
        
        printPrimaryExpression(ss, unaryExpr->primary_expr.get());
        
        decreaseIndent();
    }
    
    // 打印基本表达式
    void printPrimaryExpression(std::stringstream& ss, const PrimaryExprStmt* primaryExpr) {
        if (!primaryExpr) return;
        
        // 值
        if (primaryExpr->type == PrimaryExprStmt::PrimaryExprType::Value && primaryExpr->value) {
            printValue(ss, primaryExpr->value.get());
        } 
        // 括号内的表达式
        else if (primaryExpr->type == PrimaryExprStmt::PrimaryExprType::Parentheses && primaryExpr->expr) {
            ss << getIndent() << "ParenExpr:" << std::endl;
            increaseIndent();
            
            printExpression(ss, primaryExpr->expr.get());
            
            decreaseIndent();
        }
    }
    
    // 打印值
    void printValue(std::stringstream& ss, const ValueStmt* valueStmt) {
        if (!valueStmt) return;
        
        // 数字
        if (valueStmt->type == ValueStmt::ValueType::Number && valueStmt->number) {
            if (valueStmt->number->is_real) {
                ss << getIndent() << "Literal: REAL " << valueStmt->number->real_val << std::endl;
            } else if (valueStmt->number->is_char) {
                ss << getIndent() << "Literal: CHAR '" << valueStmt->number->char_val << "'" << std::endl;
            } else {
                ss << getIndent() << "Literal: INTEGER " << valueStmt->number->int_val << std::endl;
            }
        } 
        // 字符串
        else if (valueStmt->type == ValueStmt::ValueType::Str && valueStmt->str) {
            ss << getIndent() << "Literal: STRING '" << valueStmt->str->val << "'" << std::endl;
        } 
        // 左值
        else if (valueStmt->type == ValueStmt::ValueType::LVal && valueStmt->lval) {
            ss << getIndent() << "Identifier: " << valueStmt->lval->id << std::endl;
            
            // 数组索引
            if (!valueStmt->lval->array_index.empty()) {
                for (const auto& index : valueStmt->lval->array_index) {
                    ss << getIndent() << "Index:" << std::endl;
                    increaseIndent();
                    printExpression(ss, index.get());
                    decreaseIndent();
                }
            }
        } 
        // 函数调用
        else if (valueStmt->type == ValueStmt::ValueType::FuncCall && valueStmt->func_call) {
            ss << getIndent() << "FunctionCall: " << valueStmt->func_call->id << std::endl;
            
            // 参数列表
            if (!valueStmt->func_call->args.empty()) {
                increaseIndent();
                ss << getIndent() << "Arguments:" << std::endl;
                increaseIndent();
                
                for (const auto& arg : valueStmt->func_call->args) {
                    printExpression(ss, arg.get());
                }
                
                decreaseIndent();
                decreaseIndent();
            }
        }
    }
};