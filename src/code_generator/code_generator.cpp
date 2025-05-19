#include "code_generator/code_generator.hpp"
#include <algorithm>
#include <iostream>
#include <iomanip>

namespace codegen {

// 构造函数，初始化类型映射
CodeGenerator::CodeGenerator(const semantic::SemanticAnalyzer& s, semantic::SymbolTable& st)
        : sema(s), symbolTable(st) { initTypeMapping(); }


// 析构函数
CodeGenerator::~CodeGenerator() {
}

// 初始化Pascal类型到C类型的映射表
void CodeGenerator::initTypeMapping() {
    typeMapping["int"] = "int";
    typeMapping["float"] = "float";
    typeMapping["char"] = "char";
    typeMapping["bool"] = "int"; // Pascal的布尔类型映射到C的int
    typeMapping["string"] = "char*";
    typeMapping["void"] = "void";
}

// 获取当前缩进字符串
std::string CodeGenerator::getIndent() const {
    return std::string(indentLevel * 4, ' ');
}

// Pascal类型映射到C类型
std::string CodeGenerator::mapPascalTypeToC(const std::string& pascalType) const {
    auto it = typeMapping.find(pascalType);
    if (it != typeMapping.end()) {
        return it->second;
    }
    // 对于未知类型，默认返回void*
    return "void*";
}

// Pascal操作符映射到C操作符
std::string CodeGenerator::mapOperator(const std::string& pascalOp) const {
    if (pascalOp == "=") return "==";
    if (pascalOp == "<>") return "!=";
    if (pascalOp == "and") return "&&";
    if (pascalOp == "or") return "||";
    if (pascalOp == "not") return "!";
    if (pascalOp == "mod") return "%";
    if (pascalOp == "div") return "/";
    return pascalOp;
}

// 生成C代码主入口
std::string CodeGenerator::generate(ProgramNode* root) {
    // 重置代码生成器状态
    code.str("");
    code.clear();
    indentLevel = 0;
    currentFunction = "";
    hasReturn = false;

    // 头文件
    code << "#include <stdio.h>\n";
    code << "#include <stdlib.h>\n";
    code << "#include <sys/types.h>\n";
    code << "#include <sys/wait.h>\n";
    code << "#include <unistd.h>\n\n";

    // 遍历AST生成代码
    if (root) {
        root->accept(*this);
    }
    
    // 返回生成的代码字符串
    return code.str();
}

// 输出生成的C代码到文件
void CodeGenerator::output(std::ofstream& out) {
    // 在输出前验证C代码是否合法（基本验证）
    if (!validateGeneratedCode()) {
        // 代码可能存在结构问题，进行清理
        cleanupGeneratedCode();
    }
    
    // 将代码写入输出流
    out << code.str();
}

// 验证生成的C代码括号是否平衡
bool CodeGenerator::validateGeneratedCode() {
    std::string codeStr = code.str();
    
    // 检查基本括号平衡
    int parenBalance = 0, braceBalance = 0, bracketBalance = 0;
    for (char c : codeStr) {
        if (c == '(') parenBalance++;
        else if (c == ')') parenBalance--;
        else if (c == '{') braceBalance++;
        else if (c == '}') braceBalance--;
        else if (c == '[') bracketBalance++;
        else if (c == ']') bracketBalance--;
        
        // 如果任何括号不平衡，则认为代码有问题
        if (parenBalance < 0 || braceBalance < 0 || bracketBalance < 0) {
            return false;
        }
    }
    
    // 最终所有括号应该平衡
    return parenBalance == 0 && braceBalance == 0 && bracketBalance == 0;
}

// 修复生成代码中常见的结构性问题
void CodeGenerator::cleanupGeneratedCode() {
    std::string codeStr = code.str();
    std::string cleanCode;
    std::string currentFunction = "";
    bool inFunction = false;
    int braceCount = 0;
    std::istringstream stream(codeStr);
    std::string line;
    
    // 逐行处理
    while (std::getline(stream, line)) {
        // 检查函数定义行
        if (line.find("int main()") != std::string::npos || 
            line.find("void ") != std::string::npos || 
            line.find("int ") != std::string::npos && line.find("(") != std::string::npos && line.find(")") != std::string::npos) {
            
            // 如果之前存在未闭合的函数，闭合它
            if (inFunction && braceCount > 0) {
                for (int i = 0; i < braceCount; i++) {
                    cleanCode += "}\n";
                }
                braceCount = 0;
            }
            inFunction = true;
            braceCount = 0;
        }
        
        // 追踪花括号
        for (char c : line) {
            if (c == '{') braceCount++;
            else if (c == '}') braceCount--;
        }
        
        // 保留合法的行
        cleanCode += line + "\n";
    }
    
    // 确保所有函数都正确闭合
    if (inFunction && braceCount > 0) {
        for (int i = 0; i < braceCount; i++) {
            cleanCode += "}\n";
        }
    }
    
    // 更新代码字符串
    code.str(cleanCode);
    code.clear();
}

// 表达式节点访问
void CodeGenerator::visit(ExprNode &stmt) {
    if (stmt.rel_expr) {
        stmt.rel_expr->accept(*this);
    }
}

// 关系表达式节点访问
void CodeGenerator::visit(RelExprNode &stmt) {
    bool firstTerm = true;
    
    // 如果有多个关系操作符，用括号括起来以保证正确优先级
    bool needParentheses = stmt.terms.size() > 1;
    if (needParentheses) {
        code << "(";
    }
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case RelExprNode::RelExprType::Equal:
                    code << " == ";
                    break;
                case RelExprNode::RelExprType::NotEqual:
                    code << " != ";
                    break;
                case RelExprNode::RelExprType::Less:
                    code << " < ";
                    break;
                case RelExprNode::RelExprType::LessEqual:
                    code << " <= ";
                    break;
                case RelExprNode::RelExprType::Greater:
                    code << " > ";
                    break;
                case RelExprNode::RelExprType::GreaterEqual:
                    code << " >= ";
                    break;
                default:
                    break;
            }
        }
        term.add_expr->accept(*this);
        firstTerm = false;
    }
    if (needParentheses) {
        code << ")";
    }
}

// 加法表达式节点访问
void CodeGenerator::visit(AddExprNode &stmt) {
    bool firstTerm = true;
    
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case AddExprNode::AddExprType::Plus:
                    code << " + ";
                    break;
                case AddExprNode::AddExprType::Minus:
                    code << " - ";
                    break;
                case AddExprNode::AddExprType::Or:
                    code << " || ";
                    break;
                default:
                    break;
            }
        }
        
        // 处理类似 +b 这样的一元加法操作符
        if (firstTerm && term.type == AddExprNode::AddExprType::Plus) {
            // 这是一个特殊情况，忽略一元加号，因为它在C中没有效果
        }
        
        term.mul_expr->accept(*this);
        firstTerm = false;
    }
}

// 乘法表达式节点访问
void CodeGenerator::visit(MulExprNode &stmt) {
    bool firstTerm = true;
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case MulExprNode::MulExprType::Mul:
                    code << " * ";
                    break;
                case MulExprNode::MulExprType::Div:
                    code << " / ";
                    break;
                case MulExprNode::MulExprType::Mod:
                    code << " % ";
                    break;
                case MulExprNode::MulExprType::And:
                    code << " && ";
                    break;
                default:
                    break;
            }
        }
        term.unary_expr->accept(*this);
        firstTerm = false;
    }
}

// 一元表达式节点访问
void CodeGenerator::visit(UnaryExprNode &stmt) {
    // 如果没有一元操作符，直接处理表达式
    if (stmt.types.empty()) {
        stmt.primary_expr->accept(*this);
        return;
    }
    
    // 使用临时字符串而不是捕获stringstream状态
    std::string exprStr;
    {
        // 临时stringstream用于捕获表达式
        std::stringstream tempStream;
        // 交换当前stream和临时stream
        std::swap(code, tempStream);
        
        // 处理内部表达式并获取其字符串表示
        stmt.primary_expr->accept(*this);
        exprStr = code.str();
        
        // 恢复原始流
        std::swap(code, tempStream);
    }
    
    // 从右到左应用一元操作符
    for (size_t i = 0; i < stmt.types.size(); ++i) {
        auto type = stmt.types[i];
        switch (type) {
            case UnaryExprNode::UnaryExprType::Not:
                exprStr = "~(" + exprStr + ")";
                break;
            case UnaryExprNode::UnaryExprType::Minus:
                exprStr = "-(" + exprStr + ")";
                break;
            default:
                break;
        }
    }
    
    // 输出最终表达式
    code << exprStr;
}

// 基本表达式节点访问
void CodeGenerator::visit(PrimaryExprNode &stmt) {
    if (stmt.type == PrimaryExprNode::PrimaryExprType::Value) {
        stmt.value->accept(*this);
    } else if (stmt.type == PrimaryExprNode::PrimaryExprType::Parentheses) {
        code << "(";
        stmt.expr->accept(*this);
        code << ")";
    }
}

// 常量/变量/函数调用等值节点访问
void CodeGenerator::visit(ValueNode &stmt) {
    switch (stmt.type) {
        case ValueNode::ValueType::Number:
            stmt.number->accept(*this);
            break;
        case ValueNode::ValueType::Str:
            stmt.str->accept(*this);
            break;
        case ValueNode::ValueType::LVal:
            stmt.lval->accept(*this);
            break;
        case ValueNode::ValueType::FuncCall:
            stmt.func_call->accept(*this);
            break;
        default:
            break;
    }
}

// 数字常量节点访问
void CodeGenerator::visit(NumberNode &stmt) {
    if (stmt.is_real) {
        code << stmt.real_val;
    } else if (stmt.is_char) {
        code << "'" << stmt.char_val << "'";
    } else {
        code << stmt.int_val;
    }
}

// 字符串常量节点访问
void CodeGenerator::visit(StringNode &stmt) {
    code << "\"" << stmt.val << "\"";
}

// 左值节点访问（变量、数组、函数名）
void CodeGenerator::visit(LValueNode &stmt) {
    semantic::SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (entry && entry->type == semantic::SymbolType::FUNCTION && stmt.array_index.empty()) {
        code << stmt.id << "()";
    } else {
        code << stmt.id;
        for (auto& index : stmt.array_index) {
            code << "[";
            emitExpr(index.get());
            code << "]";
        }
        if (entry && entry->isReference) {
            code.str("(*" + code.str() + ")");
        }
    }
}

// 函数调用节点访问
void CodeGenerator::visit(FuncCallNode &stmt) {
    bool inlineExpr = exprDepth > 0;
    if (!inlineExpr) {
        code << getIndent();
    }
    code << stmt.id << "(";
    for (size_t i = 0; i < stmt.args.size(); ++i) {
        if (i) code << ", ";
        emitExpr(stmt.args[i].get());
    }
    code << ")";
    if (!inlineExpr) code << ";\n";
}

// 数组区间节点访问
void CodeGenerator::visit(PeriodNode &stmt) {
    code << (stmt.end - stmt.begin + 1);
}

// 常量声明节点访问
void CodeGenerator::visit(ConstDeclNode &stmt) {
    for (auto& pair : stmt.pairs) {
        const std::string& name = pair.first;
        ValueNode* value = pair.second.get();
        code << getIndent() << "const ";

        // 确定常量类型
        if (value->type == ValueNode::ValueType::Number) {
            NumberNode* num = value->number.get();
            if (num->is_real) {
                // 保留全部小数位，使用 double 类型
                code << "double ";
            } else if (num->is_char) {
                code << "char ";
            } else {
                code << "int ";
            }
        } else if (value->type == ValueNode::ValueType::Str) {
            code << "char* ";
        }

        code << name << " = ";
        // 对于实数常量，保留全部小数位
        if (value->type == ValueNode::ValueType::Number) {
            NumberNode* num = value->number.get();
            if (num->is_real) {
                // 使用字符串流保证精度不丢失
                code << std::setprecision(16) << num->real_val;
            } else if (num->is_char) {
                code << "'" << num->char_val << "'";
            } else {
                code << num->int_val;
            }
        } else {
            value->accept(*this);
        }
        code << ";\n";
    }
}

// 变量声明节点访问
void CodeGenerator::visit(VarDeclNode &stmt) {
    std::string cType = mapPascalTypeToC(semantic::basicTypeToString(stmt.basic_type));
    for (const auto& name : stmt.id) {
        code << getIndent() << cType << " ";
        if (stmt.is_var) {
            code << "*"; // 引用参数在C中使用指针
        }
        code << name;
        
        // 处理数组类型
        if (stmt.data_type == DataType::ArrayType) {
            for (const auto& range : stmt.array_range) {
                code << "[" << (range->end - range->begin + 1) << "]";
            }
        }
        
        // 为基本类型添加初始值，但不为数组类型添加（避免语法错误）
        if (stmt.data_type != DataType::ArrayType) {
            semantic::SymbolEntry* entry = symbolTable.lookupSymbol(name);
            if (entry && entry->type == semantic::SymbolType::VARIABLE) {
                // 根据类型添加默认初始化值
                if (entry->dataType == "int" || entry->dataType == "bool") {
                    code << " = 0";
                } else if (entry->dataType == "float" || entry->dataType == "real") {
                    code << " = 0.0";
                } else if (entry->dataType == "char") {
                    code << " = '\\0'";
                }
            }
        }
        code << ";\n";
    }
}

// 函数声明节点访问
void CodeGenerator::visit(FuncDeclNode &stmt) {
    if (stmt.header) {
        stmt.header->accept(*this);
    }
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

// 函数头节点访问
void CodeGenerator::visit(FuncHeadDeclNode &stmt) {
    currentFunction = stmt.func_name;
    hasReturn = false;
    
    // 查找函数信息以获取准确的返回类型
    semantic::SymbolEntry* funcEntry = symbolTable.lookupSymbol(stmt.func_name);
    std::string returnType;
    
    if (funcEntry && !funcEntry->returnType.empty()) {
        returnType = mapPascalTypeToC(funcEntry->returnType);
    } else {
        // 回退到从语法中推导类型
        returnType = mapPascalTypeToC(semantic::basicTypeToString(stmt.ret_type));
    }
    
    code << returnType << " " << stmt.func_name << "(";
    
    // 处理函数参数
    bool firstParam = true; // 标记是否是第一个参数
    for (const auto& arg : stmt.args) {
        for (const auto& arg_name : arg->id) {
            if (!firstParam) {
                code << ", ";
            }
            
            std::string paramType = mapPascalTypeToC(semantic::basicTypeToString(arg->basic_type));
            code << paramType << " ";
            
            if (arg->is_var) {
                code << "*"; // 引用参数在C中使用指针
            }
            code << arg_name;
            firstParam = false;
        }
    }
    code << ")";
}

// 函数体节点访问
void CodeGenerator::visit(FuncBodyDeclNode &stmt) {
    code << " {\n";
    indentLevel++;

    // 处理常量声明
    if (stmt.const_decl) {
        stmt.const_decl->accept(*this);
    }

    // 处理变量声明
    for (const auto& varDecl : stmt.var_decl) {
        varDecl->accept(*this);
    }

    // 检查变量声明区是否已经有 res
    semantic::SymbolEntry* entry = symbolTable.lookupSymbol(currentFunction);
    bool hasRes = false;
    for (const auto& varDecl : stmt.var_decl) {
        for (const auto& name : varDecl->id) {
            if (name == "res") {
                hasRes = true;
                break;
            }
        }
        if (hasRes) break;
    }

    // 只有没有 res 时才声明
    if (entry && entry->returnType != "void" && !hasRes) {
        code << getIndent() << mapPascalTypeToC(entry->returnType) << " res;\n";
    }

    // 处理函数体语句
    for (size_t idx = 0; idx < stmt.comp_stmt.size(); ++idx) {
        isLastStmtInFuncBody = (idx == stmt.comp_stmt.size() - 1);
        stmt.comp_stmt[idx]->accept(*this);
    }
    isLastStmtInFuncBody = false;

    // 如果有返回值且没有return，自动补return
    if (entry && entry->returnType != "void" && !hasReturn) {
        code << getIndent() << "return res;\n";
    }

    indentLevel--;
    code << getIndent() << "}\n\n";
    currentFunction = "";
    hasReturn = false;
}



// 赋值语句节点访问
void CodeGenerator::visit(AssignmentNode &stmt) {
    code << getIndent();

    // 如果是函数名赋值，赋给res
    if (stmt.lval->id == currentFunction) {
        code << "res = ";
        emitExpr(stmt.expr.get());
        code << ";\n";
        return;
    }
    stmt.lval->accept(*this);
    code << " = ";
    emitExpr(stmt.expr.get());                      // 右值
    code << ";\n";
}


// if语句节点访问
void CodeGenerator::visit(IfNode &stmt) {
    code << getIndent() << "if (";
    emitExpr(stmt.expr.get());                      // 条件表达式
    code << ") {\n";
    ++indentLevel;
    for (auto& s : stmt.true_stmt) s->accept(*this);
    --indentLevel;
    code << getIndent() << "}";
    if (!stmt.false_stmt.empty()) {
        code << " else {\n";
        ++indentLevel;
        for (auto& s : stmt.false_stmt) s->accept(*this);
        --indentLevel;
        code << getIndent() << "}";
    }
    code << "\n";
}

// for语句节点访问
void CodeGenerator::visit(ForNode &stmt) {
    code << getIndent() << "for (";
    code << stmt.id << " = ";
    emitExpr(stmt.begin.get());                     // 起点
    code << "; ";
    code << stmt.id << " <= ";
    emitExpr(stmt.end.get());                       // 终点
    code << "; ";
    code << stmt.id << "++) {\n";
    bool oldInLoop = isInLoop;
    isInLoop = true;
    ++indentLevel;
    for (auto& s : stmt.stmt) s->accept(*this);
    --indentLevel;
    isInLoop = oldInLoop;
    code << getIndent() << "}\n";
}

// while语句节点访问
void CodeGenerator::visit(WhileNode &stmt) {
    code << getIndent() << "while (";
    emitExpr(stmt.expr.get());                      // 条件
    code << ") {\n";
    bool oldInLoop = isInLoop;
    isInLoop = true;
    ++indentLevel;
    for (auto& s : stmt.stmt) s->accept(*this);
    --indentLevel;
    isInLoop = oldInLoop;
    code << getIndent() << "}\n";
}

// 读语句节点访问
void CodeGenerator::visit(ReadFuncNode &stmt) {
    for (const auto& lValueNode : stmt.lval) {
        std::string fmt = "%d";
        semantic::SymbolEntry* entry = symbolTable.lookupSymbol(lValueNode->id);
        if (lValueNode->id == currentFunction) {
            // 是函数返回值，判断 returnType
            if (entry) {
                if (entry->returnType == "float" || entry->returnType == "real" ) {
                    fmt = "%f";
                } else if (entry->returnType == "char") {
                    fmt = "%c";
                } else if (entry->returnType == "string") {
                    fmt = "%s";
                } else if (entry->returnType == "int" || entry->returnType == "bool") {
                    fmt = "%d";
                }
            }
            code << getIndent() << "scanf(\"" << fmt << "\", &res);\n";
        } else {
            // 普通变量，判断 dataType
            if (entry) {
                if (entry->dataType == "float" || entry->dataType == "real" ) {
                    fmt = "%f";
                } else if (entry->dataType == "char") {
                    fmt = "%c";
                } else if (entry->dataType == "string") {
                    fmt = "%s";
                } else if (entry->dataType == "int" || entry->dataType == "bool") {
                    fmt = "%d";
                }
            }
            code << getIndent() << "scanf(\"" << fmt << "\", &";
            lValueNode->accept(*this);
            code << ");\n";
        }
    }
}

// 写语句节点访问
void CodeGenerator::visit(WriteFuncNode &stmt) {
    code << getIndent() << "printf(\"";
    for (auto& e : stmt.expr)
        code << type2fmt(sema.getType(e.get()));    // sema 为 SemanticAnalyzer 引用

    code << "\"";
    for (auto& e : stmt.expr) {
        code << ", ";
        emitExpr(e.get());                          // 参数表达式
    }
    code << ");\n";
}

// 类型到printf格式字符串的映射
std::string CodeGenerator::type2fmt(const std::string& t) const {
    if (t == "int" || t == "bool") return "%d";
    if (t == "float" || t == "real") return "%.6f"; // 保证6位小数
    if (t == "char") return "%c";
    if (t == "string") return "%s";
    return "%d"; // 默认返回整数格式
}


// break语句节点访问
void CodeGenerator::visit(BreakNode &stmt) {
    if (isInLoop) {
        code << getIndent() << "break;\n";
    } else {
        code << getIndent() << "// Error: break outside of loop\n";
    }
}

// continue语句节点访问
void CodeGenerator::visit(ContinueNode &stmt) {
    if (isInLoop) {
        code << getIndent() << "continue;\n";
    } else {
        code << getIndent() << "// Error: continue outside of loop\n";
    }
}

// 程序节点访问
void CodeGenerator::visit(ProgramNode &stmt) {
    if (stmt.head) {
        stmt.head->accept(*this);
    }
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

// 程序头节点访问
void CodeGenerator::visit(ProgramHeadNode &stmt) {
    // 程序头不生成代码
}

// 程序体节点访问
void CodeGenerator::visit(ProgramBodyNode &stmt) {
    // 处理全局常量
    if (stmt.const_decl) {
        stmt.const_decl->accept(*this);
    }
    
    // 处理全局变量
    for (const auto& varDecl : stmt.var_decl) {
        varDecl->accept(*this);
    }
    
    code << "\n";
    
    // 处理函数声明
    for (const auto& funcDecl : stmt.func_decl) {
        funcDecl->accept(*this);
    }
    
    // 生成main函数
    code << "int main() {\n";
    indentLevel++;
    
    // 处理主程序语句
    for (const auto& compStmt : stmt.comp_stmt) {
        compStmt->accept(*this);
    }
    
    // 添加默认返回值
    code << getIndent() << "return 0;\n";
    
    indentLevel--;
    code << "}\n";
}



// 表达式递归输出辅助
void CodeGenerator::emitExpr(ExprNode* e) {
    ++exprDepth;
    e->accept(*this);
    --exprDepth;
}

}
