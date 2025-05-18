#include "code_generator/code_generator.hpp"
#include <algorithm>
#include <iostream>
#include <iomanip>

namespace codegen {

CodeGenerator::CodeGenerator(const semantic::SemanticAnalyzer& s, semantic::SymbolTable& st)
        : sema(s), symbolTable(st) { initTypeMapping(); }
bool CodeGenerator::isVariableUsedInScope(const std::string& varName) {
    semantic::SymbolEntry* entry = symbolTable.lookupSymbol(varName);
    if (!entry) {
        return false;
    }
    
    // 检查变量是否被使用（简化版，完整版可以检查usedLines）
    return !entry->usedLines.empty();
}

void CodeGenerator::trackVariableUse(const std::string& varName) {
    // 记录变量在当前作用域被使用
    currentVars.insert(varName);
}

CodeGenerator::~CodeGenerator() {
}

void CodeGenerator::initTypeMapping() {
    typeMapping["int"] = "int";
    typeMapping["float"] = "float";
    typeMapping["char"] = "char";
    typeMapping["bool"] = "int"; // Pascal的布尔类型映射到C的int
    typeMapping["string"] = "char*";
    typeMapping["void"] = "void";
}

std::string CodeGenerator::getIndent() const {
    return std::string(indentLevel * 4, ' ');
}

std::string CodeGenerator::mapPascalTypeToC(const std::string& pascalType) const {
    auto it = typeMapping.find(pascalType);
    if (it != typeMapping.end()) {
        return it->second;
    }
    // 对于未知类型，默认返回void*
    return "void*";
}

std::string CodeGenerator::mapOperator(const std::string& pascalOp) const {
    if (pascalOp == "=") return "==";
    if (pascalOp == "<>") return "!=";
    if (pascalOp == "and") return "&&";
    if (pascalOp == "or") return "||";
    if (pascalOp == "not") return "!";
    if (pascalOp == "mod") return "%";
    if (pascalOp == "div") return "/";
    return pascalOp; // 其他操作符保持不变
}

std::string CodeGenerator::generate(ProgramStmt* root) {
    // 重置代码生成器状态
    code.str("");
    code.clear();
    indentLevel = 0;
    currentFunction = "";
    hasReturn = false;
    currentVars.clear();
    
    // 生成头文件包含语句
    code << "#include <stdio.h>\n";
    code << "#include <stdlib.h>\n";
    code << "#include <sys/types.h>\n";
    code << "#include <sys/wait.h>\n";
    code << "#include <unistd.h>\n\n";
    
    // 访问AST生成代码
    if (root) {
        root->accept(*this);
    }
    
    // 返回生成的代码字符串
    return code.str();
}

void CodeGenerator::output(std::ofstream& out) {
    // 在输出前验证C代码是否合法（基本验证）
    if (!validateGeneratedCode()) {
        // 代码可能存在结构问题，进行清理
        cleanupGeneratedCode();
    }
    
    // 将代码写入输出流
    out << code.str();
}

// 对生成的代码进行基本验证
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

// 清理生成的代码，修复常见问题
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

// 访问者方法实现
void CodeGenerator::visit(ExprStmt &stmt) {
    if (stmt.rel_expr) {
        stmt.rel_expr->accept(*this);
    }
}

void CodeGenerator::visit(RelExprStmt &stmt) {
    bool firstTerm = true;
    
    // 如果有多个关系操作符，用括号括起来以保证正确优先级
    bool needParentheses = stmt.terms.size() > 1;
    
    if (needParentheses) {
        code << "(";
    }
    
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case RelExprStmt::RelExprType::Equal:
                    code << " == ";
                    break;
                case RelExprStmt::RelExprType::NotEqual:
                    code << " != ";
                    break;
                case RelExprStmt::RelExprType::Less:
                    code << " < ";
                    break;
                case RelExprStmt::RelExprType::LessEqual:
                    code << " <= ";
                    break;
                case RelExprStmt::RelExprType::Greater:
                    code << " > ";
                    break;
                case RelExprStmt::RelExprType::GreaterEqual:
                    code << " >= ";
                    break;
                case RelExprStmt::RelExprType::In:
                    // 'in'操作符需要特殊处理，这里简化为包含检查
                    code << " /* in */ ";
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

void CodeGenerator::visit(AddExprStmt &stmt) {
    bool firstTerm = true;
    
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case AddExprStmt::AddExprType::Plus:
                    code << " + ";
                    break;
                case AddExprStmt::AddExprType::Minus:
                    code << " - ";
                    break;
                case AddExprStmt::AddExprType::Or:
                    code << " || ";
                    break;
                default:
                    break;
            }
        }
        
        // 处理类似 +b 这样的一元加法操作符
        if (firstTerm && term.type == AddExprStmt::AddExprType::Plus) {
            // 这是一个特殊情况，忽略一元加号，因为它在C中没有效果
        }
        
        term.mul_expr->accept(*this);
        firstTerm = false;
    }
}

void CodeGenerator::visit(MulExprStmt &stmt) {
    bool firstTerm = true;
    for (auto& term : stmt.terms) {
        if (!firstTerm) {
            switch (term.type) {
                case MulExprStmt::MulExprType::Mul:
                    code << " * ";
                    break;
                case MulExprStmt::MulExprType::Div:
                    code << " / ";
                    break;
                case MulExprStmt::MulExprType::Mod:
                    code << " % ";
                    break;
                case MulExprStmt::MulExprType::And:
                    code << " && ";
                    break;
                case MulExprStmt::MulExprType::AndThen:
                    code << " && /* and then */ ";
                    break;
                default:
                    break;
            }
        }
        term.unary_expr->accept(*this);
        firstTerm = false;
    }
}

void CodeGenerator::visit(UnaryExprStmt &stmt) {
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
            case UnaryExprStmt::UnaryExprType::Not:
                exprStr = "~(" + exprStr + ")";
                break;
            case UnaryExprStmt::UnaryExprType::Minus:
                exprStr = "-(" + exprStr + ")";
                break;
            default:
                break;
        }
    }
    
    // 输出最终表达式
    code << exprStr;
}

void CodeGenerator::visit(PrimaryExprStmt &stmt) {
    if (stmt.type == PrimaryExprStmt::PrimaryExprType::Value) {
        stmt.value->accept(*this);
    } else if (stmt.type == PrimaryExprStmt::PrimaryExprType::Parentheses) {
        code << "(";
        stmt.expr->accept(*this);
        code << ")";
    }
}

void CodeGenerator::visit(ValueStmt &stmt) {
    switch (stmt.type) {
        case ValueStmt::ValueType::Number:
            stmt.number->accept(*this);
            break;
        case ValueStmt::ValueType::Str:
            stmt.str->accept(*this);
            break;
        case ValueStmt::ValueType::LVal:
            stmt.lval->accept(*this);
            break;
        case ValueStmt::ValueType::FuncCall:
            stmt.func_call->accept(*this);
            break;
        default:
            break;
    }
}

void CodeGenerator::visit(NumberStmt &stmt) {
    if (stmt.is_real) {
        code << stmt.real_val;
    } else if (stmt.is_char) {
        code << "'" << stmt.char_val << "'";
    } else {
        code << stmt.int_val;
    }
}

void CodeGenerator::visit(StrStmt &stmt) {
    code << "\"" << stmt.val << "\"";
}

void CodeGenerator::visit(LValStmt &stmt) {
    // 查找是否是函数名
    semantic::SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    
    // 如果是函数名且不是数组访问，则需要生成函数调用
    if (entry && entry->type == semantic::SymbolType::FUNCTION && stmt.array_index.empty()) {
        code << stmt.id << "()";  // 添加括号调用函数
    } else {
        code << stmt.id;
        
        // 处理数组访问
        for (auto& index : stmt.array_index) {
            code << "[";
            index->accept(*this);
            code << "]";
        }
        
        // 如果是引用类型参数，生成时需要解引用
        if (entry && entry->isReference) {
            code.str("(*" + code.str() + ")");
        }
    }
}

void CodeGenerator::visit(FuncCallStmt &stmt) {
    bool inlineExpr = exprDepth > 0;               // 判断上下文

    if (!inlineExpr) code << getIndent();          // 只有语句上下文才加缩进

    /* 输出函数名和参数 */
    code << stmt.id << "(";
    for (size_t i = 0; i < stmt.args.size(); ++i) {
        if (i) code << ", ";
        emitExpr(stmt.args[i].get());              // 用 emitExpr 递归
    }
    code << ")";

    /* 语句上下文 → 结尾补 ; 与换行 */
    if (!inlineExpr) code << ";\n";
}



void CodeGenerator::visit(PeriodStmt &stmt) {
    code << (stmt.end - stmt.begin + 1);
}

void CodeGenerator::visit(ConstDeclStmt &stmt) {
    for (auto& pair : stmt.pairs) {
        const std::string& name = pair.first;
        ValueStmt* value = pair.second.get();

        code << getIndent() << "const ";

        // 确定常量类型
        if (value->type == ValueStmt::ValueType::Number) {
            NumberStmt* num = value->number.get();
            if (num->is_real) {
                // 保留全部小数位，使用 double 类型
                code << "double ";
            } else if (num->is_char) {
                code << "char ";
            } else {
                code << "int ";
            }
        } else if (value->type == ValueStmt::ValueType::Str) {
            code << "char* ";
        }

        code << name << " = ";
        // 对于实数常量，保留全部小数位
        if (value->type == ValueStmt::ValueType::Number) {
            NumberStmt* num = value->number.get();
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

void CodeGenerator::visit(VarDeclStmt &stmt) {
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
        
        // 记录当前作用域的变量
        currentVars.insert(name);
    }
}

void CodeGenerator::visit(FuncHeadDeclStmt &stmt) {
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
    bool firstParam = true;
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

void CodeGenerator::visit(FuncBodyDeclStmt &stmt) {
    code << " {\n";
    indentLevel++;
    
    // 清空当前作用域变量列表
    currentVars.clear();
    
    // 处理常量声明
    if (stmt.const_decl) {
        stmt.const_decl->accept(*this);
    }
    
    // 处理变量声明
    for (const auto& varDecl : stmt.var_decl) {
        varDecl->accept(*this);
    }
     // 获取当前函数符号
    semantic::SymbolEntry* entry = symbolTable.lookupSymbol(currentFunction);

    // 如果有返回值，声明临时变量
    if (entry && entry->returnType != "void") {
        code << getIndent() << mapPascalTypeToC(entry->returnType) << " res = 0;\n";
    }
    
    // // 处理函数体语句
    // for (const auto& compStmt : stmt.comp_stmt) {
    //     compStmt->accept(*this);
    // }
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
    // // 如果函数没有return语句且不是void类型，添加默认返回值
    // semantic::SymbolEntry* entry = symbolTable.lookupSymbol(currentFunction);
    // if (entry && !hasReturn && entry->returnType != "void") {
    //     if (entry->returnType == "int" || entry->returnType == "bool") {
    //         code << getIndent() << "return 0;\n";
    //     } else if (entry->returnType == "float") {
    //         code << getIndent() << "return 0.0;\n";
    //     } else if (entry->returnType == "char") {
    //         code << getIndent() << "return '\\0';\n";
    //     } else {
    //         code << getIndent() << "return NULL;\n";
    //     }
    // }
    
    // indentLevel--;
    // code << getIndent() << "}\n\n";
    
    // // 重置当前函数
    // currentFunction = "";
}

void CodeGenerator::visit(FuncDeclStmt &stmt) {
    if (stmt.header) {
        stmt.header->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

void CodeGenerator::visit(AssignStmt &stmt) {
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

// 判断是否是简单表达式（变量、常量或简单运算）
bool CodeGenerator::isSimpleExpression(ExprStmt* expr) {
    if (!expr || !expr->rel_expr) {
        return false;
    }
    
    return expr->rel_expr->terms.size() <= 1;
}

void CodeGenerator::visit(IfStmt &stmt) {
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

void CodeGenerator::visit(ForStmt &stmt) {
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

void CodeGenerator::visit(WhileStmt &stmt) {
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

void CodeGenerator::visit(ReadFuncStmt &stmt) {
    code << getIndent() << "scanf(\"";
    
    // 构建格式字符串
    for (const auto& lvalStmt : stmt.lval) {
        semantic::SymbolEntry* entry = symbolTable.lookupSymbol(lvalStmt->id);
        if (entry) {
            if (entry->dataType == "int") {
                code << "%d";
            } else if (entry->dataType == "float") {
                code << "%f";
            } else if (entry->dataType == "char") {
                code << "%c";
            } else if (entry->dataType == "string") {
                code << "%s";
            }
        } else {
            code << "%d"; // 默认int
        }
    }
    
    code << "\"";
    
    // 添加参数
    for (const auto& lvalStmt : stmt.lval) {
        code << ", &";
        lvalStmt->accept(*this);
    }
    
    code << ");\n";
}

void CodeGenerator::visit(WriteFuncStmt &stmt) {
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


std::string CodeGenerator::type2fmt(const std::string& t) const {
    if (t == "int" || t == "bool") return "%d";
    if (t == "float" || t == "real") return "%.6f"; // 保证6位小数
    if (t == "char") return "%c";
    if (t == "string") return "%s";
    return "%d"; // 默认返回整数格式
}

semantic::SymbolEntry* CodeGenerator::determineExpressionType(ExprStmt* expr) {
    if (!expr || !expr->rel_expr) {
        return nullptr;
    }
    
    // 使用临时stringstream以避免修改主要的code流
    std::stringstream tempStream;
    
    try {
        // 简单情况: 单个变量
        if (expr->rel_expr->terms.size() == 1 && 
            expr->rel_expr->terms[0].add_expr && 
            expr->rel_expr->terms[0].add_expr->terms.size() == 1 &&
            expr->rel_expr->terms[0].add_expr->terms[0].mul_expr &&
            expr->rel_expr->terms[0].add_expr->terms[0].mul_expr->terms.size() == 1 &&
            expr->rel_expr->terms[0].add_expr->terms[0].mul_expr->terms[0].unary_expr &&
            expr->rel_expr->terms[0].add_expr->terms[0].mul_expr->terms[0].unary_expr->types.empty() &&
            expr->rel_expr->terms[0].add_expr->terms[0].mul_expr->terms[0].unary_expr->primary_expr) {
            
            auto primary = expr->rel_expr->terms[0].add_expr->terms[0].mul_expr->terms[0].unary_expr->primary_expr.get();
            
            if (primary->type == PrimaryExprStmt::PrimaryExprType::Value && 
                primary->value && 
                primary->value->type == ValueStmt::ValueType::LVal) {
                
                return symbolTable.lookupSymbol(primary->value->lval->id);
            }
        }
    } catch (...) {
        // 忽略可能的异常，安全地返回nullptr
    }
    
    return nullptr;
}
// 这里应该是代码生成器的成员方法，而不是全局函数

void CodeGenerator::visit(BreakStmt &stmt) {
    if (isInLoop) {
        code << getIndent() << "break; // goto " << breakLabel << "\n";
    } else {
        code << getIndent() << "// Error: break outside of loop\n";
    }
}

void CodeGenerator::visit(ContinueStmt &stmt) {
    if (isInLoop) {
        code << getIndent() << "continue; // goto " << continueLabel << "\n";
    } else {
        code << getIndent() << "// Error: continue outside of loop\n";
    }
}

void CodeGenerator::visit(ProgramHeadStmt &stmt) {
    // 程序头不生成代码
}

void CodeGenerator::visit(ProgramBodyStmt &stmt) {
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

void CodeGenerator::visit(ProgramStmt &stmt) {
    if (stmt.head) {
        stmt.head->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

void CodeGenerator::emitExpr(ExprStmt* e) {
    ++exprDepth;
    e->accept(*this);
    --exprDepth;
}
}
