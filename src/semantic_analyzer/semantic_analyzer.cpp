#include "semantic_analyzer/semantic_analyzer.hpp"
#include <iostream>
#include <algorithm>

namespace semantic {

// 辅助函数定义
std::string basicTypeToString(BasicType type) {
    switch (type) {
        case BasicType::INT: return "integer";
        case BasicType::REAL: return "real";
        case BasicType::CHAR: return "char";
        case BasicType::BOOLEAN: return "boolean";
        case BasicType::VOID: return "void";
        default: return "unknown";
    }
}

// PASCAL-S标准函数和过程
const std::vector<std::string> PASCAL_STD_FUNCTIONS = {
    "abs", "sqr", "sqrt", "sin", "cos", "arctan", "ln", "exp", 
    "trunc", "round", "ord", "chr", "succ", "pred", "odd"
};
const std::vector<std::string> PASCAL_STD_PROCEDURES = {
    "read", "readln", "write", "writeln"
};

// SymbolTable实现
SymbolTable::SymbolTable() {
    // 初始化全局作用域
    scopeStack.emplace_back();  // index 0 = 全局作用域
}

SymbolTable::~SymbolTable() {
    scopeStack.clear(); 
}

void SymbolTable::enterScope() {
    scopeStack.emplace_back();          // 新开一层
    currentScopeType = ScopeType::LOCAL;
}

void SymbolTable::exitScope() {
    if (scopeStack.size() > 1) {        // 保留第 0 层（全局）
        scopeStack.pop_back();
        currentScopeType = scopeStack.size() == 1 ? ScopeType::GLOBAL
                                                  : ScopeType::LOCAL;
    }
}

bool SymbolTable::insertSymbol(const SymbolEntry& entry) {
    // 检查在当前作用域中是否已存在同名符号
    auto& cur = scopeStack.back();
    if (cur.find(entry.name) != cur.end()) return false;
    cur[entry.name] = entry;
    return true;
}

SymbolEntry* SymbolTable::lookupSymbol(const std::string &name) {
// 从当前作用域向外层扫描
    for (auto it = scopeStack.rbegin(); it != scopeStack.rend(); ++it) {
        auto mapIt = it->find(name);
        if (mapIt != it->end()) return &(mapIt->second); // 持久地址
    }
    
    return nullptr; // 未找到符号
}

SymbolEntry* SymbolTable::lookupSymbolInCurrentScope(const std::string &name) {
    auto it = scopeStack.back().find(name);
    if (it != scopeStack.back().end()) {
        return &(it->second);
    }
    return nullptr;
}

void SymbolTable::recordUsage(const std::string &name, int line) {
    SymbolEntry* entry = lookupSymbol(name);
    if (entry) {
        entry->addUsage(line);
    }
}

void SymbolTable::printSymbolTable() {
    std::cout << "Symbol Table:" << std::endl;
    std::cout << "Current Scope: " << (currentScopeType == ScopeType::GLOBAL ? "GLOBAL" : "LOCAL") << std::endl;
    
    for (const auto& pair : scopeStack.back()) {
        const SymbolEntry& entry = pair.second;
        std::cout << "Name: " << entry.name
                  << ", Type: " << static_cast<int>(entry.type)
                  << ", DataType: " << entry.dataType
                  << ", Declared at line: " << entry.lineOfDeclaration << std::endl;
    }

    // 打印作用域栈中的符号
    std::cout << "Outer scopes:" << std::endl;
    std::vector<std::unordered_map<std::string, SymbolEntry>> tempStack = scopeStack;
    while (!tempStack.empty()) {
        for (const auto& pair : tempStack.back()) {
            const SymbolEntry& entry = pair.second;
            std::cout << "Name: " << entry.name
                      << ", Type: " << static_cast<int>(entry.type)
                      << ", DataType: " << entry.dataType
                      << ", Declared at line: " << entry.lineOfDeclaration << std::endl;
        }
        tempStack.pop_back();
    }
}

bool SymbolTable::isGlobalScope() const {
    return currentScopeType == ScopeType::GLOBAL;
}

ScopeType SymbolTable::getCurrentScopeType() const {
    return currentScopeType;
}

// SemanticAnalyzer实现
SemanticAnalyzer::SemanticAnalyzer() {
    // 初始化内置函数
    // read和write是过程(procedure)
    SymbolEntry readEntry("read", SymbolType::FUNCTION, ScopeType::GLOBAL, 
                         "procedure", false, {}, true, {"var"}, "void", 0, {}, true);
    symbolTable.insertSymbol(readEntry);
    
    SymbolEntry writeEntry("write", SymbolType::FUNCTION, ScopeType::GLOBAL,
                          "procedure", false, {}, false, {"any"}, "void", 0, {}, true);
    symbolTable.insertSymbol(writeEntry);
    
    // 添加PASCAL-S标准函数
    addStandardFunctions();
}

SemanticAnalyzer::~SemanticAnalyzer() {
}

bool SemanticAnalyzer::checkTypeConsistency(const std::string& leftType, const std::string& rightType) {
    // 类型一致性检查
    if (leftType == rightType) return true;
    
    // 允许PASCAL-S数值类型的隐式转换
    if ((leftType == "integer" && rightType == "real") ||
        (leftType == "real" && rightType == "integer") ||
        (leftType == "integer" && rightType == "char") ||
        (leftType == "char" && rightType == "integer")) {
        return true;
    }
    
    return false;
}

std::string SemanticAnalyzer::getBinaryExprType(const std::string& leftType, const std::string& rightType, const std::string& op) {
    // 计算二元表达式的结果类型
    if (!checkTypeConsistency(leftType, rightType)) {
        return "error";
    }
    
    // 关系运算符结果是布尔类型 
    if (op == "=" || op == "<>" || op == "<" || op == "<=" || op == ">" || op == ">=" || op == "in") {
        return "boolean";
    }
    
    // 逻辑运算符结果是布尔类型 
    if (op == "and" || op == "or") {
        if (leftType != "boolean" || rightType != "boolean") {
            return "error";
        }
        return "boolean";
    }
    
    //div和mod运算符只适用于整数
    if (op == "div" || op == "mod") {
        if (leftType != "integer" || rightType != "integer") {
            return "error";
        }
        return "integer";
    }
    
    // 算术运算符
    if (op == "+" || op == "-" || op == "*") {
        // 字符串连接
        if (op == "+" && leftType == "string" && rightType == "string") {
            return "string";
        }
        
        // 数值运算
        if (leftType == "real" || rightType == "real") {
            return "real";
        }
        
        if (leftType == "integer" && rightType == "integer") {
            return "integer";
        }
    }
    
    // 除法运算符/返回实数
    if (op == "/") {
        if ((leftType == "integer" || leftType == "real") && 
            (rightType == "integer" || rightType == "real")) {
            return "real";
        }
        return "error";
    }
    
    return "error"; // 默认错误
}

std::string SemanticAnalyzer::getUnaryExprType(const std::string& exprType, const std::string& op) {
    // 一元运算符的结果类型
    if (op == "not") {
        if (exprType != "boolean") {
            return "error";
        }
        return "boolean";
    }
    
    if (op == "-" || op == "+") {
        if (exprType != "integer" && exprType != "real") {
            return "error";
        }
        return exprType;
    }
    
    return "error";//不符合上述条件的是错误的一元表达式
}

void SemanticAnalyzer::addError(const std::string& message) {
    errors.push_back(message);
    std::cerr << "语义错误: " << message << std::endl;
}

bool SemanticAnalyzer::hasErrors() const {
    return !errors.empty();
}

const std::vector<std::string>& SemanticAnalyzer::getErrors() const {
    return errors;
}

SymbolTable& SemanticAnalyzer::getSymbolTable() {
    return symbolTable;
}

// Visitor方法实现
void SemanticAnalyzer::visit(PrimaryExprNode &stmt) {
    if (stmt.type == PrimaryExprNode::PrimaryExprType::Value) {
        stmt.value->accept(*this);
        setType(&stmt, getType(stmt.value.get()));
    } else if (stmt.type == PrimaryExprNode::PrimaryExprType::Parentheses) {
        stmt.expr->accept(*this);
        setType(&stmt, getType(stmt.expr.get()));
    }
}

void SemanticAnalyzer::visit(ValueNode &stmt) {
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
        default: break;
    }
    // 统一从子节点提取类型
    std::string subType;
    if      (stmt.number)    subType = getType(stmt.number.get());
    else if (stmt.str)       subType = getType(stmt.str.get());
    else if (stmt.lval)      subType = getType(stmt.lval.get());
    else if (stmt.func_call) subType = getType(stmt.func_call.get());
    else                     subType = "unknown";
    setType(&stmt, subType);
}

void SemanticAnalyzer::visit(PeriodNode &stmt) {
    // 不需要额外的语义检查
}

void SemanticAnalyzer::visit(ConstDeclNode &stmt) {
    for (auto& pair : stmt.pairs) {
        const std::string& name = pair.first;
        ValueNode* value = pair.second.get();
        
        // 检查常量名是否已在当前作用域中定义
        if (symbolTable.lookupSymbolInCurrentScope(name)) {
            addError("重定义常量: " + name);
            continue;
        }
        
        // 处理常量值表达式
        value->accept(*this);
        
        // 确定常量类型并插入符号表
        std::string dataType;
        if (value->type == ValueNode::ValueType::Number) {
            NumberNode* num = value->number.get();
            if (num->is_real) {
                dataType = "real";
            } else if (num->is_char) {
                dataType = "char";
            } else {
                dataType = "integer";
            }
        } else if (value->type == ValueNode::ValueType::Str) {
            dataType = "string";
        }
        
        SymbolEntry entry(
            name, SymbolType::CONSTANT, symbolTable.getCurrentScopeType(),
            dataType, false, {}, false, {}, "", 0, {}, true  // 常量始终被初始化
        );
        
        symbolTable.insertSymbol(entry);
    }
}

void SemanticAnalyzer::visit(VarDeclNode &stmt) {
    // 处理变量声明
    std::string dataType = basicTypeToString(stmt.basic_type);
    
    for (const auto& name : stmt.id) {
        // 检查变量名是否已在当前作用域中定义
        if (symbolTable.lookupSymbolInCurrentScope(name)) {
            addError("重定义变量: " + name);
            continue;
        }
        
        SymbolEntry entry(
            name, SymbolType::VARIABLE, symbolTable.getCurrentScopeType(),
            dataType, (stmt.data_type == DataType::ArrayType), {}, stmt.is_var, 
            {}, "", 0, {}, false  // 变量初始化设为false
        );
        
        // 处理数组类型
        if (stmt.data_type == DataType::ArrayType) {
            for (const auto& range : stmt.array_range) {
                entry.dimensions.push_back(range->end + 1);
            }
        }
        
        symbolTable.insertSymbol(entry);
    }
}

void SemanticAnalyzer::visit(FuncHeadDeclNode &stmt) {
    // 处理函数头声明
    std::string returnType = basicTypeToString(stmt.ret_type);
    currentFunction = stmt.func_name;
    currentReturnType = returnType;
    functionHasReturn = false; // 重置返回值状态
    
    // 检查函数名是否已在当前作用域中定义
    if (symbolTable.lookupSymbolInCurrentScope(stmt.func_name)) {
        addError("重定义函数: " + stmt.func_name);
        return;
    }
    
    // 确定是函数还是过程
    SymbolType symType = (returnType == "void") ? SymbolType::FUNCTION : SymbolType::FUNCTION;
    std::string dataType = (returnType == "void") ? "procedure" : "function";
    
    // 创建函数符号
    SymbolEntry funcEntry(
        stmt.func_name, symType, symbolTable.getCurrentScopeType(),
        dataType, false, {}, false, {}, returnType, 0, {}, true  // 函数自身始终被初始化
    );
    
    // 处理参数
    for (const auto& arg : stmt.args) {
        for (const auto& arg_name : arg->id) {
            funcEntry.paramTypes.push_back(basicTypeToString(arg->basic_type));
        }
    }
    symbolTable.insertSymbol(funcEntry);
    
    // 进入函数作用域
    symbolTable.enterScope();
    
    // 处理参数声明
    for (const auto& arg : stmt.args) {
        arg->accept(*this);
        
        // 为参数变量标记为已初始化，因为它们在函数调用时会被赋值
        for (const auto& arg_name : arg->id) {
            markInitialized(arg_name);
        }
    }
}

void SemanticAnalyzer::visit(FuncBodyDeclNode &stmt) {
    // 处理函数体
    
    // 处理常量声明
    if (stmt.const_decl) {
        stmt.const_decl->accept(*this);
    }
    
    // 处理变量声明
    for (const auto& varDecl : stmt.var_decl) {
        varDecl->accept(*this);
    }
    
    // 处理函数体语句
    for (const auto& compStmt : stmt.comp_stmt) {
        compStmt->accept(*this);
    }
    
    // 检查函数返回值
    // 如果当前是函数(非过程)而且没有返回语句，发出警告
    if (currentFunction.length() > 0 && currentReturnType != "void" && !functionHasReturn) {
        addError("函数 '" + currentFunction + "' 可能没有在所有执行路径上返回值");
    }
    
    // 离开函数作用域
    symbolTable.exitScope();
    currentFunction = "";
    currentReturnType = "";
    functionHasReturn = false;
}

void SemanticAnalyzer::visit(FuncDeclNode &stmt) {
    // 处理函数声明（头部+体）
    if (stmt.header) {
        stmt.header->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

void SemanticAnalyzer::visit(AssignmentNode &stmt) {
    // 处理左值
    stmt.lval->accept(*this);
    
    // 检查左值是否存在
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.lval->id);
    if (!entry) {
        addError("未定义的变量: " + stmt.lval->id);
        return;
    }
    
    // 检查左值是否可赋值（非常量）
    if (isConstantModification(stmt.lval->id)) {
        addError("不能给常量赋值: " + stmt.lval->id);
        return;
    }
    
    // 处理右值表达式
    stmt.expr->accept(*this);
    
    // 获取类型信息
    std::string lvalType = getType(stmt.lval.get());
    std::string exprType = getType(stmt.expr.get());
    
    // 类型兼容性检查
    if (!checkTypeConsistency(lvalType, exprType)) {
        addError("类型不兼容: 不能将 '" + exprType + "' 类型的值赋给 '" + lvalType + "' 类型的变量");
        return;
    }
    
    // 标记变量已初始化
    markInitialized(stmt.lval->id);
}

void SemanticAnalyzer::visit(IfNode &stmt) {
    // 处理条件表达式
    stmt.expr->accept(*this);
    
    // 处理then部分
    for (const auto& thenStmt : stmt.true_stmt) {
        thenStmt->accept(*this);
    }
    
    // 处理else部分
    for (const auto& elseStmt : stmt.false_stmt) {
        elseStmt->accept(*this);
    }
}

void SemanticAnalyzer::visit(ForNode &stmt) {
    // 检查循环变量
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (!entry) {
        addError("未定义的循环变量: " + stmt.id);
        return;
    }
    
    if (entry->type == SymbolType::CONSTANT) {
        addError("循环变量不能是常量: " + stmt.id);
        return;
    }
    
    // 在PASCAL-S中，循环变量必须是序数类型(整数、字符、布尔)
    if (!isOrdinalType(entry->dataType)) {
        addError("循环变量必须是序数类型(integer, char, boolean): " + stmt.id);
        return;
    }
    
    // 处理循环范围表达式
    stmt.begin->accept(*this);
    stmt.end->accept(*this);
    
    // 检查循环范围表达式类型
    std::string beginType = getType(stmt.begin.get());
    std::string endType = getType(stmt.end.get());
    
    // 起始值和结束值类型必须与循环变量类型兼容
    if (!checkTypeConsistency(entry->dataType, beginType)) {
        addError("循环起始值类型'" + beginType + "'与循环变量类型'" + entry->dataType + "'不兼容");
        return;
    }
    
    if (!checkTypeConsistency(entry->dataType, endType)) {
        addError("循环结束值类型'" + endType + "'与循环变量类型'" + entry->dataType + "'不兼容");
        return;
    }
    
    // 标记循环变量为已初始化
    markInitialized(stmt.id);
    
    // 进入循环体
    bool oldInLoop = inLoop;
    inLoop = true;
    
    // 处理循环体语句
    for (const auto& loopStmt : stmt.stmt) {
        loopStmt->accept(*this);
    }
    
    // 恢复循环状态
    inLoop = oldInLoop;
}

void SemanticAnalyzer::visit(WhileNode &stmt) {
    // 处理while循环
    
    // 处理条件表达式
    stmt.expr->accept(*this);
    
    // 进入循环体
    bool oldInLoop = inLoop;
    inLoop = true;
    
    // 处理循环体语句
    for (const auto& loopStmt : stmt.stmt) {
        loopStmt->accept(*this);
    }
    
    // 恢复循环状态
    inLoop = oldInLoop;
}

void SemanticAnalyzer::visit(ReadFuncNode &stmt) {
    // 处理read函数调用
    
    // 检查所有参数是否是可写的左值
    for (const auto& lValueNode : stmt.lval) {
        lValueNode->accept(*this);
        
        // 检查左值是否存在
        SymbolEntry* entry = symbolTable.lookupSymbol(lValueNode->id);
        if (!entry) {
            addError("未定义的变量: " + lValueNode->id);
            continue;
        }
        
        // 检查左值是否可赋值（非常量）
        if (entry->type == SymbolType::CONSTANT) {
            addError("不能读取到常量中: " + lValueNode->id);
            continue;
        }
        
        // 检查类型是否为基本类型(在PASCAL-S中，只有基本类型可以被read)
        if (entry->isArray && lValueNode->array_index.empty()) {
            addError("不能读取整个数组: " + lValueNode->id);
            continue;
        }
        
        // 标记变量为已初始化
        markInitialized(lValueNode->id);
    }
}

void SemanticAnalyzer::visit(WriteFuncNode &stmt) {
    // 处理write函数调用
    
    // 处理所有表达式参数
    for (const auto& exprStmt : stmt.expr) {
        exprStmt->accept(*this);
    }
}

void SemanticAnalyzer::visit(BreakNode &stmt) {
    // 处理break语句
    if (!inLoop) {
        addError("Break statement outside of loop");
    }
}

void SemanticAnalyzer::visit(ContinueNode &stmt) {
    // 处理continue语句
    if (!inLoop) {
        addError("Continue statement outside of loop");
    }
}

void SemanticAnalyzer::visit(ProgramHeadNode &stmt) {
    // 处理程序头
    // 不需要额外的语义检查
}

void SemanticAnalyzer::visit(ProgramBodyNode &stmt) {
    // 处理程序体
    
    // 处理常量声明
    if (stmt.const_decl) {
        stmt.const_decl->accept(*this);
    }
    
    // 处理变量声明
    for (const auto& varDecl : stmt.var_decl) {
        varDecl->accept(*this);
    }
    
    // 处理函数声明
    for (const auto& funcDecl : stmt.func_decl) {
        funcDecl->accept(*this);
    }
    
    // 处理主程序语句
    for (const auto& compStmt : stmt.comp_stmt) {
        compStmt->accept(*this);
    }
}

void SemanticAnalyzer::visit(ProgramNode &stmt) {
    // 处理程序声明（头部+体）
    if (stmt.head) {
        stmt.head->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

void SemanticAnalyzer::visit(NumberNode &stmt) {
    std::string t = stmt.is_real ? "float" : (stmt.is_char ? "char" : "int");
    setType(&stmt, t);
}

void SemanticAnalyzer::visit(StringNode &stmt) {
    setType(&stmt, "string");
}

void SemanticAnalyzer::visit(LValueNode &stmt) {
    // 原有声明检查不变
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (!entry) {
        addError("未定义的变量: " + stmt.id);
        return;
    }
    symbolTable.recordUsage(stmt.id, 0);
    setType(&stmt, entry->dataType);
    
    // 检查数组访问
    if (!stmt.array_index.empty()) {
        if (!entry->isArray) {
            addError("变量不是数组类型: " + stmt.id);
            return;
        }
        
        // 检查数组维度
        if (stmt.array_index.size() > entry->dimensions.size()) {
            addError("数组索引维度过多: " + stmt.id);
            return;
        }
        
        // 处理每个数组索引表达式
        for (auto& index : stmt.array_index) {
            index->accept(*this);
            std::string indexType = getType(index.get());
            
            // 确保索引表达式的类型是序数类型(整数、字符、布尔)
            if (!checkArrayIndex(indexType)) {
                addError("数组索引必须是序数类型(integer, char, boolean)，而不是 " + indexType);
                return;
            }
        }
    }
    
    // 检查变量是否已初始化（读取变量前）
    if (!checkInitialized(stmt.id)) {
        addError("变量可能在初始化前使用: " + stmt.id);
    }
}

void SemanticAnalyzer::visit(FuncCallNode &stmt) {
    // 查找函数符号
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    
    // 检查是否为标准函数或过程
    bool isStandardFunc = isPascalStandardFunction(stmt.id);
    bool isStandardProc = isPascalStandardProcedure(stmt.id);
    
    // 检查函数是否存在
    if (!entry && !isStandardFunc && !isStandardProc) {
        addError("未定义的函数或过程: " + stmt.id);
        return;
    }
    
    // 记录函数调用位置
    if (entry) {
        symbolTable.recordUsage(stmt.id, 0); // 使用实际行号替换0
        
        // 检查符号类型是否为函数
        if (entry->type != SymbolType::FUNCTION) {
            addError("'" + stmt.id + "' 不是函数");
            return;
        }
    }
    
    // 处理参数
    for (auto& arg : stmt.args) {
        arg->accept(*this);
    }
    
    // 检查参数数量
    if (entry && entry->paramTypes.size() != stmt.args.size() && 
        entry->paramTypes != std::vector<std::string>{"any"}) {
        addError("函数 '" + stmt.id + "' 参数数量不匹配，期望 " + 
                std::to_string(entry->paramTypes.size()) + " 个参数，但提供了 " + 
                std::to_string(stmt.args.size()) + " 个");
        return;
    }
    
    // 检查参数类型
    if (entry && entry->paramTypes != std::vector<std::string>{"any"}) {
        for (size_t i = 0; i < std::min(entry->paramTypes.size(), stmt.args.size()); ++i) {
            std::string argType = getType(stmt.args[i].get());
            std::string paramType = entry->paramTypes[i];
            
            if (!checkTypeConsistency(paramType, argType)) {
                addError("函数 '" + stmt.id + "' 的第 " + std::to_string(i+1) + 
                        " 个参数类型不匹配，期望 '" + paramType + 
                        "'，得到 '" + argType + "'");
            }
        }
    }
    
    // 为表达式设置类型（函数调用的结果类型）
    if (entry) {
        setType(&stmt, entry->returnType);
    } else if (isStandardFunc) {
        // 标准函数的返回类型处理
        if (stmt.id == "sqrt" || stmt.id == "sin" || stmt.id == "cos" || 
            stmt.id == "arctan" || stmt.id == "ln" || stmt.id == "exp") {
            setType(&stmt, "real");
        } else if (stmt.id == "chr") {
            setType(&stmt, "char");
        } else if (stmt.id == "odd") {
            setType(&stmt, "boolean");
        } else {
            setType(&stmt, "integer"); // 默认返回整数
        }
    } else {
        setType(&stmt, "void"); // 过程没有返回值
    }
}

// 一元表达式
void SemanticAnalyzer::visit(UnaryExprNode &stmt) {
    stmt.primary_expr->accept(*this);
    std::string cur = getType(stmt.primary_expr.get());
    for (auto op : stmt.types) {
        std::string opStr = (op==UnaryExprNode::UnaryExprType::Minus)?"-":"not";
        cur = getUnaryExprType(cur, opStr);
    }
    setType(&stmt, cur);
}

// 乘法表达式
void SemanticAnalyzer::visit(MulExprNode &stmt) {
    /* 1. 递归推断所有因子类型 */
    for (auto &term : stmt.terms)
        term.unary_expr->accept(*this);

    /* 2. 折叠类型 */
    std::string cur = getType(stmt.terms[0].unary_expr.get());

    for (size_t i = 1; i < stmt.terms.size(); ++i) {
        std::string rhs = getType(stmt.terms[i].unary_expr.get());
        MulExprNode::MulExprType op = stmt.terms[i].type;
        std::string opStr;

        switch (op) {
        case MulExprNode::MulExprType::Mul:
            opStr = "*";
            break;
        case MulExprNode::MulExprType::Div:
            opStr = "/";      // 实数除法
            break;
        case MulExprNode::MulExprType::Mod:
            opStr = "mod";
            break;
        case MulExprNode::MulExprType::And:
            opStr = "and";
            break;
        case MulExprNode::MulExprType::AndThen:
            opStr = "and then";
            break;
        default:
            opStr = "unknown";
        }

        cur = getBinaryExprType(cur, rhs, opStr);
    }

    setType(&stmt, cur);      // 把结果写进类型表
}

// 加法表达式
void SemanticAnalyzer::visit(AddExprNode &stmt) {
    for (auto& term : stmt.terms) term.mul_expr->accept(*this);

    std::string cur = getType(stmt.terms[0].mul_expr.get());
    for (size_t i=1;i<stmt.terms.size();++i) {
        std::string right = getType(stmt.terms[i].mul_expr.get());
        std::string op;
        
        switch(stmt.terms[i].type) {
            case AddExprNode::AddExprType::Plus:
                op = "+";
                break;
            case AddExprNode::AddExprType::Minus:
                op = "-";
                break;
            case AddExprNode::AddExprType::Or:
                op = "or";
                break;
            default:
                op = "unknown";
        }
        
        cur = getBinaryExprType(cur, right, op);
    }
    
    setType(&stmt, cur);
}

// 关系表达式
void SemanticAnalyzer::visit(RelExprNode &stmt) {
    for (auto& term : stmt.terms) term.add_expr->accept(*this);

    std::string cur = getType(stmt.terms[0].add_expr.get());
    for (size_t i=1;i<stmt.terms.size();++i) {
        std::string right = getType(stmt.terms[i].add_expr.get());
        std::string op;
        switch(stmt.terms[i].type){
            case RelExprNode::RelExprType::Equal:        op="=";  break; // PASCAL-S用=而非==
            case RelExprNode::RelExprType::NotEqual:     op="<>"; break; // PASCAL-S用<>而非!=
            case RelExprNode::RelExprType::Less:         op="<";  break;
            case RelExprNode::RelExprType::LessEqual:    op="<="; break;
            case RelExprNode::RelExprType::Greater:      op=">";  break;
            case RelExprNode::RelExprType::GreaterEqual: op=">="; break;
            default: op="=";
        }
        cur = getBinaryExprType(cur, right, op);
    }
    setType(&stmt, cur);
}

// 顶层表达式
void SemanticAnalyzer::visit(ExprNode &stmt) {
    if (stmt.rel_expr) {
        stmt.rel_expr->accept(*this);
        setType(&stmt, getType(stmt.rel_expr.get()));
    }
}

// 添加PASCAL-S标准函数
void SemanticAnalyzer::addStandardFunctions() {
    // 标准函数
    for (const auto& funcName : PASCAL_STD_FUNCTIONS) {
        std::string returnType = "integer";
        
        // 设置正确的返回类型
        if (funcName == "sqrt" || funcName == "sin" || funcName == "cos" || 
            funcName == "arctan" || funcName == "ln" || funcName == "exp") {
            returnType = "real";
        } else if (funcName == "chr") {
            returnType = "char";
        } else if (funcName == "odd") {
            returnType = "boolean";
        }
        
        SymbolEntry stdFuncEntry(
            funcName, SymbolType::FUNCTION, ScopeType::GLOBAL, "function",
            false, {}, false, {"any"}, returnType, 0, {}, true
        );
        symbolTable.insertSymbol(stdFuncEntry);
    }
    
    // 标准过程
    for (const auto& procName : PASCAL_STD_PROCEDURES) {
        if (procName != "read" && procName != "write") {
            SymbolEntry stdProcEntry(
                procName, SymbolType::FUNCTION, ScopeType::GLOBAL, "procedure",
                false, {}, (procName.find("read") != std::string::npos), {"any"}, "void", 0, {}, true
            );
            symbolTable.insertSymbol(stdProcEntry);
        }
    }
}

// PASCAL-S类型检查辅助函数实现
bool SemanticAnalyzer::isOrdinalType(const std::string& type) {
    // Pascal序数类型：整数、字符、布尔、枚举
    return type == "integer" || type == "char" || type == "boolean";
}

bool SemanticAnalyzer::isRealType(const std::string& type) {
    return type == "real";
}

bool SemanticAnalyzer::checkArrayIndex(const std::string& indexType) {
    // Pascal数组索引必须是序数类型
    return isOrdinalType(indexType);
}

bool SemanticAnalyzer::isConstantModification(const std::string& name) {
    SymbolEntry* entry = symbolTable.lookupSymbol(name);
    return entry && entry->type == SymbolType::CONSTANT;
}

// 变量初始化跟踪
void SemanticAnalyzer::markInitialized(const std::string& name) {
    SymbolEntry* entry = symbolTable.lookupSymbol(name);
    if (entry && entry->type == SymbolType::VARIABLE) {
        entry->isInitialized = true;
    }
}

bool SemanticAnalyzer::checkInitialized(const std::string& name) {
    SymbolEntry* entry = symbolTable.lookupSymbol(name);
    return entry && (entry->isInitialized || entry->type == SymbolType::CONSTANT);
}

// 标准函数/过程检查
bool SemanticAnalyzer::isPascalStandardFunction(const std::string& name) {
    return std::find(PASCAL_STD_FUNCTIONS.begin(), PASCAL_STD_FUNCTIONS.end(), name) 
           != PASCAL_STD_FUNCTIONS.end();
}

bool SemanticAnalyzer::isPascalStandardProcedure(const std::string& name) {
    return std::find(PASCAL_STD_PROCEDURES.begin(), PASCAL_STD_PROCEDURES.end(), name) 
           != PASCAL_STD_PROCEDURES.end();
}

}