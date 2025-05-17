#include "semantic_analyzer/semantic_analyzer.hpp"
#include <iostream>
#include <algorithm>

namespace semantic {

// 辅助函数定义
std::string basicTypeToString(BasicType type) {
    switch (type) {
        case BasicType::INT: return "int";
        case BasicType::REAL: return "float";
        case BasicType::CHAR: return "char";
        case BasicType::BOOLEAN: return "bool";
        case BasicType::VOID: return "void";
        default: return "unknown";
    }
}

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
    SymbolEntry readEntry = {
        "read", SymbolType::FUNCTION, ScopeType::GLOBAL, "void", 
        false, {}, false, {"var"}, "void", 0, {}
    };
    symbolTable.insertSymbol(readEntry);
    
    SymbolEntry writeEntry = {
        "write", SymbolType::FUNCTION, ScopeType::GLOBAL, "void", 
        false, {}, false, {"any"}, "void", 0, {}
    };
    symbolTable.insertSymbol(writeEntry);
}

SemanticAnalyzer::~SemanticAnalyzer() {
}

bool SemanticAnalyzer::checkTypeConsistency(const std::string& leftType, const std::string& rightType) {
    // 类型一致性检查
    if (leftType == rightType) return true;
    
    // 允许数值类型的隐式转换
    if ((leftType == "int" && rightType == "float") ||
        (leftType == "float" && rightType == "int") ||
        (leftType == "int" && rightType == "char") ||
        (leftType == "char" && rightType == "int")) {
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
    if (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=" || op == "in") {
        return "bool";
    }
    
    // 逻辑运算符结果是布尔类型
    if (op == "&&" || op == "||") {
        if (leftType != "bool" || rightType != "bool") {
            return "error";
        }
        return "bool";
    }
    
    // 算术运算符
    if (leftType == "float" || rightType == "float") {
        return "float";
    }
    
    return leftType; // 保持原类型
}

std::string SemanticAnalyzer::getUnaryExprType(const std::string& exprType, const std::string& op) {
    // 一元运算符的结果类型
    if (op == "!" || op == "not") {
        if (exprType != "bool") {
            return "error";
        }
        return "bool";
    }
    
    if (op == "-") {
        if (exprType != "int" && exprType != "float") {
            return "error";
        }
        return exprType;
    }
    
    return exprType;
}

void SemanticAnalyzer::addError(const std::string& message) {
    errors.push_back(message);
    std::cerr << "Semantic Error: " << message << std::endl;
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
void SemanticAnalyzer::visit(PrimaryExprStmt &stmt) {
    if (stmt.type == PrimaryExprStmt::PrimaryExprType::Value) {
        stmt.value->accept(*this);
        setType(&stmt, getType(stmt.value.get()));
    } else if (stmt.type == PrimaryExprStmt::PrimaryExprType::Parentheses) {
        stmt.expr->accept(*this);
        setType(&stmt, getType(stmt.expr.get()));
    }
}

void SemanticAnalyzer::visit(ValueStmt &stmt) {
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



void SemanticAnalyzer::visit(PeriodStmt &stmt) {
    // 不需要额外的语义检查
}

void SemanticAnalyzer::visit(ConstDeclStmt &stmt) {
    for (auto& pair : stmt.pairs) {
        const std::string& name = pair.first;
        ValueStmt* value = pair.second.get();
        
        // 检查常量名是否已在当前作用域中定义
        if (symbolTable.lookupSymbolInCurrentScope(name)) {
            addError("Redefinition of constant: " + name);
            continue;
        }
        
        // 处理常量值表达式
        value->accept(*this);
        
        // 确定常量类型并插入符号表
        std::string dataType;
        if (value->type == ValueStmt::ValueType::Number) {
            NumberStmt* num = value->number.get();
            if (num->is_real) {
                dataType = "float";
            } else if (num->is_char) {
                dataType = "char";
            } else {
                dataType = "int";
            }
        } else if (value->type == ValueStmt::ValueType::Str) {
            dataType = "string";
        }
        
        SymbolEntry entry = {
            name, SymbolType::CONSTANT, symbolTable.getCurrentScopeType(),
            dataType, false, {}, false, {}, "", 0, {}
        };
        
        symbolTable.insertSymbol(entry);
    }
}

void SemanticAnalyzer::visit(VarDeclStmt &stmt) {
    // 处理变量声明
    std::string dataType = basicTypeToString(stmt.basic_type);
    
    for (const auto& name : stmt.id) {
        // 检查变量名是否已在当前作用域中定义
        if (symbolTable.lookupSymbolInCurrentScope(name)) {
            addError("Redefinition of variable: " + name);
            continue;
        }
        
        SymbolEntry entry = {
            name, SymbolType::VARIABLE, symbolTable.getCurrentScopeType(),
            dataType, (stmt.data_type == DataType::ArrayType), {}, stmt.is_var, 
            {}, "", 0, {}
        };
        
        // 处理数组类型
        if (stmt.data_type == DataType::ArrayType) {
            for (const auto& range : stmt.array_range) {
                entry.dimensions.push_back(range->end + 1);
            }
        }
        
        symbolTable.insertSymbol(entry);
    }
}

void SemanticAnalyzer::visit(FuncHeadDeclStmt &stmt) {
    // 处理函数头声明
    std::string returnType = basicTypeToString(stmt.ret_type);
    currentFunction = stmt.func_name;
    currentReturnType = returnType;
    
    // 检查函数名是否已在当前作用域中定义
    if (symbolTable.lookupSymbolInCurrentScope(stmt.func_name)) {
        addError("Redefinition of function: " + stmt.func_name);
        return;
    }
    
    // 创建函数符号
    SymbolEntry funcEntry = {
        stmt.func_name, SymbolType::FUNCTION, symbolTable.getCurrentScopeType(),
        "function", false, {}, false, {}, returnType, 0, {}
    };
    
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
    }
}

void SemanticAnalyzer::visit(FuncBodyDeclStmt &stmt) {
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
    
    // 离开函数作用域
    symbolTable.exitScope();
    currentFunction = "";
    currentReturnType = "";
}

void SemanticAnalyzer::visit(FuncDeclStmt &stmt) {
    // 处理函数声明（头部+体）
    if (stmt.header) {
        stmt.header->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}

void SemanticAnalyzer::visit(AssignStmt &stmt) {
    // 处理赋值语句
    
    // 处理左值
    stmt.lval->accept(*this);
    
    // 检查左值是否可赋值（非常量）
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.lval->id);
    if (entry && entry->type == SymbolType::CONSTANT) {
        addError("Cannot assign to constant: " + stmt.lval->id);
        return;
    }
    
    // 处理右值表达式
    stmt.expr->accept(*this);
    
    // 类型检查（左值和右值类型是否兼容）
    // 这里可以添加更详细的类型检查
}

void SemanticAnalyzer::visit(IfStmt &stmt) {
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

void SemanticAnalyzer::visit(ForStmt &stmt) {
    // 处理for循环
    
    // 检查循环变量
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (!entry) {
        addError("Undefined loop variable: " + stmt.id);
        return;
    }
    
    if (entry->type == SymbolType::CONSTANT) {
        addError("Loop variable cannot be a constant: " + stmt.id);
        return;
    }
    
    // 处理循环范围表达式
    stmt.begin->accept(*this);
    stmt.end->accept(*this);
    
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

void SemanticAnalyzer::visit(WhileStmt &stmt) {
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

void SemanticAnalyzer::visit(ReadFuncStmt &stmt) {
    // 处理read函数调用
    
    // 检查所有参数是否是可写的左值
    for (const auto& lvalStmt : stmt.lval) {
        lvalStmt->accept(*this);
        
        // 检查左值是否可赋值（非常量）
        SymbolEntry* entry = symbolTable.lookupSymbol(lvalStmt->id);
        if (entry && entry->type == SymbolType::CONSTANT) {
            addError("Cannot read into constant: " + lvalStmt->id);
        }
    }
}

void SemanticAnalyzer::visit(WriteFuncStmt &stmt) {
    // 处理write函数调用
    
    // 处理所有表达式参数
    for (const auto& exprStmt : stmt.expr) {
        exprStmt->accept(*this);
    }
}

void SemanticAnalyzer::visit(BreakStmt &stmt) {
    // 处理break语句
    if (!inLoop) {
        addError("Break statement outside of loop");
    }
}

void SemanticAnalyzer::visit(ContinueStmt &stmt) {
    // 处理continue语句
    if (!inLoop) {
        addError("Continue statement outside of loop");
    }
}

void SemanticAnalyzer::visit(ProgramHeadStmt &stmt) {
    // 处理程序头
    // 不需要额外的语义检查
}

void SemanticAnalyzer::visit(ProgramBodyStmt &stmt) {
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

void SemanticAnalyzer::visit(ProgramStmt &stmt) {
    // 处理程序声明（头部+体）
    if (stmt.head) {
        stmt.head->accept(*this);
    }
    
    if (stmt.body) {
        stmt.body->accept(*this);
    }
}



void SemanticAnalyzer::visit(NumberStmt &stmt) {
    std::string t = stmt.is_real ? "float" : (stmt.is_char ? "char" : "int");
    setType(&stmt, t);
}

void SemanticAnalyzer::visit(StrStmt &stmt) {
    setType(&stmt, "string");
}

void SemanticAnalyzer::visit(LValStmt &stmt) {
    // 原有声明检查不变
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (!entry) {
        addError("Undefined variable: " + stmt.id);
        return;
    }
    symbolTable.recordUsage(stmt.id, 0);
    setType(&stmt, entry->dataType);
    // 检查数组访问
    if (!stmt.array_index.empty()) {
        if (!entry->isArray) {
            addError("Variable is not an array: " + stmt.id);
            return;
        }
        
        // 检查数组维度
        if (stmt.array_index.size() > entry->dimensions.size()) {
            addError("Too many indices for array: " + stmt.id);
            return;
        }
        
        // 处理每个数组索引表达式
        for (auto& index : stmt.array_index) {
            index->accept(*this);
            // 确保索引表达式的类型是整数
            // 这里可以添加更详细的类型检查
        }
    }
}
void SemanticAnalyzer::visit(FuncCallStmt &stmt) {
    SymbolEntry* entry = symbolTable.lookupSymbol(stmt.id);
    if (!entry || entry->type != SymbolType::FUNCTION) {
        addError("Undefined function: " + stmt.id);
        return;
    }
    if (entry->type != SymbolType::FUNCTION) {
        addError("Identifier is not a function: " + stmt.id);
        return;
    }
    
    // 记录函数调用位置
    symbolTable.recordUsage(stmt.id, 0); // 用实际行号替换0
    
    // 检查参数数量
    if (stmt.args.size() != entry->paramTypes.size() && entry->paramTypes != std::vector<std::string>{"any"}) {
        addError("Incorrect number of arguments for function: " + stmt.id);
        return;
    }
    for (auto& arg : stmt.args) arg->accept(*this);
    setType(&stmt, entry->returnType);
}

// 一元表达式
void SemanticAnalyzer::visit(UnaryExprStmt &stmt) {
    stmt.primary_expr->accept(*this);
    std::string cur = getType(stmt.primary_expr.get());
    for (auto op : stmt.types) {
        std::string opStr = (op==UnaryExprStmt::UnaryExprType::Minus)?"-":"not";
        cur = getUnaryExprType(cur, opStr);
    }
    setType(&stmt, cur);
}

// 乘法表达式
void SemanticAnalyzer::visit(MulExprStmt &stmt)
{
    /* 1. 递归推断所有因子类型 */
    for (auto &term : stmt.terms)
        term.unary_expr->accept(*this);

    /* 2. 折叠类型 */
    std::string cur = getType(stmt.terms[0].unary_expr.get());

    for (size_t i = 1; i < stmt.terms.size(); ++i) {
        std::string rhs = getType(stmt.terms[i].unary_expr.get());
        MulExprStmt::MulExprType op = stmt.terms[i].type;

        switch (op) {
        case MulExprStmt::MulExprType::Mul:          // *   —— 与加法规则一致
        case MulExprStmt::MulExprType::Mod:          // mod —— 结果必为 int
            cur = (cur == "float" || rhs == "float") ? "float" : "int";
            break;

        case MulExprStmt::MulExprType::Div:          // /   —— **关键修改**
            if (cur == "float" || rhs == "float")
                cur = "float";          // 只要有 float → float
            else
                cur = "int";            // 两边都是 int → int
            break;

        case MulExprStmt::MulExprType::And:
        case MulExprStmt::MulExprType::AndThen:
            cur = "bool";
            break;

        default:
            cur = "error";
        }
    }

    setType(&stmt, cur);                // 把结果写进类型表
}



// 加法表达式
void SemanticAnalyzer::visit(AddExprStmt &stmt) {
    for (auto& term : stmt.terms) term.mul_expr->accept(*this);

    std::string cur = getType(stmt.terms[0].mul_expr.get());
    for (size_t i=1;i<stmt.terms.size();++i) {
        std::string right = getType(stmt.terms[i].mul_expr.get());
        std::string op = (stmt.terms[i].type==AddExprStmt::AddExprType::Plus)?"+":
                         (stmt.terms[i].type==AddExprStmt::AddExprType::Minus)?"-":"or";
        cur = getBinaryExprType(cur,right,op);
    }
    setType(&stmt, cur);
}

// 关系表达式
void SemanticAnalyzer::visit(RelExprStmt &stmt) {
    for (auto& term : stmt.terms) term.add_expr->accept(*this);

    std::string cur = getType(stmt.terms[0].add_expr.get());
    for (size_t i=1;i<stmt.terms.size();++i) {
        std::string right = getType(stmt.terms[i].add_expr.get());
        std::string op;
        switch(stmt.terms[i].type){
            case RelExprStmt::RelExprType::Equal:        op="==";break;
            case RelExprStmt::RelExprType::NotEqual:     op="!=";break;
            case RelExprStmt::RelExprType::Less:         op="<"; break;
            case RelExprStmt::RelExprType::LessEqual:    op="<=";break;
            case RelExprStmt::RelExprType::Greater:      op=">"; break;
            case RelExprStmt::RelExprType::GreaterEqual: op=">=";break;
            default: op="==";
        }
        cur = getBinaryExprType(cur,right,op);
    }
    setType(&stmt, cur);
}

// 顶层表达式
void SemanticAnalyzer::visit(ExprStmt &stmt) {
    if (stmt.rel_expr) {
        stmt.rel_expr->accept(*this);
        setType(&stmt, getType(stmt.rel_expr.get()));
    }
}


}