#pragma once

#include <string>
#include <unordered_map>
#include <vector>
#include <memory>
#include <vector>
#include "ast/stmt.hpp"
#include "ast/visitor.hpp"

namespace semantic {

// 符号类型
enum class SymbolType {
    VARIABLE,
    FUNCTION,
    CONSTANT
};

// 作用域类型
enum class ScopeType {
    GLOBAL,
    LOCAL
};

// 符号表项
struct SymbolEntry {
    std::string name;             // 标识符名称
    SymbolType type;              // 变量、函数、常量等
    ScopeType scope;              // 作用域（全局、局部）
    std::string dataType;         // 数据类型（如 int, float）
    bool isArray;                 // 是否为数组
    std::vector<int> dimensions;  // 数组维度
    bool isReference;             // 是否为引用参数
    std::vector<std::string> paramTypes; // 函数参数类型列表
    std::string returnType;       // 函数返回类型
    int lineOfDeclaration;        // 定义所在行
    std::vector<int> usedLines;   // 记录被引用的行号
    
    void addUsage(int line) { usedLines.push_back(line); }
};

// 符号表
class SymbolTable {
private:
    std::vector<std::unordered_map<std::string, SymbolEntry>> scopeStack;
    ScopeType currentScopeType = ScopeType::GLOBAL;

public:
    SymbolTable();
    ~SymbolTable();
    
    void enterScope();
    void exitScope();
    bool insertSymbol(const SymbolEntry& entry);
    SymbolEntry* lookupSymbol(const std::string &name);
    SymbolEntry* lookupSymbolInCurrentScope(const std::string &name);
    void recordUsage(const std::string &name, int line);
    void printSymbolTable();
    bool isGlobalScope() const;
    ScopeType getCurrentScopeType() const;
};

// 语义分析器
class SemanticAnalyzer : public StmtVisitor {
    std::unordered_map<const BaseStmt*, std::string> inferredType;
private:
    SymbolTable symbolTable;
    std::vector<std::string> errors;
    std::string currentFunction;
    std::string currentReturnType;
    bool inLoop = false;
    
    // 类型检查辅助函数
    bool checkTypeConsistency(const std::string& leftType, const std::string& rightType);
    std::string getBinaryExprType(const std::string& leftType, const std::string& rightType, const std::string& op);
    std::string getUnaryExprType(const std::string& exprType, const std::string& op);
    
    void setType(const BaseStmt* n, const std::string& t) { inferredType[n] = t; }
public:
    SemanticAnalyzer();
    ~SemanticAnalyzer();
    
    // 语句访问函数
    void visit(ExprStmt &stmt) override;
    void visit(RelExprStmt &stmt) override;
    void visit(AddExprStmt &stmt) override;
    void visit(MulExprStmt &stmt) override;
    void visit(UnaryExprStmt &stmt) override;
    void visit(PrimaryExprStmt &stmt) override;
    void visit(ValueStmt &stmt) override;
    void visit(NumberStmt &stmt) override;
    void visit(StrStmt &stmt) override;
    void visit(LValStmt &stmt) override;
    void visit(FuncCallStmt &stmt) override;
    void visit(PeriodStmt &stmt) override;
    void visit(ConstDeclStmt &stmt) override;
    void visit(VarDeclStmt &stmt) override;
    void visit(FuncHeadDeclStmt &stmt) override;
    void visit(FuncBodyDeclStmt &stmt) override;
    void visit(FuncDeclStmt &stmt) override;
    void visit(AssignStmt &stmt) override;
    void visit(IfStmt &stmt) override;
    void visit(ForStmt &stmt) override;
    void visit(WhileStmt &stmt) override;
    void visit(ReadFuncStmt &stmt) override;
    void visit(WriteFuncStmt &stmt) override;
    void visit(BreakStmt &stmt) override;
    void visit(ContinueStmt &stmt) override;
    void visit(ProgramHeadStmt &stmt) override;
    void visit(ProgramBodyStmt &stmt) override;
    void visit(ProgramStmt &stmt) override;
    
    // 错误处理
    void addError(const std::string& message);
    bool hasErrors() const;
    const std::vector<std::string>& getErrors() const;
    
    // 获取符号表
    SymbolTable& getSymbolTable();

    const std::string& getType(const BaseStmt* n) const {
        static const std::string unknown = "unknown";
        auto it = inferredType.find(n);
        return it == inferredType.end() ? unknown : it->second;
    }

    const std::unordered_map<const BaseStmt*, std::string>& getInferredType() const {
        return inferredType;
    }
};

// 类型转换辅助函数
std::string basicTypeToString(BasicType type);

}