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
    bool isInitialized;           // 是否已初始化
    
    void addUsage(int line) { usedLines.push_back(line); }
    
    SymbolEntry() : isInitialized(false) {}
    
    SymbolEntry(std::string name, SymbolType type, ScopeType scope, 
                std::string dataType, bool isArray, std::vector<int> dimensions,
                bool isReference, std::vector<std::string> paramTypes, 
                std::string returnType, int lineOfDeclaration, 
                std::vector<int> usedLines, bool isInitialized = false)
        : name(name), type(type), scope(scope), dataType(dataType), 
          isArray(isArray), dimensions(dimensions), isReference(isReference),
          paramTypes(paramTypes), returnType(returnType), 
          lineOfDeclaration(lineOfDeclaration), usedLines(usedLines),
          isInitialized(isInitialized) {}
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
    std::unordered_map<const BaseNode*, std::string> inferredType;
private:
    SymbolTable symbolTable;
    std::vector<std::string> errors;
    std::string currentFunction;
    std::string currentReturnType;
    bool inLoop = false;
    bool functionHasReturn = false; // 跟踪函数是否有返回值

    // PASCAL-S标准函数
    void addStandardFunctions();
    
    // 类型检查辅助函数
    bool checkTypeConsistency(const std::string& leftType, const std::string& rightType);
    std::string getBinaryExprType(const std::string& leftType, const std::string& rightType, const std::string& op);
    std::string getUnaryExprType(const std::string& exprType, const std::string& op);
    
    // PASCAL-S类型检查辅助函数
    bool isOrdinalType(const std::string& type); // 检查是否为序数类型（整数、字符、布尔、枚举）
    bool isRealType(const std::string& type);    // 检查是否为实数类型
    bool checkArrayIndex(const std::string& indexType); // 检查数组索引类型
    bool isConstantModification(const std::string& name); // 检查是否修改常量
    
    // 变量初始化跟踪
    void markInitialized(const std::string& name);
    bool checkInitialized(const std::string& name);
    
    // 标准函数/过程检查
    bool isPascalStandardFunction(const std::string& name);
    bool isPascalStandardProcedure(const std::string& name);
    
    void setType(const BaseNode* n, const std::string& t) { inferredType[n] = t; }
public:
    SemanticAnalyzer();
    ~SemanticAnalyzer();
    
    // 语句访问函数
    void visit(ExprNode &stmt) override;
    void visit(RelExprNode &stmt) override;
    void visit(AddExprNode &stmt) override;
    void visit(MulExprNode &stmt) override;
    void visit(UnaryExprNode &stmt) override;
    void visit(PrimaryExprNode &stmt) override;
    void visit(ValueNode &stmt) override;
    void visit(NumberNode &stmt) override;
    void visit(StringNode &stmt) override;
    void visit(LValueNode &stmt) override;
    void visit(FuncCallNode &stmt) override;
    void visit(PeriodNode &stmt) override;
    void visit(ConstDeclNode &stmt) override;
    void visit(VarDeclNode &stmt) override;
    void visit(FuncHeadDeclNode &stmt) override;
    void visit(FuncBodyDeclNode &stmt) override;
    void visit(FuncDeclNode &stmt) override;
    void visit(AssignmentNode &stmt) override;
    void visit(IfNode &stmt) override;
    void visit(ForNode &stmt) override;
    void visit(WhileNode &stmt) override;
    void visit(ReadFuncNode &stmt) override;
    void visit(WriteFuncNode &stmt) override;
    void visit(BreakNode &stmt) override;
    void visit(ContinueNode &stmt) override;
    void visit(ProgramHeadNode &stmt) override;
    void visit(ProgramBodyNode &stmt) override;
    void visit(ProgramNode &stmt) override;
    
    // 错误处理
    void addError(const std::string& message);
    bool hasErrors() const;
    const std::vector<std::string>& getErrors() const;
    
    // 获取符号表
    SymbolTable& getSymbolTable();

    const std::string& getType(const BaseNode* n) const {
        static const std::string unknown = "unknown";
        auto it = inferredType.find(n);
        return it == inferredType.end() ? unknown : it->second;
    }

    const std::unordered_map<const BaseNode*, std::string>& getInferredType() const {
        return inferredType;
    }
};

// 类型转换辅助函数
std::string basicTypeToString(BasicType type);

}