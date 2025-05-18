#pragma once

#include <fstream>
#include <sstream>
#include <string>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include "ast/stmt.hpp"
#include "ast/visitor.hpp"
#include "semantic_analyzer/semantic_analyzer.hpp"

namespace codegen {

// 代码生成器类
class CodeGenerator : public StmtVisitor {
    const semantic::SemanticAnalyzer& sema; 
private:
    std::stringstream code;              // 生成的C代码
    int indentLevel = 0;                 // 缩进级别
    semantic::SymbolTable& symbolTable;  // 符号表（从语义分析器获取）
    
    std::unordered_map<std::string, std::string> typeMapping; // Pascal到C类型映射
    std::unordered_set<std::string> currentVars;              // 当前作用域的变量
    
    std::string currentFunction;         // 当前处理的函数名
    bool hasReturn = false;              // 当前函数是否有返回语句
    
    bool isInLoop = false;               // 是否在循环内
    std::string breakLabel;              // break跳转标签
    std::string continueLabel;           // continue跳转标签
    
    // 辅助函数
    std::string getIndent() const;
    std::string mapPascalTypeToC(const std::string& pascalType) const;
    std::string mapOperator(const std::string& pascalOp) const;
    void initTypeMapping();
    
    // 分析表达式类型的辅助函数
    semantic::SymbolEntry* determineExpressionType(ExprStmt* expr);
    bool hasFloatingPointOperation(ExprStmt* expr);
    bool isSimpleExpression(ExprStmt* expr);
    
    // 分析变量使用的辅助函数
    bool isVariableUsedInScope(const std::string& varName);
    void trackVariableUse(const std::string& varName);
    
    // 验证和清理生成的代码
    bool validateGeneratedCode();
    void cleanupGeneratedCode();

    std::string type2fmt(const std::string& t) const;

    int exprDepth = 0;              // >0 表示当前在“表达式”上下文

    void emitExpr(ExprStmt *e);
    
    bool isLastStmtInFuncBody = false;
public:
    CodeGenerator(const semantic::SemanticAnalyzer& s, semantic::SymbolTable& st);
    ~CodeGenerator();
    
    // 生成C代码
    std::string generate(ProgramStmt* root);
    
    // 输出C代码到文件
    void output(std::ofstream& out);
    
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
    
    // 处理return语句
    void handleReturn(ExprStmt& expr);
};

} // namespace codegen