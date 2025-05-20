#pragma once

#include <fstream>
#include <sstream>
#include <string>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include "ast/ast.hpp"
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
    std::string currentFunction;         // 当前处理的函数名
    bool hasReturn = false;              // 当前函数是否有返回语句
    bool isInLoop = false;               // 是否在循环内
    
    // 辅助函数
    std::string getIndent() const;
    std::string mapPascalTypeToC(const std::string& pascalType) const;
    std::string mapOperator(const std::string& pascalOp) const;
    void initTypeMapping();
    
    // 验证和清理生成的代码
    bool validateGeneratedCode();
    void cleanupGeneratedCode();

    std::string type2fmt(const std::string& t) const;

    int exprDepth = 0;              // >0 表示当前在“表达式”上下文

    void emitExpr(ExprNode *e);
    
    bool isLastStmtInFuncBody = false;

    void emitExprWithType(ExprNode* expr, const std::string& expectType);
    std::string currentExprType; // 用于跟踪当前表达式类型
public:
    CodeGenerator(const semantic::SemanticAnalyzer& s, semantic::SymbolTable& st);
    ~CodeGenerator();
    
    // 生成C代码
    std::string generate(ProgramNode* root);
    
    // 输出C代码到文件
    void output(std::ofstream& out);
    
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
};

} // namespace codegen