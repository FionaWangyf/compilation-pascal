#pragma once

class ExprStmt;
class RelExprStmt;
class AddExprStmt;
class MulExprStmt;
class UnaryExprStmt;
class PrimaryExprStmt;
class ValueStmt;
class NumberStmt;
class StrStmt;
class LValStmt;

class FuncCallStmt;
class PeriodStmt;
class ConstDeclStmt;
class VarDeclStmt;
class FuncHeadDeclStmt;
class FuncBodyDeclStmt;
class FuncDeclStmt;
class AssignStmt;
class IfStmt;
class ForStmt;
class WhileStmt;
class ReadFuncStmt;
class WriteFuncStmt;
class BreakStmt;
class ContinueStmt;
class ProgramHeadStmt;
class ProgramBodyStmt;
class ProgramStmt;

/**
 * @brief AST节点访问器基类
 *
 * 采用访问者模式，用于遍历和操作AST树
 * 具体的功能（如代码生成、静态检查等）通过继承此类实现
 */
class StmtVisitor {
public:
    virtual ~StmtVisitor() = default;

    // 表达式相关节点访问方法
    virtual void visit(ExprStmt &stmt) = 0;
    virtual void visit(RelExprStmt &stmt) = 0;
    virtual void visit(AddExprStmt &stmt) = 0;
    virtual void visit(MulExprStmt &stmt) = 0;
    virtual void visit(UnaryExprStmt &stmt) = 0;
    virtual void visit(PrimaryExprStmt &stmt) = 0;
    virtual void visit(ValueStmt &stmt) = 0;
    virtual void visit(NumberStmt &stmt) = 0;
    virtual void visit(StrStmt &stmt) = 0;
    virtual void visit(LValStmt &stmt) = 0;
    virtual void visit(FuncCallStmt &stmt) = 0;

    // 声明相关节点访问方法
    virtual void visit(PeriodStmt &stmt) = 0;
    virtual void visit(ConstDeclStmt &stmt) = 0;
    virtual void visit(VarDeclStmt &stmt) = 0;
    virtual void visit(FuncHeadDeclStmt &stmt) = 0;
    virtual void visit(FuncBodyDeclStmt &stmt) = 0;
    virtual void visit(FuncDeclStmt &stmt) = 0;

    // 语句相关节点访问方法
    virtual void visit(AssignStmt &stmt) = 0;
    virtual void visit(IfStmt &stmt) = 0;
    virtual void visit(ForStmt &stmt) = 0;
    virtual void visit(WhileStmt &stmt) = 0;
    virtual void visit(ReadFuncStmt &stmt) = 0;
    virtual void visit(WriteFuncStmt &stmt) = 0;
    virtual void visit(BreakStmt &stmt) = 0;
    virtual void visit(ContinueStmt &stmt) = 0;

    // 程序结构相关节点访问方法
    virtual void visit(ProgramHeadStmt &stmt) = 0;
    virtual void visit(ProgramBodyStmt &stmt) = 0;
    virtual void visit(ProgramStmt &stmt) = 0;
};