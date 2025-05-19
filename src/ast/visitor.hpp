#pragma once

class ExprNode;
class RelExprNode;
class AddExprNode;
class MulExprNode;
class UnaryExprNode;
class PrimaryExprNode;
class ValueNode;
class NumberNode;
class StringNode;
class LValueNode;

class FuncCallNode;
class PeriodNode;
class ConstDeclNode;
class VarDeclNode;
class FuncHeadDeclNode;
class FuncBodyDeclNode;
class FuncDeclNode;
class AssignmentNode;
class IfNode;
class ForNode;
class WhileNode;
class ReadFuncNode;
class WriteFuncNode;
class BreakNode;
class ContinueNode;
class ProgramHeadNode;
class ProgramBodyNode;
class ProgramNode;

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
    virtual void visit(ExprNode &stmt) = 0;
    virtual void visit(RelExprNode &stmt) = 0;
    virtual void visit(AddExprNode &stmt) = 0;
    virtual void visit(MulExprNode &stmt) = 0;
    virtual void visit(UnaryExprNode &stmt) = 0;
    virtual void visit(PrimaryExprNode &stmt) = 0;
    virtual void visit(ValueNode &stmt) = 0;
    virtual void visit(NumberNode &stmt) = 0;
    virtual void visit(StringNode &stmt) = 0;
    virtual void visit(LValueNode &stmt) = 0;
    virtual void visit(FuncCallNode &stmt) = 0;

    // 声明相关节点访问方法
    virtual void visit(PeriodNode &stmt) = 0;
    virtual void visit(ConstDeclNode &stmt) = 0;
    virtual void visit(VarDeclNode &stmt) = 0;
    virtual void visit(FuncHeadDeclNode &stmt) = 0;
    virtual void visit(FuncBodyDeclNode &stmt) = 0;
    virtual void visit(FuncDeclNode &stmt) = 0;

    // 语句相关节点访问方法
    virtual void visit(AssignmentNode &stmt) = 0;
    virtual void visit(IfNode &stmt) = 0;
    virtual void visit(ForNode &stmt) = 0;
    virtual void visit(WhileNode &stmt) = 0;
    virtual void visit(ReadFuncNode &stmt) = 0;
    virtual void visit(WriteFuncNode &stmt) = 0;
    virtual void visit(BreakNode &stmt) = 0;
    virtual void visit(ContinueNode &stmt) = 0;

    // 程序结构相关节点访问方法
    virtual void visit(ProgramHeadNode &stmt) = 0;
    virtual void visit(ProgramBodyNode &stmt) = 0;
    virtual void visit(ProgramNode &stmt) = 0;
};