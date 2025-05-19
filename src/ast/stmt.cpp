#include "ast/visitor.hpp"
#include "ast/stmt.hpp"

// 简化的访问者模式实现
// 使用宏减少重复代码
#define IMPLEMENT_ACCEPT(class_name) \
    void class_name::accept(StmtVisitor& visitor) { \
        visitor.visit(*this); \
    }

// 表达式语句
IMPLEMENT_ACCEPT(ExprNode)
IMPLEMENT_ACCEPT(RelExprNode)
IMPLEMENT_ACCEPT(AddExprNode)
IMPLEMENT_ACCEPT(MulExprNode)
IMPLEMENT_ACCEPT(UnaryExprNode)
IMPLEMENT_ACCEPT(PrimaryExprNode)
IMPLEMENT_ACCEPT(ValueNode)
IMPLEMENT_ACCEPT(NumberNode)
IMPLEMENT_ACCEPT(StringNode)
IMPLEMENT_ACCEPT(LValueNode)
IMPLEMENT_ACCEPT(FuncCallNode)

// 声明语句
IMPLEMENT_ACCEPT(PeriodNode)
IMPLEMENT_ACCEPT(ConstDeclNode)
IMPLEMENT_ACCEPT(VarDeclNode)
IMPLEMENT_ACCEPT(FuncHeadDeclNode)
IMPLEMENT_ACCEPT(FuncBodyDeclNode)
IMPLEMENT_ACCEPT(FuncDeclNode)

// 功能语句
IMPLEMENT_ACCEPT(AssignmentNode)
IMPLEMENT_ACCEPT(IfNode)
IMPLEMENT_ACCEPT(ForNode)
IMPLEMENT_ACCEPT(WhileNode)
IMPLEMENT_ACCEPT(ReadFuncNode)
IMPLEMENT_ACCEPT(WriteFuncNode)
IMPLEMENT_ACCEPT(BreakNode)
IMPLEMENT_ACCEPT(ContinueNode)

// 主程序
IMPLEMENT_ACCEPT(ProgramHeadNode)
IMPLEMENT_ACCEPT(ProgramBodyNode)
IMPLEMENT_ACCEPT(ProgramNode)

// 清理宏定义，避免污染后续代码
#undef IMPLEMENT_ACCEPT