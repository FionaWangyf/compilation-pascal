#include "ast/visitor.hpp"
#include "ast/stmt.hpp"

// 简化的访问者模式实现
// 使用宏减少重复代码
#define IMPLEMENT_ACCEPT(class_name) \
    void class_name::accept(StmtVisitor& visitor) { \
        visitor.visit(*this); \
    }

// 表达式语句
IMPLEMENT_ACCEPT(ExprStmt)
IMPLEMENT_ACCEPT(RelExprStmt)
IMPLEMENT_ACCEPT(AddExprStmt)
IMPLEMENT_ACCEPT(MulExprStmt)
IMPLEMENT_ACCEPT(UnaryExprStmt)
IMPLEMENT_ACCEPT(PrimaryExprStmt)
IMPLEMENT_ACCEPT(ValueStmt)
IMPLEMENT_ACCEPT(NumberStmt)
IMPLEMENT_ACCEPT(StrStmt)
IMPLEMENT_ACCEPT(LValStmt)
IMPLEMENT_ACCEPT(FuncCallStmt)

// 声明语句
IMPLEMENT_ACCEPT(PeriodStmt)
IMPLEMENT_ACCEPT(ConstDeclStmt)
IMPLEMENT_ACCEPT(VarDeclStmt)
IMPLEMENT_ACCEPT(FuncHeadDeclStmt)
IMPLEMENT_ACCEPT(FuncBodyDeclStmt)
IMPLEMENT_ACCEPT(FuncDeclStmt)

// 功能语句
IMPLEMENT_ACCEPT(AssignStmt)
IMPLEMENT_ACCEPT(IfStmt)
IMPLEMENT_ACCEPT(ForStmt)
IMPLEMENT_ACCEPT(WhileStmt)
IMPLEMENT_ACCEPT(ReadFuncStmt)
IMPLEMENT_ACCEPT(WriteFuncStmt)
IMPLEMENT_ACCEPT(BreakStmt)
IMPLEMENT_ACCEPT(ContinueStmt)

// 主程序
IMPLEMENT_ACCEPT(ProgramHeadStmt)
IMPLEMENT_ACCEPT(ProgramBodyStmt)
IMPLEMENT_ACCEPT(ProgramStmt)

// 清理宏定义，避免污染后续代码
#undef IMPLEMENT_ACCEPT