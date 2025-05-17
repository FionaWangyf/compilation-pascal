#include "builder/builder.hpp"
#include "common/log/log.hpp"
#include "ir/ir.hpp"
#include "common/setting/settings.hpp"
#include "ir/ir_gen.hpp"

namespace builder {

void Builder::HandleBasicBlock(std::string prefix, std::shared_ptr<ir::BasicBlock> bb, std::stringstream &out)
{
    // 检查是否已经处理过该基本块
    if (processed_bbs_.find(bb.get()) != processed_bbs_.end()) {
        return;
    }

    // 判断基本块是否为空
    if (bb->instructions_.empty()) {
        if (bb->name_ == "body_basic_block" || bb->name_ == "then_basic_block") {
            out << "{}";
        }
        return;
    }

    // 标记为已处理
    processed_bbs_.insert(bb.get());

    if (bb->name_ == "body_basic_block" || bb->name_ == "then_basic_block" || bb->name_ == "body_begin_basic_block") {
        out << prefix << "{\n";
    }
    if (bb->name_ == "else_basic_block") {
        out << prefix << "else {\n";
    }

    // 遍历基本块中指令列表
    for (int k = 0; k < bb->instructions_.size(); k++) {
        std::shared_ptr<ir::Instruction> inst = bb->instructions_[k];
        if (inst->op_id_ == ir::Instruction::OpID::Br) {
            out << prefix << inst->print() << "\n";
        } else {
            out << prefix << inst->print() << ";\n";
        }
    }

    // 遍历后续基本块
    for (int k = 0; k < bb->succ_bbs_.size(); k++) {
        std::weak_ptr<ir::BasicBlock> nextBlock = bb->succ_bbs_[k];
        std::shared_ptr<ir::BasicBlock> nextBlockShared = nextBlock.lock();
        if (nextBlockShared) {
            HandleBasicBlock(prefix + "\t", nextBlockShared, out);
        }
    }

    if (bb->name_ == "body_basic_block" || bb->name_ == "then_basic_block" || bb->name_ == "else_basic_block" || bb->name_ == "body_end_basic_block") {
        out << prefix << "}\n";
    }
}

void Builder::build(ir::Module &program)
{
    // 初始化 stream
    std::stringstream out;

    // 加上 C 语言的头文件
    out << "#include <stdio.h>\n";
    out << "#include <stdlib.h>\n";
    out << "#include <sys/types.h>\n";
    out << "#include <sys/wait.h>\n";
    out << "#include <unistd.h>\n";

    // 遍历全局变量和常量
    for (const auto &global : program.global_identifiers_) {
        if (global->is_const_) {
            out << "const " + global->type_->print() << " " << global->name_ << " = " << global->init_val_->print()
                << ";\n";
        } else {
            std::string s = global->type_->print();
            int ps = s.find(" ");
            if (ps == std::string::npos) {
                out << global->type_->print() << " " << global->name_ << ";\n";
            } else {
                out << s.substr(0, ps) << " " << global->name_ << s.substr(ps + 1) << ";\n";
            }
        }
    }

    // 遍历函数
    for (int i = 0; i < program.functions_.size(); i++) {
        std::weak_ptr<ir::Function> func = program.functions_[i];

        // 加入函数头
        out << func.lock()->print() << "\n";

        // 加入函数左大括号
        out << "{\n";

        // 遍历局部变量
        for (const auto &local : func.lock()->local_identifiers_) {
            bool isFuncParam = false;
            for (const auto &local_args : func.lock()->args_) {
                if (local->name_ == local_args->name_) {
                    isFuncParam = true;
                    break;
                }
            }
            if (local->is_const_ && !isFuncParam) {
                out << "const " + local->type_->print() << " " << local->name_ << " = " << local->init_val_->print()
                    << ";\n";
            } else if (!isFuncParam) {
                std::string s = local->type_->print();
                int ps = s.find(" ");
                if (ps == std::string::npos) {
                    out << local->type_->print() << " " << local->name_ << ";\n";
                } else {
                    out << s.substr(0, ps) << " " << local->name_ << s.substr(ps + 1) << ";\n";
                }
            }
        }

        // 遍历基本块
        for (int j = 0; j < func.lock()->basic_blocks_.size(); j++) {
            std::shared_ptr<ir::BasicBlock> bb = func.lock()->basic_blocks_[j];
            HandleBasicBlock("", bb, out);
        }

        // 加入函数右大括号
        if (func.lock()->print() == "int main()") {
            out << "return 0;\n";
        } else if (func.lock()->func_type_.lock()->result_->tid_ != ir::Type::VoidTID) {
            out << "return __;\n";
        }
        out << "}\n";
    }

    code_ = out.str();
    LOG_DEBUG("Builder: code generated");
}

void Builder::output(std::ofstream &out)
{
    out << code_;
}

} // namespace builder