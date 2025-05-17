#pragma once

#include <fstream>
#include <sstream>
#include <string>
#include <memory>
#include <unordered_set>
#include "ir/ir.hpp"

namespace builder {

class Builder {
public:
    Builder() = default;
    ~Builder() = default;

    /**
     * @brief 构建目标代码
     * @param program 中间代码
     * @details 接受中间代码，生成目标代码，并保存在类内部
     */
    void build(ir::Module &program);

    /**
     * @brief 输出目标代码
     * @details 输出目标代码到文件输出流
     */
    void output(std::ofstream &out);

private:
    std::string code_; // 存储生成的 C 语言代码

    // 辅助函数：处理基本块
    void HandleBasicBlock(std::string prefix, std::shared_ptr<ir::BasicBlock> bb, std::stringstream &out);

    // 已处理的基本块集合，用于避免重复处理
    std::unordered_set<const ir::BasicBlock *> processed_bbs_;
};

} // namespace builder