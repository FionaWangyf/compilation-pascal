#include <cstddef>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <vector>

#include "common/log/log.hpp"
#include "ast/ast.hpp"
#include "common/setting/settings.hpp"
#include "semantic_analyzer/semantic_analyzer.hpp"
#include "code_generator/code_generator.hpp"
#include "parser/yacc_pascal.hpp"




int code_parse(const char *code, ProgramNode **program_stmt);

void init_env()
{
    if (SETTINGS.output_file.empty())
    {
        size_t pos = SETTINGS.input_file.find_last_of('.');
        if (pos == std::string::npos)
        {
            SETTINGS.output_file = SETTINGS.input_file + ".c";
        }
        else
        {
            SETTINGS.output_file = SETTINGS.input_file.substr(0, pos) + ".c";
        }
        pos = SETTINGS.input_file.find_last_of("/\\");
        std::string filename;
        if (pos != std::string::npos)
            filename = SETTINGS.input_file.substr(pos + 1);
        else
            filename = SETTINGS.input_file;

        LOG_DEBUG("Output file: %s", SETTINGS.output_file.c_str());
    }
#ifndef ONLINE_JUDGE
    switch (SETTINGS.log_level)
    {
        case 0:
            common::g_log = new common::Log(common::FATAL);
            break;
        case 1:
            common::g_log = new common::Log(common::ERROR);
            break;
        case 2:
            common::g_log = new common::Log(common::WARN);
            break;
        case 3:
            common::g_log = new common::Log(common::INFO);
            break;
        case 4:
            common::g_log = new common::Log(common::DEBUG);
            break;
        default:
            common::g_log = new common::Log(common::WARN);
            break;
    }
#endif
}

int main(int argc, char *argv[])
{
    // 解析命令行参数
    SETTINGS.parse_args(argc, argv);

    // 初始化环境
    init_env();

    // 管理日志
    std::unique_ptr<common::Log> log(common::g_log);

    // 打开输入文件并读取内容
    std::ifstream fin(SETTINGS.input_file);
    if (!fin)
    {
        LOG_FATAL("无法打开输入文件: %s", SETTINGS.input_file.c_str());
        return -1;
    }
    std::string code((std::istreambuf_iterator<char>(fin)), std::istreambuf_iterator<char>());
    fin.close();

    // 语法分析
    LOG_DEBUG("开始语法分析...");
    ProgramNode* root = nullptr;
    int parse_result = code_parse(code.c_str(), &root);
    if (parse_result != 0)
    {
        LOG_ERROR("语法分析失败。");
        return parse_result;
    }
    LOG_DEBUG("语法分析完成。");
    if (!root)
    {
        LOG_FATAL("未生成语法树，程序退出。");
        return -1;
    }

    // 语义分析
    LOG_DEBUG("开始语义分析...");
    semantic::SemanticAnalyzer analyzer;
    root->accept(analyzer);

    if (analyzer.hasErrors())
    {
        LOG_ERROR("语义分析发现错误：");
        for (const auto& err : analyzer.getErrors())
        {
            LOG_ERROR("%s", err.c_str());
        }
        delete root;
        return -1;
    }
    LOG_DEBUG("语义分析完成。");

    // 代码生成
    LOG_DEBUG("开始生成C代码...");
    LOG_INFO("正在生成C代码...");
    codegen::CodeGenerator generator(analyzer, analyzer.getSymbolTable());
    std::string c_code = generator.generate(root);

    // 写入输出文件
    std::ofstream fout(SETTINGS.output_file);
    if (!fout)
    {
        LOG_FATAL("无法打开输出文件: %s", SETTINGS.output_file.c_str());
        delete root;
        return -1;
    }
    fout << c_code;
    fout.close();
    LOG_DEBUG("C代码生成完毕。");

    // 释放AST资源
    delete root;

    return 0;
}