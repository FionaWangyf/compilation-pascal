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
#include "ast/stmt.hpp"
#include "common/setting/settings.hpp"
#include "semantic_analyzer/semantic_analyzer.hpp"
#include "code_generator/code_generator.hpp"
#include "parser/yacc_pascal.hpp"

int code_parse(const char *code, ProgramNode **program_stmt);

void init_env()
{
    if (G_SETTINGS.output_file.empty())
    {
        size_t pos = G_SETTINGS.input_file.find_last_of('.');
        if (pos == std::string::npos)
        {
            G_SETTINGS.output_file = G_SETTINGS.input_file + ".c";
        }
        else
        {
            G_SETTINGS.output_file = G_SETTINGS.input_file.substr(0, pos) + ".c";
        }
        pos = G_SETTINGS.input_file.find_last_of("/\\");
        std::string filename;
        if (pos != std::string::npos)
            filename = G_SETTINGS.input_file.substr(pos + 1);
        else
            filename = G_SETTINGS.input_file;

        LOG_DEBUG("Output file: %s", G_SETTINGS.output_file.c_str());
    }
#ifndef ONLINE_JUDGE
    switch (G_SETTINGS.log_level)
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
    G_SETTINGS.parse_args(argc, argv);
    // 初始化环境
    init_env();
    // 管理日志
    std::unique_ptr<common::Log> log(common::g_log);
    // 从输入文件中读取代码
    std::ifstream input_file(G_SETTINGS.input_file);
    if (!input_file.is_open())
    {
        LOG_FATAL("Can't open input file: %s", G_SETTINGS.input_file.c_str());
    }
    std::stringstream buffer;
    buffer << input_file.rdbuf();
    std::string code = buffer.str();
    input_file.close();
    
    // 第一步：词法分析 and 语法分析
    LOG_DEBUG("Start parsing code...");
    ProgramNode* program_stmt;
    int ret = code_parse(code.c_str(), &program_stmt);
    if (ret != 0)
    {
        LOG_ERROR("Parsing code failed.");
        return ret;
    }
    LOG_DEBUG("Parsing code done.");
    if(!program_stmt) {
        LOG_FATAL("Program exit");
        return -1;
    }
    
    // 第二步: 语义分析
    LOG_DEBUG("Start semantic analysis...");
    semantic::SemanticAnalyzer semanticAnalyzer;
    program_stmt->accept(semanticAnalyzer);
    
    if (semanticAnalyzer.hasErrors()) {
        LOG_ERROR("Semantic analysis found errors.");
        for (const auto& error : semanticAnalyzer.getErrors()) {
            LOG_ERROR("%s", error.c_str());
        }
        delete program_stmt;
        return -1;
    }
    LOG_DEBUG("Semantic analysis done.");
    // std::cout << "Inferred Types:" << std::endl;
    // for (const auto& pair : semanticAnalyzer.getInferredType()) {
    //     const BaseNode* stmt = pair.first;
    //     const std::string& type = pair.second;
    //     std::cout << "  Statement: " << typeid(*stmt).name() << ", Type: " << type << std::endl;
    // }
    
    // 第三步: 代码生成
    LOG_DEBUG("Start generating target code...");
    LOG_INFO("Generating C code...");
    codegen::CodeGenerator codeGenerator(semanticAnalyzer, semanticAnalyzer.getSymbolTable());
    std::string generatedCode = codeGenerator.generate(program_stmt);
    
    // 输出结果到文件
    std::ofstream output_file(G_SETTINGS.output_file);
    if (!output_file.is_open()) {
        LOG_FATAL("Can't open output file: %s", G_SETTINGS.output_file.c_str());
        delete program_stmt;
        return -1;
    }
    
    output_file << generatedCode;
    output_file.close();
    LOG_DEBUG("Generating target code done.");
    
    // 释放AST资源
    delete program_stmt;
    
    return 0;
}