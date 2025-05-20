#include <algorithm>
#include <cstddef>
#include <dirent.h>
#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>
#include <vector>

#include "ast/ast.hpp"
#include "common/log/log.hpp"
#include "ASTPrettyPrinter.hpp"

// 声明外部函数
extern int code_parse(const char *code_str, ProgramNode **program);
extern bool getSyntaxErrorFlag(); // 获取语法错误标志
extern void resetSyntaxError();   // 重置语法错误标志

void single_point_test(const std::string folderPath,
                       std::vector<std::string> files) {
  // 输入数字 0 - 69 分别对应 70 个文件
  size_t fileIndex;
  std::cout << "Please input the index of the file you want to analyze: ";
  std::cin >> fileIndex;
  if (fileIndex >= files.size()) {
    LOG_FATAL("ERROR : Invalid index.");
  }
  std::string fileName = files[fileIndex];
  std::ifstream ifs(folderPath + "/" + fileName);
  std::stringstream ss;
  ss << ifs.rdbuf();
  std::string content = ss.str();
  LOG_INFO("File: %s", fileName.c_str());
  
  ProgramNode *program = nullptr;
  resetSyntaxError(); // 重置语法错误标志
  code_parse(content.c_str(), &program);

  if (program == nullptr) {
    LOG_FATAL("ERROR : Parsing failed completely.");
  } else if (getSyntaxErrorFlag()) {
    LOG_ERROR("Syntax errors detected in file %s, not generating AST visualization.", fileName.c_str());
    delete program; // 清理不完整的AST
  } else {
    LOG_INFO("File %s Parsing succeeded.", fileName.c_str());
    
    // 只有在没有语法错误时才打印AST
    ASTPrettyPrinter printer;
    std::string astOutput = printer.printProgram(program);
    std::cout << astOutput << std::endl;
    
    delete program;
  }
}

void batch_test(int beginIndex, const std::string folderPath,
                std::vector<std::string> files) {
  for (size_t i = beginIndex; i < files.size(); i++) {
    std::string fileName = files[i];
    std::ifstream ifs(folderPath + "/" + fileName);
    std::stringstream ss;
    ss << ifs.rdbuf();
    std::string content = ss.str();
    
    ProgramNode *program = nullptr;
    resetSyntaxError(); // 重置语法错误标志
    code_parse(content.c_str(), &program);
    
    if (program == nullptr) {
      LOG_ERROR("ERROR : File %s: Parsing failed completely.", fileName.c_str());
    } else if (getSyntaxErrorFlag()) {
      LOG_ERROR("File %s: Syntax errors detected, not generating AST visualization.", fileName.c_str());
      delete program; // 清理不完整的AST
    } else {
      LOG_INFO("File %s Parsing succeeded.", fileName.c_str());
      
      // 只有在没有语法错误时才打印AST
      ASTPrettyPrinter printer;
      std::string astOutput = printer.printProgram(program);
      std::cout << astOutput << std::endl;
      
      delete program;
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Usage: " << argv[0] << " <folderPath>\n";
    return 1;
  }

  const std::string folderPath = argv[1];
  std::vector<std::string> files;
  DIR *dir;
  struct dirent *ent;
  if ((dir = opendir(folderPath.c_str())) != NULL) {
    while ((ent = readdir(dir)) != NULL) {
      std::string fileName = ent->d_name;
      if (fileName != "." && fileName != "..") {
        files.push_back(fileName);
      }
    }
    closedir(dir);
  } else {
    std::cerr << "ERROR : Could not open directory." << std::endl;
    return 1;
  }

  std::sort(files.begin(), files.end());
  for (size_t i = 0; i < files.size(); i++) {
    // 识别出第fileIndex个 以 .pas 结尾的文件
    if (files[i].find(".pas") == std::string::npos) {
      files.erase(files.begin() + i);
      i--;
    }
  }
  
  common::g_log = new common::Log(common::DEBUG);
  
  // 用户可以选择测试模式
  std::cout << "Select test mode:\n";
  std::cout << "1. Single file test\n";
  std::cout << "2. Batch test (all files)\n";
  std::cout << "Enter your choice (1 or 2): ";
  
  int choice;
  std::cin >> choice;
  
  if (choice == 1) {
    single_point_test(folderPath, files);
  } else if (choice == 2) {
    batch_test(0, folderPath, files);
  } else {
    std::cerr << "Invalid choice.\n";
  }
  
  delete common::g_log;
  return 0;
}