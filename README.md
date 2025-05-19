# compilation-pascal

本项目是一个基于 C++ 的编译器实现，支持从 Pascal 源代码到最终C语言代码的转换，包含词法分析、语法分析、语义分析、代码生成模块。

---

## 📦 功能模块

- **词法分析**
- **语法分析** 
- **语义分析**
- **代码生成**

---

## 🛠️ 构建步骤

请确保你的系统已安装以下依赖：

- `CMake` ≥ 3.16
- `g++` ≥ C++17

然后执行以下命令构建项目：

```bash
# 在项目根目录下执行
mkdir build && mkdir bin
cd build
cmake ..
make
```

编译成功后，会在 `bin` 目录下生成编译器可执行文件pascc。

---

## 🚀 使用方法

运行编译器进行编译：

```bash
./bin/pascc -i <输入文件路径>
```

示例：

```bash
./bin/pascc -i test_set/00_main.pas
```

---

## 📁 项目目录结构

```
.
├── src                     # 源码目录
│   ├── main.cpp            # 主程序入口
│   ├── parser              # 词法分析与语法分析（Flex + Bison）
│   ├── ast                 # 抽象语法树定义与构建
│   ├── common              # 公共工具与辅助函数
│   ├── code_generator      # 代码生成
│   ├── semantic_analyzer   # 语义分析
│   └── token               # 
├── build                   # 构建目录（由用户创建）
├── bin                     # 可执行文件输出目录
```

---

## 📄 示例输入

你可以将 Pascal 源代码放入 `test_set` 目录中进行测试。