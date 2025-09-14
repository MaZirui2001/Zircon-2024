# Ziron-2024

Zircon-2024是一款RISC-V处理器，由Zircon项目迭代而来。

## 文档

## 处理器架构

[处理器架构主页](https://mazirui2001.github.io/Zircon-2024-Web/Zircon2024/)

## 目录概览

以下是一些关键目录：

```bash
.
├── src
│   ├── main/scala
│   │   ├── Frontend        # 前端代码
│   │   ├── Dispatch        # 调度器代码
│   │   ├── Backend         # 后端代码
│   │   ├── Commit          # 提交段代码
│   │   ├── Config          # 处理器配置
│   │   ├── Utils           # 工具函数
│   │   └── CPU.scala       # 处理器主模块
│   └── test/scala
│       ├── emulator        # 仿真器代码
│       ├── simulator       # 模拟器代码
│       └── EmuMain.scala   # 仿真器主模块
├── build.mill              # mill构建文件
├── Makefile                # 构建脚本
└── mill                    # mill

```

## 构建项目

本项目基于Chisel开发，使用mill+Makefile进行构建，所有测试依赖于openJDK21+的Java以及Scala3环境。

### 编译Verilog源代码

进入项目根目录，执行：

```bash
make verilog
```

该命令会将verilog代码输出到verilog/目录下。

### 运行测试

* Zircon-2024采用Verilator作为后端仿真器，因此请确保在系统中安装了Verilator。

* 在RV-Software任意测试下，执行：
    ```bash
    make run
    ```
    注意：RV-Software的编译需要依赖支持riscv32的clang编译器和对应的lld链接器。
* 运行每一个测试后，仿真环境都会自动生成一份处理器性能报告，位于`test_run_dir/reports/`目录下。