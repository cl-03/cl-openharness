# cl-openharness

Open Agent Harness 的 Common Lisp 实现 - 用于构建 AI Agent 的核心基础设施。

## 概述

**cl-openharness** 是一个用纯 Common Lisp 编写的轻量级 Agent Harness，参考了 [OpenHarness](https://github.com/HKUDS/OpenHarness) 项目的架构设计。它提供了：

- 🔄 **Agent Loop** - 流式工具调用循环
- 🔧 **工具系统** - 43+ 内置工具（Bash、文件操作、搜索等）
- 📚 **技能系统** - 按需加载的技能
- 🔌 **插件系统** - 可扩展的插件架构
- 🛡️ **权限控制** - 多层级权限模式
- ⚡ **钩子系统** - PreToolUse/PostToolUse 生命周期钩子

## 架构

```
cl-openharness/
├── src/
│   ├── package.lisp        # 包定义
│   ├── utils.lisp          # 工具函数
│   ├── messages.lisp       # 消息协议
│   ├── tools.lisp          # 工具注册表和执行
│   ├── permissions.lisp    # 权限检查器
│   ├── hooks.lisp          # 钩子执行引擎
│   ├── api-client.lisp     # API 客户端封装
│   ├── query-engine.lisp   # 查询引擎和 Agent 循环
│   └── runtime.lisp        # 运行时和 Harness 主接口
├── tests/
│   ├── package.lisp
│   ├── test-messages.lisp
│   ├── test-tools.lisp
│   ├── test-permissions.lisp
│   └── run-tests.lisp
├── examples.lisp           # 使用示例
├── cl-openharness.asd      # ASDF 系统定义
└── README.lisp             # 本文件
```

## 安装

### 依赖

- Common Lisp 实现 (SBCL, CCL, ECL 等)
- Quicklisp
- 以下 Quicklisp 库:
  - `dexador` - HTTP 客户端
  - `jonathan` - JSON 编码/解码
  - `closer-mop` - MOP 支持
  - `bordeaux-threads` - 线程支持
  - `split-sequence` - 序列分割

### 安装依赖

```lisp
(ql:quickload '(dexador jonathan closer-mop bordeaux-threads split-sequence))
```

### 加载系统

```lisp
;; 添加 ASDF 源路径
(asdf:initialize-source-registry
 `(:source-registry
   (:tree ,"/path/to/cl-openharness")
   :inherit-configuration))

;; 加载系统
(asdf:load-system :cl-openharness)
```

## 快速开始

### 简单示例

```lisp
(use-package :cl-openharness)

;; 运行单个提示
(harness-run-single "What is the capital of France?"
                    :api-key "your-anthropic-api-key"
                    :model "claude-sonnet-4-6")
```

### 交互式会话

```lisp
(use-package :cl-openharness)

(with-harness (h :api-key "your-api-key"
                 :model "claude-sonnet-4-6"
                 :max-turns 10)
  ;; 第一轮
  (harness-run h "What is 2 + 2?")

  ;; 继续对话
  (harness-run h "Now multiply that by 3")

  ;; 查看使用量
  (harness-usage h))
```

### 自定义工具

```lisp
;; 创建自定义工具
(let ((calculator (make-function-tool
                   "calculate"
                   "Perform calculations"
                   (lambda (args ctx)
                     (let ((expr (gethash "expression" args)))
                       (make-tool-result (format nil "Result: ~a" expr)))))))
  (with-harness (h :api-key "your-key"
                   :tools (list calculator))
    (harness-run h "Calculate 5 + 3")))
```

## 核心组件

### 消息协议

```lisp
;; 创建文本块
(make-text-block "Hello, World!")

;; 创建工具使用块
(make-tool-use-block "Bash"
                     (alist-to-hash-table '(("command" . "ls -la"))))

;; 创建工具结果块
(make-tool-result-block "toolu_123" "output" nil)

;; 创建对话消息
(conversation-message-from-user-text "Hello!")
```

### 工具系统

```lisp
;; 创建工具注册表
(let ((registry (make-tool-registry)))
  ;; 注册内置工具
  (register-builtin-tools registry)

  ;; 注册自定义工具
  (tool-registry-register registry
    (make-function-tool "my-tool" "Description"
                        (lambda (args ctx) ...)))

  ;; 获取工具
  (tool-registry-get registry "my-tool"))
```

### 权限控制

```lisp
;; 创建权限设置
(let ((settings (make-permission-settings
                 :mode "default"  ; "auto", "default", "plan"
                 :denied-tools '("DangerousTool")
                 :path-rules (list (make-path-rule "/etc/*" nil)))))
  (let ((checker (make-permission-checker settings)))
    ;; 评估权限
    (permission-evaluate checker "Write"
                         :is-read-only nil
                         :file-path "/etc/passwd")))
```

### 钩子系统

```lisp
;; 创建钩子注册表
(let ((registry (make-hook-registry)))
  ;; 添加 PreToolUse 钩子
  (hook-registry-add registry
    (make-command-hook +hook-event-pre-tool-use+
                       "echo 'Tool: $ARGUMENTS'"
                       :matcher "*"))

  ;; 添加 PostToolUse 钩子
  (hook-registry-add registry
    (make-command-hook +hook-event-post-tool-use+
                       "echo 'Done: $ARGUMENTS'"))

  ;; 创建钩子执行器
  (let ((executor (make-hook-executor
                   registry
                   (make-hook-execution-context cwd api-client))))
    ;; 执行钩子
    (hook-execute executor +hook-event-pre-tool-use+
                  (list (cons "tool_name" "Bash")))))
```

## 权限模式

| 模式 | 行为 | 使用场景 |
|------|------|---------|
| `auto` | 允许所有操作 | 沙箱环境 |
| `default` | 变更操作需要确认 | 日常开发 |
| `plan` | 阻止所有变更操作 | 代码审查、规划 |

## 内置工具

| 工具 | 描述 | 只读 |
|------|------|------|
| `Bash` | 执行 shell 命令 | 否 |
| `Read` | 读取文件 | 是 |
| `Write` | 写入文件 | 否 |
| `Edit` | 编辑文件 | 否 |
| `Glob` | 查找匹配的文件 | 是 |
| `Grep` | 搜索文件内容 | 是 |

## 运行测试

```lisp
(asdf:load-system :cl-openharness/test)
(cl-openharness/test:run-all-tests)
```

## 运行示例

```lisp
(load "examples.lisp")
(cl-openharness:run-all-examples)
```

## API 配置

### Anthropic 官方

```lisp
(with-harness (h :api-key "your-anthropic-key"
                 :base-url "https://api.anthropic.com"
                 :model "claude-sonnet-4-6")
  ...)
```

### Anthropic 兼容 API (Kimi, GLM 等)

```lisp
(with-harness (h :api-key "your-kimi-key"
                 :base-url "https://api.moonshot.cn/anthropic"
                 :model "kimi-k2.5")
  ...)
```

## 会话管理

```lisp
;; 创建 harness
(let ((harness (make-harness :bundle (make-runtime-bundle
                                      :api-key "your-key"))))
  ;; 获取会话 ID
  (harness-session-id harness)

  ;; 运行对话
  (harness-run harness "Hello!")

  ;; 查看消息历史
  (harness-messages harness)

  ;; 清空对话
  (harness-clear harness)

  ;; 停止 harness
  (harness-stop harness))
```

## 许可证

MIT License

## 致谢

本项目参考了以下开源项目的设计：
- [OpenHarness](https://github.com/HKUDS/OpenHarness) - Python 实现的 Open Agent Harness
