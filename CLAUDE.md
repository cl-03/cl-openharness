# cl-openharness

纯 Common Lisp 实现的 Open Agent Harness。

## 项目结构

```
cl-openharness/
├── src/                        # 源代码
│   ├── package.lisp            # 包定义和导出
│   ├── utils.lisp              # 工具函数 (UUID, JSON, 重试逻辑)
│   ├── messages.lisp           # 消息协议 (TextBlock, ToolUseBlock, etc.)
│   ├── tools.lisp              # 工具注册表和执行
│   ├── permissions.lisp        # 权限检查器
│   ├── hooks.lisp              # 钩子执行引擎
│   ├── api-client.lisp         # Anthropic API 客户端
│   ├── query-engine.lisp       # 查询引擎和 Agent 循环
│   └── runtime.lisp            # Harness 运行时接口
├── tests/                      # 测试
│   ├── package.lisp
│   ├── test-messages.lisp      # 消息测试
│   ├── test-tools.lisp         # 工具测试
│   ├── test-permissions.lisp   # 权限测试
│   └── run-tests.lisp          # 测试运行器
├── examples.lisp               # 使用示例
├── cl-openharness.asd          # ASDF 系统定义
└── README.md                   # 文档
```

## 快速开始

```lisp
;; 加载依赖
(ql:quickload '(dexador jonathan closer-mop bordeaux-threads split-sequence))

;; 加载系统
(asdf:load-system :cl-openharness)

;; 使用
(use-package :cl-openharness)

;; 简单示例
(harness-run-single "Hello!" :api-key "your-key")
```

## 核心 API

### Harness

- `make-harness` - 创建 Harness 实例
- `harness-run` - 运行提示
- `harness-continue` - 继续对话
- `harness-stop` - 停止 Harness
- `harness-clear` - 清空对话
- `harness-messages` - 获取消息历史
- `harness-usage` - 获取 token 使用量

### 消息

- `make-text-block` - 创建文本块
- `make-tool-use-block` - 创建工具使用块
- `make-tool-result-block` - 创建工具结果块
- `conversation-message-from-user-text` - 创建用户消息

### 工具

- `make-tool-registry` - 创建工具注册表
- `tool-registry-register` - 注册工具
- `tool-registry-get` - 获取工具
- `make-function-tool` - 创建函数工具
- `register-builtin-tools` - 注册内置工具

### 权限

- `make-permission-settings` - 创建权限设置
- `make-permission-checker` - 创建权限检查器
- `permission-evaluate` - 评估权限
- `make-path-rule` - 创建路径规则

### 钩子

- `make-hook-registry` - 创建钩子注册表
- `hook-registry-add` - 添加钩子
- `make-hook-executor` - 创建钩子执行器
- `hook-execute` - 执行钩子
- `make-command-hook` - 创建命令钩子

## 测试

```lisp
(asdf:load-system :cl-openharness/test)
(cl-openharness/test:run-all-tests)
```

## TODO

- [ ] 添加技能系统
- [ ] 添加 MCP 客户端
- [ ] 添加会话持久化
- [ ] 添加更多内置工具
- [ ] 添加流式响应支持
