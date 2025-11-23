# Org Ladder - Org-mode 游戏化系统

Org Ladder 是一个基于 org-mode 任务完成时间的游戏化系统，灵感来源于炉石传说的天梯排名机制。

## 功能特性

- 🎯 **实时积分计算**：基于 org-agenda 文件自动计算任务完成时间
- 📊 **段位系统**：青铜 → 白银 → 黄金 → 白金 → 钻石 → 传说
- 🔄 **月度重置**：每月1号重置积分，保留10%作为起步优势
- 💾 **持久化存储**：自动保存月度历史记录
- 🖥️ **集成显示**：在 org-agenda 中显示当前段位状态
- ⚡ **缓存优化**：5分钟缓存避免重复计算

## 安装与配置

### 1. 加载包

```elisp
;; 添加到你的 Emacs 配置中
(add-to-list 'load-path "/path/to/org-ladder")
(require 'org-ladder)
```

### 2. 初始化系统

```elisp
;; 在 org-mode 配置后初始化
(org-ladder-initialize)
```

### 3. 自定义配置（可选）

```elisp
;; 修改默认任务时长（分钟）
(setq org-ladder-default-duration 5)

;; 修改月度重置日（1-31）
(setq org-ladder-monthly-reset-day 1)

;; 修改保留比例（0.0-1.0）
(setq org-ladder-retention-rate 0.1)

;; 指定要扫描的 org 文件列表（可选）
(setq org-ladder-files '("~/org/inbox.org" "~/org/agenda.org"))
;; 如果为 nil，则使用 org-agenda-files（如果已定义）
;; 如果两者都为 nil，则不扫描任何文件
```

## 使用说明

### 基本使用

系统会自动扫描你的 org-agenda 文件，统计当月完成的 DONE 任务。

**积分计算规则：**
- 如果任务有 effort 属性且实际耗时 ≤ effort：使用 effort 时长
- 如果任务有 effort 但实际耗时 > effort：使用实际耗时
- 如果任务没有 effort 或 clock 数据：使用默认时长（5分钟）

### 段位系统

| 段位 | 积分范围 | 小段位 |
|------|----------|--------|
| 青铜 | 0-500 | 10个 |
| 白银 | 501-1200 | 10个 |
| 黄金 | 1201-2100 | 10个 |
| 白金 | 2101-3200 | 10个 |
| 钻石 | 3201-4500 | 10个 |
| 传说 | 4501+ | 无上限 |

### 命令列表

- `M-x org-ladder-display-current-status` - 显示当前段位状态
- `M-x org-ladder-show-detailed-status` - 显示详细状态（包含历史）
- `M-x org-ladder-calculate-current-score` - 强制重新计算积分

### Org Agenda 集成

系统会自动添加一个自定义 agenda 命令 "L"，在 agenda 头部显示当前段位状态：

```
Org-agenda-for-today:
Org Ladder: Gold 5/10 (Next: 120 minutes) | Total: 1650 minutes
```

## 任务格式要求

为了系统正常工作，请确保你的 org 任务包含以下属性：

### 基本要求

```org
* TODO 任务标题
  :PROPERTIES:
  :CLOSED: [2024-11-23 周五 10:00]
  :EFFORT: 1:30
  :END:
  :LOGBOOK:
  CLOCK: [2024-11-23 周五 09:30]--[2024-11-23 周五 10:00] =>  0:30
  :END:
```

### 属性说明

- **CLOSED**: 任务完成时间（必需）
- **EFFORT**: 预估耗时（可选，用于积分计算）
- **CLOCK**: 实际耗时（可选，用于积分计算）

### 示例任务

```org
* DONE 学习 Emacs Lisp
  :PROPERTIES:
  :CLOSED: [2024-11-23 周五 15:00]
  :EFFORT: 2:00
  :END:
  :LOGBOOK:
  CLOCK: [2024-11-23 周五 14:00]--[2024-11-23 周五 15:30] =>  1:30
  :END:
```

在这个例子中，系统会使用 effort 的 120 分钟作为积分（因为实际耗时 90 分钟 ≤ effort）。

## 月度重置机制

- 每月1号自动重置积分
- 保留上月总积分的 10% 作为起步优势
- 历史记录保存在 `~/.emacs.d/org-ladder-history.el`
- 传说段位玩家重置后可能直接从黄金/白金开始

## 故障排除

### 常见问题

1. **积分计算不准确**
   - 检查任务是否有正确的 CLOSED 时间戳
   - 确认任务在当月完成
   - 验证 EFFORT 和 CLOCK 格式正确

2. **段位显示错误**
   - 运行 `M-x org-ladder-calculate-current-score t` 强制重新计算
   - 检查 org-agenda 文件是否包含已完成任务

3. **月度重置未执行**
   - 确认当前日期是重置日（默认每月1号）
   - 检查 `org-ladder-monthly-reset-day` 设置

### 调试命令

```elisp
;; 强制重新计算并显示调试信息
(org-ladder-calculate-current-score t)
(org-ladder-display-current-status)

;; 查看历史记录
org-ladder--monthly-history
```

## 贡献

欢迎提交 Issue 和 Pull Request 来改进这个系统！

## 许可证

MIT License