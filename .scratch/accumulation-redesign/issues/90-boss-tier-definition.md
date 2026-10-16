# Boss 层定义：成熟卡升级条件与主动挑战入口

Type: grilling
Status: resolved

## Question

Boss 层的设计要解答：

1. **升级条件**：成熟卡（预设阈值，待票 01 定）升为 Boss 的条件是什么？纯粹是"间隔超过阈值"，还是"间隔 + 复习次数 + 没有近期 again"的组合？
2. **主动发起入口**：用户如何触发一次 Boss 战？选项有：
   - a. 在 `org-sm-review-confirm` 里新增一个 Boss 模式键位
   - b. 独立的 `M-x org-sm-boss-review` 命令
   - c. 沿用现有的 `org-sm-explore` skill（已实现，通过 pi 触发 AI 对话）
   - 上述可以组合：轻量 Emacs 内版本 + 现有 explore skill
3. **挑战类型**：Boss 战用哪种提问方式？候选（来自 2026-08-07 访谈）：边界题、对比题、因果题、迁移/应用题。是否需要按卡片类型（topic vs cloze）选择不同挑战模式？
4. **和 `org-sm-explore` 的关系**：`org-sm-explore` skill 已经实现了"source-grounded choice-based Explore"，Boss 战是复用它、扩展它，还是另起一条路？
5. **Boss 战结果写回**：挑战通过之后，写什么进永久层？"已交手次数"、"首次通过日期"，还是别的？

## 参考数据

- 当前 again 事件只有 9 次（92% 是 good），即 Boss 极少。
- `org-sm-explore` 已在 `~/.agents/skills/org-sm-explore/` 实现，支持 locate_org_sm_card.py 按标题或内容定位。
- 2026-08-07 约束：挑战必须用户主动触发；第一版必须保留原有快速复习路径不变。

## Answer

2026-10-16 用户重新定义并确认（**「森林与守林 Boss」统一模型**）：

1. **Boss 的本质**：不再按 `again` 定义（cloze 全库仅 9 次 again），而是**复习（照面）了很多次仍未完全拆解转化为 cloze 并退役的 topic 卡**。
2. **成为 Boss 的条件**：未 `dismiss` 的 topic 卡，自身 `LOGBOOK` 中复习次数 `>= 5`（`org-ladder-stock-boss-threshold`，当前共 48 张）。
   - 若子树下已有派生的子 topic / 子 cloze，只要原 topic 本身尚未 `dismiss`，仍算**攻略中的 Boss（已削血：`xT/yC`）**。
3. **击败 Boss（Victory）**：将已达 Boss 阈值的 topic 卡执行 `dismiss`（当前历史已击败 67 张）；复习时触发击杀反馈 `🏆 击败 Boss！(共照面 N 次 · 产出 M 张子卡)`。照面 `< 5` 次即 `dismiss` 的卡仍计为常规"已筛选"。
4. **遇到 Boss 时的动作**：在日常复习流中刷到该 topic 时，提示栏与 `Topic:` 菜单直接显示 `⚔️ Boss (已照面 11 次 · 0 cloze)` 或 `⚔️ Boss [已削血: 2T/1C] (已照面 8 次)`。
5. **与五阶森林存量的关系**：五阶成长柱状图（🌱 幼苗 ~ ⛰️ 远古）**100% 只统计 Cloze**（真正种下并随时间生长的记忆）；Topic 全部归入上游「阅读池与守林 Boss 讨伐」通道。
6. **架构边界**：`org-sm.el` 只保留纯复习核心并提供只读 `org-sm-card-info` 与 4 个通用 Hook；阶段升阶、篝火小结、Boss 抬头与击杀反馈全部在 `org-ladder`（`org-ladder-stock.el` + `org-ladder-sm.el`）实现。
