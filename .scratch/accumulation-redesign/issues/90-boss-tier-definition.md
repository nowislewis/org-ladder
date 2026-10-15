# Boss 层定义：成熟卡升级条件与主动挑战入口

Type: grilling
Status: out-of-scope

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

<!-- 结票时填写 -->
