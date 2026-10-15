# Map: 从任务流量到知识存量（accumulation-redesign）

## Destination

一份锁定的 spec，内容只有四件：**topic 卡的处理动作**（复习时一次做一步、知道该做什么）、**永久存量**（每张卡已跨过的天数，只增不减、永不清零）、**季节层降级**、**积累在何时被看见**（复习当下 / 停下时 / 面板）。写完这个 spec 即结束，实现属于后续会话。

## Notes

- 领域：Emacs / org-mode / org-ladder（本仓库）/ org-sm（`~/.emacs.d/lib/org-sm`，独立仓库）。
- 每个会话先读 wayfinder 与 grilling。
- 用户已明确的设计立场（2026-08-07 org-sm 设计访谈，见 llm-wiki）：复习依从性是绑定约束；无聊出现在复习过程中、做完几张卡后最强；第一版必须保留原有快速复习路径，任何变化可逆、可衡量。
- 动机原型：魂系（核心循环：挑战、失败即信息、突破；篝火 = 合法停下；捷径永久打开）、怪物猎人（战利品陈列）、炉石（赛季装饰、徽章永久）。
- **核心诊断（2026-10-16 修正）**：不想复习的主因不是缺积累感，而是 topic 卡复习时没有可完成的动作。237 张活跃 topic 中 235 张没有子卡，44 张复习 ≥3 次仍未处理，219 张已到期。用户的两难：拆多了浪费，拆少了怕丢。积累感是下游，先要有"处理完一张卡"。
- 数据事实（2026-10 扫描 `~/Documents/emacs/01-orgmode/xnotes/incremental`，94 个文件）：
  - 557 张卡，400 张有复习记录，1620 次复习。
  - topic 292 / cloze 108；dismissed 177。
  - 阶段分布：未排期 27、<7d 209、7–27d 16、28–62d 56、63–179d 66、180+d 5；最长 209 天。
  - topic 的 A-Factor 恒为 1.5（1007/1021），间隔增长是固定倍数。
  - cloze 评分：good 384 / hard 25 / again 9 / easy 1。
  - cloze 间隔 = `SCHEDULED - SRS_LAST`；topic 的 LOGBOOK 记 `a=1.5 63d`。
  - org-sm 没有复习后 hook（`org-ladder-sm-setup-review-hook` 引用的 `org-sm-after-review-functions` 不存在，一直没生效）。
- issue tracker：本地 markdown（`.scratch/<effort>/`）。
- **本图刻意保持小**：只留 3 张票。实现在实现会话自行决定，不占用决策票。

## Decisions so far

<!-- 已结票的索引 -->

- [topic 卡复习时的处理动作](issues/01-topic-processing-action.md)：快速路径不变；复习菜单加一个主动触发键"挑一段"（org-sm 侧已实现：`org-sm-gptel-pick`），AI 只推荐一段并附理由，提交后不自动跳下一张。
- [永久存量：阶段阈值、dismissed 口径、赛季降级](issues/02-permanent-stock-metrics.md)：阶段 7/30/90/180；dismissed 计为已筛选（正向处理量）；月度段位降为装饰、徽章永久陈列、遗迹改按累计存量解锁（已实现：`org-ladder-stock.el`、`org-ladder-art.el`、`org-ladder.el`）。
- [装备清单实验：列出最远的 20 张卡，验证"距离感"](issues/03-farthest-cards-experiment.md)：前 20 全是 cloze（3–5 次复习跨 150–209 天），并暴露出上游 topic 阻塞；面板支持 `C-u M-x org-ladder-stock-show` 列最远卡片。
- [积累在什么时刻被看见](issues/04-feedback-moments.md)：复习当下显示间隔跃迁与跨阶段提示；结束或按 `a` 停下均显示篝火小结且不报剩余到期数（已实现于 `org-sm.el`）。

- [topic 卡复习时的处理动作](issues/01-topic-processing-action.md)：快速路径不变；复习菜单加一个主动触发键"挑一段"，复用 card-workbench + 新 AI 模式（只推荐一段并附理由），提交后自动推后并跳下一张。

## Not yet specified

- GTD 线的"完成档案"（按项目 / 成果回看，而非分钟数）：是否复用永久存量的概念，等本图定稿后再定。
- 视觉皮肤的承载方式（星图 / 装备图鉴 / 花园）与数据接口。
- spec 交付后如何拆实现票。

## Out of scope

- **Boss 层与 `again` 重定性**：属于"复习无聊"的独立问题，不是"积累感"。票已建但关闭：[[90-boss-tier-definition]]、[[91-again-as-informative-death]]。需要推动时另开一个 effort。
- **视觉皮肤、RPG 角色行走、Canvas 游戏**：皮肤不改变奖励函数，属于 spec 之后的独立工作。
- **真实的多人对照（排行榜、其他玩家数据）**：本图只做"与过去的自己比"。
