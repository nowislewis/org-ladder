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
- [永久存量：阶段阈值、dismissed 口径、赛季降级](issues/02-permanent-stock-metrics.md)：五阶森林（7/30/90/180）只统计 Cloze；Topic 归入上游阅读池与守林 Boss 讨伐；月度段位降为装饰、徽章永久陈列、遗迹改按累计存量解锁（`org-ladder-stock.el`、`org-ladder-art.el`、`org-ladder.el`）。
- [装备清单实验：列出最远的 20 张卡，验证"距离感"](issues/03-farthest-cards-experiment.md)：前 20 全是 cloze（3–5 次复习跨 150–209 天），并暴露出上游 topic 阻塞；面板支持 `C-u M-x org-ladder-stock-show` 列最远 Cloze 与盘踞最久 Boss。
- [积累在什么时刻被看见](issues/04-feedback-moments.md)：复习当下显示间隔跃迁、Cloze 升阶与 Boss 抬头/击杀反馈；结束或按 `a` 停下均显示篝火小结且不报剩余到期数（通过 `org-sm` 通用 Hook 在 `org-ladder-sm.el` 实现）。
- [Boss 层定义：成熟卡升级条件与主动挑战入口](issues/90-boss-tier-definition.md)：照面 ≥5 次仍未 dismiss 的 Topic 为守林 Boss，拆出子卡为削血（`xT/yC`），`dismiss` 为击杀；`org-sm.el` 保持纯复习核心，游戏化逻辑全部收敛至 `org-ladder`。

## Implementation verification (2026-10-16)

- `org-sm-card.el` is the shared read-only metadata module; stock classification
  and review gamification remain in `org-ladder`. Public operation/event
  contracts are documented in `../org-sm/README.org`.
- Review-session membership matches the exact current card marker. Card-change
  notifications invalidate score caches; postpone and out-of-session actions
  do not enter bonfire review counts.
- 29 ERT tests include disposable-buffer Boss dismissal, cloze reveal/grade,
  restore/restart/abort, cleanup, parser equivalence and unsaved buffer reads.
  Batch integration stubs only `recenter` and the menu input; actual desktop
  redisplay/window appearance still needs a manual smoke check.
- Shared Org reader retained: approximately 179 ms per warmed scan of 25 files,
  572 cards. Property snapshots were slower; a restricted parser experiment
  gained roughly 16% but lacked format compatibility and was not adopted.
- No user-vault migration performed. Recovery snapshots remain under
  `/tmp/org-sm-recovery-uohcyT`. Future speed work requires measured UX need.

## Not yet specified

- GTD 线的"完成档案"（按项目 / 成果回看，而非分钟数）：是否复用永久存量的概念，等本图定稿后再定。
- 视觉皮肤的承载方式（星图 / 装备图鉴 / 花园）与数据接口。
- spec 交付后如何拆实现票。

## Out of scope

- **`again` 重定性**：cloze 全库仅 9 次 again，暂不需要单独建模：[[91-again-as-informative-death]]。
- **视觉皮肤、RPG 角色行走、Canvas 游戏**：皮肤不改变奖励函数，属于 spec 之后的独立工作。
- **真实的多人对照（排行榜、其他玩家数据）**：本图只做"与过去的自己比"。
