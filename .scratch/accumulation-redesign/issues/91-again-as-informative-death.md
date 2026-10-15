# again 重定性：倒下即信息，并记入永久交手记录

Type: grilling
Status: out-of-scope

## Question

`again`（以及 topic 的 postpone？）在新模型中的含义与记录方式：

1. `again` 是否从"失败"改记为"倒下"，并计入永久层的"已交手"次数？
2. "击败过的 Boss"如何判定：曾经 again ≥1 次，之后连续 N 次 good 且间隔跨过某阈值？N 和阈值是多少？
3. 这些记录是否只需从 LOGBOOK 回放推导（无额外存储），还是需要单独字段？历史数据（9 次 again）能否完整回放？
4. topic 卡有没有对应的"倒下"事件（例如 explain / postpone），还是倒下只属于 cloze 与 Boss 战？
5. 复习时 `again` 后的即时提示措辞方向：信息性（"你看清了它的出招"）而非惩罚性；是否需要展示该卡的历史倒下次数？

## 参考数据

- cloze 评分：good 384 / hard 25 / again 9 / easy 1。≥2 次 again 的卡只有 1 张（`[C] discernibly`）。
- 依赖票 01 的阶段阈值。

## Answer

<!-- 结票时填写 -->
