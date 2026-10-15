# 装备清单实验：列出最远的 20 张卡，验证"距离感"

Type: task
Status: resolved

## Question

用户已同意（Q7'）：列出当前间隔最长的 20 张卡（标题、所属书、类型、间隔、复习次数、是否倒下过），让用户看后判断"这是不是我打下来的东西"。

- 有感觉 → 第一屏做装备清单（按距离排序）。
- 没感觉 → 换为按书汇总 / 转化率，并在票 03 调整方向。

AFK 部分：生成清单（`.scratch/accumulation-redesign/assets/farthest-20.md`）。
HITL 部分：用户阅读后给一句反馈。

## Answer

清单已生成于 `assets/farthest-20.md`（最长 209d，前 20 全为 cloze，每张仅复习 3–5 次）。用户在检视时定位到真正的上游阻塞：237 张活跃 topic 中 235 张无子卡，复习时"不知道怎么处理"，从而引出票 01（`x` 键 `org-sm-gptel-pick`）。面板第一屏采用按阶段分布 + 最远卡 + `C-u M-x org-ladder-stock-show` 列出最远 N 张。
