# Org Ladder Vector Relics (`arts/`)

This directory stores sequential vector relics (`relic-01.art`, `relic-02.art`, …) unlocked every **4500** cumulative points in `org-ladder-art.el`.

## 1. Theme Families (AI Generation Guide)

When generating new relics (`relic-13.art`, `relic-14.art`, …), rotate across these four preferred themes:

1. **机动战士高达 (Gundam)** — Iconic mecha head/bust close-ups, V-fins, mono-eyes, Fin Funnels, or faction crests (e.g., Zeta, Wing Zero, Sazabi, Strike Freedom, Gouf).
2. **艾尔登法环 / 魂系 (Elden Ring & Dark Souls)** — Sites of Grace, Coiled Sword, Great Runes, legendary armaments (Moonlight Greatsword, Rivers of Blood), or demigod helms (Malenia, Radahn, Ranni).
3. **怪物猎人：世界 (Monster Hunter: World)** — Tribal guild-card monster emblems with bold silhouettes, horns, wings, and elemental accents (e.g., Nergigante, Rathalos, Zinogre, Fatalis, Alatreon).
4. **炉石传说 (Hearthstone)** — Tavern emblems, Legendary golden dragon frames, mana crystals, and iconic card-back medallions.

## 2. File & Canvas Specification

- **Filename**: `arts/relic-NN.art` (2-digit or 3-digit zero-padded sequential number, sorted by `string<`).
- **Canvas Size**: `264 × 240` px (divided into a `6 × 6` grid of `44 × 40` px fog sectors; keep the main focal silhouette centered within `x: 20..244, y: 15..235`).
- **Supported `:shapes` Primitives** (rendered in order on a `#0D1117` dark canvas):
  - `(poly ((x1 . y1) (x2 . y2) ...) :fill "#HEX" :stroke "#HEX" :stroke-width N)`
  - `(circle cx cy r :fill "#HEX" :stroke "#HEX" :stroke-width N)`
  - `(rect x y w h :fill "#HEX" :stroke "#HEX" :stroke-width N :rx R :ry R)`
  - `(line x1 y1 x2 y2 :stroke "#HEX" :stroke-width N)`
- **Spoiler-Free `:title` & `:lore` Storage**:
  - Encode every character in `:title` and `:lore` as `\uXXXX` hex escapes inside double quotes. Standard Elisp `(read)` decodes `"\uXXXX..."` natively with zero runtime decryption code, while preventing spoilers in `rg` or file previews.

## 3. One-Step Helper for AI to Inspect Existing Relics & Write a New One

```python
import glob, re

# 1. Inspect existing relic titles (so AI avoids duplicates):
for p in sorted(glob.glob("arts/relic-*.art")):
    m = re.search(r':title\s+"([^"]+)"', open(p).read())
    title = m.group(1).encode("ascii").decode("unicode_escape") if m else ""
    print(f"{p}: {title}")

# 2. Encode a string as \uXXXX literal for writing a new relic-NN.art:
def u_esc(s: str) -> str:
    return "".join(f"\\u{ord(c):04x}" for c in s)
```
