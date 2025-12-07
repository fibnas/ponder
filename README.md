# ponder

Rust TUI mind-map (Excalidraw-in-terminal vibes) with movable nodes, notes, colors, undo/redo, autosave, search, and JSON persistence — all in your terminal.

[![Crates.io](https://img.shields.io/crates/v/ponder.svg?cb=1)](https://crates.io/crates/ponder)
[![Docs](https://docs.rs/ponder/badge.svg?cb=1)](https://docs.rs/ponder)
[![License](https://img.shields.io/badge/license-MIT-blue.svg?cb=1)](https://github.com/fibnas/ponder/blob/main/LICENSE)

![Mind map screenshot](https://raw.githubusercontent.com/fibnas/ponder/main/assets/mindmap.png)

> Sketch project trees, CLI ideas, and scratch notes without leaving the keyboard.
> Maps are just JSON, so they version cleanly in git.

---

## Install

Then install the crate:

```bash
cargo install ponder
```

Update existing install:

```bash
cargo install ponder --force
```

Or build locally:

```bash
git clone https://github.com/fibnas/ponder
cd ponder
cargo run --release
```

---

## Features

- **Mind maps as JSON**
  - Create/open/save maps on disk.
  - Autosave with configurable interval and path.
- **Fast node editing**
  - Add/clone/delete nodes, move them freely, and jump to parent/child.
  - Per-node colors to visually group branches.
- **Notes everywhere**
  - Multi-line notes per node with a dedicated overlay editor.
  - Search across titles and notes.
- **Comfortable navigation**
  - Arrow-key navigation, Tab/Shift-Tab cycling, and directional selection.
  - Undo/redo history with bounded snapshots.
- **TUI quality-of-life**
  - Separate panes for canvas, selected node details, shortcuts, and messages.
  - Lightweight dependencies: `ratatui` + `crossterm`.

---

## Usage

### Launch

```bash
ponder         # new map
ponder map.json  # open or create specific file
```

- If the file exists → it is loaded
- If it does not → a new map is created and saved there when you hit `s`

### Layout

- **Canvas:** mind map graph
- **Selected:** title, color, notes
- **Shortcuts:** keybinding cheat sheet
- **Messages:** status/toast area

---

## Keybindings

### Core

- Quit: `q`
- Help: `?`

### Navigation

- Move: arrows
- Next/Prev node: `Tab` / `Shift-Tab`
- Parent/Child: `[` / `]`

### Nodes

- Add child: `a`
- Clone subtree: `c`
- Delete subtree: `d`
- Move mode: `m` (arrows; hold `Shift` = bigger steps)
- Rename: `t`
- Edit notes: `Enter` (multi-line; `Esc` closes)
- Color cycle: `k`

### Files

- Open: `o`
- Save: `s`
- Save As: `S`
- New map: `n`

### Search

- Start search: `/`
- Cycle hits: `Up` / `Down`
- Jump: `Enter`
- Close: `Esc`

### Undo/Redo

- Undo: `u` or `Ctrl+Z`
- Redo: `r` or `Ctrl+Y`

---

## Autosave

- Set autosave path: `A`
- Set autosave interval: `I` (seconds; `0` disables)
- Status bar shows:
  - autosave on/off
  - autosave path
  - last autosave time

Autosave writes normal JSON, which you can open with `o`.

---

## Data Format

Pretty-printed JSON:

- `name`
- `root` id
- `next_id`
- `nodes[]` containing:
  - `id`, `title`, `note`
  - `color`
  - `position {x,y}`
  - `parent`
  - `children[]`

This format is stable, readable, and git-friendly.

---

## Tips

- Use colors to group topics or branches.
- Keep the root broad (“Projects”, “Roadmap”, “Research”).
- Use notes for details to keep the canvas clean.

---

## Roadmap

- Sibling/root jumping
- Tagging + filtering
- Export formats
- Zoom presets

---

## License

MIT — see `LICENSE`.
