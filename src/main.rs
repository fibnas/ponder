use std::{
    collections::{HashMap, HashSet},
    fs,
    io::stdout,
    path::{Path, PathBuf},
    time::Duration,
};

use anyhow::{Context, Result};
use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyEventKind, KeyModifiers},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    backend::CrosstermBackend,
    prelude::*,
    widgets::canvas::{Canvas, Line},
    widgets::{Block, Borders, Clear, LineGauge, Paragraph, Wrap, block::Title},
};
use serde::{Deserialize, Serialize};

type NodeId = u64;

#[derive(Debug, Clone, Serialize, Deserialize)]
struct MindMap {
    name: String,
    nodes: Vec<Node>,
    root: NodeId,
    next_id: NodeId,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Node {
    id: NodeId,
    title: String,
    note: String,
    position: Position,
    parent: Option<NodeId>,
    children: Vec<NodeId>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
struct Position {
    x: f64,
    y: f64,
}

#[derive(Debug, Clone)]
struct App {
    map: MindMap,
    selected: NodeId,
    file_path: Option<PathBuf>,
    mode: Mode,
    message: Option<String>,
    dirty: bool,
    show_help: bool,
}

#[derive(Debug, Clone)]
enum Mode {
    Normal,
    Moving,
    Renaming { buffer: String },
    Prompt { kind: PromptKind, buffer: String },
    NoteEditor { buffer: String },
}

#[derive(Debug, Clone, Copy)]
enum PromptKind {
    Open,
    SaveAs,
}

#[derive(Debug, Clone, Copy)]
enum NavDir {
    Left,
    Right,
    Up,
    Down,
}

fn main() -> Result<()> {
    let initial_path = std::env::args().nth(1).map(PathBuf::from);
    let initial_map = match initial_path.as_ref() {
        Some(path) if path.exists() => load_map(path).unwrap_or_else(|_| MindMap::new("Mind Map")),
        Some(path) => {
            let mut map = MindMap::new(path.to_string_lossy().as_ref());
            map.name = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("Mind Map")
                .to_string();
            map
        }
        None => MindMap::new("Mind Map"),
    };

    let mut app = App::new(initial_map, initial_path);

    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let res = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen)?;
    terminal.show_cursor()?;

    res
}

fn run_app(
    terminal: &mut Terminal<CrosstermBackend<std::io::Stdout>>,
    app: &mut App,
) -> Result<()> {
    let tick_rate = Duration::from_millis(50);
    loop {
        terminal.draw(|f| draw(f, app))?;

        if event::poll(tick_rate)? {
            match event::read()? {
                Event::Key(key) if key.kind == KeyEventKind::Press => {
                    if handle_key(app, key)? {
                        return Ok(());
                    }
                }
                Event::Resize(_, _) => {}
                _ => {}
            }
        }
    }
}

fn handle_key(app: &mut App, key: KeyEvent) -> Result<bool> {
    let mode = app.mode.clone();
    match mode {
        Mode::Normal => handle_normal_mode(app, key),
        Mode::Moving => handle_moving_mode(app, key),
        Mode::Renaming { .. } => handle_renaming_mode(app, key),
        Mode::Prompt { .. } => handle_prompt_mode(app, key),
        Mode::NoteEditor { .. } => handle_note_mode(app, key),
    }
}

fn handle_normal_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    match key.code {
        KeyCode::Char('q') => return Ok(true),
        KeyCode::Char('?') => {
            app.show_help = !app.show_help;
        }
        KeyCode::Char('s') => {
            if let Some(path) = app.file_path.clone() {
                save_current_map(app, path)?;
            } else {
                app.mode = Mode::Prompt {
                    kind: PromptKind::SaveAs,
                    buffer: String::new(),
                };
            }
        }
        KeyCode::Char('S') => {
            app.mode = Mode::Prompt {
                kind: PromptKind::SaveAs,
                buffer: String::new(),
            };
        }
        KeyCode::Char('o') => {
            app.mode = Mode::Prompt {
                kind: PromptKind::Open,
                buffer: String::new(),
            };
        }
        KeyCode::Char('n') => {
            app.reset_to_new();
        }
        KeyCode::Char('a') => {
            if let Some(new_id) = app.map.add_child(
                app.selected,
                "New node".to_string(),
                offset_for_new_child(&app.map, app.selected),
            ) {
                app.selected = new_id;
                app.dirty = true;
            }
        }
        KeyCode::Char('d') => {
            if let Some(next) = app.map.delete_subtree(app.selected) {
                app.selected = next;
                app.dirty = true;
            } else {
                app.message = Some("Cannot delete the root node".to_string());
            }
        }
        KeyCode::Char('c') => {
            if let Some(new_id) = app.map.clone_subtree(app.selected) {
                app.selected = new_id;
                app.dirty = true;
            }
        }
        KeyCode::Char('m') => {
            app.mode = Mode::Moving;
        }
        KeyCode::Char('t') => {
            if let Some(node) = app.map.node(app.selected) {
                app.mode = Mode::Renaming {
                    buffer: node.title.clone(),
                };
            }
        }
        KeyCode::Enter => {
            if let Some(node) = app.map.node(app.selected) {
                app.mode = Mode::NoteEditor {
                    buffer: node.note.clone(),
                };
            }
        }
        KeyCode::Left => app.map_select_direction(NavDir::Left),
        KeyCode::Right => app.map_select_direction(NavDir::Right),
        KeyCode::Up => app.map_select_direction(NavDir::Up),
        KeyCode::Down => app.map_select_direction(NavDir::Down),
        KeyCode::Tab => {
            if let Some(next) = app.map.next_node(app.selected) {
                app.selected = next;
            }
        }
        KeyCode::BackTab => {
            if let Some(prev) = app.map.prev_node(app.selected) {
                app.selected = prev;
            }
        }
        _ => {}
    }
    Ok(false)
}

fn handle_moving_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    let step = if key.modifiers.contains(KeyModifiers::SHIFT) {
        2.5
    } else {
        1.0
    };

    match key.code {
        KeyCode::Esc | KeyCode::Char('m') => {
            app.mode = Mode::Normal;
        }
        KeyCode::Left => {
            app.map.move_node(app.selected, -step, 0.0);
            app.dirty = true;
        }
        KeyCode::Right => {
            app.map.move_node(app.selected, step, 0.0);
            app.dirty = true;
        }
        KeyCode::Up => {
            app.map.move_node(app.selected, 0.0, step);
            app.dirty = true;
        }
        KeyCode::Down => {
            app.map.move_node(app.selected, 0.0, -step);
            app.dirty = true;
        }
        _ => {}
    }

    Ok(false)
}

fn handle_renaming_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    if let Mode::Renaming { buffer } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                app.mode = Mode::Normal;
            }
            KeyCode::Enter => {
                let new_title = buffer.trim().to_string();
                if let Some(node) = app.map.node_mut(app.selected) {
                    node.title = new_title;
                    app.dirty = true;
                }
                app.mode = Mode::Normal;
            }
            KeyCode::Backspace => {
                buffer.pop();
            }
            KeyCode::Char(c) => {
                buffer.push(c);
            }
            _ => {}
        }
    }
    Ok(false)
}

fn handle_prompt_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    if let Mode::Prompt { kind, buffer } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                app.mode = Mode::Normal;
            }
            KeyCode::Enter => {
                let path = PathBuf::from(buffer.trim());
                match kind {
                    PromptKind::Open => match load_map(&path) {
                        Ok(map) => {
                            app.map = map;
                            app.selected = app.map.root;
                            app.file_path = Some(path);
                            app.message = Some("Opened map".to_string());
                            app.dirty = false;
                        }
                        Err(err) => {
                            app.message = Some(format!("Failed to open: {err}"));
                        }
                    },
                    PromptKind::SaveAs => {
                        if path.as_os_str().is_empty() {
                            app.message = Some("Path cannot be empty".to_string());
                            return Ok(false);
                        }
                        save_current_map(app, path)?;
                    }
                }
                app.mode = Mode::Normal;
            }
            KeyCode::Backspace => {
                buffer.pop();
            }
            KeyCode::Char(c) => {
                buffer.push(c);
            }
            _ => {}
        }
    }
    Ok(false)
}

fn handle_note_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    if let Mode::NoteEditor { buffer } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                if let Some(node) = app.map.node_mut(app.selected) {
                    node.note = buffer.clone();
                    app.dirty = true;
                }
                app.mode = Mode::Normal;
            }
            KeyCode::Backspace => {
                buffer.pop();
            }
            KeyCode::Enter => {
                buffer.push('\n');
            }
            KeyCode::Char(c) => {
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    buffer.push(c);
                }
            }
            _ => {}
        }
    }

    Ok(false)
}

fn save_current_map(app: &mut App, path: PathBuf) -> Result<()> {
    save_map(&path, &app.map)?;
    app.file_path = Some(path);
    app.dirty = false;
    app.message = Some("Map saved".to_string());
    Ok(())
}

fn draw(f: &mut Frame, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(10),
            Constraint::Length(6),
        ])
        .split(f.size());

    render_header(f, chunks[0], app);

    let body_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(65), Constraint::Percentage(35)])
        .split(chunks[1]);

    render_map(f, body_chunks[0], app);
    render_details(f, body_chunks[1], app);

    render_footer(f, chunks[2], app);

    match &app.mode {
        Mode::Renaming { buffer } => render_input_overlay(f, "Rename node", buffer),
        Mode::Prompt { kind, buffer } => {
            let label = match kind {
                PromptKind::Open => "Open map path",
                PromptKind::SaveAs => "Save map as",
            };
            render_input_overlay(f, label, buffer);
        }
        Mode::NoteEditor { buffer } => render_note_overlay(f, buffer),
        Mode::Moving | Mode::Normal => {}
    }

    if app.show_help {
        render_help(f);
    }
}

fn render_header(f: &mut Frame, area: Rect, app: &App) {
    let file_label = app
        .file_path
        .as_ref()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|| "unsaved".to_string());
    let mode = match app.mode {
        Mode::Normal => "normal",
        Mode::Moving => "move node",
        Mode::Renaming { .. } => "rename",
        Mode::Prompt { .. } => "prompt",
        Mode::NoteEditor { .. } => "notes",
    };

    let title_line = format!(
        "ponder | {} | {}{}",
        app.map.name,
        file_label,
        if app.dirty { " *" } else { "" }
    );

    let info = format!(
        "selected: {} | mode: {} | nodes: {}",
        app.map
            .node(app.selected)
            .map(|n| n.title.as_str())
            .unwrap_or(""),
        mode,
        app.map.nodes.len()
    );

    let header = Paragraph::new(format!("{title_line}\n{info}"))
        .block(Block::default().borders(Borders::BOTTOM).title("Status"))
        .wrap(Wrap { trim: true });
    f.render_widget(header, area);
}

fn render_map(f: &mut Frame, area: Rect, app: &App) {
    let (min_x, max_x, min_y, max_y) = app.map.bounds();
    let canvas = Canvas::default()
        .block(
            Block::default()
                .title(Title::from("Mind map").alignment(Alignment::Center))
                .borders(Borders::ALL),
        )
        .x_bounds([min_x - 2.0, max_x + 2.0])
        .y_bounds([min_y - 2.0, max_y + 2.0])
        .paint(|ctx| {
            for node in &app.map.nodes {
                for child_id in &node.children {
                    if let Some(child) = app.map.node(*child_id) {
                        ctx.draw(&Line {
                            x1: node.position.x,
                            y1: node.position.y,
                            x2: child.position.x,
                            y2: child.position.y,
                            color: Color::DarkGray,
                        });
                    }
                }
            }

            for node in &app.map.nodes {
                let is_selected = node.id == app.selected;
                let color = if is_selected {
                    Color::Cyan
                } else {
                    Color::White
                };
                ctx.print(
                    node.position.x,
                    node.position.y,
                    Span::styled(node.title.clone(), Style::default().fg(color)),
                );
            }
        });

    f.render_widget(canvas, area);
}

fn render_details(f: &mut Frame, area: Rect, app: &App) {
    let node = app.map.node(app.selected);
    let (title, note) = node
        .map(|n| (n.title.clone(), n.note.clone()))
        .unwrap_or_default();
    let content = format!(
        "{}\n\n{}",
        title,
        if note.is_empty() {
            "No notes yet."
        } else {
            note.as_str()
        }
    );
    let block = Block::default()
        .title(Title::from("Selected").alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(content)
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(paragraph, area);
}

fn render_footer(f: &mut Frame, area: Rect, app: &App) {
    let msg = app
        .message
        .clone()
        .unwrap_or_else(|| {
            "q quit - o open - s save - S save as - n new - a add child - c clone - d delete - m move - t rename - Enter notes - ? help"
                .to_string()
        });
    let gauge = LineGauge::default()
        .block(Block::default().borders(Borders::TOP).title("Shortcuts"))
        .gauge_style(Style::default().fg(Color::LightMagenta))
        .line_set(ratatui::symbols::line::THICK)
        .label(msg)
        .ratio(if app.dirty { 0.7 } else { 0.3 });
    f.render_widget(gauge, area);
}

fn render_input_overlay(f: &mut Frame, label: &str, buffer: &str) {
    let area = centered_rect(60, 20, f.size());
    let block = Block::default()
        .title(Title::from(label).alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(buffer.to_string() + "_").block(block);
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
}

fn render_note_overlay(f: &mut Frame, buffer: &str) {
    let area = centered_rect(90, 80, f.size());
    let block = Block::default()
        .title(Title::from("Notes (Esc to close)").alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(buffer.to_string())
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
}

fn render_help(f: &mut Frame) {
    let area = centered_rect(70, 60, f.size());
    let lines = [
        "Movement: arrow keys",
        "Select next/prev: Tab / Shift-Tab",
        "Add child: a | Clone: c | Delete: d",
        "Move node: m then arrows (hold Shift for bigger steps)",
        "Rename: t | Notes: Enter (multi-line, Esc to close)",
        "Open: o | Save: s | Save As: S | New map: n",
        "Toggle help: ? | Quit: q",
    ];
    let block = Block::default()
        .title(Title::from("Help").alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(lines.join("\n"))
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
}

fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Percentage((100 - percent_y) / 2),
                Constraint::Percentage(percent_y),
                Constraint::Percentage((100 - percent_y) / 2),
            ]
            .as_ref(),
        )
        .split(r);

    Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [
                Constraint::Percentage((100 - percent_x) / 2),
                Constraint::Percentage(percent_x),
                Constraint::Percentage((100 - percent_x) / 2),
            ]
            .as_ref(),
        )
        .split(popup_layout[1])[1]
}

impl App {
    fn new(map: MindMap, file_path: Option<PathBuf>) -> Self {
        let selected = map.root;
        Self {
            map,
            selected,
            file_path,
            mode: Mode::Normal,
            message: None,
            dirty: false,
            show_help: false,
        }
    }

    fn reset_to_new(&mut self) {
        self.map = MindMap::new("Mind Map");
        self.selected = self.map.root;
        self.file_path = None;
        self.dirty = false;
        self.message = Some("Started a new map".to_string());
    }

    fn map_select_direction(&mut self, dir: NavDir) {
        if let Some(next) = self.map.nearest_in_direction(self.selected, dir) {
            self.selected = next;
        }
    }
}

impl MindMap {
    fn new(name: &str) -> Self {
        let root_id = 1;
        Self {
            name: name.to_string(),
            nodes: vec![Node {
                id: root_id,
                title: "Central Idea".to_string(),
                note: String::new(),
                position: Position { x: 0.0, y: 0.0 },
                parent: None,
                children: Vec::new(),
            }],
            root: root_id,
            next_id: root_id + 1,
        }
    }

    fn node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.iter().find(|n| n.id == id)
    }

    fn node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.iter_mut().find(|n| n.id == id)
    }

    fn node_index(&self, id: NodeId) -> Option<usize> {
        self.nodes.iter().position(|n| n.id == id)
    }

    fn next_node(&self, current: NodeId) -> Option<NodeId> {
        let idx = self.node_index(current)?;
        let next_idx = (idx + 1) % self.nodes.len();
        self.nodes.get(next_idx).map(|n| n.id)
    }

    fn prev_node(&self, current: NodeId) -> Option<NodeId> {
        let idx = self.node_index(current)?;
        let prev_idx = if idx == 0 {
            self.nodes.len().saturating_sub(1)
        } else {
            idx - 1
        };
        self.nodes.get(prev_idx).map(|n| n.id)
    }

    fn add_child(&mut self, parent: NodeId, title: String, offset: Position) -> Option<NodeId> {
        let parent_pos = self.node(parent)?.position;
        let id = self.next_id;
        self.next_id += 1;
        let node = Node {
            id,
            title,
            note: String::new(),
            position: Position {
                x: parent_pos.x + offset.x,
                y: parent_pos.y + offset.y,
            },
            parent: Some(parent),
            children: Vec::new(),
        };
        self.nodes.push(node);
        if let Some(p) = self.node_mut(parent) {
            p.children.push(id);
        }
        Some(id)
    }

    fn delete_subtree(&mut self, id: NodeId) -> Option<NodeId> {
        if id == self.root {
            return None;
        }
        let parent_id = self.node(id)?.parent?;
        let mut to_remove = Vec::new();
        self.collect_subtree(id, &mut to_remove);
        let remove_set: HashSet<NodeId> = to_remove.iter().copied().collect();
        self.nodes.retain(|n| !remove_set.contains(&n.id));
        if let Some(parent) = self.node_mut(parent_id) {
            parent.children.retain(|c| *c != id);
        }
        Some(parent_id)
    }

    fn collect_subtree(&self, id: NodeId, out: &mut Vec<NodeId>) {
        out.push(id);
        if let Some(node) = self.node(id) {
            for child in &node.children {
                self.collect_subtree(*child, out);
            }
        }
    }

    fn clone_subtree(&mut self, id: NodeId) -> Option<NodeId> {
        let parent = self.node(id).and_then(|n| n.parent);
        let target_parent = parent.unwrap_or(self.root);
        let mut mapping = HashMap::new();
        self.duplicate_recursive(
            id,
            Some(target_parent),
            &mut mapping,
            Position { x: 2.0, y: 0.5 },
        )?;
        mapping.get(&id).copied()
    }

    fn duplicate_recursive(
        &mut self,
        id: NodeId,
        parent: Option<NodeId>,
        mapping: &mut HashMap<NodeId, NodeId>,
        offset: Position,
    ) -> Option<()> {
        let original = self.node(id)?.clone();
        let new_id = self.next_id;
        self.next_id += 1;

        let mut new_node = original.clone();
        new_node.id = new_id;
        new_node.parent = parent;
        new_node.children.clear();
        new_node.position.x += offset.x;
        new_node.position.y += offset.y;

        self.nodes.push(new_node);
        if let Some(p) = parent {
            if let Some(parent_node) = self.node_mut(p) {
                parent_node.children.push(new_id);
            }
        }
        mapping.insert(id, new_id);

        for child in original.children {
            self.duplicate_recursive(child, Some(new_id), mapping, offset)?;
        }
        Some(())
    }

    fn move_node(&mut self, id: NodeId, dx: f64, dy: f64) {
        if let Some(node) = self.node_mut(id) {
            node.position.x += dx;
            node.position.y += dy;
        }
    }

    fn bounds(&self) -> (f64, f64, f64, f64) {
        let mut min_x: f64 = 0.0;
        let mut max_x: f64 = 0.0;
        let mut min_y: f64 = 0.0;
        let mut max_y: f64 = 0.0;
        for node in &self.nodes {
            min_x = min_x.min(node.position.x);
            max_x = max_x.max(node.position.x);
            min_y = min_y.min(node.position.y);
            max_y = max_y.max(node.position.y);
        }
        if min_x == max_x {
            min_x -= 5.0;
            max_x += 5.0;
        }
        if min_y == max_y {
            min_y -= 5.0;
            max_y += 5.0;
        }
        (min_x, max_x, min_y, max_y)
    }

    fn nearest_in_direction(&self, from: NodeId, dir: NavDir) -> Option<NodeId> {
        let current = self.node(from)?;
        let (vx, vy) = match dir {
            NavDir::Left => (-1.0, 0.0),
            NavDir::Right => (1.0, 0.0),
            NavDir::Up => (0.0, 1.0),
            NavDir::Down => (0.0, -1.0),
        };
        let mut best: Option<(NodeId, f64)> = None;

        for node in &self.nodes {
            if node.id == from {
                continue;
            }
            let dx = node.position.x - current.position.x;
            let dy = node.position.y - current.position.y;
            let dot = dx * vx + dy * vy;
            if dot <= 0.0 {
                continue;
            }
            let dist2 = dx * dx + dy * dy;
            let perp_penalty = (dx * vy - dy * vx).abs();
            let score = dist2 + perp_penalty * 2.0;
            if best.map(|(_, s)| score < s).unwrap_or(true) {
                best = Some((node.id, score));
            }
        }
        best.map(|(id, _)| id)
    }
}

fn offset_for_new_child(map: &MindMap, parent: NodeId) -> Position {
    let count = map.node(parent).map(|n| n.children.len()).unwrap_or(0);
    Position {
        x: 3.0,
        y: (count as f64) * 1.5 - 1.5,
    }
}

fn load_map(path: &Path) -> Result<MindMap> {
    let data = fs::read_to_string(path)
        .with_context(|| format!("Unable to read {}", path.to_string_lossy()))?;
    let mut map: MindMap =
        serde_json::from_str(&data).with_context(|| "File is not a valid mindmap")?;
    if map.nodes.is_empty() {
        map = MindMap::new("Recovered");
    }
    Ok(map)
}

fn save_map(path: &Path, map: &MindMap) -> Result<()> {
    let serialized = serde_json::to_string_pretty(map)?;
    fs::write(path, serialized)
        .with_context(|| format!("Unable to write {}", path.to_string_lossy()))
}
