use std::{
    collections::{HashMap, HashSet},
    fs,
    io::stdout,
    path::{Path, PathBuf},
    time::{Duration, Instant},
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
    widgets::{Block, Borders, Clear, Paragraph, Wrap, block::Title},
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
    #[serde(default)]
    color: NodeColor,
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
    autosave_path: PathBuf,
    mode: Mode,
    message: Option<Toast>,
    dirty: bool,
    show_help: bool,
    undo_stack: Vec<AppState>,
    redo_stack: Vec<AppState>,
    last_autosave: Option<Instant>,
    autosave_interval: Duration,
    moving_snapshot: bool,
}

#[derive(Debug, Clone)]
struct AppState {
    map: MindMap,
    selected: NodeId,
}

#[derive(Debug, Clone)]
enum Mode {
    Normal,
    Moving,
    Renaming {
        buffer: String,
    },
    Prompt {
        kind: PromptKind,
        buffer: String,
    },
    NoteEditor {
        buffer: String,
        cursor: usize,
    },
    Search {
        query: String,
        results: Vec<NodeId>,
        index: usize,
    },
}

#[derive(Debug, Clone, Copy)]
enum PromptKind {
    Open,
    SaveAs,
    AutosavePath,
    AutosaveInterval,
}

#[derive(Debug, Clone, Copy)]
enum NavDir {
    Left,
    Right,
    Up,
    Down,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
enum NodeColor {
    Default,
    Red,
    Green,
    Blue,
    Yellow,
    Magenta,
    Cyan,
}

impl NodeColor {
    fn to_color(self) -> Color {
        match self {
            NodeColor::Default => Color::White,
            NodeColor::Red => Color::Red,
            NodeColor::Green => Color::Green,
            NodeColor::Blue => Color::Blue,
            NodeColor::Yellow => Color::Yellow,
            NodeColor::Magenta => Color::Magenta,
            NodeColor::Cyan => Color::Cyan,
        }
    }

    fn cycle_next(self) -> Self {
        match self {
            NodeColor::Default => NodeColor::Red,
            NodeColor::Red => NodeColor::Green,
            NodeColor::Green => NodeColor::Blue,
            NodeColor::Blue => NodeColor::Yellow,
            NodeColor::Yellow => NodeColor::Magenta,
            NodeColor::Magenta => NodeColor::Cyan,
            NodeColor::Cyan => NodeColor::Default,
        }
    }

    fn label(self) -> &'static str {
        match self {
            NodeColor::Default => "default",
            NodeColor::Red => "red",
            NodeColor::Green => "green",
            NodeColor::Blue => "blue",
            NodeColor::Yellow => "yellow",
            NodeColor::Magenta => "magenta",
            NodeColor::Cyan => "cyan",
        }
    }
}

impl Default for NodeColor {
    fn default() -> Self {
        NodeColor::Default
    }
}

#[derive(Debug, Clone)]
struct Toast {
    text: String,
    expires_at: Option<Instant>,
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
        app.prune_message();
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
        app.maybe_autosave();
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
        Mode::Search { .. } => handle_search_mode(app, key),
    }
}

fn handle_normal_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    match key.code {
        KeyCode::Char('q') => return Ok(true),
        KeyCode::Char('?') => {
            app.show_help = !app.show_help;
        }
        KeyCode::Char('/') => {
            app.mode = Mode::Search {
                query: String::new(),
                results: Vec::new(),
                index: 0,
            };
            app.update_search_results();
        }
        KeyCode::Char('A') => {
            app.mode = Mode::Prompt {
                kind: PromptKind::AutosavePath,
                buffer: app.autosave_path.to_string_lossy().to_string(),
            };
        }
        KeyCode::Char('I') => {
            app.mode = Mode::Prompt {
                kind: PromptKind::AutosaveInterval,
                buffer: app.autosave_interval.as_secs().to_string(),
            };
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
            app.push_undo();
            if let Some(new_id) = app.map.add_child(
                app.selected,
                "New node".to_string(),
                offset_for_new_child(&app.map, app.selected),
            ) {
                app.selected = new_id;
                app.dirty = true;
            }
        }
        KeyCode::Char('[') => {
            app.jump_to_parent();
        }
        KeyCode::Char(']') => {
            app.jump_to_first_child();
        }
        KeyCode::Char('d') => {
            app.push_undo();
            if let Some(next) = app.map.delete_subtree(app.selected) {
                app.selected = next;
                app.dirty = true;
            } else {
                app.set_message("Cannot delete the root node");
            }
        }
        KeyCode::Char('c') => {
            app.push_undo();
            if let Some(new_id) = app.map.clone_subtree(app.selected) {
                app.selected = new_id;
                app.dirty = true;
            }
        }
        KeyCode::Char('m') => {
            app.mode = Mode::Moving;
            app.moving_snapshot = false;
        }
        KeyCode::Char('t') => {
            if let Some(node) = app.map.node(app.selected) {
                app.mode = Mode::Renaming {
                    buffer: node.title.clone(),
                };
            }
        }
        KeyCode::Char('k') => {
            app.push_undo();
            if let Some(node) = app.map.node_mut(app.selected) {
                let new_color = node.color.cycle_next();
                node.color = new_color;
                app.dirty = true;
                let label = new_color.label().to_string();
                app.set_message(format!("Color set to {label}"));
            }
        }
        KeyCode::Enter => {
            if let Some(node) = app.map.node(app.selected) {
                app.mode = Mode::NoteEditor {
                    buffer: node.note.clone(),
                    cursor: node.note.len(),
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
        KeyCode::Char('u') | KeyCode::Char('z')
            if key.modifiers.contains(KeyModifiers::CONTROL) =>
        {
            app.undo();
        }
        KeyCode::Char('u') => {
            app.undo();
        }
        KeyCode::Char('r') | KeyCode::Char('y')
            if key.modifiers.contains(KeyModifiers::CONTROL) =>
        {
            app.redo();
        }
        KeyCode::Char('r') => {
            app.redo();
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
            app.moving_snapshot = false;
        }
        KeyCode::Left => {
            if !app.moving_snapshot {
                app.push_undo();
                app.moving_snapshot = true;
            }
            app.map.move_node(app.selected, -step, 0.0);
            app.dirty = true;
        }
        KeyCode::Right => {
            if !app.moving_snapshot {
                app.push_undo();
                app.moving_snapshot = true;
            }
            app.map.move_node(app.selected, step, 0.0);
            app.dirty = true;
        }
        KeyCode::Up => {
            if !app.moving_snapshot {
                app.push_undo();
                app.moving_snapshot = true;
            }
            app.map.move_node(app.selected, 0.0, step);
            app.dirty = true;
        }
        KeyCode::Down => {
            if !app.moving_snapshot {
                app.push_undo();
                app.moving_snapshot = true;
            }
            app.map.move_node(app.selected, 0.0, -step);
            app.dirty = true;
        }
        _ => {}
    }

    Ok(false)
}

fn handle_renaming_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    let mut commit: Option<String> = None;
    let mut exit_mode = false;
    if let Mode::Renaming { buffer } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                exit_mode = true;
            }
            KeyCode::Enter => {
                commit = Some(buffer.trim().to_string());
                exit_mode = true;
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
    if let Some(new_title) = commit {
        app.push_undo();
        if let Some(node) = app.map.node_mut(app.selected) {
            node.title = new_title;
            app.dirty = true;
        }
    }
    if exit_mode {
        app.mode = Mode::Normal;
    }
    Ok(false)
}

fn insert_at_cursor(buffer: &mut String, cursor: &mut usize, ch: char) {
    clamp_cursor(buffer, cursor);
    buffer.insert(*cursor, ch);
    *cursor += ch.len_utf8();
}

fn backspace(buffer: &mut String, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    if let Some(prev) = prev_char_boundary(buffer, *cursor) {
        buffer.drain(prev..*cursor);
        *cursor = prev;
    }
}

fn delete_at_cursor(buffer: &mut String, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    if let Some(next) = next_char_boundary(buffer, *cursor) {
        if next > *cursor {
            buffer.drain(*cursor..next);
        }
    }
}

fn move_cursor_left(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    if let Some(prev) = prev_char_boundary(buffer, *cursor) {
        *cursor = prev;
    }
}

fn move_cursor_right(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    if let Some(next) = next_char_boundary(buffer, *cursor) {
        *cursor = next;
    }
}

fn move_cursor_line_start(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    *cursor = line_start(buffer, *cursor);
}

fn move_cursor_line_end(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    *cursor = line_end(buffer, *cursor);
}

fn move_cursor_up(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    let current_start = line_start(buffer, *cursor);
    if current_start == 0 {
        *cursor = 0;
        return;
    }
    let current_col = *cursor - current_start;
    let prev_end = current_start - 1;
    let prev_start = buffer[..prev_end]
        .rfind('\n')
        .map(|idx| idx + 1)
        .unwrap_or(0);
    let prev_len = prev_end.saturating_sub(prev_start);
    *cursor = prev_start + current_col.min(prev_len);
}

fn move_cursor_down(buffer: &str, cursor: &mut usize) {
    clamp_cursor(buffer, cursor);
    let current_start = line_start(buffer, *cursor);
    let current_col = *cursor - current_start;
    let current_end = line_end(buffer, *cursor);
    if current_end == buffer.len() {
        *cursor = buffer.len();
        return;
    }
    let next_start = current_end + 1;
    let next_end = line_end(buffer, next_start);
    let next_len = next_end.saturating_sub(next_start);
    *cursor = next_start + current_col.min(next_len);
}

fn line_start(buffer: &str, cursor: usize) -> usize {
    let cursor = cursor.min(buffer.len());
    buffer[..cursor].rfind('\n').map(|idx| idx + 1).unwrap_or(0)
}

fn line_end(buffer: &str, cursor: usize) -> usize {
    let cursor = cursor.min(buffer.len());
    buffer[cursor..]
        .find('\n')
        .map(|idx| cursor + idx)
        .unwrap_or(buffer.len())
}

fn prev_char_boundary(text: &str, cursor: usize) -> Option<usize> {
    let cursor = cursor.min(text.len());
    text[..cursor]
        .char_indices()
        .next_back()
        .map(|(idx, _)| idx)
}

fn next_char_boundary(text: &str, cursor: usize) -> Option<usize> {
    let cursor = cursor.min(text.len());
    if cursor == text.len() {
        return None;
    }
    text[cursor..]
        .char_indices()
        .nth(1)
        .map(|(idx, _)| cursor + idx)
        .or(Some(text.len()))
}

fn clamp_cursor(buffer: &str, cursor: &mut usize) {
    if *cursor > buffer.len() {
        *cursor = buffer.len();
    }
}

fn handle_prompt_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    if let Mode::Prompt { kind, buffer } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                app.mode = Mode::Normal;
            }
            KeyCode::Enter => {
                match kind {
                    PromptKind::Open => {
                        let path = PathBuf::from(buffer.trim());
                        match load_map(&path) {
                            Ok(map) => {
                                app.map = map;
                                app.selected = app.map.root;
                                app.file_path = Some(path.clone());
                                app.autosave_path = path;
                                app.set_message("Opened map");
                                app.dirty = false;
                                app.undo_stack.clear();
                                app.redo_stack.clear();
                                app.last_autosave = None;
                            }
                            Err(err) => {
                                app.set_message(format!("Failed to open: {err}"));
                            }
                        }
                    }
                    PromptKind::SaveAs => {
                        let path = PathBuf::from(buffer.trim());
                        if path.as_os_str().is_empty() {
                            app.set_message("Path cannot be empty");
                            return Ok(false);
                        }
                        save_current_map(app, path.clone())?;
                        app.autosave_path = path;
                    }
                    PromptKind::AutosavePath => {
                        let path = PathBuf::from(buffer.trim());
                        if path.as_os_str().is_empty() {
                            app.set_message("Autosave path cannot be empty");
                            return Ok(false);
                        }
                        app.set_autosave_path(path);
                    }
                    PromptKind::AutosaveInterval => {
                        let text = buffer.trim();
                        let secs: u64 = text.parse().unwrap_or(0);
                        app.set_autosave_interval(secs);
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
    let mut commit_note: Option<String> = None;
    let mut exit_mode = false;
    if let Mode::NoteEditor { buffer, cursor } = &mut app.mode {
        match key.code {
            KeyCode::Esc => {
                commit_note = Some(buffer.clone());
                exit_mode = true;
            }
            KeyCode::Left => move_cursor_left(buffer, cursor),
            KeyCode::Right => move_cursor_right(buffer, cursor),
            KeyCode::Up => move_cursor_up(buffer, cursor),
            KeyCode::Down => move_cursor_down(buffer, cursor),
            KeyCode::Home => move_cursor_line_start(buffer, cursor),
            KeyCode::End => move_cursor_line_end(buffer, cursor),
            KeyCode::Backspace => {
                backspace(buffer, cursor);
            }
            KeyCode::Delete => {
                delete_at_cursor(buffer, cursor);
            }
            KeyCode::Enter => {
                insert_at_cursor(buffer, cursor, '\n');
            }
            KeyCode::Char(c) => {
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    insert_at_cursor(buffer, cursor, c);
                }
            }
            _ => {}
        }
    }
    if let Some(note) = commit_note {
        app.push_undo();
        if let Some(node) = app.map.node_mut(app.selected) {
            node.note = note;
            app.dirty = true;
        }
    }
    if exit_mode {
        app.mode = Mode::Normal;
    }
    Ok(false)
}

fn handle_search_mode(app: &mut App, key: KeyEvent) -> Result<bool> {
    if let Mode::Search {
        query,
        results,
        index,
    } = &mut app.mode
    {
        match key.code {
            KeyCode::Esc => {
                app.mode = Mode::Normal;
            }
            KeyCode::Enter => {
                if let Some(id) = results.get(*index) {
                    app.selected = *id;
                }
                app.mode = Mode::Normal;
            }
            KeyCode::Up => {
                if !results.is_empty() {
                    if *index == 0 {
                        *index = results.len() - 1;
                    } else {
                        *index -= 1;
                    }
                }
            }
            KeyCode::Down => {
                if !results.is_empty() {
                    *index = (*index + 1) % results.len();
                }
            }
            KeyCode::Backspace => {
                query.pop();
                app.update_search_results();
            }
            KeyCode::Char(c) => {
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    query.push(c);
                    app.update_search_results();
                }
            }
            _ => {}
        }
    }
    Ok(false)
}

fn save_current_map(app: &mut App, path: PathBuf) -> Result<()> {
    save_map(&path, &app.map)?;
    app.file_path = Some(path.clone());
    app.autosave_path = path;
    app.dirty = false;
    app.set_message("Map saved");
    app.last_autosave = Some(Instant::now());
    Ok(())
}

fn draw(f: &mut Frame, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(12),
            Constraint::Length(3),
        ])
        .split(f.size());

    render_header(f, chunks[0], app);

    let body_chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(65), Constraint::Percentage(35)])
        .split(chunks[1]);

    let side_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(55), Constraint::Percentage(45)])
        .split(body_chunks[1]);

    render_map(f, body_chunks[0], app);
    render_details(f, side_chunks[0], app);
    render_shortcuts(f, side_chunks[1], app);

    render_footer(f, chunks[2], app);

    match &app.mode {
        Mode::Renaming { buffer } => render_input_overlay(f, "Rename node", buffer),
        Mode::Prompt { kind, buffer } => {
            let label = match kind {
                PromptKind::Open => "Open map path",
                PromptKind::SaveAs => "Save map as",
                PromptKind::AutosavePath => "Autosave path",
                PromptKind::AutosaveInterval => "Autosave interval (seconds, 0 disables)",
            };
            render_input_overlay(f, label, buffer);
        }
        Mode::NoteEditor { buffer, cursor } => render_note_overlay(f, buffer, *cursor),
        Mode::Search {
            query,
            results,
            index,
        } => {
            render_search_overlay(f, query, *index, results.len());
        }
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
        Mode::Search { .. } => "search",
    };

    let title_line = format!(
        "ponder | {} | {}{}",
        app.map.name,
        file_label,
        if app.dirty { " *" } else { "" }
    );

    let info = format!(
        "selected: {} | mode: {} | nodes: {} | undo: {} | redo: {}",
        app.map
            .node(app.selected)
            .map(|n| n.title.as_str())
            .unwrap_or(""),
        mode,
        app.map.nodes.len(),
        app.undo_stack.len(),
        app.redo_stack.len()
    );

    let autosave_info = if app.autosave_interval.is_zero() {
        format!(
            "autosave: off | path {} | last {}",
            app.autosave_path.to_string_lossy(),
            app.format_last_autosave()
        )
    } else {
        format!(
            "autosave: every {}s to {} | last {}",
            app.autosave_interval.as_secs(),
            app.autosave_path.to_string_lossy(),
            app.format_last_autosave()
        )
    };

    let header = Paragraph::new(format!("{title_line}\n{info}\n{autosave_info}"))
        .block(Block::default().borders(Borders::BOTTOM).title("Status"))
        .wrap(Wrap { trim: true });
    f.render_widget(header, area);
}

fn render_map(f: &mut Frame, area: Rect, app: &App) {
    let (min_x, max_x, min_y, max_y) = app.map.bounds();
    let mut search_hits = HashSet::new();
    let mut focused_hit: Option<NodeId> = None;
    if let Mode::Search { results, index, .. } = &app.mode {
        for id in results {
            search_hits.insert(*id);
        }
        if !results.is_empty() && *index < results.len() {
            focused_hit = Some(results[*index]);
        }
    }
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
                let base_color = node.color.to_color();
                let mut style = Style::default().fg(base_color);
                if Some(node.id) == focused_hit {
                    style = style
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED);
                } else if is_selected {
                    style = style
                        .add_modifier(Modifier::BOLD | Modifier::UNDERLINED)
                        .bg(Color::DarkGray);
                } else if search_hits.contains(&node.id) {
                    style = style.fg(Color::LightYellow);
                }
                ctx.print(
                    node.position.x,
                    node.position.y,
                    Span::styled(node.title.clone(), style),
                );
            }
        });

    f.render_widget(canvas, area);
}

fn render_details(f: &mut Frame, area: Rect, app: &App) {
    let node = app.map.node(app.selected);
    let (title, note, color_label) = node
        .map(|n| (n.title.clone(), n.note.clone(), n.color.label().to_string()))
        .unwrap_or_else(|| ("".to_string(), "".to_string(), "default".to_string()));
    let content = format!(
        "{}\ncolor: {}\n\n{}",
        title,
        color_label,
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

fn render_shortcuts(f: &mut Frame, area: Rect, _app: &App) {
    let lines = [
        "q quit | o open | s save | S save as | n new",
        "A autosave path | I autosave interval",
        "a add | c clone | d delete | m move | t rename | k color",
        "[ parent | ] child | arrows move/select | Tab/Shift-Tab cycle",
        "Enter notes | / search | u undo | r redo | ? toggle help",
    ];
    let block = Block::default()
        .title(Title::from("Shortcuts").alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(lines.join("\n"))
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(paragraph, area);
}

fn render_footer(f: &mut Frame, area: Rect, app: &App) {
    let msg = app.message_text();
    let block = Block::default().borders(Borders::TOP).title("Messages");
    let paragraph = Paragraph::new(msg).block(block).wrap(Wrap { trim: true });
    f.render_widget(paragraph, area);
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

fn render_search_overlay(f: &mut Frame, query: &str, index: usize, count: usize) {
    let area = centered_rect(60, 20, f.size());
    let title = format!(
        "Search ({}/{})",
        if count == 0 { 0 } else { index + 1 },
        count
    );
    let block = Block::default()
        .title(Title::from(title).alignment(Alignment::Center))
        .borders(Borders::ALL);
    let hint = "\nUp/Down to cycle, Enter to jump, Esc to close";
    let paragraph = Paragraph::new(query.to_string() + "_" + hint).block(block);
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
}

fn render_note_overlay(f: &mut Frame, buffer: &str, cursor: usize) {
    let area = centered_rect(90, 80, f.size());
    let block = Block::default()
        .title(Title::from("Notes (Esc to close)").alignment(Alignment::Center))
        .borders(Borders::ALL);
    let paragraph = Paragraph::new(buffer.to_string())
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(Clear, area);
    f.render_widget(paragraph, area);
    if let Some((x, y)) = note_cursor_position(buffer, cursor, area) {
        f.set_cursor(x, y);
    }
}

fn note_cursor_position(buffer: &str, cursor: usize, area: Rect) -> Option<(u16, u16)> {
    if area.width < 2 || area.height < 2 {
        return None;
    }
    let content_width = area.width.saturating_sub(2).max(1) as usize;
    let content_height = area.height.saturating_sub(2) as usize;
    if content_height == 0 {
        return None;
    }

    let cursor = cursor.min(buffer.len());
    let mut x = 0usize;
    let mut y = 0usize;
    for ch in buffer[..cursor].chars() {
        if ch == '\n' {
            x = 0;
            y += 1;
        } else {
            x += 1;
            if x >= content_width {
                x = 0;
                y += 1;
            }
        }
    }

    if y >= content_height {
        y = content_height - 1;
        x = x.min(content_width.saturating_sub(1));
    }

    let x = area
        .x
        .saturating_add(1)
        .saturating_add(x as u16)
        .min(area.x.saturating_add(area.width.saturating_sub(1)));
    let y = area
        .y
        .saturating_add(1)
        .saturating_add(y as u16)
        .min(area.y.saturating_add(area.height.saturating_sub(1)));
    Some((x, y))
}

fn render_help(f: &mut Frame) {
    let area = centered_rect(70, 60, f.size());
    let lines = [
        "Movement: arrows; parent [: child ]",
        "Select next/prev: Tab / Shift-Tab",
        "Add child: a | Clone: c | Delete: d",
        "Move node: m then arrows (hold Shift for bigger steps)",
        "Rename: t | Notes: Enter (multi-line, Esc to close) | Color: k",
        "Search: / (type, Enter to jump, Up/Down cycle)",
        "Undo: u or Ctrl+Z | Redo: r or Ctrl+Y",
        "Autosave path: A | Autosave interval: I (seconds, 0 = off)",
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
        let autosave_path = file_path
            .clone()
            .unwrap_or_else(|| PathBuf::from("ponder_autosave.json"));
        Self {
            map,
            selected,
            file_path,
            autosave_path,
            mode: Mode::Normal,
            message: None,
            dirty: false,
            show_help: false,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            last_autosave: None,
            autosave_interval: Duration::from_secs(10),
            moving_snapshot: false,
        }
    }

    fn set_message(&mut self, text: impl Into<String>) {
        self.set_message_with_timeout(text, Some(Duration::from_secs(4)));
    }

    fn set_message_with_timeout(&mut self, text: impl Into<String>, timeout: Option<Duration>) {
        let expires_at = timeout.map(|d| Instant::now() + d);
        self.message = Some(Toast {
            text: text.into(),
            expires_at,
        });
    }

    fn prune_message(&mut self) {
        if let Some(toast) = &self.message {
            if let Some(expire) = toast.expires_at {
                if Instant::now() >= expire {
                    self.message = None;
                }
            }
        }
    }

    fn message_text(&self) -> String {
        self.message
            .as_ref()
            .map(|m| m.text.clone())
            .unwrap_or_else(|| "Ready. Press ? for help.".to_string())
    }

    fn reset_to_new(&mut self) {
        self.map = MindMap::new("Mind Map");
        self.selected = self.map.root;
        self.file_path = None;
        self.autosave_path = PathBuf::from("ponder_autosave.json");
        self.dirty = false;
        self.set_message("Started a new map");
        self.undo_stack.clear();
        self.redo_stack.clear();
        self.moving_snapshot = false;
        self.last_autosave = None;
    }

    fn map_select_direction(&mut self, dir: NavDir) {
        if let Some(next) = self.map.nearest_in_direction(self.selected, dir) {
            self.selected = next;
        }
    }

    fn jump_to_parent(&mut self) {
        if let Some(parent) = self.map.node(self.selected).and_then(|n| n.parent) {
            self.selected = parent;
            self.set_message("Jumped to parent");
        } else {
            self.set_message("No parent");
        }
    }

    fn jump_to_first_child(&mut self) {
        if let Some(child) = self
            .map
            .node(self.selected)
            .and_then(|n| n.children.first())
            .copied()
        {
            self.selected = child;
            self.set_message("Jumped to child");
        } else {
            self.set_message("No child");
        }
    }

    fn push_undo(&mut self) {
        self.push_current_to_undo();
        self.redo_stack.clear();
    }

    fn push_current_to_undo(&mut self) {
        const HISTORY_LIMIT: usize = 64;
        self.undo_stack.push(AppState {
            map: self.map.clone(),
            selected: self.selected,
        });
        if self.undo_stack.len() > HISTORY_LIMIT {
            self.undo_stack.remove(0);
        }
    }

    fn undo(&mut self) {
        if let Some(state) = self.undo_stack.pop() {
            let redo_state = AppState {
                map: self.map.clone(),
                selected: self.selected,
            };
            self.redo_stack.push(redo_state);
            self.map = state.map;
            self.selected = state.selected;
            self.dirty = true;
            self.set_message("Undo");
        } else {
            self.set_message("Nothing to undo");
        }
    }

    fn redo(&mut self) {
        if let Some(state) = self.redo_stack.pop() {
            self.push_current_to_undo();
            self.map = state.map;
            self.selected = state.selected;
            self.dirty = true;
            self.set_message("Redo");
        } else {
            self.set_message("Nothing to redo");
        }
    }

    fn maybe_autosave(&mut self) {
        if !self.dirty {
            return;
        }
        if let Some(last) = self.last_autosave {
            if last.elapsed() < self.autosave_interval {
                return;
            }
        }
        if self.autosave_interval.is_zero() {
            return;
        }
        let path = self.autosave_path.clone();
        if save_map(&path, &self.map).is_ok() {
            self.last_autosave = Some(Instant::now());
            self.set_message(format!("Autosaved {}", path.to_string_lossy()));
        }
    }

    fn set_autosave_path(&mut self, path: PathBuf) {
        self.autosave_path = path.clone();
        self.set_message(format!("Autosave path set to {}", path.to_string_lossy()));
    }

    fn set_autosave_interval(&mut self, secs: u64) {
        self.autosave_interval = Duration::from_secs(secs);
        if secs == 0 {
            self.set_message("Autosave disabled");
        } else {
            self.set_message(format!("Autosave every {}s", secs));
        }
    }

    fn format_last_autosave(&self) -> String {
        if self.autosave_interval.is_zero() {
            return "disabled".to_string();
        }
        if let Some(last) = self.last_autosave {
            let secs = last.elapsed().as_secs();
            if secs == 0 {
                "just now".to_string()
            } else {
                format!("{secs}s ago")
            }
        } else {
            "never".to_string()
        }
    }

    fn update_search_results(&mut self) {
        if let Mode::Search {
            query,
            results,
            index,
        } = &mut self.mode
        {
            let q = query.to_lowercase();
            results.clear();
            if q.is_empty() {
                return;
            }
            for node in &self.map.nodes {
                let title = node.title.to_lowercase();
                let note = node.note.to_lowercase();
                if title.contains(&q) || note.contains(&q) {
                    results.push(node.id);
                }
            }
            if results.is_empty() {
                *index = 0;
            } else if *index >= results.len() {
                *index = 0;
            }
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
                color: NodeColor::Default,
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
            color: NodeColor::Default,
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
