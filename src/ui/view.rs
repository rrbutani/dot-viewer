use crate::{
    ui::{surrounding_block, utils::htmlparser},
    viewer::{Focus, View},
};

use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use graphviz_rs::prelude::*;

use rayon::prelude::*;
use tui::{
    backend::Backend,
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Spans, Text},
    widgets::{Block, List, ListItem, Paragraph, Wrap},
    Frame,
};

pub(super) fn draw_view<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(35), Constraint::Percentage(65)].as_ref())
        .split(chunk);

    draw_left(f, chunks[0], view);
    draw_right(f, chunks[1], view);
}

fn draw_left<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    if view.matches.items.is_empty() && view.selection.is_empty() {
        draw_current(f, chunk, view);
    } else {
        let [current, bottom] = Layout::default()
            .direction(Direction::Vertical)
            .constraints([Constraint::Percentage(99), Constraint::Percentage(1)].as_ref())
            .split(chunk)[..]
        else { unreachable!() };

        let [bottom_left, bottom_right] = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
            .split(bottom)[..]
        else { unreachable!() };

        draw_current(f, current, view);
        draw_match(f, bottom_right, view);
        draw_selected(f, bottom_left, view);
    }
}

fn draw_right<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(chunk);

    draw_adjacent(f, chunks[0], view);
    draw_metadata(f, chunks[1], view);
}

fn mk_selection_info_span(idx: &usize, view: &View) -> Span<'static> {
    if view.selection.contains(idx) {
        Span::styled("✓ ", Style::default().fg(Color::Rgb(150, 255, 150)))
    } else {
        Span::raw("  ")
    }
}

fn draw_current<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    view.viewport_info.current_list_height = (chunk.height as usize).saturating_sub(2);

    let progress = view.progress_current();
    let title = format!("Nodes {progress}");
    let block = surrounding_block(title, view.focus == Focus::Current);

    let froms: HashSet<&String> = HashSet::from_iter(&view.prevs.items);
    let tos: HashSet<&String> = HashSet::from_iter(&view.nexts.items);
    let mut matches = HashMap::new();
    for (idx, highlight) in &view.matches.items {
        matches.insert(*idx, highlight);
    }

    // TODO(perf): it'd be nice if we only did this computation for the nodes in
    // the viewport...
    //
    // Unfortunately (because `ListState` is opaque and because `List` insists
    // on receiving a `Vec` instead of an iterator that it could `skip`) we
    // don't have a great way of achieving this..
    //
    // For now we'll cheat a little bit and conservatively assume that the
    // offset is within ± height of the selected index. (we can actually do
    // better by essentially replicating the logic in `List::get_items_bounds`
    // but this is already material savings for large node lists).
    //
    // Note that we're still _allocating_ a larger-than-needed `Vec` but.. I'll
    // leave the issue for another day.
    let range = {
        let height = chunk.height - 2;
        let curr = view.current.state.selected().unwrap_or(0);
        let min = curr.saturating_sub(height as usize);
        let max = curr.saturating_add(height as usize).min(view.current.items.len());

        min.saturating_sub(1)..=max
    };
    let blank = ListItem::new(Text::raw("error!!! you should never see this!"));

    let list: Vec<ListItem> = (view.current.items.par_iter())
        .enumerate()
        .map(|(idx, id)| {
            // See above.
            if !range.contains(&idx) {
                return blank.clone();
            }

            let mut spans = Vec::with_capacity(id.len() + 2);

            // Selection status:
            //
            // TODO: perhaps this'd look better right-aligned? requires some
            // extra trickery on our part (either reinvent the logic in `List`
            // or use the width in `chunk` to truncate/pad the node labels +
            // append selection status)

            // TODO(perf): we could take advantage of the fact that the btreeset
            // is already to sorted and iterate through it in step with the list
            // (i.e. always know the _next_ selected node, once we've hit it
            // pull from the selection iterator again) for some speedup
            //
            // btreeset lookup is `O(log(n))` and we do it n times; this'd be
            // linear
            //
            // however... turning this into a parallel iterator (very doable!)
            // requires some work so I'll leave this for another day
            spans.push(mk_selection_info_span(&idx, view));
            let offs = spans.len();

            // Node name with chars styled based on the current search matches:
            spans.extend(id.chars().map(|c| Span::raw(c.to_string())));
            if let Some(&highlight) = matches.get(&idx) {
                for &idx in highlight {
                    spans[offs + idx].style =
                        Style::default().bg(Color::Rgb(120, 120, 120)).add_modifier(Modifier::BOLD);
                }
            }

            let mut item = ListItem::new(Spans(spans));

            if froms.contains(&id) {
                item = item.style(Style::default().fg(Color::Rgb(255, 150, 150)));
            } else if tos.contains(&id) {
                item = item.style(Style::default().fg(Color::Rgb(150, 150, 255)));
            }

            item
        })
        .collect();

    let list = List::new(list)
        .block(block)
        .highlight_style(Style::default().fg(Color::Green).add_modifier(Modifier::BOLD))
        .highlight_symbol("> ");

    f.render_stateful_widget(list, chunk, &mut view.current.state);
}

fn draw_match<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &View) {
    let title = if view.matches.items.is_empty() { String::new() } else { view.progress_matches() };
    let block = Block::default().title(title).title_alignment(Alignment::Right);

    f.render_widget(block, chunk);
}

fn draw_selected<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &View) {
    let title = if view.selection.is_empty() {
        Spans::default()
    } else {
        let (selected, total, percentage) = view.progress_selection();
        // format!("[{} of {} selected ({:.3}%)]", selected, total, percentage)
        Spans::from(vec![
            "[".into(),
            Span::styled(format!("{selected}"), Style::default().fg(Color::Rgb(150, 255, 150))),
            format!(" of {total} selected ({percentage:.3}%)]").into(),
        ])
    };
    let block = Block::default().title(title).title_alignment(Alignment::Left);

    f.render_widget(block, chunk);
}

fn draw_adjacent<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
        .split(chunk);

    draw_prevs(f, chunks[0], view);
    draw_nexts(f, chunks[1], view);
}

fn draw_prevs<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    view.viewport_info.prev_list_height = (chunk.height as usize).saturating_sub(2);

    let block = surrounding_block("Prev Nodes".to_string(), view.focus == Focus::Prev);

    let list: Vec<ListItem> = (view.prevs.items.par_iter())
        .map(|id| (id, mk_selection_info_span(&view.current_node_to_idx_map[id], view)))
        .map(|(id, sel_info)| ListItem::new(Spans::from(vec![sel_info, Span::raw(id.as_str())])))
        .collect();

    let list = List::new(list)
        .block(block)
        .highlight_style(Style::default().fg(Color::Red).add_modifier(Modifier::BOLD))
        .highlight_symbol("> ");

    f.render_stateful_widget(list, chunk, &mut view.prevs.state);
}

fn draw_nexts<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &mut View) {
    view.viewport_info.next_list_height = (chunk.height as usize).saturating_sub(2);

    let block = surrounding_block("Next Nodes".to_string(), view.focus == Focus::Next);

    let list: Vec<ListItem> = (view.nexts.items.par_iter())
        .map(|id| (id, mk_selection_info_span(&view.current_node_to_idx_map[id], view)))
        .map(|(id, sel_info)| ListItem::new(Spans::from(vec![sel_info, Span::raw(id.as_str())])))
        .collect();

    let list = List::new(list)
        .block(block)
        .highlight_style(Style::default().fg(Color::Blue).add_modifier(Modifier::BOLD))
        .highlight_symbol("> ");

    f.render_stateful_widget(list, chunk, &mut view.nexts.state);
}

fn draw_metadata<B: Backend>(f: &mut Frame<B>, chunk: Rect, view: &View) {
    let block = surrounding_block("Attrs".to_string(), false);

    let id = view.current_id();
    let node = view.graph.search_node(&id).unwrap();

    let paragraph = Paragraph::new(pretty_metadata(node)).block(block).wrap(Wrap { trim: true });

    f.render_widget(paragraph, chunk);
}

fn pretty_metadata(node: &Node) -> String {
    let mut metadata = String::new();

    let id = node.id();
    writeln!(metadata, "[{id}]").unwrap();
    writeln!(metadata).unwrap();

    let attrs = node.attrs();

    if let Some(label) = attrs.get("label") {
        if label.is_html() {
            let label = htmlparser::parse(label.value());

            for data in label {
                if data.starts_with("Input") {
                    continue;
                }

                let values = data.split("\\l");
                for value in values {
                    writeln!(metadata, "{value}").unwrap();
                }
            }
        } else {
            let values = label.value().split("\\l");
            for value in values {
                writeln!(metadata, "{value}").unwrap();
            }
        }
    } else {
        for attr in attrs {
            let key = attr.key();
            let value = attr.value();

            writeln!(metadata, "{key} : {value}").unwrap();
        }
    }

    metadata
}
