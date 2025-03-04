use crate::{ui, viewer::App};

use std::io::Stdout;
use std::{error::Error, io};

use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use log::error;
use tui::{
    backend::{Backend, CrosstermBackend},
    Terminal,
};

pub fn launch(path: String) -> Result<(), Box<dyn Error>> {
    // setup terminal
    let mut terminal = setup()?;

    // create and run app
    let app = App::new(&path).map_err(|err| {
        let _ = cleanup();

        Box::<dyn Error>::from(format!("user should provide path to a valid dot file: {err}"))
    })?;
    let _ = run(&mut terminal, app);

    // restore terminal
    cleanup()?;

    Ok(())
}

fn setup() -> Result<Terminal<CrosstermBackend<Stdout>>, Box<dyn Error>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend)?;

    // setup panic hook to ensure that the terminal is restored even on panics
    setup_panic_hook();

    Ok(terminal)
}

fn setup_panic_hook() {
    let panic_handler = better_panic::Settings::auto().create_panic_handler();
    std::panic::set_hook(Box::new(move |info| {
        let _ = cleanup();

        error!("dot-viewer {}", info);

        panic_handler(info);
    }));
}

fn run<B: Backend>(terminal: &mut Terminal<B>, mut app: App) -> io::Result<()> {
    loop {
        terminal.draw(|f| ui::draw_app(f, &mut app))?;

        if app.quit {
            break;
        }

        loop {
            // Do not redraw unless we get a key event or a window resize event.
            match event::read()? {
                Event::Key(key) => app.key(key),
                Event::Resize(_, _) => {}

                Event::FocusGained | Event::FocusLost | Event::Mouse(_) | Event::Paste(_) => {
                    continue;
                }
            }

            break;
        }
    }

    Ok(())
}

fn cleanup() -> Result<(), Box<dyn Error>> {
    let mut stdout = io::stdout();
    execute!(stdout, LeaveAlternateScreen, DisableMouseCapture)?;
    disable_raw_mode()?;

    Ok(())
}
