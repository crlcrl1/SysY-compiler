use colored::Colorize;
use std::process::exit;

pub fn show_error(error: &str, exit_code: i32) -> ! {
    eprintln!("{}: {}", "Error".red().bold(), error);
    exit(exit_code)
}

pub fn show_warning(warning: &str) {
    eprintln!("{}: {}", "Warning".yellow().bold(), warning);
}
