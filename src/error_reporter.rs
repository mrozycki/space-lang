pub struct CodeLocation {
    pub line: usize,
    pub column: usize,
}

pub trait ErrorReporter {
    fn report(&self, message: &str, location: Option<&CodeLocation>);
}

pub struct ConsoleErrorReporter {}

impl ConsoleErrorReporter {
    pub fn new() -> Self {
        ConsoleErrorReporter {}
    }
}

impl ErrorReporter for ConsoleErrorReporter {
    fn report(&self, message: &str, location: Option<&CodeLocation>) {
        match location {
            Some(CodeLocation { line, column }) => {
                eprintln!("Error in line {}, col {}: {}", line, column, message)
            }
            None => eprintln!("Error at the end of file: {}", message),
        }
    }
}
