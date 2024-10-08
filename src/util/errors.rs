use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidTypeError {
    pub expected: String,
    pub found: String,
    pub location: (usize, usize),
}

impl fmt::Display for InvalidTypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Invalid type: expected {:?}, found {} at {}:{}",
            self.expected, self.found, self.location.0, self.location.1
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberOutOfRangeError {
    pub number_str: String,
    pub location: (usize, usize),
}

impl fmt::Display for NumberOutOfRangeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Number out of range: {} at {}:{}",
            self.number_str, self.location.0, self.location.1
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticError {
    InvalidType(InvalidTypeError),
    NumberOutOfRange(NumberOutOfRangeError),
}
