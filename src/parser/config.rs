// Configures the parser.

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParserConfig {
    pub indent: u32,
    pub text_size_limit: usize,
}

impl Default for ParserConfig {
    fn default() -> Self {
        ParserConfig {
            indent: 4,
            // TODO: use this to provide a little bit of safety.
            text_size_limit: 16_000_000,
        }
    }
}
