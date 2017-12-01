// Token types
#[derive(Debug)]
pub enum TokenType
{
    Number,
    Identifier,

    // All other characters
    Character,
}

// Represents a position in the file.
// Used for generating error messages
#[derive(Copy, Clone, Debug)]
pub struct TokenFilePosition
{
    pub line: i32,
    pub col: i32,
}

#[derive(Debug)]
pub struct Token
{
    pub tokenType : TokenType,
    pub string    : String,
    pub pos       : TokenFilePosition,
}

