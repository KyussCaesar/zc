// Token types
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType
{
    Number,
    Identifier,
    Character,
    StringLiteral,
    Keyword,
}

use std::collections::VecDeque;
pub type TokenStream = VecDeque<Token>;

pub fn print_ts(ts: &TokenStream)
{
    println!("");
    for t in ts
    {
        print_token(t);
    }
}

pub fn print_token(t: &Token)
{
    println!("Token\ttype({:?})\tstring({})", t.token_type, t.string);
}

// Represents a position in the file.
// Used for generating error messages
#[derive(Copy, Clone, Debug)]
pub struct TokenFilePosition
{
    pub line : i32,
    pub col  : i32,
}

#[derive(Debug, Clone)]
pub struct Token
{
    pub token_type : TokenType,
    pub string     : String,
    pub pos        : TokenFilePosition,
}

