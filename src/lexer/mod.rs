use super::token::*;

// The file to be lexed is passed to lex() as a (long-ass) string.
// This is treated as an iterator in this context.
// SourceFile is a wrapper around that iterator which keeps track of the current
// position within the file.
struct SourceFile
{
    file: String,
    pos: TokenFilePosition,
}

const RESERVED_WORDS: [&str;10] = 
[
    "if",
    "for",
    "use",
    "yield",
    "return",
    "func",
    "class",
    "var",
    "is",
    "->",
];

impl SourceFile
{
    fn new(file: String) -> SourceFile
    {
        SourceFile
        {
            file: file.chars().rev().collect(),
            pos: TokenFilePosition { line: 1, col: 0 },
        }
    }

    fn next(&mut self) -> Option<char>
    {
        let next: char;

        match self.file.pop()
        {
            Some(c) => next = c,
            None => return None
        }
        
        if next == '\n'
        {
            self.pos.line += 1;
            self.pos.col = 0;
        }
        else
        {
            self.pos.col += 1;
        }

        return Some(next);
    }
}

// The lexer is considered to be in one of these states at all times.
// See loop in function below.
enum LexerState
{
    Other,
    Identifier,
    Number,
    StringLiteral,
}

// Main lexing method
pub fn lex(file: String) -> TokenStream
{
    // Apply lexing rules:
    //  alpha: consume id characters until non-id character
    //  num: consume number characters until non-number characters
    //  other chars just map to token directly

    // 'f' 'iter' -> 'fIter' -> 'fitter'
    let mut fitter = SourceFile::new(file);

    // Tokenised file
    let mut tokens: TokenStream = TokenStream::new();

    // Persistent state for the loop below
    let mut lexerstate = LexerState::Other;
    let mut escapeswitch = false;

    // Iterating over the characters in the file.
    while let Some(c) = fitter.next()
    {
        if let LexerState::Other = lexerstate
        {
            if c.is_alphabetic()
            {
                lexerstate = LexerState::Identifier;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::Identifier,
                        string:     c.to_string(),
                        pos:        fitter.pos
                    }
                )
            }

            else if c.is_numeric()
            {
                lexerstate = LexerState::Number;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::Number,
                        string:     c.to_string(),
                        pos:        fitter.pos
                    }
                );
            }

            else if c.is_whitespace()
            {
                continue;
            }

            else if c == '"' || c == '\''
            {
                lexerstate = LexerState::StringLiteral;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::StringLiteral,
                        string:     String::new(),
                        pos:        fitter.pos,
                    }
                );
            }
            
            else
            {
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::Character,
                        string:     c.to_string(),
                        pos:        fitter.pos
                    }
                );
            }
        }

        else if let LexerState::Identifier = lexerstate
        {
            if c.is_alphabetic() || c.is_numeric() || c == '_'
            {
                // Note: due to construction of state machine,
                // the vector must always contain at least one element,
                // so using unwrap() here is fine
                let mut t = tokens.pop_back().unwrap();
                t.string.push(c);
                tokens.push_back(t);
            }

            else if c.is_whitespace()
            {
                lexerstate = LexerState::Other;
            }

            else if c == '"' || c == '\''
            {
                lexerstate = LexerState::StringLiteral;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::StringLiteral,
                        string:     String::new(),
                        pos:        fitter.pos,
                    }
                );
            }

            else
            {
                lexerstate = LexerState::Other;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::Character,
                        string:     c.to_string(),
                        pos:        fitter.pos,
                    }
                );
            }
        }

        else if let LexerState::Number = lexerstate
        {
            if c.is_numeric() 
            {
                let mut t = tokens.pop_back().unwrap();
                t.string.push(c);
                tokens.push_back(t);
            }

            else if c.is_whitespace()
            {
                lexerstate = LexerState::Other;
            }

            else if c == '"' || c == '\''
            {
                lexerstate = LexerState::StringLiteral;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::StringLiteral,
                        string:     String::new(),
                        pos:        fitter.pos,
                    }
                );
            }

            else
            {
                lexerstate = LexerState::Other;
                tokens.push_back(
                    Token
                    {
                        token_type: TokenType::Character,
                        string:     c.to_string(),
                        pos:        fitter.pos,
                    }
                );
            }
        }

        else if let LexerState::StringLiteral = lexerstate
        {
            if (c == '"' || c == '\'') && !escapeswitch
            {
                lexerstate = LexerState::Other;
            }

            else if c == '\\'
            {
                escapeswitch = true;
            }

            else
            {
                escapeswitch = false;
                let mut t = tokens.pop_back().unwrap();
                t.string.push(c);
                tokens.push_back(t);
            }
        }
    }

    return process_keywords(tokens);
}

fn process_keywords(mut ts: TokenStream) -> TokenStream
{
    use lexer::RESERVED_WORDS;

    let mut new_ts: TokenStream = TokenStream::new();

    while let Some(mut t) = ts.pop_front()
    {
        if t.token_type == TokenType::Identifier
        {
            if RESERVED_WORDS.contains(&&*t.string)
            {
                t.token_type = TokenType::Keyword;
            }
        }

        new_ts.push_back(t);
    }

    return new_ts;
}

#[cfg(test)]
mod tests
{
    // Testing module for the lexer.

    use lexer::lex;
    use token::TokenType;
    use token::TokenType::*;
    use token::print_ts;

    // Main lexer testing function
    fn lexer_test
    (
        test_string: String,         // The string to be tested
        num_tokens: usize,           // Expected number of tokens in the lexed string
        token_types: Vec<TokenType>, // Expected types of the tokens
        token_strings: Vec<&str>     // Expected values of the tokens
    ) 
    {
        let tokenstream = lex(test_string);
        print_ts(&tokenstream);

        // Check that the lexer returns the expected number of tokens
        assert_eq!(tokenstream.len(), num_tokens);

        for ((token, ttype), tstring) in 
            tokenstream.into_iter()
            .zip(token_types.into_iter())
            .zip(token_strings.into_iter())
        {
            // Check that each token is the expected type and value.
            assert_eq!(token.token_type, ttype);
            assert_eq!(token.string, tstring);
        }
    }

    #[test]
    fn plain_int()
    {
        lexer_test(
            String::from("1"),
            1,
            vec![Number],
            vec!["1"],
        );
    }

    #[test]
    fn quick_maths()
    {
        lexer_test(
            String::from("2+2-1;"),
            6,
            vec![Number , Character , Number , Character , Number , Character],
            vec!["2"    , "+"       , "2"    , "-"       , "1"    , ";"],
        );
    }

    #[test]
    fn identifiers()
    {
        lexer_test(
            String::from("hello world"),
            2,
            vec![Identifier , Identifier] ,
            vec!["hello"    , "world"]    ,
        );
    }

    #[test]
    fn string_literal_1()
    {
        lexer_test(
            String::from("\"hello\" for i"),
            3,
            vec![StringLiteral , Keyword , Identifier] ,
            vec!["hello"       , "for"   , "i"]        ,
        );
    }

    #[test]
    fn string_literal_2()
    {
        lexer_test(
            String::from("\"hello i am inside a string literal\", return good;"),
            5,
            vec![StringLiteral                        , Character , Keyword  , Identifier , Character] ,
            vec!["hello i am inside a string literal" , ","       , "return" , "good"     , ";"] ,
        );
    }

    #[test]
    fn string_literal_3()
    {
        lexer_test(
            String::from("'hello i am inside a string literal', return good;"),
            5,
            vec![StringLiteral                        , Character , Keyword  , Identifier , Character] ,
            vec!["hello i am inside a string literal" , ","       , "return" , "good"     , ";"] ,
        );
    }
}
