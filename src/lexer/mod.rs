use super::token::*;

// The file to be lexed is passed to lex() as a (long-ass) string.
// This is treated as an iterator in this context.
// SourceFile is a wrapper around that iterator which keeps track of the current
// position within the file.
struct SourceFile
{
    file: String,           // This is the type returned from String::chars().peekable()
    pos: TokenFilePosition, // The current position in the file.
}

impl SourceFile
{
    fn new(file: String) -> SourceFile
    {
        SourceFile
        {
            file: file.chars().rev().collect(),
            pos: TokenFilePosition { line: 1, col: 0 }
        }
    }

    // Return the next character without incrementing the position
    fn peek(&self) -> Option<char>
    {
        self.file.chars().last()
    }

    fn next(&mut self) -> Option<char>
    {
        let c: char;

        match self.file.pop()
        {
            Some(C) => c = C,
            None => return None
        }
        
        if c == '\n'
        {
            self.pos.line += 1;
            self.pos.col = 0;
        }
        else
        {
            self.pos.col += 1;
        }

        return Some(c);
    }
}

// The lexer is considered to be in one of these states at all times.
// See loop in function below.
enum LexerState
{
    Other,
    Identifier,
    Number,
}

// Main lexing method
pub fn lex(file: String) -> Vec<Token>
{
    // Apply lexing rules:
    //  alpha: consume id characters until non-id character
    //  num: consume number characters until non-number characters
    //  other chars just map to token directly

    // 'f' 'iter' -> 'fIter' -> 'fitter'
    let mut fitter = SourceFile::new(file);

    // Tokenised file
    let mut tokens: Vec<Token> = Vec::new();

    // Persistent state for the loop below
    let mut lexerstate = LexerState::Other;

    // Iterating over the characters in the file.
    while let Some(C) = fitter.next()
    {
        if let LexerState::Other = lexerstate
        {
            if C.is_alphabetic()
            {
                lexerstate = LexerState::Identifier;
                tokens.push(
                    Token
                    {
                        tokenType: TokenType::Identifier,
                        string:    C.to_string(),
                        pos:       fitter.pos
                    }
                )
            }

            else if C.is_numeric()
            {
                lexerstate = LexerState::Number;
                tokens.push(
                    Token
                    {
                        tokenType: TokenType::Number,
                        string:    C.to_string(),
                        pos:       fitter.pos
                    }
                )
            }

            else
            {
                tokens.push(
                    Token
                    {
                        tokenType: TokenType::Character,
                        string:    C.to_string(),
                        pos:       fitter.pos
                    }
                )
            }
        }

        else if let LexerState::Identifier = lexerstate
        {
            if C.is_alphabetic() || C.is_numeric() || C == '_'
            {
                // Note: due to construction of state machine,
                // the vector must always contain at least one element,
                // so using unwrap() here is fine
                let mut t = tokens.pop().unwrap();
                t.string.push(C);
                tokens.push(t);
            }
            else
            {
                lexerstate = LexerState::Other;
                tokens.push(
                    Token
                    {
                        tokenType: TokenType::Character,
                        string:    C.to_string(),
                        pos:       fitter.pos
                    }
                )
            }
        }

        else if let LexerState::Number = lexerstate
        {
            if C.is_numeric() 
            {
                let mut t = tokens.pop().unwrap();
                t.string.push(C);
                tokens.push(t);
            }
            else
            {
                lexerstate = LexerState::Other;
                tokens.push(
                    Token
                    {
                        tokenType: TokenType::Character,
                        string:    C.to_string(),
                        pos:       fitter.pos
                    }
                )
            }
        }
    }
    return tokens;
}

#[cfg(test)]
mod tests
{
    use super::*;

    fn begin_test(test_string: String) -> Vec<Token>
    {
        println!("Test string: {}", test_string);
        return lex(test_string)
    }

    #[test]
    fn plain_int()
    {
        let a = begin_test(String::from("1"));
        assert_eq!(a.len(), 1);

        let mut a = a.iter();
        println!("{:?}", a.next());
        println!("{:?}", a.next());
    }

    #[test]
    fn quick_maths()
    {
        // Should be list of tokens:
        // Number("2")
        // Plus
        // Number("2")
        // Dash
        // Number("1")
        // Semicolon
        let a = begin_test(String::from("2+2-1;"));
        assert_eq!(a.len(), 6);

        let mut a = a.iter();
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
    }

    #[test]
    fn identifiers()
    {
        let a = begin_test(String::from("hello world"));
        // assert_eq!(a.len(), 2);

        let mut a = a.iter();
        println!("{:?}", a.next());
        println!("{:?}", a.next());
        println!("{:?}", a.next());
    }
}
