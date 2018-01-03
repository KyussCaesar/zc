use token::{TokenStream, TokenType};

// Placeholder for later
struct SymbolTableEntry;

enum AST
{
    Assignment { left: SymbolTableEntry, right: Box<AST> },
    Add { left: Box<AST>, right: Box<AST> },
    Sub { left: Box<AST>, right: Box<AST> },
    Mul { left: Box<AST>, right: Box<AST> },
    Div { left: Box<AST>, right: Box<AST> },
    NumberLiteral(String),
    StringLiteral(String),
    Identifier(SymbolTableEntry),
    VariableDeclaration(SymbolTableEntry, Option<Box<AST>>),
}

/* Represents the result of applying a rule in the grammar.

Ownership of the token stream passes through the functions and out again. This
is because I like to pass ownership around rather than references. (this is
probably shit rust code).

    Yes(ast, ts)    Indicates that applying the specified rule in the grammar
                    was successful. It has the AST for this application of the
                    rule, and the modified token stream.

    No(ts)          "Failure without error", e.g, tried a rule but it doesn't
                    look like it fits.

    Error           "Failure with error", e.g, it looked like a rule matched,
                    but parser found invalid tokens before the end. e.g, an if
                    statement without a conditional expression.
*/
pub enum Production
{
    Yes(AST, TokenStream),
    No(TokenStream),
    Error,
}

// Builds a whole program.
fn Program(mut ts: TokenStream) -> Vec<AST>
{
    use parser::Production::*;

    let mut statements: Vec<AST> = Vec::new();

    while let Yes(ast, new_ts) = Statement(ts)
    {
        ts = new_ts;
        statements.push(ast);
    }

    return statements;
}

/*
The following functions each implement a rule in the grammar, and return
a Production value. This encapaulates the three results that applying a rule
can have.
*/
macro_rules! do_match
{
    ($func:ident, $ts:ident) =>
    {
        match $func($ts)
        {
            Yes(ast, new_ts) => return Yes(ast, new_ts),
            No(new_ts) => $ts = new_ts,
            Error => return Error,
        }
    }
}

fn Statement(mut ts: TokenStream) -> Production
{
    use parser::Production::*;

    do_match!(ClassDefinition, ts);
    do_match!(TraitDefinition, ts);
    do_match!(FunctionDefinition, ts);

    if let Yes(ast, mut new_ts) = VariableDeclaration(ts)
    {
        if let Some(token) = new_ts.pop_front()
        {
            // We expect the next token to be the ';' character
            if
                (token.token_type == TokenType::Character) &&
                (token.string == ";".to_string())
            {
                // TODO: Log
                // Found variable declaration
                return Yes(ast, new_ts);
            }
            else
            {
                // TODO: Emit error message
                // Expected semicolon after variable declaration
                return Error;
            }
        }
        else
        {
            // TODO: Emit error message
            // No tokens remain in input after variable declaration
            // (Expected a semicolon after variable declaration)
            return Error;
        }
    }

    // TODO: Emit error:
    // Expected a class, trait, or function definition, or variable declaration.
    return Error;
}

fn ClassDefinition(ts: TokenStream) -> Production
{
    use parser::Production::*;
    return No(ts);
}

fn TraitDefinition(ts: TokenStream) -> Production
{
    use parser::Production::*;
    return No(ts);
}

fn FunctionDefinition(ts: TokenStream) -> Production
{
    use parser::Production::*;
    return No(ts);
}

fn VariableDeclaration(ts: TokenStream) -> Production
{
    use parser::Production::*;
    use token::TokenType;

    let mut tcpy = ts.clone();

    if let Some(token) = tcpy.pop_front()
    {
        if 
            token.token_type == TokenType::Keyword &&
            token.string == "var".to_string()
        {
            // let ast = AST::VariableDeclaration(token, None);
        }
    }

    return No(ts);
}

