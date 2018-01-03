use parser::Production;
use token::{TokenStream,TokenType};
use lexer::lex;

use std::collections::HashMap;

// Grammar:
// Represents a grammar.
//
// String: The name of the rule
// Rule: The rule.
struct Grammar(HashMap<String, Rule>);

struct Rule(Vec<Case>);

struct Case(Vec<ModifiedGroup>);

enum Modifier
{
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}

struct ModifiedGroup(Option<Modifier>, Group);

enum Group
{
    single(Unit),
    many(Vec<ModifiedGroup>),
}

enum Unit
{
    Terminal(String),
    // build: if token_type is StringLiteral
    // parse: check token.string == Terminal(String).string

    NonTerminal(String),
    // build: if token_type is identifier
    // parse: lookup in hashmap and apply rule

    Identifier,
    // build: if token_type is identifier AND string == "identifier"
    // parse: match any identifier

    Or,
    // build: none
    // parse: allow previous modified group to fail
}

// build up the grammar in memory as a tree
// apply rules to token stream

// Specify each rule in a single string.
// This function tokenises each string and performs some basic checks:
// * strings take the format: ident ':' anything ';'
// * if there are any single character tokens, they must be one of:
//      '(' ')' '*' '?' '+' '|' ':' ';'
fn basic_checks(rules: Vec<String>) -> Result<Vec<TokenStream>, String>
{
    // This vector is for checking single-character tokens.
    // Single character tokens are only allowed if they are
    // in this set.
    let allowed_chars = vec!
    [
    '('.to_string(),
    ')'.to_string(),
    '*'.to_string(),
    '?'.to_string(),
    '+'.to_string(),
    '|'.to_string(),
    ';'.to_string(),
    ':'.to_string(),
    ];

    let mut results: Vec<TokenStream> = Vec::new();

    for rule in rules
    {
        // Tokenise
        let ts = lex(rule);

        /*
        From the rules defined for the grammar, we know that
        each rule must have at least 4 elements:

        an identifier
        a colon
        at least one *something*
        a semicolon
        */
        if ts.len() < 4
        {
            return Err(format!("Expected at least 4 tokens, found {}", ts.len()));
        }

        // New scope so that the borrow ends before we move ts
        {
            let mut titer = ts.iter();
            
            // First token must be an identifier
            match titer.next()
            {
                Some(t) =>
                {
                    if t.token_type != TokenType::Identifier { return Err("First token was not an identifier".to_string()); };
                },
                None => return Err("Expected a token but found nothing".to_string()),
            }

            // Next token must be the colon character
            match titer.next()
            {
                Some(t) =>
                {
                    if t.token_type != TokenType::Character { return Err(format!("Expected colon character but found {}", t.string)); };
                    if t.string != ":".to_string() { return Err(format!("Expected colon character but found {}", t.string)); };
                },
                None => return Err("Expected a token but found nothing".to_string()),
            }

            // Now iterate over the remaining tokens.
            // If there are single character tokens, they must be members of the
            // set defined above.
            let mut contains_semicolon = false;
            while let Some(t) = titer.next()
            {
                if t.token_type == TokenType::Character
                {
                    if !allowed_chars.contains(&t.string)
                    {
                        return Err(format!("Invalid character in input: {}", t.string));
                    }

                    if t.string == ";".to_string()
                    {
                        contains_semicolon = true;
                    }
                }
            }

            if !contains_semicolon
            {
                return Err("Rule does not contain a semicolon".to_string());
            }
        }

        results.push(ts);
    }

    return Ok(results);
}

enum BuildResult<T>
{
    New(T),
    Finished,
    Error,
}

// Builds a grammar in memory.
fn build_grammar(rules: Vec<TokenStream>) -> BuildResult<Grammar>
{
    use grammar2::BuildResult::*;

    let mut hashmap: HashMap<String, Rule> = HashMap::new();

    for mut rule in rules
    {
        // First token should be an identifier
        let name = match rule.pop_front()
        {
            Some(t) =>
            {
                if t.token_type != TokenType::Identifier
                {
                    // TODO: Emit error message
                    // First token in grammar rule should be an identifier
                    return Error;
                }
                t.string
            },
            None =>
            {
                // TODO: Emit error message
                // No tokens in input
                return Error;
            }
        };

        // Next token should be a colon
        match rule.pop_front()
        {
            Some(t) =>
            {
                if t.string != ":".to_string()
                {
                    // TODO: Emit error message
                    // Expected colon after rule name
                    return Error;
                }
            },
            None =>
            {
                // TODO: Emit error message
                // Expected colon after rule name
                return Error;
            }
        }

        // Now build rule
        let rule = match build_rule(rule)
        {
            New(r) => r,
            Finished => return Error,
            Error => return Error,
        };

        hashmap.insert(name, rule);
    }

    return New(Grammar(hashmap));
}

fn build_rule(mut ts: TokenStream) -> BuildResult<Rule>
{
    use grammar2::BuildResult::*;

    let mut cases: Vec<Case> = Vec::new();
    loop
    {
        match build_case(&mut ts)
        {
            New(case) =>
            {
                // Expect semicolon after case
                match ts.pop_front()
                {
                    Some(token) =>
                    {
                        if token.string != ";".to_string()
                        {
                            // TODO: Emit error message
                            // Expected semicolon after case
                            return Error;
                        }
                    },
                    None =>
                    {
                        // TODO: Emit error message
                        // Expected semicolon after case
                        // (Note: might allow this in future)
                        return Error;
                    }
                }

                // Add the new case
                cases.push(case);
            },
            Finished => break,
            Error => return Error,
        }
    }

    if cases.len() == 0
    {
        // TODO: Emit error message
        // Rule must have at least one case
        return Error;
    }

    // TODO: Log
    // Successfully built rule
    return New(Rule(cases));
}

fn build_case(ts: &mut TokenStream) -> BuildResult<Case>
{
    use grammar2::BuildResult::*;

    let mut modgroups: Vec<ModifiedGroup> = Vec::new();

    loop
    {
        match build_modified_group(ts)
        {
            New(mg) => modgroups.push(mg),
            Finished =>
            {
                if modgroups.len() == 0
                {
                    return Finished;
                }
                break;
            },
            Error => return Error,
        }
    }

    return New(Case(modgroups));
}

fn build_modified_group(ts: &mut TokenStream) -> BuildResult<ModifiedGroup>
{
    use grammar2::BuildResult::*;

    let modifier: Option<Modifier>;
    match ts.pop_front()
    {
        Some(t) =>
        {
            if t.string == "*".to_string()
            {
                modifier = Some(Modifier::ZeroOrMore);
            }

            else if t.string == "+".to_string()
            {
                modifier = Some(Modifier::OneOrMore);
            }

            else if t.string == "?".to_string()
            {
                modifier = Some(Modifier::ZeroOrOne);
            }

            else
            {
                // Whoops! Better put that back
                ts.push_front(t);
                modifier = None;
            }
        },
        None =>
        {
            // TODO: Emit error message
            // No tokens in input
            return Error;
        }
    };

    let group = match build_group(ts)
    {
        New(gp) => gp,
        Finished => return Finished,
        Error => return Error,
    };

    return New(ModifiedGroup(modifier, group));
}

fn build_group(ts: &mut TokenStream) -> BuildResult<Group>
{
    use grammar2::BuildResult::*;

    let t = match ts.pop_front()
    {
        Some(t) => t,
        None =>
        {
            // TODO: Emit error message
            // No tokens in input
            return Error;
        }
    };

    // Repeated case: braces followed by at least one modified group
    if t.string == "(".to_string()
    {       
        loop
        {
            let mut mgs: Vec<ModifiedGroup> = Vec::new();
            match build_modified_group(ts)
            {
                New(mg) =>
                {
                    mgs.push(mg);
                },
                Finished => 
                {
                    // Expect ')' after group
                    match ts.pop_front()
                    {
                        Some(t) =>
                        {
                            if t.string != ")".to_string()
                            {
                                // TODO: Emit error message
                                // Expected closing brace
                                return Error;
                            }
                        },
                        None =>
                        {
                            // TODO: Emit error message
                            // Expected closing brace
                            return Error;
                        }
                    }

                    return New(Group::many(mgs));
                },
                Error => return Error,
            }

        }

    }

    else
    {
        // Put this back
        ts.push_front(t);

        // Singular case: one unit
        match build_unit(ts)
        {
            New(unit) => 
            {
                return New(Group::single(unit));
            },
            Finished => return Finished,
            Error => return Error,
        }
    }
}

fn build_unit(ts: &mut TokenStream) -> BuildResult<Unit>
{
    use grammar2::BuildResult::*;

    match ts.pop_front()
    {
        Some(t) =>
        {
            if t.token_type == TokenType::StringLiteral
            {
                return New(Unit::Terminal(t.string));
            }

            if t.token_type == TokenType::Identifier
            {
                if t.string == "identifier"
                {
                    return New(Unit::Identifier);
                }

                else
                {
                    return New(Unit::NonTerminal(t.string));
                }
            }

            if t.token_type == TokenType::Character
            {
                if t.string == ";".to_string()
                {
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == "(".to_string()
                {
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == ")".to_string()
                {
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == "|".to_string()
                {
                    return New(Unit::Or);
                }

                else
                {
                    // TODO: Expected one of ; ( ) | but found other
                    return Error;
                }
            }

            else
            {
                // TODO: Emit error message
                // invalid unit
                return Error;
            }
        },
        None =>
        {
            // TODO: Emit error message
            // No tokens in input
            return Error;
        }
    }
}

// fn parse_with_grammar(grammar: Grammar, ts: TokenStream) -> AST
// {
// }

// fn parse_rule(rule: Rule, ptr: TsIter) -> AST
// {
// }

#[cfg(test)]
mod tests
{
    use super::*;

    // Tests for basic checks

    fn basic_checks_meta(rules: Vec<String>) -> Vec<TokenStream>
    {
        match basic_checks(rules)
        {
            Ok(v) => return v,
            Err(s) => panic!("Basic check FAILED {}", s),
        }
    }

    #[test]
    fn basic_checkA()
    {
        // One rule, almost simplest possible
        basic_checks_meta(vec!["basic: identifier;".to_string()]);
    }

    #[test]
    fn basic_checkB()
    {
        basic_checks_meta(vec!["basic: identifier; 'lit' identifier;".to_string()]);
    }

    #[test]
    fn basic_checkC()
    {
        // A more real-world test
        basic_checks_meta(vec!["vardecl: 'var' identifier ?(':' identifier);".to_string()]);
    }

    #[test]
    #[should_panic]
    fn no_semicolon()
    {
        basic_checks_meta(vec!["basic: identifier identifier".to_string()]);
    }

    #[test]
    #[should_panic]
    fn too_short()
    {
        basic_checks_meta(vec!["basic: identifier".to_string()]);
    }
}
