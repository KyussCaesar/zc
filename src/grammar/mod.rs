use std::collections::HashMap;

use token::{TokenStream,TokenType};
use lexer::lex;

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
    Single(Unit),
    Many(Vec<ModifiedGroup>),
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

// Specify each rule in a Single string.
// This function tokenises each string and performs some basic checks:
// * strings take the format: ident ':' anything ';'
// * if there are any Single character tokens, they must be one of:
//      '(' ')' '*' '?' '+' '|' ':' ';'
fn basic_checks(rules: Vec<String>) -> Result<Vec<TokenStream>, String>
{
    // This vector is for checking Single-character tokens.
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
                    if 
                        t.token_type != TokenType::Identifier &&
                        t.token_type != TokenType::Keyword
                    { 
                        return Err("First token was not an identifier".to_string());
                    }
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
            // If there are Single character tokens, they must be members of the
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
    use grammar::BuildResult::*;

    let mut hashmap: HashMap<String, Rule> = HashMap::new();

    for mut rule in rules
    {
        // First token should be an identifier
        let name = match rule.pop_front()
        {
            Some(t) =>
            {
                if 
                    t.token_type != TokenType::Identifier &&
                    t.token_type != TokenType::Keyword
                {
                    // TODO: Emit error message
                    error!("First token in grammar rule should be an identifier");
                    return Error;
                }
                t.string
            },
            None =>
            {
                // TODO: Emit error message
                error!("[build_grammar] No tokens at beginning of rule");
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
                    error!("Expected colon after rule name");
                    return Error;
                }
            },
            None =>
            {
                // TODO: Emit error message
                error!("Expected colon after rule name");
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
    use grammar::BuildResult::*;

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
                            error!("Expected semicolon after case");
                            return Error;
                        }
                    },
                    None =>
                    {
                        // TODO: Emit error message
                        error!("Expected semicolon after case");
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
        error!("Rule must have at least one case");
        return Error;
    }

    // TODO: Log
    info!("Successfully built rule");
    return New(Rule(cases));
}

fn build_case(ts: &mut TokenStream) -> BuildResult<Case>
{
    use grammar::BuildResult::*;

    // If the token stream is empty then we are finished
    if ts.is_empty()
    {
        return Finished;
    }

    let mut modgroups: Vec<ModifiedGroup> = Vec::new();

    loop
    {
        match build_modified_group(ts)
        {
            New(mg) => modgroups.push(mg),
            Finished => break,
            Error => return Error,
        }
    }

    return New(Case(modgroups));
}

fn build_modified_group(ts: &mut TokenStream) -> BuildResult<ModifiedGroup>
{
    use grammar::BuildResult::*;

    let modifier: Option<Modifier>;
    match ts.pop_front()
    {
        Some(t) =>
        {
            if t.string == "*".to_string()
            {
                trace!("[build_modified_group] found zero or more");
                modifier = Some(Modifier::ZeroOrMore);
            }

            else if t.string == "+".to_string()
            {
                trace!("[build_modified_group] found one or more");
                modifier = Some(Modifier::OneOrMore);
            }

            else if t.string == "?".to_string()
            {
                trace!("[build_modified_group] found zero or one");
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
            error!("[build_modified_group] Expected to find a token when building modified group.");
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
    use grammar::BuildResult::*;

    let t = match ts.pop_front()
    {
        Some(t) => t,
        None =>
        {
            // TODO: Emit error message
            error!("[build_group] Expected to find a token when building a group, found nothing.");
            return Error;
        }
    };

    // Repeated case: braces followed by at least one modified group
    if t.string == "(".to_string()
    {       
        trace!("[build_group] found compound group opening brace");
        let mut mgs: Vec<ModifiedGroup> = Vec::new();
        loop
        {
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
                                error!("Expected closing brace");
                                return Error;
                            }
                        },
                        None =>
                        {
                            // TODO: Emit error message
                            error!("Expected closing brace");
                            return Error;
                        }
                    }

                    return New(Group::Many(mgs));
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
                return New(Group::Single(unit));
            },
            Finished => return Finished,
            Error => return Error,
        }
    }
}

fn build_unit(ts: &mut TokenStream) -> BuildResult<Unit>
{
    use grammar::BuildResult::*;

    match ts.pop_front()
    {
        Some(t) =>
        {
            if t.token_type == TokenType::StringLiteral
            {
                trace!("[build_unit] found terminal: {}", &t.string);
                return New(Unit::Terminal(t.string));
            }

            else if 
                t.token_type == TokenType::Identifier ||
                t.token_type == TokenType::Keyword
            {
                if t.string == "identifier"
                {
                    trace!("[build_unit] found identifier keyword");
                    return New(Unit::Identifier);
                }

                else
                {
                    trace!("[build_unit] found non-terminal: {}", &t.string);
                    return New(Unit::NonTerminal(t.string));
                }
            }

            else if t.token_type == TokenType::Character
            {
                if t.string == ";".to_string()
                {
                    trace!("[build_unit] found semicolon");
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == "(".to_string()
                {
                    trace!("[build_unit] found opening brace");
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == ")".to_string()
                {
                    trace!("[build_unit] found closing brace");
                    ts.push_front(t);
                    return Finished;
                }

                if t.string == "|".to_string()
                {
                    trace!("[build_unit] found pipe (or)");
                    return New(Unit::Or);
                }

                else
                {
                    // TODO:
                    error!("Expected one of ; ( ) | but found other");
                    return Error;
                }
            }

            else
            {
                // TODO: Emit error message
                error!("[build_unit] token had an invalid type, or was an invalid character.");
                return Error;
            }
        },
        None =>
        {
            // TODO: Emit error message
            error!("[build_unit] Expected a token when building unit but found nothing.");
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

    extern crate simple_logger;

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
    fn basic_check_a()
    {
        // One rule, almost simplest possible
        basic_checks_meta(vec!["basic: identifier;".to_string()]);
    }

    #[test]
    fn basic_check_b()
    {
        basic_checks_meta(vec!["basic: identifier; 'lit' identifier;".to_string()]);
    }

    #[test]
    fn basic_check_real()
    {
        // A more real-world test
        basic_checks_meta(vec!["vardecl: 'var' identifier ?(':' identifier);".to_string()]);
    }

    #[test]
    fn basic_check_many()
    {
        basic_checks_meta(vec!
            [
            "vardecl: 'var' identifier ?(':' identifier);".to_string(),
            "yield: 'yield' expression ';';".to_string()
            ]
        );
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

    fn big_tests_init
    (
        grammarspec      : Vec<String>, // The grammar to parse
        expect_num_rules : usize,       // The number of rules in the grammar
        keys             : Vec<String>, // The keys that you should expect to find in the grammar
    )
    -> Grammar
    {
        // meta function for the big tests.
        // performs some basic sanity checks like "a grammar with three rules should have three
        // rules in it.

        // Initialise logger
        simple_logger::init().unwrap();
        println!(""); // So that logging output begins on a new line

        use grammar::BuildResult::*;

        let vts = basic_checks_meta(grammarspec); // "vts" -> "Vector of 'T'oken 'S'treams"

        // Assert that there are the expected number of rules in the Vec<TokenStream>
        assert_eq!(expect_num_rules, vts.len());

        // build the grammar
        let Grammar(g) = match build_grammar(vts)
        {
            New(g) => g,
            Finished => panic!("[big_tests_meta] build_grammar should not return finished"),
            Error => panic!("vardecl grammar check FAILED"),
        };

        // Assert that there are the expected number of rules in the resulting Grammar
        assert_eq!(expect_num_rules, g.len());

        // Assert that the grammar contains the expected keys
        for key in keys
        {
            println!("checking '{}' is in grammar", &key);
            assert!(g.contains_key(&key));
        }

        return Grammar(g);
    }

    #[test]
    fn vardecl()
    {
        // This test tests that vardecl is correctly parsed.

        use grammar::Group::*;
        use grammar::Unit::*;
        use grammar::Modifier::*;

        // Do basic checks
        let Grammar(grammar) = big_tests_init(
            vec!["vardecl: 'var' identifier ?(':' identifier);".to_string()],
            1,
            vec!["vardecl".to_string()],
        );

        // Deconstruct the rule
        let vec_cases = match grammar.get("vardecl")
        {
            Some(&Rule(ref v)) => v,
            None => panic!("what the hell"),
        };

        // vardecl only has one case
        assert_eq!(1, vec_cases.len());

        // Deconstruct the case
        let &Case(ref vec_mg) = vec_cases.into_iter().next().unwrap();

        // There should be three modified groups at the top level
        assert_eq!(3, vec_mg.len());

        // FIRST MODIFIED GROUP
        let ModifiedGroup(ref modifier, ref group) = vec_mg[0];
        match modifier
        {
            &Some(_) => panic!("first modified group should not have a modifier"),
            &None => (),
        }

        match group
        {
            &Single(ref u) =>
            {
                match u
                {
                    &Terminal(ref s) => assert_eq!("var", s),
                    _ => panic!("first modified group should have been interpreted as a terminal"),
                }
            },
            &Many(_) => panic!("first modified group should be a Single, not Many"),
        }

        // SECOND MODIFIED GROUP
        let ModifiedGroup(ref modifier, ref group) = vec_mg[1];
        match modifier
        {
            &Some(_) => panic!("second modified group should not have a modifier"),
            &None => (),
        }

        match group
        {
            &Single(ref u) =>
            {
                match u
                {
                    &Identifier => (), // Yay!
                    _ => panic!("second modified group should have been interpreted as identifier"),
                }
            },
            &Many(_) => panic!("second modified group should be a Single, not Many"),
        }

        // THIRD MODIFIED GROUP
        let ModifiedGroup(ref modifier, ref group) = vec_mg[2];
        match modifier
        {
            &Some(ref a) =>
            {
                match a
                {
                    &ZeroOrOne => (),
                    _ => panic!("third modified group should have 'ZeroOrOne' modifier"),
                }
            },
            &None => panic!("third modified group should have 'ZeroOrOne' modifier"),
        }

        match group
        {
            &Single(_) => panic!("third modified group should be Many, not Single"),
            &Many(ref v) =>
            {
                // modified group should contain 2 sub-groups
                assert_eq!(2, v.len());

                // first sub-group should be the ':' literal
                let ModifiedGroup(ref modifier, ref group) = v[0];
                match modifier
                {
                    &Some(_) => panic!("third group, first subgroup, should have no modifier"),
                    &None => (),
                }

                match group
                {
                    &Single(ref u) =>
                    {
                        match u
                        {
                            &Terminal(ref s) =>
                            {
                                if *s != ":".to_string()
                                {
                                    panic!("expected colon character");
                                }
                            },
                            _ => panic!("expected terminal"),
                        }
                    },
                    &Many(_) => panic!("third group, first subgroup, should be a Single unit"),
                }

                // second subgroup
                let ModifiedGroup(ref modifier, ref group) = v[1];
                match modifier
                {
                    &Some(_) => panic!("third group, second sub, should not have a modifier"),
                    &None => (),
                }

                match group
                {
                    &Single(ref u) =>
                    {
                        match u
                        {
                            &Identifier => (),
                            _ => panic!("third group, second sub should be identifier"),
                        }
                    }
                    &Many(_) => panic!("third group, second sub, should be Single, not Many"),
                }
            }
        }

        // Success!!!!
    }

    #[test]
    fn expression()
    {

    }
}
