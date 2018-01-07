use std::collections::HashMap;

use token::{TokenStream,TokenType};
use lexer::lex;

// Grammar:
// Represents a grammar.
//
// String: The name of the rule
// Rule: The rule.
#[derive(Clone)]
struct Grammar(HashMap<String, Rule>);

#[derive(Clone)]
struct Rule(Vec<Case>);

#[derive(Clone)]
struct Case(Vec<ModifiedGroup>);

#[derive(Clone, PartialEq, Debug)]
enum Modifier
{
    ZeroOrOne,
    ZeroOrMore,
    OneOrMore,
}
#[derive(Clone)]
struct ModifiedGroup(Option<Modifier>, Group);

#[derive(Clone)]
enum Group
{
    Single(Unit),
    Many(Vec<ModifiedGroup>),
}

#[derive(Debug, Clone)]
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

// The result of calling the 6 functions below.
// If a function encounters an error, then it emits an error message and tells
// the next function in the call stack that it has done so, by returning the Error
// value
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
    if t.string == "(".to_string() && t.token_type == TokenType::Character
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

    // Compares two grammars
    // Used for testing
    fn cmp_grammar(g1: Grammar, g2: Grammar)
    {
        info!("Comparing two grammars");

        // Extract hashmaps
        let Grammar(mut g1) = g1;
        let Grammar(mut g2) = g2;

        // They should have the same number of keys
        assert_eq!(g1.keys().count(), g2.keys().count(),
            "the grammars have a different number of keys");

        // Now we check each key in sequence
        for key in g1.keys()
        {
            assert!(g2.contains_key(key), "key '{}' is contained in g1 but not g2");
        }

        for key in g2.keys()
        {
            assert!(g1.contains_key(key), "key '{}' is contained in g2 but not g1");
        }

        // Now check each rule in sequence
        for key in g1.clone().keys()
        {
            let r1 = match g1.remove(key)
            {
                Some(r1) => r1,
                None => panic!("SHOULD NOT HAPPEN"),
            };

            let r2 = match g2.remove(key)
            {
                Some(r2) => r2,
                None => panic!("SHOULD NOT HAPPEN"),
            };

            assert!(cmp_rule(r1, r2), "rules '{}' do not match", key);
        }

        info!("Success!");
    }

    // interface:
    // each function returns true false
    // if the function is to return false then it should emit the error message first
    // only the top level function can panic

    // emits an error and returns false
    macro_rules! err_false
    {
        ( $error_s:expr, $( $args:expr ),* ) =>
        {{
            error!($error_s, $($args),*);
            return false;
        }};

        ( $error_s:expr ) =>
        {{
            error!($error_s);
            return false;
        }};
    }

    // emits an error if the expression evaluates to false
    macro_rules! error_if
    {
        ( $a:expr, $error_s:expr, $( $args:expr ),* ) =>
            { if $a { err_false!($error_s, $($args),*); } };

        ( $a:expr, $error_s:expr ) =>
            { if $a { err_false!($error_s); } };
    }

    // emits an error if the two args are not equal
    macro_rules! error_if_neq
    {
        ( $a:expr, $b:expr, $error_s:expr, $( $args:expr ),* ) =>
            { error_if!($a != $b, $error_s, $($args),*); };

        ( $a:expr, $b:expr, $error_s:expr ) =>
            { error_if!($a != $b, $error_s); }
    }

    fn cmp_rule(r1: Rule, r2: Rule) -> bool
    {
        // extract vector of cases
        let Rule(r1) = r1;
        let Rule(r2) = r2;

        // assert same number of cases
        error_if_neq!(r1.len(), r2.len(), "rules have different number of cases: r1: {}\tr2: {}", r1.len(), r2.len());

        for ((i, case_a), case_b) in r1.into_iter().enumerate().zip(r2.into_iter())
        {
            error_if!(cmp_case(case_a, case_b), "FAILURE! case {} failed", i);
        }

        return true;
    }

    fn cmp_case(c1: Case, c2: Case) -> bool
    {
        // extract vector of modified groups
        let Case(c1) = c1;
        let Case(c2) = c2;

        error_if_neq!(c1.len(), c2.len(), "cases have different number of mgroups: c1: {}\tc2: {}", c1.len(), c2.len());

        for ((i, mga), mgb) in c1.into_iter().enumerate().zip(c2.into_iter())
        {
            error_if!(cmp_mgroup(mga, mgb), "FAILURE! mgroup {} failed", i);
        }

        return true;
    }

    fn cmp_mgroup(m1: ModifiedGroup, m2: ModifiedGroup) -> bool
    {
        // extract components
        let ModifiedGroup(m1, g1) = m1;
        let ModifiedGroup(m2, g2) = m2;

        // Check modifiers
        match (m1, m2)
        {
            (Some(a), Some(b)) => {error_if_neq!(a, b, "modified groups had different modifers: a: {:?}\tb: {:?}", a, b);},
            (None, None)       => (),
            (Some(a), None)    => {err_false!("m1 had modifier: {:?}, m2 had no modifier", a)},
            (None, Some(b))    => {err_false!("m2 had modifier: {:?}, m1 had no modifier", b)},
        }

        error_if!(cmp_group(g1, g2), "modified groups had different groups");

        return true;
    }

    fn cmp_group(g1: Group, g2: Group) -> bool
    {
        use grammar::Group::*;

        match (g1, g2)
        {
            (Single(u1) , Single(u2)) =>
            {
                error_if!(
                    cmp_unit(u1, u2),
                    "Both groups were Group::Single, but the units were different"
                );
            },
            (Many(v1), Many(v2)) =>
            {
                error_if_neq!(
                    v1.len(), v2.len(),
                    "found two groups of many, but they had a different number of elements: v1: {}\tv2: {}", v1.len(), v2.len()
                );

                for ((i, mg1), mg2) in v1
                    .into_iter()
                    .enumerate()
                    .zip(v2.into_iter())
                {
                    error_if!(
                        cmp_mgroup(mg1, mg2),
                        "found group of many, but sub-group {} differed",
                        i
                    );
                }
            },
            (Single(_), Many(_)) => err_false!("g1 was Single, g2 was Many"),
            (Many(_), Single(_)) => err_false!("g1 was Many, g1 was Single"),
        }

        return true;
    }

    fn cmp_unit(u1: Unit, u2: Unit) -> bool
    {
        use grammar::Unit::*;

        match (&u1, &u2)
        {
            ( &Terminal(ref s1)    , &Terminal(ref s2)    ) => {error_if_neq!(s1, s2, "both units were terminals but they were different: u1: {} u2: {}", s1, s2)},
            ( &NonTerminal(ref s1) , &NonTerminal(ref s2) ) => {error_if_neq!(s1, s2, "both units were non-terminals but they were different: u1: {} u2: {}", s1, s2)},
            ( &Identifier          , &Identifier          ) => (),
            ( &Or                  , &Or                  ) => (),

            // cbf writing out all 12 failure cases
            _ => {err_false!("units were different variants: u1: {:?}\tu2: {:?}", &u1, &u2)},
        }

        return true;
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
        match simple_logger::init()
        {
            Ok(_) => (),
            Err(_) => (),
        }

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
        // tests a reduced form of grammar for parsing expressions

        let Grammar(grammar) = big_tests_init(
            vec!
            [
                "expression:
                    term *(('+'|'-') term);".to_string(),
                "term:
                    factor *(('*'|'/') factor);".to_string(),
                "factor:
                    identifier;
                    '(' expression ')';
                    '-' factor;".to_string(),
            ],
            3,
            vec!["expression".to_string(), "term".to_string(), "factor".to_string()],
        );

        let expr_manual =
        Rule(vec!
        {
            Case(vec!
            [
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::NonTerminal( "term".to_string() ))
                ),
                ModifiedGroup
                (
                    Some(Modifier::ZeroOrMore),
                    Group::Many(vec!
                    [
                        ModifiedGroup
                        (
                            None,
                            Group::Many(vec!
                            [
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Terminal( "+".to_string() ))
                                ),
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Or),
                                ),
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Terminal( "-".to_string() ))
                                )
                            ])
                        ),
                        ModifiedGroup
                        (
                            None,
                            Group::Single(Unit::NonTerminal( "term".to_string() ))
                        )
                    ])
                )
            ])
        });

        let term_manual =
        Rule(vec!
        {            
            Case(vec!
            [
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::NonTerminal( "factor".to_string() ))
                ),
                ModifiedGroup
                (
                    Some(Modifier::ZeroOrMore),
                    Group::Many(vec!
                    [
                        ModifiedGroup
                        (
                            None,
                            Group::Many(vec!
                            [
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Terminal( "*".to_string() ))
                                ),
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Or),
                                ),
                                ModifiedGroup
                                (
                                    None,
                                    Group::Single(Unit::Terminal( "/".to_string() ))
                                )
                            ])
                        ),
                        ModifiedGroup
                        (
                            None,
                            Group::Single(Unit::NonTerminal( "factor".to_string() ))
                        )
                    ])
                )
            ])
        });

        let factor_manual =
        Rule(vec!
        {            
            Case(vec!
            [
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::Identifier)
                )
            ]),
            Case(vec!
            [
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::Terminal("(".to_string() )),
                ),
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::NonTerminal("expression".to_string() ))
                ),
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::Terminal(")".to_string() )),
                )
            ]),
            Case(vec!
            [
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::Terminal("-".to_string())),
                ),
                ModifiedGroup
                (
                    None,
                    Group::Single(Unit::NonTerminal("factor".to_string() ))
                ),
            ]),
        });

        let mut grammar_manual: HashMap<String, Rule> = HashMap::new();

        grammar_manual.insert("expression".to_string(), expr_manual);
        grammar_manual.insert("term".to_string(), term_manual);
        grammar_manual.insert("factor".to_string(), factor_manual);

        cmp_grammar(
            Grammar(grammar_manual),
            Grammar(grammar)
        );
    }
}
