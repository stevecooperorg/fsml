use crate::Result;
use pom::char_class::{alpha, alphanum, multispace};
use pom::parser::*;
use std::str::FromStr;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct StateMachine {
    pub name: String,
    pub states: Vec<State>,
    pub accept_states: Vec<AcceptState>
}

#[derive(Debug, Clone, PartialEq)]
pub struct AcceptState(StateId, StateId);

#[derive(Debug, Clone, PartialEq)]
pub struct StateId(String);

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    pub id: StateId,
    pub is_starting_state: bool,
    pub description: Option<String>
}

impl AcceptState {
    pub fn source(&self) -> &StateId {
        &self.0
    }

    pub fn target(&self) -> &StateId {
        &self.1
    }
}


impl Display for StateId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

/// space, tab, etc
fn ws<'a>() -> Parser<'a, u8, ()> {
    is_a(multispace).discard()
}

/// whitespace and comments
fn space<'a>() -> Parser<'a, u8, ()> {
    (ws() | comment()).repeat(0..).discard()
}

fn semi<'a>() -> Parser<'a, u8, ()> {
    keyword(b";").name("semi")
}

fn to_eol<'a>() -> Parser<'a, u8, String> {
    fn anything_else(term: u8) -> bool {
        !is_cr(term) && !is_lf(term)
    }

    is_a(anything_else)
        .repeat(0..)
        .map(|u8s| String::from_utf8(u8s).expect("can only parse utf"))
}

fn line_comment<'a>() -> Parser<'a, u8, ()> {
    (seq(b"//") * to_eol() - eol())
        .discard()
        .name("line comment")
}

fn eol<'a>() -> Parser<'a, u8, ()> {
    ((is_a(is_cr) * is_a(is_lf)) | is_a(is_lf) | is_a(is_cr)).discard()
}

fn keyword<'a>(keyword: &'static [u8]) -> Parser<'a, u8, ()> {
    literal(keyword).discard().name("keyword")
}

fn literal<'a>(literal: &'static [u8]) -> Parser<'a, u8, String> {
    spaced(seq(literal))
        .map(|u8s| String::from_utf8(u8s.to_vec()).expect("can only parse utf"))
        .name("literal")
}

fn star_comment<'a>() -> Parser<'a, u8, ()> {
    fn anything_else(term: u8) -> bool {
        term != b'*'
    }

    (seq(b"/*") * is_a(anything_else).repeat(0..) - seq(b"*/")).discard()
}

fn comment<'a>() -> Parser<'a, u8, ()> {
    line_comment() | star_comment()
}

/// a parser wrapped in whitespace
fn spaced<'a, T>(parser: Parser<'a, u8, T>) -> Parser<'a, u8, T>
    where
        T: 'a,
{
    space() * parser - space()
}

fn is_cr(term: u8) -> bool {
    term == b'\r'
}

fn is_lf(term: u8) -> bool {
    term == b'\n'
}

fn is_underscore(term: u8) -> bool {
    term == b'_'
}

fn state_id<'a>() -> Parser<'a, u8, StateId> {
    (identifier())
        .map(|(ident)| StateId(ident))
}

fn identifier<'a>() -> Parser<'a, u8, String> {
    let it = ((is_a(alpha) | is_a(is_underscore))
        + (is_a(alphanum) | is_a(is_underscore)).repeat(0..))
        .map(|(first, rest)| format!("{}{}", first as char, String::from_utf8(rest).unwrap()));

    spaced(it).name("name")
}

fn string<'a>() -> Parser<'a, u8, String> {
    let special_char = sym(b'\\')
        | sym(b'/')
        | sym(b'"')
        | sym(b'b').map(|_| b'\x08')
        | sym(b'f').map(|_| b'\x0C')
        | sym(b'n').map(|_| b'\n')
        | sym(b'r').map(|_| b'\r')
        | sym(b't').map(|_| b'\t');
    let escape_sequence = sym(b'\\') * special_char;
    let string = sym(b'"') * (none_of(b"\\\"") | escape_sequence).repeat(0..) - sym(b'"');
    string.convert(String::from_utf8)
}

fn state<'a>() -> Parser<'a, u8, State> {
    let raw = keyword(b"state") * identifier() + string().opt()
        - semi();

    raw.map(move |(identifier, description)| State {
        id: StateId(identifier),
        is_starting_state: false,
        description
    })
}

fn state_list<'a>() -> Parser<'a, u8, Vec<State>> {
    fn tag_starting_state(idx: usize, state: State) -> State {
        State {
            is_starting_state: idx == 0,
            ..state
        }
    };
    state().repeat(0..).map(|states| states.into_iter().enumerate().map(|(idx, state)| tag_starting_state(idx, state)).collect())
}

fn accept_states_list<'a>() -> Parser<'a, u8, Vec<AcceptState>> {
    accept_states_chain()
        .repeat(0..)
        .map(|chains| chains.into_iter().flatten().collect())
}

fn accept_states_chain<'a>() -> Parser<'a, u8, Vec<AcceptState>> {
    let raw = spaced(list(spaced(state_id()), keyword(b"->"))) - semi();

    raw.map(move |(state_ids)| {
        if state_ids.len() < 2 {
            return vec![];
        }

        let mut result = vec![];
        for i in 0..state_ids.len() -1 {
            let left = state_ids[i].clone();
            let right = state_ids[i+1].clone();
            let accept = AcceptState(left, right);

            result.push(accept);
        }
        return result;
    })
}

pub fn state_machine<'a>() -> Parser<'a, u8, StateMachine> {

    let header = keyword(b"machine") * identifier() - semi();
    let raw = header
        + state_list()
        + accept_states_list();

    raw.map(move |((name, states), accept_states)| StateMachine {
        name,
        states,
        accept_states
    })
}

#[cfg(test)]
mod test {
    use super::*;
    use std::cmp::min;
    use std::path::{Path, PathBuf};
    use std::{fs, io};

    macro_rules! assert_consumes_all {
        ( $ parser: expr, $input: expr ) => {
            let terminating_parser = $parser - space() - end();
            let res = terminating_parser.parse($input);
            if let Err(_) = res {
                panic!("parser failed to match and consume everything")
            }
        };
        ( $ parser: expr, $input: expr, $expected: expr) => {
            let terminating_parser = $parser - space() - end();
            let res = terminating_parser.parse($input);
            match res {
                Ok(answer) => {
                    // it parsed, but was it right?
                    assert_eq!(answer, $expected)
                }
                Err(_) => {
                    //
                    panic!("parser failed to match and consume everything")
                }
            }
        };
    }

    #[test]
    fn parse_keywords() -> Result<()> {
        assert_consumes_all![eol(), b"\r"];
        assert_consumes_all![eol(), b"\r\n"];
        assert_consumes_all![eol(), b"\n"];

        assert_consumes_all![space(), b""];
        assert_consumes_all![space(), b"  "];
        assert_consumes_all![space(), b"  \t \n \r "];

        assert_consumes_all![line_comment(), b"//\r"];
        assert_consumes_all![line_comment(), b"//\n"];
        assert_consumes_all![line_comment(), b"//\r\n"];
        assert_consumes_all![line_comment(), b"// xyz \r\n"];

        assert_consumes_all![star_comment(), b"/*  thing */"];
        assert_consumes_all![star_comment(), b"/*  thing \r\n thing */"];

        assert_consumes_all!(
            identifier(),
            b"foo"
        );

        assert_consumes_all!(
            state_id(),
            b"foo"
        );

        assert_consumes_all!(
            accept_states_chain(),
            b"foo-> bar ->   baz;",
            vec![
                AcceptState(StateId("foo".into()), StateId("bar".into())),
                AcceptState(StateId("bar".into()), StateId("baz".into())),
            ]
        );

        assert_consumes_all!(
            accept_states_list(),
            b"foo-> bar ->   baz; baz -> quux;",
            vec![
                AcceptState(StateId("foo".into()), StateId("bar".into())),
                AcceptState(StateId("bar".into()), StateId("baz".into())),
                AcceptState(StateId("baz".into()), StateId("quux".into())),
            ]
        );

        Ok(())
    }

    #[test]
    fn parse_state_machines() -> Result<()> {
        let emptymachine = StateMachine {
            name: "foo".into(),
            states: Default::default(),
            accept_states: vec![]
        };

        assert_consumes_all!(
            state_machine(),
            b"machine foo;",
            emptymachine
        );

        assert_consumes_all!(
            state_machine(),
            b"
machine foo;

state bar \"it's a bar thing\";
state baz;

bar -> baz;
",
            StateMachine {
                name: "foo".into(),
                states: vec![
                    State {
                        id: StateId("bar".into()),
                        is_starting_state: true,
                        description: Some("it's a bar thing".into())
                    },
                    State {
                        id: StateId("baz".into()),
                        is_starting_state: false,
                        description: None
                    },
                ],
                accept_states: vec![
                    AcceptState(StateId("bar".into()), StateId("baz".into()))
                ]
        }
        );

        Ok(())
    }

    fn count_lines(byte_slice: &[u8]) -> usize {
        let line_parser = (to_eol() - eol()).repeat(0..);
        let parse_result = line_parser.parse(byte_slice).unwrap();
        parse_result.len()
    }

    #[test]
    fn line_counter_works() {
        let file_path_str = "assets/fsml/simple-state-machine.fsml";
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let actual = count_lines(&byte_vec);
        assert_eq!(12, actual);
    }

    #[test]
    fn parse_state_machine_file() {
        let file_path_str = "assets/fsml/simple-state-machine.fsml";
        assert_parse_file(PathBuf::from_str(file_path_str).unwrap().as_path());
    }

    #[test]
    fn parse_all_files() -> Result<()> {
        let mut entries = fs::read_dir("assets/fsml")?
            .map(|res| res.map(|e| e.path()))
            //.filter(|f| )
            .collect::<std::result::Result<Vec<_>, io::Error>>()?;

        entries.sort();

        for file_path_str in entries {
            println!("");
            println!("{}", file_path_str.to_str().unwrap());
            println!("");
            assert_parse_file(file_path_str.as_path());
        }

        Ok(())
    }

    fn assert_parse_file(file_path_str: &Path) {
        let byte_vec: Vec<u8> = std::fs::read(file_path_str).unwrap();
        let file_content =
            String::from_utf8(byte_vec.clone()).expect("should be able to read the file");
        let byte_slice: &[u8] = &byte_vec;
        let parser = state_machine();
        let parse_result = match parser.parse(byte_slice) {
            Ok(parse_result) => parse_result,
            Err(pom::Error::Mismatch { message, position }) => {
                let start_str = &byte_vec[0..position];
                let line = count_lines(start_str) + 1;
                let end = min(position + 50, file_content.len() - 1);
                let extract = &file_content[position..end];
                let extract = extract
                    .to_string()
                    .replace("\n", "\\n")
                    .replace("\r", "\\r")
                    .replace("\t", "\\t");
                let err_location = format!("{}:{}:{}", file_path_str.to_str().unwrap(), line, 1);
                // thread 'idl_parser::test::parse_full_html5_file' panicked at 'whoops', src/idl_parser.rs:428:9
                let better_message = format!(
                    "thread 'idl_parser::test::parse_full_html5_file' panicked at 'parsing', {}\n\n{}",
                    err_location, extract
                );
                println!("{}", better_message);
                panic!(message)
            }
            Err(e) => panic!("{}", e),
        };
        println!("{:?}", parse_result);
    }
}
