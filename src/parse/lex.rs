use itertools::put_back;
use itertools::Itertools;
//use snafu::Snafu;

/*#[derive(Debug, Snafu)]
enum Error {
    #[snafu(display("Expected {:?}. Found {}.", expected, found))]
    UnmatchedToken { expected: Token, found: char },
}

type Result<T, E = Error> = std::result::Result<T, E>;*/

#[derive(Debug, PartialEq)]
pub enum Token {
    OpenBrace,
    CloseBrace,
    OpenParenthesis,
    CloseParenthesis,
    Semicolon,
    Keyword(Keyword),
    Identifier(String),
    Literal(Literal),
    Negative,
    Complement,
    Negation,
    Unidentified,
    Addition,
    Multiplication,
    Division,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(u32),
    // For error purposes
    None,
}

pub fn lex(s: &str) -> Vec<Token> {
    let mut tok = Vec::new();
    let mut it = put_back(
        s.chars()
            .skip_while(|&c| c == ' ' || c == '\t' || c == '\n'),
    );
    while let Some(c) = it.next() {
        //        print!("{} =>", c);
        tok.push(match c {
            '{' => Token::OpenBrace,
            '}' => Token::CloseBrace,
            '(' => Token::OpenParenthesis,
            ')' => Token::CloseParenthesis,
            ';' => Token::Semicolon,
            '-' => Token::Negative,
            '~' => Token::Complement,
            '+' => Token::Addition,
            '*' => Token::Multiplication,
            '/' => Token::Division,
            '%' => Token::Modulo,
            '^' => Token::BitXor,
            '&' => match it.next().expect("Unexpected EOF") {
                '&' => Token::And,
                t => {
                    it.put_back(t);
                    Token::BitAnd
                }
            },
            '|' => match it.next().expect("Unexpected EOF") {
                '|' => Token::Or,
                t => {
                    it.put_back(t);
                    Token::BitOr
                }
            },
            '=' => match it.next().expect("Unexpected EOF") {
                '=' => Token::Equal,
                t => {
                    it.put_back(t);
                    Token::Unidentified
                }
            },
            '!' => match it.next().expect("Unexpected EOF") {
                '=' => Token::NotEqual,
                t => {
                    it.put_back(t);
                    Token::Negation
                }
            },
            '<' => match it.next().expect("Unexpected EOF") {
                '=' => Token::LessThanEqual,
                '<' => Token::ShiftLeft,
                t => {
                    it.put_back(t);
                    Token::LessThan
                }
            },
            '>' => match it.next().expect("Unexpected EOF") {
                '=' => Token::GreaterThanEqual,
                '>' => Token::ShiftRight,
                t => {
                    it.put_back(t);
                    Token::GreaterThan
                }
            },
            c @ 'A'..='Z' | c @ 'a'..='z' => {
                it.put_back(c);
                match it
                    .by_ref()
                    .peeking_take_while(|&c| {
                        (c >= 'A' && c <= 'Z')
                            || (c >= 'a' && c <= 'z')
                            || (c >= '0' && c <= '9')
                            || c == '-'
                            || c == '_'
                    })
                    .collect::<String>()
                    .as_ref()
                {
                    "int" => Token::Keyword(Keyword::Int),
                    "return" => Token::Keyword(Keyword::Return),
                    s => Token::Identifier(String::from(s)),
                }
            }
            c @ '0'..='9' => {
                it.put_back(c);
                Token::Literal(Literal::Int(
                    it.by_ref()
                        .peeking_take_while(|&c| c >= '0' && c <= '9')
                        .collect::<String>()
                        .parse()
                        .expect("Failed to parse integer literal"),
                ))
            }
            _ => Token::Unidentified,
        });
        it.by_ref()
            .peeking_take_while(|&c| c == '\t' || c == ' ' || c == '\n')
            .last();
    }
    tok
}
