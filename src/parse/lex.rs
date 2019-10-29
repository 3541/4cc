use peeking_take_while::PeekableExt;

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
    Unidentified,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Int,
    Return,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(u32),
}

pub fn lex(s: &str) -> Vec<Token> {
    let mut tok = Vec::new();
    let mut it = s
        .chars()
        .skip_while(|&c| c == ' ' || c == '\t' || c == '\n')
        .peekable();
    while let Some(c) = it.peek() {
        //        print!("{} =>", c);
        tok.push(match c {
            '{' => {
                it.next();
                Token::OpenBrace
            }
            '}' => {
                it.next();
                Token::CloseBrace
            }
            '(' => {
                it.next();
                Token::OpenParenthesis
            }
            ')' => {
                it.next();
                Token::CloseParenthesis
            }
            ';' => {
                it.next();
                Token::Semicolon
            }
            'A'..='Z' | 'a'..='z' => {
                match it
                    .by_ref()
                    .peeking_take_while(|&c| (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
                    .collect::<String>()
                    .as_ref()
                {
                    "int" => Token::Keyword(Keyword::Int),
                    "return" => Token::Keyword(Keyword::Return),
                    s => Token::Identifier(String::from(s)),
                }
            }
            '0'..='9' => Token::Literal(Literal::Int(
                it.by_ref()
                    .peeking_take_while(|&c| c >= '0' && c <= '9')
                    .collect::<String>()
                    .parse()
                    .expect("Failed to parse integer literal"),
            )),
            _ => {
                it.next();
                Token::Unidentified
            }
        });
        it.by_ref()
            .peeking_take_while(|&c| c == '\t' || c == ' ' || c == '\n')
            .last();
        //        println!("Got {:?}", tok.last().unwrap());
    }
    tok
}
