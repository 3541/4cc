use super::lex::{Keyword, Literal, Token};

pub trait ASTNode {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Self;
    fn emit(self) -> String;
}

#[derive(Debug)]
pub struct Program(Function);

impl ASTNode for Program {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Program {
        Program(Function::parse(t))
    }

    fn emit(self) -> String {
        self.0.emit()
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    body: Statement,
}

impl ASTNode for Function {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Function {
        consume_token(t, Token::Keyword(Keyword::Int));

        if let Token::Identifier(name) = t.next().unwrap() {
            consume_token(t, Token::OpenParenthesis);
            consume_token(t, Token::CloseParenthesis);
            consume_token(t, Token::OpenBrace);
            let ret = Function {
                name,
                body: Statement::parse(t),
            };
            consume_token(t, Token::CloseBrace);
            return ret;
        }

        panic!("Invalid function body");
    }

    fn emit(self) -> String {
        format!(
            "\
             global {0}\n\
             {0}:\n\
             {1} \
             ",
            self.name,
            self.body.emit()
        )
    }
}

#[derive(Debug)]
enum Statement {
    Return(Expression),
}

impl ASTNode for Statement {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Statement {
        let ret = match t.next().unwrap() {
            Token::Keyword(Keyword::Return) => Statement::Return(Expression::parse(t)),
            _ => panic!("Expected a statement"),
        };
        consume_token(t, Token::Semicolon);
        ret
    }

    fn emit(self) -> String {
        match self {
            Statement::Return(e) => format!(
                "\
                 {}\n\
                 ret",
                e.emit()
            ),
        }
    }
}

#[derive(Debug)]
enum Expression {
    Constant(Constant),
}

impl ASTNode for Expression {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Expression {
        Expression::Constant(Constant::parse(t))
    }
    fn emit(self) -> String {
        match self {
            Expression::Constant(c) => format!("mov eax, {}", c.emit()),
        }
    }
}

#[derive(Debug)]
enum Constant {
    Int(u32),
}

impl ASTNode for Constant {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Constant {
        match t.next() {
            Some(Token::Literal(Literal::Int(i))) => Constant::Int(i),
            _ => panic!("Expected a constant"),
        }
    }
    fn emit(self) -> String {
        match self {
            Constant::Int(i) => i.to_string(),
        }
    }
}

fn consume_token<I: Iterator<Item = Token>>(t: &mut I, tok: Token) {
    let next = t.by_ref().next().unwrap();
    if next != tok {
        panic!(format!("Unexpected {:?}. Was expecting {:?}.", next, tok));
    }
}

pub fn parse(t: Vec<Token>) -> Program {
    Program::parse(&mut t.into_iter())
}
