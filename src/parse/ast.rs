use snafu::Snafu;

use super::lex::{Keyword, Literal, Token};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display("Expected {:?}. Found {:?} instead.", expected, found))]
    UnexpectedToken {
        wanted: ,
        expected: Token,
        found: Token,
    },

    InvalidSyntax,
}

type Result<T, E = Error> = std::result::Result<T, E>;

pub trait ASTNode: Sized + std::fmt::Debug {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Self>;
    fn emit(self) -> String;
}

#[derive(Debug)]
pub struct Program(Function);

impl ASTNode for Program {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Program> {
        Ok(Program(Function::parse(t)?))
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
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Function> {
        consume_token(t, Token::Keyword(Keyword::Int));

        if let Token::Identifier(name) = t.next().unwrap() {
            consume_token(t, Token::OpenParenthesis);
            consume_token(t, Token::CloseParenthesis);
            consume_token(t, Token::OpenBrace);
            let ret = Function {
                name,
                body: Statement::parse(t)?,
            };
            consume_token(t, Token::CloseBrace);
            return Ok(ret);
        }

        Err(Error::InvalidSyntax)
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
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Statement> {
        match t.next().unwrap() {
            Token::Keyword(Keyword::Return) => {
                consume_token(t, Token::Semicolon);
                Ok(Statement::Return(Expression::parse(t)?))
            }
            tok => Err(Error::UnexpectedToken {
                expected: Token::Keyword(Keyword::Return),
                found: tok,
            }),
        }
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
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
}

impl ASTNode for Expression {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Expression> {
        match t.next().unwrap() {
            // FIXME: awful hack
            tok @ Token::Negative | tok @ Token::Negation | tok @ Token::Complement => {
                let op = UnaryOperator::parse(vec![tok].into_iter().by_ref())?;
                let e = Expression::parse(t)?;
                Ok(Expression::Unary(op, Box::new(e)))
            }
            tok @ Token::Literal(_) => Ok(Expression::Constant(Constant::parse(
                vec![tok].into_iter().by_ref(),
            )?)),
            tok => panic!("Expected an expression"),
        }
    }
    fn emit(self) -> String {
        match self {
            Expression::Constant(c) => format!("mov rax, {}\n", c.emit()),
            Expression::Unary(op, e) => format!(
                "\
                 {} \
                 {} \
                 ",
                e.emit(),
                op.emit()
            ),
            Expression::Binary(_, _, _) => unimplemented!(),
        }
    }
}

#[derive(Debug)]
enum Constant {
    Int(u32),
}

impl ASTNode for Constant {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<Constant> {
        match t.next() {
            Some(Token::Literal(Literal::Int(i))) => Ok(Constant::Int(i)),
            tok => panic!(format!("Expected a constant, got {:?}", tok)),
        }
    }

    fn emit(self) -> String {
        match self {
            Constant::Int(i) => i.to_string(),
        }
    }
}

#[derive(Debug)]
enum UnaryOperator {
    Negative,
    Complement,
    Negation,
}

impl ASTNode for UnaryOperator {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<UnaryOperator> {
        match t.next().unwrap() {
            Token::Complement => Ok(UnaryOperator::Complement),
            Token::Negative => Ok(UnaryOperator::Negative),
            Token::Negation => Ok(UnaryOperator::Negation),
            t => panic!(format!("Expected a unary operator, got {:?}", t)),
        }
    }

    fn emit(self) -> String {
        match self {
            UnaryOperator::Negative => String::from("neg rax\n"),
            UnaryOperator::Complement => String::from("not rax\n"),
            UnaryOperator::Negation => String::from(
                "\
                 cmp rax, 0 \n\
                 mov rax, 0 \n\
                 sete al \n\
                 ",
            ),
        }
    }
}

#[derive(Debug)]
enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl ASTNode for BinaryOperator {
    fn parse<I: Iterator<Item = Token>>(t: &mut I) -> Result<BinaryOperator> {
        match t.next().unwrap() {
            Token::Addition => Ok(BinaryOperator::Addition),
            Token::Negative => Ok(BinaryOperator::Subtraction),
            Token::Multiplication => Ok(BinaryOperator::Multiplication),
            Token::Division => Ok(BinaryOperator::Division),
            tok => panic!(format!("Expected binary operator, got {:?}", tok)),
        }
    }

    fn emit(self) -> String {
        unimplemented!()
    }
}

fn consume_token<I: Iterator<Item = Token>>(t: &mut I, tok: Token) {
    let next = t.next().unwrap();
    if next != tok {
        panic!(format!("Unexpected {:?}. Was expecting {:?}.", next, tok));
    }
}

pub fn parse(t: Vec<Token>) -> Result<Program> {
    Program::parse(&mut t.into_iter())
}
