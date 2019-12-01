use itertools::{put_back, PutBack};
use snafu::Snafu;

use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::lex::{Keyword, Literal, Token};

#[derive(Debug, Snafu)]
pub enum Error {
    #[snafu(display(
        "Failed trying to parse a {}.\n\tExpected one of {:?}.\n\tFound {:?} instead.\n\tRemaining context: {:?}",
        wanted,
        expected,
        found,
        tokens
    ))]
    UnexpectedToken {
        wanted: &'static str,
        expected: Vec<Token>,
        found: Token,
        tokens: Vec<Token>,
    },

    InvalidSyntax,
    #[snafu(display(
        "Failed trying to parse a {}.\n\tEncountered end of token stream instead.",
        wanted
    ))]
    UnexpectedEnd {
        wanted: &'static str,
    },
}

type Result<T, E = Error> = std::result::Result<T, E>;

static LABEL_COUNT: AtomicUsize = AtomicUsize::new(0);

fn gen_label() -> String {
    format!("__ccgen{}", LABEL_COUNT.fetch_add(1, Ordering::Relaxed))
}

pub trait ASTNode: Sized + std::fmt::Debug {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Self>;
    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String;
}

#[derive(Debug)]
pub struct Program(Function);

impl ASTNode for Program {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Program> {
        Ok(Program(Function::parse(t)?))
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
        self.0.emit(vmap, stack_index)
    }
}

#[derive(Debug)]
struct Function {
    name: String,
    body: Vec<Statement>,
}

impl ASTNode for Function {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Function> {
        consume_token(t, Token::Keyword(Keyword::Int))?;

        if let Token::Identifier(name) = t.next().unwrap() {
            consume_token(t, Token::OpenParenthesis)?;
            consume_token(t, Token::CloseParenthesis)?;
            consume_token(t, Token::OpenBrace)?;
            let mut body = Vec::new();
            loop {
                let tok = t.next().unwrap();
                if tok == Token::CloseBrace {
                    break;
                }

                t.put_back(tok);
                body.push(Statement::parse(t)?);
            }

            return Ok(Function { name, body });
        }

        Err(Error::InvalidSyntax)
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
        format!(
            "\
             global {0}\n\
             {0}:\n\
             push rbp\n\
             mov rbp, rsp\n\
             {1} \n\
             mov esp, ebp\n\
             pop rbp\n\
             ret
             ",
            self.name,
            self.body
                .into_iter()
                .map(|s| s.emit(vmap, stack_index))
                .collect::<String>()
        )
    }
}

#[derive(Debug)]
enum Statement {
    Return(Expression),
    Declaration(String, Option<Expression>),
    Expression(Expression),
}

impl ASTNode for Statement {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Statement> {
        match t.next().ok_or(Error::UnexpectedEnd { wanted: "Keyword" })? {
            Token::Keyword(Keyword::Return) => Ok(Statement::Return(match Expression::parse(t)? {
                //Expression::Null => Expression::Null,
                e => {
                    consume_token(t, Token::Semicolon)?;
                    e
                }
            })),
            Token::Keyword(Keyword::Int) => match t.next().ok_or(Error::UnexpectedEnd {
                wanted: "Statement",
            })? {
                Token::Identifier(s) => match t.next().ok_or(Error::UnexpectedEnd {
                    wanted: "Identifier",
                })? {
                    Token::Semicolon => Ok(Statement::Declaration(s, None)),
                    Token::Assign => {
                        t.put_back(Token::Assign);
                        let ret = Ok(Statement::Declaration(s, Some(Expression::parse(t)?)));
                        consume_token(t, Token::Semicolon)?;
                        ret
                    }
                    tok => Err(Error::UnexpectedToken {
                        wanted: "Statement part",
                        expected: vec![Token::Semicolon, Token::Assign],
                        found: tok,
                        tokens: t.collect(),
                    }),
                },
                tok => Err(Error::UnexpectedToken {
                    wanted: "Identifier",
                    expected: vec![Token::Identifier(String::from("_"))],
                    found: tok,
                    tokens: t.collect(),
                }),
            },
            tok @ Token::Identifier(_) => {
                t.put_back(tok);
                let ret = Ok(Statement::Expression(Expression::parse(t)?));
                consume_token(t, Token::Semicolon)?;
                ret
            }
            tok => Err(Error::UnexpectedToken {
                wanted: "Statement",
                expected: vec![
                    Token::Keyword(Keyword::Return),
                    Token::Keyword(Keyword::Int),
                    Token::Identifier(String::from("")),
                ],
                found: tok,
                tokens: t.collect(),
            }),
        }
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
        match self {
            //TODO: Stack index must be scoped to function...
            Statement::Declaration(s, v) => {
                vmap.insert(
            },
            Statement::Expression(e) => e.emit(vmap, stack_index),
            Statement::Return(e) => format!(
                "\
                 {}\n\
                 ret",
                e.emit(vmap, stack_index)
            ),
        }
    }
}

#[derive(Debug)]
enum Expression {
    Constant(Constant),
    Var(String),
    Unary(UnaryOperator, Box<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Assign(String, Box<Expression>),
    //    Null,
}

#[derive(PartialEq)]
enum Associativity {
    Left,
    Right,
}

impl ASTNode for Expression {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Expression> {
        fn parse_atom<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Expression> {
            match t.next().ok_or(Error::UnexpectedEnd {
                wanted: "Expression",
            })? {
                tok @ Token::Negative | tok @ Token::Negation | tok @ Token::Complement => {
                    t.put_back(tok);
                    let op = UnaryOperator::parse(t)?;
                    let e = parse_atom(t)?;
                    Ok(Expression::Unary(op, Box::new(e)))
                }
                tok @ Token::Literal(_) => {
                    t.put_back(tok);
                    Ok(Expression::Constant(Constant::parse(t)?))
                }
                Token::OpenParenthesis => {
                    let v = parse_expr(t, 1);
                    consume_token(t, Token::CloseParenthesis)?;
                    v
                }
                Token::Identifier(s) => Ok(Expression::Var(s)),
                tok => Err(Error::UnexpectedToken {
                    wanted: "Expression atom",
                    expected: vec![
                        Token::Negative,
                        Token::Negation,
                        Token::Complement,
                        Token::OpenParenthesis,
                        Token::Literal(Literal::None),
                    ],
                    found: tok,
                    tokens: t.collect(),
                }),
            }
        };

        fn parse_expr<I: Iterator<Item = Token>>(
            t: &mut PutBack<I>,
            min_precedence: u8,
        ) -> Result<Expression> {
            let mut lhs = parse_atom(t)?;

            enum Symb {
                Bin(BinaryOperator),
                Assign,
            }

            loop {
                let (op, prec, assoc, pbtok) = match t.next().ok_or(Error::UnexpectedEnd {
                    wanted: "Expression",
                })? {
                    Token::Addition => (
                        Symb::Bin(BinaryOperator::Addition),
                        11,
                        Associativity::Left,
                        Token::Addition,
                    ),
                    Token::Negative => (
                        Symb::Bin(BinaryOperator::Subtraction),
                        11,
                        Associativity::Left,
                        Token::Negative,
                    ),
                    Token::Multiplication => (
                        Symb::Bin(BinaryOperator::Multiplication),
                        12,
                        Associativity::Left,
                        Token::Multiplication,
                    ),
                    Token::Division => (
                        Symb::Bin(BinaryOperator::Division),
                        12,
                        Associativity::Left,
                        Token::Division,
                    ),
                    Token::LessThan => (
                        Symb::Bin(BinaryOperator::LessThan),
                        9,
                        Associativity::Left,
                        Token::LessThan,
                    ),
                    Token::LessThanEqual => (
                        Symb::Bin(BinaryOperator::LessThanEqual),
                        9,
                        Associativity::Left,
                        Token::LessThan,
                    ),
                    Token::GreaterThan => (
                        Symb::Bin(BinaryOperator::GreaterThan),
                        9,
                        Associativity::Left,
                        Token::GreaterThan,
                    ),
                    Token::GreaterThanEqual => (
                        Symb::Bin(BinaryOperator::GreaterThanEqual),
                        9,
                        Associativity::Left,
                        Token::GreaterThanEqual,
                    ),
                    Token::Equal => (
                        Symb::Bin(BinaryOperator::Equal),
                        8,
                        Associativity::Left,
                        Token::Equal,
                    ),
                    Token::NotEqual => (
                        Symb::Bin(BinaryOperator::NotEqual),
                        8,
                        Associativity::Left,
                        Token::NotEqual,
                    ),
                    Token::And => (
                        Symb::Bin(BinaryOperator::And),
                        4,
                        Associativity::Left,
                        Token::And,
                    ),
                    Token::Or => (
                        Symb::Bin(BinaryOperator::Or),
                        3,
                        Associativity::Left,
                        Token::Or,
                    ),
                    Token::Assign => (Symb::Assign, 1, Associativity::Right, Token::Assign),
                    tok => {
                        t.put_back(tok);
                        break;
                    }
                };

                if prec < min_precedence {
                    t.put_back(pbtok);
                    break;
                }

                let next_min = if assoc == Associativity::Left {
                    prec + 1
                } else {
                    prec
                };

                let rhs = Box::new(parse_expr(t, next_min)?);
                //                lhs = Expression::Binary(op, Box::new(lhs), Box::new(parse_expr(t, next_min)?));
                lhs = match op {
                    Symb::Bin(op) => Expression::Binary(op, Box::new(lhs), rhs),
                    Symb::Assign => match lhs {
                        Expression::Var(v) => Expression::Assign(v, rhs),
                        _ => Err(Error::InvalidSyntax)?,
                    },
                };
            }
            Ok(lhs)
        };
        parse_expr(t, 1)
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
        match self {
            Expression::Var(_) => unimplemented!(),
            Expression::Assign(_, _) => unimplemented!(),
            Expression::Constant(c) => format!("mov rax, {}\n", c.emit(vmap, stack_index)),
            Expression::Unary(op, e) => format!(
                "\
                 {} \
                 {} \
                 ",
                e.emit(vmap, stack_index),
                op.emit(vmap, stack_index)
            ),
            Expression::Binary(op, e1, e2)
                if op != BinaryOperator::And && op != BinaryOperator::Or =>
            {
                format!(
                    "\
                     {}\
                     push rax\n\
                     {}\
                     pop rcx\n\
                     {}\
                     ",
                    e1.emit(vmap, stack_index),
                    e2.emit(vmap, stack_index),
                    op.emit(vmap, stack_index)
                )
            }
            Expression::Binary(op, e1, e2) => match op {
                BinaryOperator::And => format!(
                    "\
                     {0}\
                     cmp rax, 0\n\
                     jne {2}\n\
                     jmp {3}\n\
                     {2}:\n\
                     {1}\
                     cmp rax, 0\n\
                     mov rax, 0\n\
                     setne al\n\
                     {3}:\n\
                     ",
                    e1.emit(vmap, stack_index),
                    e2.emit(vmap, stack_index),
                    gen_label(),
                    gen_label()
                ),
                BinaryOperator::Or => format!(
                    "\
                     {0}\
                     cmp rax, 0
                     je {2}\n\
                     jmp {3}\n\
                     {2}:\n\
                     {1}\
                     cmp rax, 0\n\
                     mov rax, 0\n\
                     setne al\n\
                     {3}:\n\
                     ",
                    e1.emit(vmap, stack_index),
                    e2.emit(vmap, stack_index),
                    gen_label(),
                    gen_label()
                ),
                _ => panic!("invalid syntax"),
            },
            //Expression::Null => String::from(""),
        }
    }
}

#[derive(Debug)]
enum Constant {
    Int(u32),
}

impl ASTNode for Constant {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<Constant> {
        match t.next().unwrap() {
            Token::Literal(Literal::Int(i)) => Ok(Constant::Int(i)),
            tok => Err(Error::UnexpectedToken {
                wanted: "Constant",
                expected: vec![Token::Literal(Literal::Int(0))],
                found: tok,
                tokens: t.collect(),
            }),
        }
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
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
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<UnaryOperator> {
        match t.next().unwrap() {
            Token::Complement => Ok(UnaryOperator::Complement),
            Token::Negative => Ok(UnaryOperator::Negative),
            Token::Negation => Ok(UnaryOperator::Negation),
            tok => Err(Error::UnexpectedToken {
                wanted: "UnaryOperator",
                expected: vec![Token::Complement, Token::Negation, Token::Negative],
                found: tok,
                tokens: t.collect(),
            }),
        }
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
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

#[derive(Debug, PartialEq)]
enum BinaryOperator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
    Modulo,
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,
    And,
    Or,
}

impl ASTNode for BinaryOperator {
    fn parse<I: Iterator<Item = Token>>(t: &mut PutBack<I>) -> Result<BinaryOperator> {
        match t.next().unwrap() {
            Token::Addition => Ok(BinaryOperator::Addition),
            Token::Negative => Ok(BinaryOperator::Subtraction),
            Token::Multiplication => Ok(BinaryOperator::Multiplication),
            Token::Division => Ok(BinaryOperator::Division),
            Token::Modulo => Ok(BinaryOperator::Modulo),
            Token::BitAnd => Ok(BinaryOperator::BitAnd),
            Token::BitOr => Ok(BinaryOperator::BitOr),
            Token::BitXor => Ok(BinaryOperator::BitXor),
            Token::ShiftLeft => Ok(BinaryOperator::ShiftLeft),
            Token::ShiftRight => Ok(BinaryOperator::ShiftRight),
            tok => Err(Error::UnexpectedToken {
                wanted: "BinaryOperator",
                expected: vec![
                    Token::Addition,
                    Token::Negative,
                    Token::Multiplication,
                    Token::Division,
                    Token::Modulo,
                    Token::BitAnd,
                    Token::BitOr,
                    Token::BitXor,
                    Token::ShiftLeft,
                    Token::ShiftRight,
                ],
                found: tok,
                tokens: t.collect(),
            }),
        }
    }

    fn emit(self, vmap: &mut HashMap<String, usize>, stack_index: usize) -> String {
        match self {
            BinaryOperator::Addition => String::from("add rax, rcx\n"),
            BinaryOperator::Subtraction => String::from(
                "\
                 sub rcx, rax\n\
                 mov rax, rcx\n\
                 ",
            ),
            BinaryOperator::Multiplication => String::from("imul rax, rcx\n"),
            BinaryOperator::Division => String::from(
                "\
                 mov rbx, rax\n\
                 mov rax, rcx\n\
                 cqo\n\
                 idiv rbx\n\
                 ",
            ),
            BinaryOperator::Modulo => String::from(
                "\
                 mov rbx, rax\n\
                 mov rax, rcx\n\
                 cqo\n\
                 idiv rbx\n\
                 mov rax, rdx\n\
                 ",
            ),
            BinaryOperator::BitAnd => String::from(
                "\
                 and rcx, rax
                 mov rax, rcx
                 ",
            ),
            BinaryOperator::BitOr => String::from(
                "\
                 or rcx, rax
                 mov rax, rcx
                 ",
            ),
            BinaryOperator::BitXor => String::from(
                "\
                 xor rcx, rax
                 mov rax, rcx
                 ",
            ),
            BinaryOperator::ShiftLeft => String::from(
                "\
                 shl rcx, rax
                 mov rax, rcx
                 ",
            ),
            BinaryOperator::ShiftRight => String::from(
                "\
                 shr rcx, rax
                 mov rax, rcx
                 ",
            ),
            BinaryOperator::LessThan => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 setl al\n\
                 ",
            ),
            BinaryOperator::LessThanEqual => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 setle al\n\
                 ",
            ),
            BinaryOperator::GreaterThan => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 setg al\n\
                 ",
            ),
            BinaryOperator::GreaterThanEqual => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 setge al\n\
                 ",
            ),
            BinaryOperator::Equal => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 sete al\n\
                 ",
            ),
            BinaryOperator::NotEqual => String::from(
                "\
                 cmp rcx, rax\n\
                 mov rax, 0\n\
                 setne al\n\
                 ",
            ),
            _ => unimplemented!(),
        }
    }
}

fn consume_token<I: Iterator<Item = Token>>(t: &mut I, tok: Token) -> Result<()> {
    let next = t.next().unwrap();
    if next != tok {
        Err(Error::UnexpectedToken {
            wanted: "",
            expected: vec![tok],
            found: next,
            tokens: t.collect(),
        })
    } else {
        Ok(())
    }
}

pub fn parse(t: Vec<Token>) -> Result<Program> {
    Program::parse(&mut put_back(t.into_iter()))
}
