use crate::lexer::Token;

#[derive(Debug)]
pub enum Expression {
    BinaryOp {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    UnaryOp {
        operator: Token,
        right: Box<Expression>,
    },
    Literal {
        value: Token,
    },
    Variable {
        name: Token,
    },
    Assignment {
        variable: Token,
        value: Box<Expression>,
    },
    FunctionCall {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
}

#[derive(Debug)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Block {
        statements: Vec<Statement>,
    },
    Print {
        expr: Expression,
        newline: bool,
    },
    Definition {
        variable: Token,
        value: Expression,
    },
    Conditional {
        condition: Expression,
        if_true: Box<Statement>,
        if_false: Option<Box<Statement>>,
    },
    Loop {
        condition: Expression,
        body: Box<Statement>,
    },
    FunctionDefinition {
        name: String,
        parameters: Vec<String>,
        body: Box<Statement>,
    },
    Return {
        expression: Option<Expression>,
    },
}
