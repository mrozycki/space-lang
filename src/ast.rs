use crate::lexer::Token;

#[allow(dead_code)]
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
    FunctionCall {
        function_name: Token,
        arguments: Vec<Box<Expression>>,
    },
}

#[allow(dead_code)]
pub enum Statement {
    Expression {
        expr: Expression,
    },
    Block {
        statements: Vec<Box<Statement>>,
    },
    Print {
        expr: Expression,
    },
    Definition {
        variable: Token,
        value: Expression,
    },
    Assignment {
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
        parameters: Vec<Token>,
        body: Box<Statement>,
    },
}
