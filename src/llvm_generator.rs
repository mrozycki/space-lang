use crate::ast::{Expression, Statement};
use crate::lexer::{Token, TokenType};

pub struct LLVMGenerator<'a> {
    body: &'a [Statement],
    reg: u32,
}

impl<'a> LLVMGenerator<'a> {
    pub fn new(body: &'a [Statement]) -> Self {
        LLVMGenerator { body, reg: 1 }
    }

    fn eval(&mut self, expr: &Expression) -> (String, String) {
        match expr {
            Expression::BinaryOp {
                operator,
                left,
                right,
            } => {
                let mut result = String::new();
                let (code_left, ref_left) = self.eval(left);
                result.push_str(&code_left);
                let (code_right, ref_right) = self.eval(right);
                result.push_str(&code_right);

                let result_reg = format!("%{}", self.reg);
                self.reg += 1;
                match operator.token_type {
                    TokenType::Plus => result.push_str(&format!(
                        "{} = add i32 {}, {}\n",
                        result_reg, ref_left, ref_right
                    )),
                    TokenType::Minus => result.push_str(&format!(
                        "{} = sub i32 {}, {}\n",
                        result_reg, ref_left, ref_right
                    )),
                    TokenType::Star => result.push_str(&format!(
                        "{} = mul i32 {}, {}\n",
                        result_reg, ref_left, ref_right
                    )),
                    TokenType::Slash => result.push_str(&format!(
                        "{} = sdiv i32 {}, {}\n",
                        result_reg, ref_left, ref_right
                    )),
                    TokenType::Modulo => todo!(),
                    TokenType::And => todo!(),
                    TokenType::Or => todo!(),
                    TokenType::Equal => todo!(),
                    TokenType::NotEqual => todo!(),
                    TokenType::LessThan => todo!(),
                    TokenType::LessThanOrEqual => todo!(),
                    TokenType::GreaterThan => todo!(),
                    TokenType::GreaterThanOrEqual => todo!(),
                    _ => panic!("Unrecognized operator"),
                };
                (result, result_reg)
            }
            Expression::UnaryOp { operator, right } => todo!(),
            Expression::Literal { value } => {
                if let TokenType::Integer(value) = &value.token_type {
                    ("".to_string(), value.to_string())
                } else {
                    panic!("Unrecognized literal")
                }
            }
            Expression::Variable { name } => todo!(),
            Expression::ArrayLiteral { elements } => todo!(),
            Expression::ArrayRef { array, index } => todo!(),
            Expression::StructLiteral {
                struct_type,
                fields,
            } => todo!(),
            Expression::StructFieldRef { target, field_name } => todo!(),
            Expression::Assignment { target, value } => todo!(),
            Expression::FunctionCall { callee, arguments } => todo!(),
        }
    }

    pub fn generate(&mut self) -> String {
        let mut result = "define i32 @main()\n{\n".to_string();
        for statement in self.body {
            match statement {
                Statement::Expression { expr } => result.push_str(&self.eval(expr).0),
                Statement::Block { statements } => todo!(),
                Statement::Definition { variable, value } => todo!(),
                Statement::Conditional {
                    condition,
                    if_true,
                    if_false,
                } => todo!(),
                Statement::Loop { condition, body } => todo!(),
                Statement::FunctionDefinition {
                    name,
                    parameters,
                    body,
                    export,
                } => todo!(),
                Statement::Return { expression } => todo!(),
                Statement::Break => todo!(),
                Statement::Continue => todo!(),
                Statement::ExportVariable { name, value } => todo!(),
                Statement::Import { name } => todo!(),
                Statement::CallBuiltin { function } => todo!(),
                Statement::StructDefinition {
                    struct_type,
                    fields,
                    export,
                } => todo!(),
                Statement::Invalid => todo!(),
            }
        }

        result.push_str("ret i32 0\n}\n");
        result
    }
}
