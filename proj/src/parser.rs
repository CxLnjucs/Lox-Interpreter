use crate::lexer::{TokenType, Token};

#[derive(Debug, Clone)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Option<Object>, // Object 可以是你的 Lox 值类型
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    Super {
        keyword: Token,
        method: Token,
    },
    This {
        keyword: Token,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Class {
        name: Token,
        superclass: Option<Token>,
        methods: Vec<Stmt>,
    },
    Expression {
        expression: Expr,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expression: Expr,
    },
    Return {
        keyword: Token,
        value: Option<Expr>,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    // For 待添加
}

#[derive(Debug, Clone)]
pub enum Object {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil, // 实际不使用
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // 语法分析入口
    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            if let Some(stmt) = self.statement() {
                statements.push(stmt);
            }
        }
        statements
    }

    // 工具方法
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }
    
    fn match_token(&mut self, ttype: TokenType) -> bool {
        if self.check(ttype) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tokens(&mut self, types: &[TokenType]) -> bool {
        for ttype in types {
            if self.check(*ttype) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, ttype: TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == ttype
    }

    fn consume(&mut self, ttype: TokenType, message: &str) -> &Token {
        if self.check(ttype) {
            self.advance()
        } else {
            // 原型阶段简单panic
            panic!("{} at line {}", message, self.peek().lineno)
        }
    }

    fn statement(&mut self) -> Option<Stmt> {
        if self.match_token(TokenType::LeftCurly) {
            Some(Stmt::Block {
                statements: self.block_stmt(),
            })
        } else if self.match_token(TokenType::Class) {
            Some(self.class_stmt())
        } else if self.match_token(TokenType::Fun) {
            Some(self.function_stmt())
        } else if self.match_token(TokenType::If) {
            Some(self.if_stmt())
        } else if self.match_token(TokenType::Print) {
            Some(self.print_stmt())
        } else if self.match_token(TokenType::Return) {
            Some(self.return_stmt())
        } else if self.match_token(TokenType::Var) {
            Some(self.var_stmt())
        } else if self.match_token(TokenType::While) {
            Some(self.while_stmt())
        } else {
            self.expression_stmt()
        }
    }

    fn block_stmt(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightCurly) && !self.is_at_end() {
            if let Some(stmt) = self.statement() {
                statements.push(stmt);
            }
        }

        self.consume(TokenType::RightCurly, "Expect '}' after block.");
        statements
    }

    fn class_stmt(&mut self) -> Stmt {
        let name = self.consume(TokenType::Id, "Expect class name.").clone();
        
        let superclass = if self.match_token(TokenType::Less) {
            Some(self.consume(TokenType::Id, "Expect superclass name").clone())
        } else {
            None
        };

        self.consume(TokenType::LeftCurly, "Expect '{' before class body.");

        let mut methods = Vec::new();
        while !self.check(TokenType::RightCurly) && !self.is_at_end() {
            methods.push(self.function_stmt()); // 直接复用 function_stmt 解析方法
        }

        self.consume(TokenType::RightCurly, "Expect '}' after class body.");

        Stmt::Class {
            name,
            superclass,
            methods,
        }
    }

    // 不能解析出 class, fun, var, 待处理
    fn if_stmt(&mut self) -> Stmt {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        let condition = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after if condition.");

        let then_branch = Box::new(self.statement().expect("Expect statement after 'if'."));
        let else_branch = if self.match_token(TokenType::Else) {
            Some(Box::new(self.statement().expect("Expect statement after 'else'.")))
        } else {
            None
        };

        Stmt::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    // 不能解析出 class, fun, var, 待处理
    fn while_stmt(&mut self) -> Stmt {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after while condition.");

        let body = Box::new(self.statement().expect("Expect statement after 'while'."));

        Stmt::While { condition, body }
    }


    fn var_stmt(&mut self) -> Stmt {
        let name = self.consume(TokenType::Id, "Expect variable name.").clone();

        let initializer = if self.match_token(TokenType::Assignop) {
            Some(self.expression())
        } else {
            None
        };

        self.consume(TokenType::Semi, "Expect ';' after variable declaration.");
        Stmt::Var { name, initializer }
    }

    fn expression_stmt(&mut self) -> Option<Stmt> {
        let expr = self.expression();
        self.consume(TokenType::Semi, "Expect ';' after expression.");
        Some(Stmt::Expression { expression: expr })
    }

    fn print_stmt(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(TokenType::Semi, "Expect ';' after value.");
        Stmt::Print { expression: expr }
    }

    fn return_stmt(&mut self) -> Stmt {
        let keyword = self.previous().clone(); // 记录 `return` 关键字
        let value = if !self.check(TokenType::Semi) {
            Some(self.expression())
        } else {
            None
        };

        self.consume(TokenType::Semi, "Expect ';' after return value.");
        Stmt::Return { keyword, value }
    }

    fn function_stmt(&mut self) -> Stmt {
        let name = self.consume(TokenType::Id, "Expect function name.").clone();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");

        let mut params = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                params.push(self.consume(TokenType::Id, "Expect parameter name.").clone());
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftCurly, "Expect '{' before function body.");

        let body = self.block_stmt();

        Stmt::Function { name, params, body }
    }

    /// 解析 `expression → assignment`
    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    /// 解析 `assignment → ( call "." )? IDENTIFIER "=" assignment 先不处理
    //                   | logic_or`
    fn assignment(&mut self) -> Expr {
        let expr = self.logic_or();

        if self.match_token(TokenType::Assignop) {
            let equals = self.previous().clone();
            let value = self.assignment();
    
            if let Expr::Variable { name } = expr {
                return Expr::Assign {
                    name,
                    value: Box::new(value),
                };
            } else if let Expr::Get { object, name } = expr {
                return Expr::Set {
                    object,
                    name,
                    value: Box::new(value),
                };
            }
    
            panic!("Invalid assignment target at {:?}", equals);
        }

        expr
    }

    /// 解析 `logic_or → logic_and ( "or" logic_and )*`
    fn logic_or(&mut self) -> Expr {
        let mut expr = self.logic_and();

        while self.match_token(TokenType::Or) {
            let operator = self.previous().clone();
            let right = self.logic_and();
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `logic_and → equality ( "and" equality )*`
    fn logic_and(&mut self) -> Expr {
        let mut expr = self.equality();

        while self.match_token(TokenType::And) {
            let operator = self.previous().clone();
            let right = self.equality();
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `equality → comparison ( ( "!=" | "==" ) comparison )*`
    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();

        while self.match_tokens(&[TokenType::NotEqual, TokenType::Equal]) {
            let operator = self.previous().clone();
            let right = self.comparison();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*`
    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `term → factor ( ( "-" | "+" ) factor )*`
    fn term(&mut self) -> Expr {
        let mut expr = self.factor();

        while self.match_tokens(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous().clone();
            let right = self.factor();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `factor → unary ( ( "/" | "*" ) unary )*`
    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();

        while self.match_tokens(&[TokenType::Div, TokenType::Star]) {
            let operator = self.previous().clone();
            let right = self.unary();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        expr
    }

    /// 解析 `unary → ( "!" | "-" ) unary | call`
    fn unary(&mut self) -> Expr {
        if self.match_tokens(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary());
            return Expr::Unary { operator, right };
        }

        self.call()
    }

    /// 解析 `call → primary ( "(" arguments? ")" )*`  TODO: .id 
    fn call(&mut self) -> Expr {
        let mut expr = self.primary();

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_call(expr);
            } else if self.match_token(TokenType::Dot) {
                let name = self.consume(TokenType::Id, "Expect property name after '.'.").clone();
                expr = Expr::Get {
                    object: Box::new(expr),
                    name,
                };
            } else {
                break;
            }
        }

        expr
    }

    fn finish_call(&mut self, callee: Expr) -> Expr {
        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(self.expression());
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen, "Expect ')' after arguments.").clone();
        Expr::Call {
            callee: Box::new(callee),
            paren,
            arguments,
        }
    }

    /// 解析 `primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")"
    ///                 | IDENTIFIER | "super" "." IDENTIFIER`
    fn primary(&mut self) -> Expr {
        if self.match_token(TokenType::False) {
            return Expr::Literal {
                value: Some(Object::Boolean(false)),
            };
        }
        if self.match_token(TokenType::True) {
            return Expr::Literal {
                value: Some(Object::Boolean(true)),
            };
        }
        if self.match_token(TokenType::Nil) {
            return Expr::Literal { value: None };
        }

        if self.match_token(TokenType::Number) {
            return Expr::Literal {
                value: Some(Object::Number(self.previous().lexeme.parse::<f64>().expect("Expected a valid number"))),
            };
        }
        if self.match_token(TokenType::String) {
            return Expr::Literal {
                value: Some(Object::String(self.previous().lexeme.trim_matches('"').to_string())),
            };
        }

        if self.match_token(TokenType::Id) {
            return Expr::Variable {
                name: self.previous().clone(),
            };
        }

        if self.match_token(TokenType::LeftParen) {
            let expr = self.expression();
            self.consume(TokenType::RightParen, "Expect ')' after expression.");
            return Expr::Grouping {
                expression: Box::new(expr),
            };
        }

        if self.match_token(TokenType::This) {
            return Expr::This {
                keyword: self.previous().clone(),
            };
        }
    
        if self.match_token(TokenType::Super) {
            let keyword = self.previous().clone();
            self.consume(TokenType::Dot, "Expect '.' after 'super'.");
            let method = self.consume(TokenType::Id, "Expect superclass method name.").clone();
            return Expr::Super { keyword, method };
        }

        panic!("Unexpected expression.");
    }
}