mod lexer;
use lexer::{TokenType, Token};

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
        superclass: Option<Box<Expr>>,
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
    Nil,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    // 解析入口
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

    fn consume(&mut self, ttype: TokenType, message: &str) -> Option<&Token> {
        if self.check(ttype) {
            Some(self.advance())
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
        let name = self.consume(TokenType::Id, "Expect variable name.");

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
        let keyword = self.previous(); // 记录 `return` 关键字
        let value = if !self.check(TokenType::Semi) {
            Some(self.expression())
        } else {
            None
        };

        self.consume(TokenType::Semi, "Expect ';' after return value.");
        Stmt::Return { keyword, value }
    }

    fn function_stmt(&mut self) -> Stmt {
        let name = self.consume(TokenType::Id, "Expect function name.");
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");

        let mut params = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                params.push(self.consume(TokenType::Id, "Expect parameter name."));
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

    
}