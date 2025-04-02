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
    For {
        initializer: Option<Box<Stmt>>,
        condition: Option<Expr>,
        increment: Option<Expr>,
        body: Box<Stmt>,
    },
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
            statements.push(self.statement());
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

    fn statement(&mut self) -> Stmt {
        if self.match_token(TokenType::LeftCurly) {
            Stmt::Block { statements: self.block_stmt() }
        } else if self.match_token(TokenType::Class) {
            self.class_stmt()
        } else if self.match_token(TokenType::Fun) {
            self.function_stmt()
        } else if self.match_token(TokenType::If) {
            self.if_stmt()
        } else if self.match_token(TokenType::For) {
            self.for_stmt()
        } else if self.match_token(TokenType::Print) {
            self.print_stmt()
        } else if self.match_token(TokenType::Return) {
            self.return_stmt()
        } else if self.match_token(TokenType::Var) {
            self.var_stmt()
        } else if self.match_token(TokenType::While) {
            self.while_stmt()
        } else {
            self.expression_stmt()
        }
    }

    fn block_stmt(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();

        while !self.check(TokenType::RightCurly) && !self.is_at_end() {
            statements.push(self.statement());
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

        let then_branch = Box::new(self.statement());
        let else_branch = if self.match_token(TokenType::Else) {
            Some(Box::new(self.statement()))
        } else {
            None
        };

        Stmt::If {
            condition,
            then_branch,
            else_branch,
        }
    }

    fn for_stmt(&mut self) -> Stmt {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
    
        // 解析初始化语句（变量声明、表达式语句或空）
        let initializer = if self.match_token(TokenType::Semi) {
            None
        } else if self.match_token(TokenType::Var) {
            Some(Box::new(self.var_stmt()))
        } else {
            Some(Box::new(self.expression_stmt()))
        };
    
        // 解析循环条件（可选）
        let condition = if !self.check(TokenType::Semi) {
            Some(self.expression())
        } else {
            None
        };
        self.consume(TokenType::Semi, "Expect ';' after loop condition.");
    
        // 解析递增语句（可选）
        let increment = if !self.check(TokenType::RightParen) {
            Some(self.expression())
        } else {
            None
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.");
    
        // 解析循环体
        let body = Box::new(self.statement());
    
        Stmt::For {
            initializer,
            condition,
            increment,
            body,
        }
    }
    

    // 不能解析出 class, fun, var, 待处理
    fn while_stmt(&mut self) -> Stmt {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        let condition = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after while condition.");

        let body = Box::new(self.statement());

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

    fn expression_stmt(&mut self) -> Stmt {
        let expr = self.expression();
        self.consume(TokenType::Semi, "Expect ';' after expression.");
        Stmt::Expression { expression: expr }
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

    /// expression → assignment
    fn expression(&mut self) -> Expr {
        self.assignment()
    }

    /// assignment → ( call "." )? IDENTIFIER "=" assignment 
    //             | logic_or`
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

    /// logic_or → logic_and ( "or" logic_and )*
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

    /// logic_and → equality ( "and" equality )*
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

    /// equality → comparison ( ( "!=" | "==" ) comparison )*
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

    /// comparison → term ( ( ">" | ">=" | "<" | "<=" ) term )*
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

    /// term → factor ( ( "-" | "+" ) factor )*
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

    /// factor → unary ( ( "/" | "*" ) unary )*
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

    /// unary → ( "!" | "-" ) unary 
    //        | call
    fn unary(&mut self) -> Expr {
        if self.match_tokens(&[TokenType::Not, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = Box::new(self.unary());
            return Expr::Unary { operator, right };
        }

        self.call()
    }

    /// call → primary ( "(" arguments? ")" | "." IDENTIFIER )*
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

pub struct DebugAstPrinter;

impl DebugAstPrinter {
    pub fn print_expr(&self, expr: &Expr, indent: usize) {
        let prefix = "  ".repeat(indent);
        match expr {
            Expr::Assign { name, value } => {
                println!("{}AssignExpr:", prefix);
                println!("{}  Name: {}", prefix, name.lexeme);
                self.print_expr(value, indent + 1);
            }
            Expr::Binary { left, operator, right } => {
                println!("{}BinaryExpr:", prefix);
                println!("{}  Operator: {}", prefix, operator.lexeme);
                self.print_expr(left, indent + 1);
                self.print_expr(right, indent + 1);
            }
            Expr::Call { callee, arguments, .. } => {
                println!("{}CallExpr:", prefix);
                println!("{}  Callee:", prefix);
                self.print_expr(callee, indent + 1);
                println!("{}  Arguments:", prefix);
                for arg in arguments {
                    self.print_expr(arg, indent + 2);
                }
            }
            Expr::Grouping { expression } => {
                println!("{}GroupingExpr:", prefix);
                self.print_expr(expression, indent + 1);
            }
            Expr::Literal { value } => {
                println!("{}LiteralExpr: {:?}", prefix, value);
            }
            Expr::Variable { name } => {
                println!("{}VariableExpr: {}", prefix, name.lexeme);
            }
            Expr::Unary { operator, right } => {
                println!("{}UnaryExpr:", prefix);
                println!("{}  Operator: {}", prefix, operator.lexeme);
                self.print_expr(right, indent + 1);
            }
            _ => {
                println!("{}(Unhandled Expression Type)", prefix);
            }
        }
    }

    pub fn print_stmt(&self, stmt: &Stmt, indent: usize) {
        let prefix = "  ".repeat(indent);
        match stmt {
            Stmt::Expression { expression } => {
                println!("{}ExpressionStmt:", prefix);
                self.print_expr(expression, indent + 1);
            }
            Stmt::Print { expression } => {
                println!("{}PrintStmt:", prefix);
                self.print_expr(expression, indent + 1);
            }
            Stmt::Var { name, initializer } => {
                println!("{}VarStmt:", prefix);
                println!("{}  Name: {}", prefix, name.lexeme);
                if let Some(init) = initializer {
                    println!("{}  Initializer:", prefix);
                    self.print_expr(init, indent + 1);
                }
            }
            Stmt::Block { statements } => {
                println!("{}BlockStmt:", prefix);
                for s in statements {
                    self.print_stmt(s, indent + 1);
                }
            }
            Stmt::If { condition, then_branch, else_branch } => {
                println!("{}IfStmt:", prefix);
                println!("{}  Condition:", prefix);
                self.print_expr(condition, indent + 1);
                println!("{}  Then Branch:", prefix);
                self.print_stmt(then_branch, indent + 1);
                if let Some(else_branch) = else_branch {
                    println!("{}  Else Branch:", prefix);
                    self.print_stmt(else_branch, indent + 1);
                }
            }
            _ => {
                println!("{}(Unhandled Statement Type)", prefix);
            }
        }
    }
}
