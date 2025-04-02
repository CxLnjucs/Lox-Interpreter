use regex::Regex;

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    //Comment,        // "//" 单行注释

    LeftParen,      // "("
    RightParen,     // ")"
    LeftCurly,      // "{"
    RightCurly,     // "}"
    
    Comma,          // ","
    Semi,           // ";"
    Dot,            // "."
    
    Minus,          // "-"
    Plus,           // "+"
    Div,            // "/"
    Star,           // "*"
    
    Assignop,       // "="
    Not,            // "!"
    NotEqual,       // "!="
    Equal,          // "=="
    Greater,        // ">"
    GreaterEqual,   // ">="
    Less,           // "<"
    LessEqual,      // "<="
    And,            // "and"
    Or,             // "or"
    True,           // "true"
    False,          // "false"
    
    If,             // "if"
    Else,           // "else"
    While,          // "while"
    For,            // "for"
    Nil,            // "nil"
    
    Class,          // "class"    
    Var,            // "var"
    Fun,            // "fun"
    Print,          // "print"
    Return,         // "return"
    Super,          // "super"
    This,           // "this"
    
    String,         // 字符串
    Number,         // 数值
    Id,             // 标识符

    LineFeed,        // \r?\n 兼容windows的换行符
    OtherBlank,      // 除换行符外的空白符
    Error,
    Eof,            // 文件结束符
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lineno: usize,
    pub lexeme: String,
}

pub struct Lexer {
    input: String,
    position: usize,
    lineno: usize, // 追踪当前行号
    regex_map: Vec<(Regex, TokenType)>, // 存储正则表达式和对应的 TokenType
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let patterns = vec![
            //(r"^//.*", TokenType::Comment),
            (r"^\(", TokenType::LeftParen),
            (r"^\)", TokenType::RightParen),
            (r"^\{", TokenType::LeftCurly),
            (r"^\}", TokenType::RightCurly),
            (r"^,", TokenType::Comma),
            (r"^;", TokenType::Semi),
            (r"^\.", TokenType::Dot),
            (r"^-", TokenType::Minus),
            (r"^\+", TokenType::Plus),
            (r"^/", TokenType::Div),
            (r"^\*", TokenType::Star),
            (r"^=", TokenType::Assignop),
            (r"^!", TokenType::Not),
            (r"^!=", TokenType::NotEqual),
            (r"^==", TokenType::Equal),
            (r"^>", TokenType::Greater),
            (r"^>=", TokenType::GreaterEqual),
            (r"^<", TokenType::Less),
            (r"^<=", TokenType::LessEqual),
            (r"^\b(and)\b", TokenType::And),
            (r"^\b(or)\b", TokenType::Or),
            (r"^\b(true)\b", TokenType::True),
            (r"^\b(false)\b", TokenType::False),
            (r"^\b(if)\b", TokenType::If),
            (r"^\b(else)\b", TokenType::Else),
            (r"^\b(while)\b", TokenType::While),
            (r"^\b(for)\b", TokenType::For),
            (r"^\b(nil)\b", TokenType::Nil),
            (r"^\b(class)\b", TokenType::Class),
            (r"^\b(var)\b", TokenType::Var),
            (r"^\b(fun)\b", TokenType::Fun),
            (r"^\b(print)\b", TokenType::Print),
            (r"^\b(return)\b", TokenType::Return),
            (r"^\b(super)\b", TokenType::Super),
            (r"^\b(this)\b", TokenType::This),
            (r#"^"([^"]*)""#, TokenType::String),
            (r"^\d+(\.\d+)?", TokenType::Number),
            (r"^[a-zA-Z_][a-zA-Z0-9_]*", TokenType::Id),
            (r"^\r?\n", TokenType::LineFeed), // 兼容windows的换行符。。。。。。
            (r"^[ \t\f]+", TokenType::OtherBlank),
            (r".", TokenType::Error),
        ];

        let regex_map = patterns
            .into_iter()
            .map(|(pat, tok)| (Regex::new(pat).unwrap(), tok))
            .collect();

        Lexer {
            input,
            position: 0,
            lineno: 1,
            regex_map,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while self.position < self.input.len() {
            let remaining = &self.input[self.position..];

            let mut longest_match: Option<(usize, TokenType, &str)> = None;

            // 遍历所有正则表达式，找到最长匹配
            for (regex, token_type) in &self.regex_map {
                if let Some(mat) = regex.find(remaining) {
                    let match_len = mat.end();
                    if longest_match.as_ref().map(|t| t.0).unwrap_or(0) < match_len {
                        longest_match = Some((match_len, token_type.clone(), mat.as_str()));
                    }
                }
            }

            if let Some((match_len, token_type, matched_str)) = longest_match {
                self.position += match_len;

                // 处理换行符
                if token_type == TokenType::LineFeed {
                    self.lineno += 1;
                    continue;
                }
                // 跳过空白符，不返回 Token
                // || token_type == TokenType::Comment
                if token_type == TokenType::OtherBlank {
                    continue;
                }
                // 处理ERROR
                if token_type == TokenType::Error {
                    eprintln!("Lexer Error at line {}: Unrecognized token '{}'", self.lineno, matched_str);
                    continue; 
                }

                return Some(Token {
                    token_type,
                    lexeme: matched_str.to_string(),
                    lineno: self.lineno,
                });
            }
        }

        Some(Token {
            token_type: TokenType::Eof,
            lexeme: "".to_string(),
            lineno: self.lineno,
        })
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.next_token() {
            if token.token_type == TokenType::Eof {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }

        tokens
    }
}