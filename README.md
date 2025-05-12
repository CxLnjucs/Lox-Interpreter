# Lox-Interpreter实现文档

## 小组成员

221220157 李灿祥

## 仓库地址



## 项目简介

本项目为基于《Crafting Interpreters》中定义的 Lox 语言规范，用 Rust 实现的解释器，支持表达式求值、变量作用域、函数调用、类与对象、继承机制等语言特性。实现过程中除词法分析器使用 Rust regex 处理正则匹配，其余包括语法分析器、作用域环境、解释器执行引擎等模块均为手搓，力求深入理解解释器运行机制。

## 项目结构

```text
src/
├── main.rs           
├── grammar     ← Lox Grammar
├── lexer.rs
├── parser.rs
├── interpreter.rs
```

## 设计思路

### lexer

#### 实现说明

lexer的实现比较容易，利用正则匹配即可。lexer的可扩展性好，添加新的`TokenType`只需增加对应正则规则。

#### 数据结构定义

`TokenType`(枚举)：表示表示所有可能的词法单元类型，涵盖关键字，常量，空白符，注释等。

```rust
pub enum TokenType {
    LineComment,    // 单行注释
    BlockComment,   // 多行注释    


    LeftParen,      // "("
    RightParen,     // ")"
    LeftCurly,      // "{"
    RightCurly,     // "}"
    
    ...
}
```

`Token`(结构体)：

|字段名 | 类型 | 描述 |
|------ | ---- | ---- |
|token_type | TokenType | 词法单元类型 |
|lineno | usize | 词法单元所在的源代码行号 |
|lexeme | String | 实际匹配的源码字符串片段 |

`Lexer`(结构体)：

|字段名 | 类型 | 描述|
|------ | ---- | ---- |
|input | String | 源代码输入文本|
|position | usize | 当前扫描位置|
|lineno | usize | 当前行号|
|regex_map | Vec<(Regex, TokenType)> | 正则表达式和对应 TokenType 的映射列表|

方法说明：

`Lexer::new(input: String) -> Self`

+ 构造函数，初始化词法分析器。

+ 加载所有正则规则到 regex_map。

+ 默认行号为 1，起始位置为 0。

`Lexer::next_token(&mut self) -> Option<Token>`

+ 从当前位置开始，尝试匹配下一个 Token。

+ 模仿 Flex 的规则，匹配时选择最长匹配，如果有多个最长匹配，则先出现的规则优先。

+ 跳过空白符、注释。

+ 遇到非法字符时输出错误提示。

+ lineno 处理。对于换行符和多行注释，都能正确更新 lineno 。

`Lexer::tokenize(&mut self) -> Vec<Token>`

+ 迭代调用 next_token()，直到读取完所有 Token。

+ 返回 Token 序列，并以 Eof 结尾。



### parser

#### 实现说明

使用递归下降分析实现语法分析器，它负责将词法分析阶段生成的 Token 序列转换为抽象语法树（AST），以供后续语义分析与解释执行。Lox语法规则已经实现了表达式优先级处理，为每条语法规则实现对应的解析函数即可。现阶段没有实现错误恢复，而是采用简单的`panic!`处理。

#### AST设计

AST用`Vec<Stmt>`实现。每个`Stmt`的结构可以参考下面的数据结构定义。

#### 数据结构定义

完全参照《Crafting Interpreters》的设计，将语法单元分为`Expr`和`Stmt`两种。

```rust
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
        value: Option<Object>, 
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
```

```rust
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
```

使用`Box`处理`Expr`或`Stmt`的嵌套结构。使用`Option`处理可能的空值。

Parser 结构体定义：

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

| 字段名     | 类型         | 描述                 |
| ------- | ---------- | ------------------ |
| tokens  | Vec<Token> | 从词法分析器生成的 Token 列表 |
| current | usize      | 当前指针，指向待分析 Token   |


方法说明：

一系列工具方法：

`advance(&mut self) -> &Token`：将当前索引指向下一个 Token，并返回之前的 Token。

`previous(&self) -> &Token`：返回最近一次 advance() 移动前的 Token。

`peek(&self) -> &Token`：查看当前 Token，但不推进。

`is_at_end(&self) -> bool`：判断是否已到达 Token 列表末尾（即遇到 EOF）。

`match_token(&mut self, ttype: TokenType) -> bool`：如果当前 Token 匹配指定类型，则调用 advance()，返回 true，否则返回 false。

`match_tokens(&mut self, types: &[TokenType]) -> bool`：如果当前 Token 的 TokenType 属于给定的集合，则调用 advance()，返回 true，否则返回 false。

`check(&self, ttype: TokenType) -> bool`：检查当前 Token 是否为指定 TokenType（不移动）。

`consume(&mut self, ttype: TokenType, message: &str) -> &Token`：强制要求当前 Token 为指定类型。若匹配，则调用 advance() 并返回该 Token，否则 panic! 报错。

语法分析入口：

```rust
pub fn parse(&mut self) -> Vec<Stmt> {
    let mut statements = Vec::new();
    while !self.is_at_end() {
        statements.push(self.statement());
    }
    statements
}
```

stmt解析函数：

```rust
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
```

对应的各个具体stmt解析函数根据`Stmt`枚举类型的定义实现即可。

expr解析函数在包含`Expr`的`Stmt`解析时被调用。由于Lox语法规则已经很好地处理了表达式的优先级，无需特别处理，对每个语法规则实现对应的解析函数即可，不再赘述。

#### AST printer

为了方便Debug，实现了一个`DebugAstPrinter`结构体用于打印抽象语法树。



### interpreter

#### 实现说明

对于AST（`Vec<Stmt>`），遍历并执行每个`Stmt`即可。

#### 数据结构定义

`Value`（枚举）：

```rust
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
}
```

包括了所有Lox运行时值表示。使用`Rc`处理函数、类两种共享的不可变数据，使用`Rc<Refcell<LoxInstance>>`处理类实例这种共享的可变数据。`LoxFuntion`、`LoxClass`和`LoxInstance`的具体实现在函数调用和类与实例部分再作说明。

`Environment`（结构体）：

```rust
pub struct Environment {
    values: HashMap<String, Value>,
    enclosing: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self { values: HashMap::new(), enclosing: None }
    }

    pub fn new_enclosed(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self { values: HashMap::new(), enclosing: Some(enclosing) }
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &str, value: Value) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(ref parent) = self.enclosing {
            parent.borrow_mut().assign(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", name))
        }
    }

    pub fn get(&self, name: &str) -> Option<Value> {
        self.values.get(name).cloned().or_else(|| self.enclosing.as_ref()?.borrow().get(name))
    }
}
```

| 字段名     | 类型         | 描述                 |
| ------- | ---------- | ------------------ |
| values  | HashMap<String, Value> | 当前作用域内的变量映射，包括变量名和值的映射、函数名和函数定义的映射、类名和类定义的映射 |
| enclosing |   Option<Rc<RefCell<Environment>>>    | 外层作用域，用于支持嵌套作用域查找   |


方法说明：

`new_enclosed(enclosing: Rc<RefCell<Environment>>) -> Self`：创建一个有父作用域的嵌套环境。用于块语句或函数调用，允许嵌套作用域继承外部定义的变量。

`define(&mut self, name: &str, value: Value)`：在当前作用域中定义一个新变量。

`assign(&mut self, name: &str, value: Value) -> Result<(), String>`：为已存在的变量赋新值。如果当前作用域不存在该变量，会向外层作用域递归查找并尝试赋值。如果成功赋值，返回`Ok(())`；如果变量在整个作用域链中未被定义，返回`Err(String)`。

`get(&self, name: &str) -> Option<Value>`：获取变量的值。如果当前作用域没有该变量，会向外层递归查找。如果找到，返回`Some(Value)`；如果变量在整个作用域链中未被定义，返回`None`。

`Interpreter`(结构体)：

```rust
pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>, // 当前的作用域
    globals: Rc<RefCell<Environment>>, // 全局作用域
}
```

方法说明：

`interpret(&mut self, statements: &[Stmt])`：遍历每条语句并逐一执行。

`execute(&mut self, stmt: &Stmt) -> Result<(), ReturnSignal>`：执行一条语句。返回`Ok(())`说明语句成功执行；返回`Err(ReturnSignal)`则表示函数调用中触发了返回语义。其中 `ReturnSignal`封装了返回值：

```rust
pub struct ReturnSignal {
    pub value: Value,
}
```

`evaluate(&mut self, expr: &Expr) -> Value`：表达式求值。

由于`execute`和`evaluate`涉及的内容较多，只选取其中比较核心和复杂的部分说明。

#### 函数执行

函数执行流程开始于`evaluate`的`Expr::Call`部分：

```rust
pub fn evaluate(&mut self, expr: &Expr) -> Value {
    match expr {
        ...
        Expr::Call { callee, arguments, .. } => {
            let callee_val = self.evaluate(callee);
            let args: Vec<Value> = arguments.iter().map(|arg| self.evaluate(arg)).collect();
            match callee_val {
                Value::Function(f) => f.call(self, args), // 普通函数调用 or 类方法调用
                Value::Class(c) => LoxClass::call(c.clone(), self, args), // 类的构造函数调用
                _ => panic!("Can only call functions or classes")
            }
        }
        ...
    }
}
```

先介绍普通函数调用。这时控制流转向`f.call(self, args)`，查看`LoxFunction`的定义：

```rust
pub struct LoxFunction {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>, // 定义时的环境
    pub is_initializer: bool,
}

impl LoxFunction {
    pub fn call(&self, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Value {
        let new_env = Rc::new(RefCell::new(Environment::new_enclosed(self.closure.clone())));

        for (param, arg) in self.params.iter().zip(arguments.iter()) {
            new_env.borrow_mut().define(&param.lexeme, arg.clone());
        }

        let result = match interpreter.execute_block(&self.body, new_env) {
            // 这里不应该出现init，init的情况在LoxClass::call处理。并且Lox中init不能被显式调用
            Ok(_) => {
                Value::Nil
            }
            Err(ret) => {
                ret.value
            }
        };

        result
    }

    /// 绑定实例为 `this` 并返回一个新函数（闭包环境扩展）
    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> LoxFunction {
        let mut env = Environment::new_enclosed(self.closure.clone());
        env.define("this", Value::Instance(instance));
        LoxFunction {
            name: self.name.clone(),
            params: self.params.clone(),
            body: self.body.clone(),
            closure: Rc::new(RefCell::new(env)),
            is_initializer: self.is_initializer,
        }
    }
}
```

在`call`中，创建一个新的 Environment，其父环境为函数定义时所捕获的闭包环境`self.closure`。接着将实参值绑定到新环境中形参对应的变量名。然后调用`excute_block`并根据`Result<>`判断是否显式返回。

```rust
pub fn execute_block(&mut self, statements: &[Stmt], env: Rc<RefCell<Environment>>) -> Result<(), ReturnSignal>{
        
    let previous = self.environment.clone();
    self.environment = env;

    // 保证环境在 Err 或 Ok 时都能恢复
    let result = statements.iter().try_for_each(|stmt| self.execute(stmt));

    self.environment = previous;
    result
}
```

类的构造函数以及类方法的调用在类与实例部分说明。

#### 类与实例

##### 类的构造函数

如果是类的构造函数，控制流转向`LoxClass::call(c.clone(), self, args)`，查看`LoxClass`的定义：

```rust
pub struct LoxClass {
    pub name: Token,
    pub superclass: Option<Rc<LoxClass>>,
    pub methods: HashMap<String, Rc<LoxFunction>>,
}

impl LoxClass {
    pub fn find_method(&self, name: &str) -> Option<Rc<LoxFunction>> {
        if let Some(method) = self.methods.get(name) {
            Some(method.clone())
        } else if let Some(ref superclass) = self.superclass {
            superclass.find_method(name)
        } else {
            None
        }
    }

    pub fn call(class_rc: Rc<LoxClass>, interpreter: &mut Interpreter, arguments: Vec<Value>) -> Value {
        let instance = Rc::new(RefCell::new(LoxInstance {
            class: class_rc.clone(),
            fields: HashMap::new(),
        }));

        if let Some(initializer) = class_rc.find_method("init") {
            initializer
                .bind(instance.clone())
                .call(interpreter, arguments);
        }

        Value::Instance(instance)
    }
}
```

`LoxClass::call`首先创建一个新的`LoxInstance`实例，其字段集合为空，记录所属类。接着查找构造函数`init`，在执行`init`前，先调用`LoxFunction::bind`将`this` 绑定为这个新创建的实例，接着调用`LoxFunction::call`执行构造函数逻辑。最后返回实例。

##### 类方法

以`instance.method()`为例说明类方法调用的执行流程。

首先，`instance.method()`会被解析为`Expr::Get{ object, name }`，查看`evaluate`对应部分：

```rust
fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        ...
        Expr::Get { object, name } => {
            let obj = self.evaluate(object)?;
            if let Value::Instance(instance) = obj {
                return instance.borrow().get(name);
            }
            Err(RuntimeError::new("Only instances have properties."))
        }
        ...
    }
}
```

此时调用的是`LoxInstance::get(name)`。查看`LoxInstance`的定义：

```rust
pub struct LoxInstance {
    pub class: Rc<LoxClass>,
    pub fields: HashMap<String, Value>,
}

impl LoxInstance {
    pub fn get(this: &Rc<RefCell<Self>>, name: &str) -> Value {
        if let Some(value) = this.borrow().fields.get(name) {
            return value.clone();
        }
        if let Some(method) = this.borrow().class.find_method(name) {
            return Value::Function(Rc::new(method.bind(this.clone())));
        }
        panic!("Undefined property '{}'", name);
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.fields.insert(name, value);
    }
}
```

`get()`的逻辑是先从字段中查找；找不到再从类及其父类中查找方法；如果找到方法，则调用`bind()`将方法中的`this`绑定为当前实例，返回一个新的`LoxFunction`。

此后执行到`Expr::Call`时，由于已经将实例绑定到`this`，因此可以处理类方法中可能出现的`this`，接下来的执行流程就和普通函数调用一样了，不赘述。

##### 继承

###### 子类重写父类方法

在`LoxClass`的`find_method()`方法中递归向上查找，即可实现优先返回子类重写的方法。

###### Super的处理

在 Lox 中，`super`关键字用于在子类方法中调用其超类的方法。

```rust
fn evaluate(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
    match expr {
        ...
        Expr::Super { keyword, method } => {
            let superclass = self.environment.borrow().get("super").unwrap();
            let object = self.environment.borrow().get("this").unwrap();
            if let (Value::Class(sc), Value::Instance(inst)) = (superclass, object) {
                let method_val = sc.find_method(&method.lexeme).unwrap();
                Value::Function(Rc::new(method_val.bind(inst)))
            } else {
                panic!("super error")
            }
        }
        ...
    }
}
```

`Expr::Super`的处理过程如下：

1. 从当前环境中获取 super 和 this：

    + super 表示当前类的超类（在当前类定义时注入类方法的闭包）。

    + this 是当前绑定的实例对象（参考之前的说明，类方法运行时自动注入环境）。


2. 查找超类中的目标方法：

    `sc.find_method(&method.lexeme)`会在超类及其祖先中查找方法名对应的方法。

3. 找到方法后，绑定当前实例作为方法中可能出现的`this`。最终返回`Value::Function(...)`，供后续`Expr::Call`表达式调用。









