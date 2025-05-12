use std::fmt;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use crate::lexer::{TokenType, Token};
use crate::parser::{Stmt, Expr, Object};

#[derive(Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil,
    Function(Rc<LoxFunction>),
    Class(Rc<LoxClass>),
    Instance(Rc<RefCell<LoxInstance>>),
}

#[derive(Clone)]
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

#[derive(Clone)]
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

#[derive(Clone)]
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

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            Value::Number(n) => write!(f, "{}", n),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Function(_) => write!(f, "<fn>"),
            Value::Class(_) => write!(f, "<class>"),
            Value::Instance(_) => write!(f, "<instance>"),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            //(Value::Instance(a), Value::Instance(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct ReturnSignal {
    pub value: Value,
}

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

pub struct Interpreter {
    pub environment: Rc<RefCell<Environment>>,
    globals: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::new(Environment::new()));
        Self {
            environment: globals.clone(),
            globals,
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) {
        for stmt in statements {
            self.execute(stmt);
        }
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), ReturnSignal> {
        match stmt {
            Stmt::Block { statements } => {
                let new_env = Rc::new(RefCell::new(Environment::new_enclosed(self.environment.clone())));
                self.execute_block(statements, new_env)
            }
            Stmt::Expression { expression } => {
                self.evaluate(expression);
                Ok(()) 
            }
            Stmt::Print { expression } => {
                let val = self.evaluate(expression);
                println!("{:?}", val);
                Ok(())
            }
            Stmt::Var { name, initializer } => {
                let value = initializer.as_ref().map(|e| self.evaluate(e)).unwrap_or(Value::Nil);
                self.environment.borrow_mut().define(&name.lexeme, value);
                Ok(())
            }
            Stmt::If { condition, then_branch, else_branch } => {
                if is_truthy(&self.evaluate(condition)) {
                    self.execute(then_branch)?;
                } else if let Some(else_branch) = else_branch {
                    self.execute(else_branch)?;
                }
                Ok(())
            }
            Stmt::While { condition, body } => {
                while is_truthy(&self.evaluate(condition)) {
                    self.execute(body)?;
                }
                Ok(())
            }
            Stmt::For { initializer, condition, increment, body } => {
                let new_env = Rc::new(RefCell::new(Environment::new_enclosed(self.environment.clone())));
                self.environment = new_env.clone();
                if let Some(init) = initializer {
                    self.execute(init)?;
                }
                while {
                    // 判断循环条件（如果没有则默认为 true）
                    condition.as_ref().map(|c| is_truthy(&self.evaluate(c))).unwrap_or(true)
                } {
                    self.execute(body)?;
                    if let Some(inc) = increment {
                        self.evaluate(inc);
                    }
                }
                self.environment = new_env.borrow().enclosing.clone().unwrap();
                Ok(())
            }
            Stmt::Function { name, params, body } => {
                let function = LoxFunction {
                    name: name.clone(),
                    params: params.clone(),
                    body: body.clone(),
                    closure: self.environment.clone(),
                    is_initializer: false,
                };
                self.environment.borrow_mut().define(
                    &name.lexeme,
                    Value::Function(Rc::new(function)),
                );
                Ok(())
            }
            Stmt::Return { value, .. } => {
                let result = value.as_ref().map(|e| self.evaluate(e)).unwrap_or(Value::Nil);
                Err(ReturnSignal { value: result })
            }
            Stmt::Class { name, superclass, methods } => {
                let sc = if let Some(sc_token) = superclass {
                    match self.environment.borrow().get(&sc_token.lexeme) {
                        Some(Value::Class(class)) => Some(class.clone()),
                        _ => panic!("Superclass must be a class.")
                    }
                } else {
                    None
                };
                
                if let Some(sc_rc) = &sc {
                    self.environment = Rc::new(RefCell::new(Environment::new_enclosed(self.environment.clone())));
                    self.environment.borrow_mut().define("super", Value::Class(sc_rc.clone()));
                }

                let mut methods_map = HashMap::new();
                for method in methods {
                    if let Stmt::Function { name: mname, params, body } = method {
                        // 以当前环境为闭包，创建 LoxFunction（尚未绑定 this）
                        let function = LoxFunction {
                            name: mname.clone(),
                            params: params.clone(),
                            body: body.clone(),
                            closure: self.environment.clone(),
                            // 如果方法名是 "init"，标记构造器
                            is_initializer: mname.lexeme == "init",
                        };
                        methods_map.insert(mname.lexeme.clone(), Rc::new(function));
                    }
                }

                let klass = LoxClass {
                    name: name.clone(),
                    superclass: sc.clone(),
                    methods: methods_map,
                };

                if sc.is_some() {
                    let enclosing = self.environment.borrow().enclosing.clone().unwrap();
                    self.environment = enclosing;
                }

                self.environment.borrow_mut().define(&name.lexeme, Value::Class(Rc::new(klass)));
                Ok(())
            }
            _ => unimplemented!("Stmt {:?} not yet supported", stmt),
        }
    }

    pub fn evaluate(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Literal { value } => match value {
                Some(Object::String(s)) => Value::String(s.clone()),
                Some(Object::Number(n)) => Value::Number(*n),
                Some(Object::Boolean(b)) => Value::Boolean(*b),
                _ => Value::Nil,
            },
            Expr::Grouping { expression } => self.evaluate(expression),
            Expr::Unary { operator, right } => {
                let right = self.evaluate(right);
                match operator.token_type {
                    TokenType::Minus => if let Value::Number(n) = right { Value::Number(-n) } else { panic!("Expect number.") },
                    TokenType::Not => Value::Boolean(!is_truthy(&right)),
                    _ => panic!("Unknown unary operator")
                }
            }
            Expr::Binary { left, operator, right } => {
                let l = self.evaluate(left);
                let r = self.evaluate(right);
                match operator.token_type {
                    TokenType::Plus => match (l, r) {
                        (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
                        (Value::String(a), Value::String(b)) => Value::String(a + &b),
                        _ => panic!("Operands must be same type"),
                    },
                    TokenType::Minus => math_binop(l, r, |a, b| a - b),
                    TokenType::Star => math_binop(l, r, |a, b| a * b),
                    TokenType::Div => math_binop(l, r, |a, b| a / b),
                    TokenType::Greater => cmp_binop(l, r, |a, b| a > b),
                    TokenType::GreaterEqual => cmp_binop(l, r, |a, b| a >= b),
                    TokenType::Less => cmp_binop(l, r, |a, b| a < b),
                    TokenType::LessEqual => cmp_binop(l, r, |a, b| a <= b),
                    TokenType::Equal => Value::Boolean(l == r),
                    TokenType::NotEqual => Value::Boolean(l != r),
                    _ => panic!("Unsupported binary operator")
                }
            }
            Expr::Logical { left, operator, right } => {
                let l = self.evaluate(left);
                match operator.token_type {
                    TokenType::Or => {
                        if is_truthy(&l) {
                            l
                        } else {
                            self.evaluate(right)
                        }
                    }
                    TokenType::And => {
                        if !is_truthy(&l) {
                            l
                        } else {
                            self.evaluate(right)
                        }
                    }
                    _ => panic!("Unsupported logical operator: {:?}", operator.token_type),
                }
            }
            Expr::Variable { name } => self.environment.borrow().get(&name.lexeme).unwrap_or(Value::Nil),// TODO:后面改成报错
            Expr::Assign { name, value } => {
                let val = self.evaluate(value);
                self.environment.borrow_mut().assign(&name.lexeme, val.clone()).unwrap();
                val
            }
            Expr::Call { callee, arguments, .. } => {
                let callee_val = self.evaluate(callee);
                let args: Vec<Value> = arguments.iter().map(|arg| self.evaluate(arg)).collect();
                match callee_val {
                    Value::Function(f) => f.call(self, args),
                    Value::Class(c) => LoxClass::call(c.clone(), self, args),
                    _ => panic!("Can only call functions or classes")
                }
            }
            Expr::This { keyword } => self.environment.borrow().get(&keyword.lexeme).unwrap_or(Value::Nil),
            Expr::Get { object, name } => {
                let obj = self.evaluate(object);
                if let Value::Instance(inst) = obj {
                    LoxInstance::get(&inst, &name.lexeme)
                } else {
                    panic!("Only instances have properties.")
                }
            }
            Expr::Set { object, name, value } => {
                let obj = self.evaluate(object);
                let val = self.evaluate(value);
                if let Value::Instance(inst) = obj {
                    inst.borrow_mut().set(name.lexeme.clone(), val.clone());
                    val
                } else {
                    panic!("Only instances have fields.")
                }
            }
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
            _ => unimplemented!("Expr {:?} not yet implemented", expr)
        }
    }

    pub fn execute_block(&mut self, statements: &[Stmt], env: Rc<RefCell<Environment>>) -> Result<(), ReturnSignal>{
        
        let previous = self.environment.clone();
        self.environment = env;

        // 保证环境在 Err 或 Ok 时都能恢复
        let result = statements.iter().try_for_each(|stmt| self.execute(stmt));

        self.environment = previous;
        result
    }
}

fn is_truthy(val: &Value) -> bool {
    match val {
        Value::Boolean(b) => *b,
        Value::Nil => false,
        _ => true,
    }
}

fn math_binop(a: Value, b: Value, op: impl Fn(f64, f64) -> f64) -> Value {
    if let (Value::Number(x), Value::Number(y)) = (a, b) {
        Value::Number(op(x, y))
    } else {
        panic!("Operands must be numbers")
    }
}

fn cmp_binop(a: Value, b: Value, op: impl Fn(f64, f64) -> bool) -> Value {
    if let (Value::Number(x), Value::Number(y)) = (a, b) {
        Value::Boolean(op(x, y))
    } else {
        panic!("Operands must be numbers")
    }
}
