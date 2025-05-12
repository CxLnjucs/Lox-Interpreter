mod lexer;
mod parser;
mod interpreter;
use lexer::Lexer;
use parser::{Parser, DebugAstPrinter};
use interpreter::{Interpreter};
use std::env;
use std::fs;

fn main() {
    // 获取命令行参数
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("用法: {} <输入文件路径>", args[0]);
        return;
    }
    let file_path = &args[1];

    // 读取文件内容
    let input = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("读取文件失败: {}", err);
            return;
        }
    };

    //let input = input.replace("\r", "");

    let mut lexer = Lexer::new(input);
    let tokens = lexer.tokenize();
    for token in &tokens {
        println!("{:?}", token);
    }


    let mut parser = Parser::new(tokens);
    let stmts = parser.parse();

    // parser test
    
    let printer = DebugAstPrinter;
    for stmt in &stmts {
        printer.print_stmt(&stmt, 0);
    }

    let mut interpreter = Interpreter::new();

    interpreter.interpret(&stmts);
}