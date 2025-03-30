mod lexer;
use lexer::Lexer;
use std::fs;

fn main() {
    let file_path = "input.txt"; // 你的输入文件

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
    


    for token in tokens {
        println!("{:?}", token);
    }
}