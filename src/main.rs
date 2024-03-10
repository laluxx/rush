use nom::{
    bytes::complete::{tag, is_not},
    character::complete::{char, multispace0, multispace1},
    sequence::{delimited, preceded},
    multi::{many0, separated_list0},
    IResult,
};

use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

extern crate inkwell;
use inkwell::context::Context;



#[derive(Debug, PartialEq)]
enum Token {
    FnKeyword,
    Identifier(String),
    FunctionCall {
        name: String,
        args: Vec<String>, // NOTE Multiple string arguments.
    },
    StringLiteral(String),
    Semicolon,
}


use std::fmt;

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::FnKeyword => write!(f, "FnKeyword"),
            Token::Identifier(id) => write!(f, "Identifier({})", id),
            Token::FunctionCall { name, args } => write!(f, "FunctionCall({}, {:?})", name, args),
            Token::StringLiteral(s) => write!(f, "StringLiteral({})", s),
            Token::Semicolon => write!(f, "Semicolon"),
        }
    }
}



#[derive(Debug, PartialEq)]
enum AstNode {
    Program(Vec<AstNode>),
    FunctionDecl {
        name: String,
        body: Vec<FunctionCall>,
    },
}

#[derive(Debug, PartialEq)]
struct FunctionCall {
    name: String,
    args: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
enum Expression {
    StringLiteral(String),
}


fn parse_function_call(input: &str) -> IResult<&str, Token> {
    let (input, name) = parse_identifier(input)?;
    let (input, _) = char('(')(input)?;
    
    // Parsing arguments inside the function call, separated by commas.
    let (input, args) = separated_list0(char(','), parse_string_literal)(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = char(';')(input)?;

    let args_str = args.into_iter().map(|arg| {
        if let Token::StringLiteral(str_lit) = arg { str_lit } else { String::new() }
    }).collect();

    Ok((input, Token::FunctionCall { name: name.to_string(), args: args_str }))
}

fn parse_function(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("()")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;

    // Collecting function calls within the function body, properly handling whitespace and semicolons.
    let (input, function_calls) = many0(preceded(multispace0, parse_function_call))(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;

    let tokens = vec![Token::FnKeyword, identifier];
    let mut tokens_with_calls = tokens;
    tokens_with_calls.extend(function_calls);

    Ok((input, tokens_with_calls))
}


fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (input, id) = is_not(" \t\n\r(){};")(input)?;
    Ok((input, Token::Identifier(id.to_string())))
}

fn parse_string_literal(input: &str) -> IResult<&str, Token> {
    let (input, lit) = delimited(char('"'), is_not("\""), char('"'))(input)?;
    Ok((input, Token::StringLiteral(lit.to_string())))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn tokens_to_ast(tokens: Vec<Token>) -> AstNode {
    // Assuming only one function (main) for simplicity, but designed for potential expansion
    let mut main_function_body: Vec<FunctionCall> = Vec::new();

    // Iterate over tokens to build function calls
    for token in tokens {
        match token {
            Token::FunctionCall { name, args } => {
                let arguments = args.iter().map(|arg| Expression::StringLiteral(arg.clone())).collect();
                let function_call = FunctionCall { name, args: arguments };
                main_function_body.push(function_call);
            },
            _ => {}
        }
    }

    // Construct the main function node
    let main_function_node = AstNode::FunctionDecl {
        name: "main".to_string(), // Assuming "main" is the entry point
        body: main_function_body,
    };

    // Construct the program node containing the main function
    AstNode::Program(vec![main_function_node])
}



fn generate_ir_from_ast<'ctx>(
    ast: AstNode,
    context: &'ctx Context,
    module: &inkwell::module::Module<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
) {
    let i32_type = context.i32_type();
    let fn_type = i32_type.fn_type(&[], false);
    let main_function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(main_function, "entry");
    builder.position_at_end(basic_block);

    // Assuming the printf function is declared in the module
    let printf_fn = module.get_function("printf").unwrap_or_else(|| {
        let printf_type = i32_type.fn_type(&[context.i8_type().ptr_type(inkwell::AddressSpace::from(0)).into()], true);
        module.add_function("printf", printf_type, None)
    });

    if let AstNode::Program(functions) = ast {
        for function in functions {
            if let AstNode::FunctionDecl { name, body } = function {
                if name == "main" {
                    for function_call in body {
                        match function_call.name.as_str() {
                            // Adjust the match pattern as necessary
                            "Identifier(println)" => {
                                for arg in &function_call.args {
                                    if let Expression::StringLiteral(text) = arg {
                                        let formatted_text = format!("{}\n\0", text); // Ensure null-termination.
                                        let global_str = builder.build_global_string_ptr(&formatted_text, "formatted_str")
                                            .expect("Failed to build global string ptr");

                                        // Directly get the pointer value from the Result
                                        let global_str_ptr = global_str.as_pointer_value();
                                        builder.build_call(printf_fn, &[global_str_ptr.into()], "printf_call")
                                            .expect("Failed to build call to printf");
                                    }
                                }
                            },
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    builder.build_return(Some(&i32_type.const_int(0, false))).expect("Failed to build return");
}




use std::fs;
use std::io::Write;
use std::process::Command;

fn main() {
    let file_path = "./src/hello.rh";
    match read_file(file_path) {
        Ok(program) => {
            match parse_function(&program) {
                Ok((_, tokens)) => {
                    println!("Tokens: {:#?}", tokens);
                    let ast = tokens_to_ast(tokens);
                    println!("Generated AST: {:#?}", ast);

                    // Setup LLVM context, module, and builder
                    let context = Context::create();
                    let module = context.create_module("my_program");
                    let builder = context.create_builder();

                    // Generate IR from AST
                    generate_ir_from_ast(ast, &context, &module, &builder);

                    // Ensure the build directory exists
                    let build_dir = "./build";
                    fs::create_dir_all(build_dir).expect("Failed to create build directory");

                    // Write LLVM IR to file
                    let ir_file_path = Path::new(build_dir).join("ir.ll");
                    let mut file = File::create(&ir_file_path).expect("Failed to create ir.ll file");
                    let ir_string = module.print_to_string().to_string();
                    file.write_all(ir_string.as_bytes()).expect("Failed to write IR to file");

                    println!("Generated LLVM IR written to {:?}", ir_file_path);

                    // Compile the IR to an executable
                    let exe_path = Path::new(build_dir).join("executable");
                    let output = Command::new("clang")
                        .arg(ir_file_path.to_str().unwrap()) // Convert the PathBuf to a string slice
                        .arg("-o")
                        .arg(exe_path.to_str().unwrap()) // Specify the output executable path
                        .output();

                    match output {
                        Ok(output) => {
                            if output.status.success() {
                                println!("Compiled executable successfully.");
                            } else {
                                let stderr = String::from_utf8_lossy(&output.stderr);
                                eprintln!("Failed to compile executable: {}", stderr);
                            }
                        },
                        Err(e) => eprintln!("Failed to execute clang: {}", e),
                    }
                },
                Err(e) => println!("Error parsing program: {:?}", e),
            }
        },
        Err(e) => println!("Error reading file: {:?}", e),
    }
}

