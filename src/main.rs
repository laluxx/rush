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
    FunctionDecl {
        name: String,
        args: Vec<String>, // For simplicity, assume no arguments for now
        body: Vec<Token>,  // This will contain tokens representing the function body
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
            Token::FunctionDecl { name, args, body } => write!(f, "FunctionDecl({}, {:?})", name, args),
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

fn parse_program(input: &str) -> IResult<&str, Vec<Token>> {
    many0(parse_function)(input)
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

fn parse_function(input: &str) -> IResult<&str, Token> {
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("fn")(input)?;
    let (input, _) = multispace1(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("()")(input)?; // Simplifies assumption: no arguments
    let (input, _) = multispace0(input)?;
    let (input, _) = char('{')(input)?;
    let (input, _) = multispace0(input)?;

    let (input, body) = many0(preceded(multispace0, parse_function_call))(input)?;

    let (input, _) = multispace0(input)?;
    let (input, _) = char('}')(input)?;

    if let Token::Identifier(name) = identifier {
        Ok((input, Token::FunctionDecl {
            name,
            args: vec![], // Simplified: no arguments handled
            body,
        }))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)))
    }
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
    let functions = tokens.into_iter().filter_map(|token| match token {
        Token::FunctionDecl { name, body, .. } => {
            let function_calls: Vec<FunctionCall> = body.into_iter().filter_map(|token| {
                if let Token::FunctionCall { name, args } = token {
                    Some(FunctionCall {
                        name,
                        args: args.into_iter().map(Expression::StringLiteral).collect(),
                    })
                } else { None }
            }).collect();

            Some(AstNode::FunctionDecl { name, body: function_calls })
        },
        _ => None,
    }).collect();

    AstNode::Program(functions)
}

fn generate_ir_from_ast<'ctx>(
    ast: AstNode,
    context: &'ctx Context,
    module: &inkwell::module::Module<'ctx>,
    builder: &inkwell::builder::Builder<'ctx>,
) {
    let i32_type = context.i32_type();
    let void_type = context.void_type();
    let printf_fn_type = i32_type.fn_type(&[context.i8_type().ptr_type(inkwell::AddressSpace::from(0)).into()], true);
    let printf_fn = module.add_function("printf", printf_fn_type, None);

    // AstNode::Program contains a Vec<AstNode> of FunctionDecl
    if let AstNode::Program(function_decls) = ast {
        // Declare functions first
        for function_decl in &function_decls {
            if let AstNode::FunctionDecl { ref name, .. } = function_decl {
                let fn_type = match name.as_str() {
                    "main" => i32_type.fn_type(&[], false),
                    _ => void_type.fn_type(&[], false),
                };
                module.add_function(name, fn_type, None);
            }
        }

        // Generate function bodies
        for function_decl in function_decls {
            if let AstNode::FunctionDecl { name, body } = function_decl {
                let llvm_function = module.get_function(&name).expect("Function not found");
                let entry = context.append_basic_block(llvm_function, "entry");
                builder.position_at_end(entry);

                for function_call in body {
                    let FunctionCall { name, args } = function_call;
                    match name.as_str() {
                        // Adjust based on your FunctionCall structure and naming
                        "Identifier(println)" => {
                            for Expression::StringLiteral(text) in args {
                                let formatted_text = format!("{}\n\0", text);
                                let global_str = builder.build_global_string_ptr(&formatted_text, "str")
                                                        .expect("Failed to build global string pointer");
                                builder.build_call(printf_fn, &[global_str.as_pointer_value().into()], "printf_call")
                                       .expect("Failed to build call to printf");
                            }
                        },
                        _ => {
                            let called_function = module.get_function(&name.trim_start_matches("Identifier(").trim_end_matches(")"))
                                                         .expect("Called function not found");
                            builder.build_call(called_function, &[], "").expect("Failed to build call to function");
                        },
                    }
                }

                if name == "main" {
                    builder.build_return(Some(&i32_type.const_int(0, false)));
                } else {
                    builder.build_return(None);
                }
            }
        }
    }
}


use std::fs;
use std::io::Write;
use std::process::Command;

fn main() {
    let file_path = "./src/hello.rh";
    let program = match read_file(file_path) {
        Ok(program) => program,
        Err(e) => {
            println!("Error reading file: {:?}", e);
            return;
        }
    };

    let tokens = match parse_program(&program) {
        Ok((_, tokens)) => tokens,
        Err(e) => {
            println!("Error parsing program: {:?}", e);
            return;
        }
    };

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
    let build_dir = Path::new("./build");
    fs::create_dir_all(build_dir).expect("Failed to create build directory");

    // Write LLVM IR to file and compile it
    generate_and_write_ir(&context, &module, build_dir);
}


fn compile_to_executable(ir_file_path: &Path, exe_path: &Path) {
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
}

fn generate_and_write_ir(context: &Context, module: &inkwell::module::Module, build_dir: &Path) {
    let ir_file_path = build_dir.join("ir.ll");
    let mut file = File::create(&ir_file_path).expect("Failed to create ir.ll file");
    let ir_string = module.print_to_string().to_string();
    file.write_all(ir_string.as_bytes()).expect("Failed to write IR to file");
    println!("Generated LLVM IR written to {:?}", ir_file_path);

    // Compile the IR to an executable
    let exe_path = build_dir.join("executable");
    compile_to_executable(&ir_file_path, &exe_path);
}

