use nom::{
    bytes::complete::{tag, is_not},
    character::complete::{char, multispace0, multispace1},
    sequence::delimited,
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
    PrintlnMacro,
    StringLiteral(String),
    Semicolon,
}

// Define AST nodes
#[derive(Debug, PartialEq)]
enum AstNode {
    Program(Vec<AstNode>), // Contains a list of function declarations
    FunctionDecl {
        name: String,
        body: Vec<Statement>,
    },
    Statement(Statement),
}

#[derive(Debug, PartialEq)]
enum Statement {
    Println(Expression),
}

#[derive(Debug, PartialEq)]
enum Expression {
    StringLiteral(String),
}


fn parse_keyword_fn(input: &str) -> IResult<&str, Token> {
    let (input, _) = tag("fn")(input)?;
    Ok((input, Token::FnKeyword))
}

fn parse_identifier(input: &str) -> IResult<&str, Token> {
    let (input, id) = is_not(" \t\n\r(){};")(input)?;
    Ok((input, Token::Identifier(id.to_string())))
}

fn parse_println(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, _) = tag("println!")(input)?;
    let (input, _) = char('(')(input)?;
    let (input, string_lit) = parse_string_literal(input)?;
    let (input, _) = char(')')(input)?;
    let (input, _) = char(';')(input)?;
    Ok((input, vec![Token::PrintlnMacro, string_lit, Token::Semicolon]))
}

fn parse_string_literal(input: &str) -> IResult<&str, Token> {
    let (input, lit) = delimited(char('"'), is_not("\""), char('"'))(input)?;
    Ok((input, Token::StringLiteral(lit.to_string())))
}

#[allow(dead_code)]
fn parse_semicolon(input: &str) -> IResult<&str, Token> {
    let (input, _) = char(';')(input)?;
    Ok((input, Token::Semicolon))
}

fn parse_function(input: &str) -> IResult<&str, Vec<Token>> {
    let (input, _) = multispace0(input)?;
    let (input, fn_keyword) = parse_keyword_fn(input)?;
    let (input, _) = multispace1(input)?;
    let (input, identifier) = parse_identifier(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("()")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("{")(input)?;
    let (input, _) = multispace0(input)?;
    let (input, println_tokens) = parse_println(input)?;
    let (input, _) = multispace0(input)?;
    let (input, _) = tag("}")(input)?;

    let mut tokens = vec![fn_keyword, identifier];
    tokens.extend(println_tokens);

    Ok((input, tokens))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn tokens_to_ast(tokens: Vec<Token>) -> AstNode {
    let mut body_statements = Vec::new();

    let mut iter = tokens.iter();
    while let Some(token) = iter.next() {
        match token {
            Token::PrintlnMacro => {
                // Directly after PrintlnMacro, expect a StringLiteral
                if let Some(next_token) = iter.next() {
                    if let Token::StringLiteral(text) = next_token {
                        body_statements.push(Statement::Println(Expression::StringLiteral(text.clone())));
                    }
                }
            },
            _ => {}
        }
    }

    AstNode::Program(vec![
        AstNode::FunctionDecl {
            name: "main".to_string(),
            body: body_statements,
        },
    ])
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

    match ast {
        AstNode::Program(functions) => {
            for function in functions {
                if let AstNode::FunctionDecl { name, body } = function {
                    if name == "main" {
                        for statement in body {
                            if let Statement::Println(expr) = statement {
                                if let Expression::StringLiteral(text) = expr {
                                    let printf_type = i32_type.fn_type(&[context.i8_type().ptr_type(inkwell::AddressSpace::from(0)).into()], true);
                                    let printf_func = module.add_function("printf", printf_type, None);
                                    let global_str = builder.build_global_string_ptr(&text, "str").unwrap();
                                    
                                    // Correctly specify the type of elements after the GEP operation
                                    let i8_ptr_type = context.i8_type().ptr_type(inkwell::AddressSpace::from(0));
                                    let indices = [i32_type.const_int(0, false).into(), i32_type.const_int(0, false).into()];
                                    
                                    // Correctly call build_gep with the pointee type
                                    let ptr = unsafe {
                                        builder.build_gep(i8_ptr_type, global_str.as_pointer_value(), &indices, "str_ptr").unwrap()
                                    };
                                    
                                    // Correctly call printf_func
                                    builder.build_call(printf_func, &[ptr.into()], "printf_call").unwrap();
                                }
                            }
                        }
                    }
                }
            }
        },
        _ => {}
    }

    builder.build_return(Some(&i32_type.const_int(0, false)));
}



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
                    println!("Generated LLVM IR:");
                    module.print_to_stderr();
                    
                },
                Err(e) => println!("Error parsing program: {:?}", e),
            }
        },
        Err(e) => println!("Error reading file: {:?}", e),
    }
}

