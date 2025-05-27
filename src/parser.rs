use std::{collections::VecDeque, process};

use crate::lexer::Token;



#[derive(Debug, Clone)]
pub struct Program {
    pub function_declarations: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub block_items: Vec<BlockItem>
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: String,
    pub params: Vec<String>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub enum Declaration {
    Function(FunctionDeclaration),
    Variable(VariableDeclaration),
}

#[derive(Debug, Clone)]
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    Increment { postfix: bool },
    Decrement { postfix: bool },
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,
    LogicalAnd,
    LogicalOr,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub enum AssignmentOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Const(i32),
    Var(String),
    Unary {
        operator: UnaryOperator,
        inner_expression: Box<Expression>
    },
    Binary {
        operator: BinaryOperator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assignment(Option<AssignmentOperator>, Box<Expression>, Box<Expression>),
    Conditional {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        _else: Box<Expression>,
    },
    FunctionCall {
        identifier: String,
        args: Vec<Expression>,
    },
}

#[derive(Debug, Clone)]
pub enum Statement {
    Return(Expression),
    If {
        condition: Expression,
        then: Box<Statement>,
        _else: Option<Box<Statement>>,
    },
    Expression(Expression),
    Label(String, Box<Statement>),
    Goto(String),
    Compound(Block),
    Case(Expression, String),
    Default(String),
    Switch {
        control_expression: Expression,
        body: Block,
        label: String,
        cases: Option<Vec<Expression>>,
        default: bool,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
        label: String,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
        label: String,
    },
    For {
        init: ForInitializer,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
        label: String,
    },
    Break(String),
    Continue(String),
    Null,
}

#[derive(Debug, Clone)]
pub enum ForInitializer {
    Declaration(VariableDeclaration),
    Expression(Option<Expression>)
}

type Tokens = VecDeque<Token>;

pub fn parse(tokens: Vec<Token>) -> Program {
    let mut tokens_deque: Tokens = Tokens::from(tokens.iter().cloned().collect::<Vec<_>>());
    let program = parse_program(&mut tokens_deque);

    expect_token(&mut tokens_deque, Token::Eof);

    return program;
}

fn parse_program(tokens: &mut Tokens) -> Program {
    let mut functions = Vec::<FunctionDeclaration>::new();

    while !tokens.is_empty() && *tokens.front().expect("Expect token") != Token::Eof {
        let Declaration::Function(function_declaration) = parse_declaration(tokens) else {
            println!("Variable declarations can't appear at top level!");
            process::exit(2);
        };

        functions.push(function_declaration);
    }

    return Program {
        function_declarations: functions,
    }
}

fn parse_block(tokens: &mut Tokens) -> Block {
    expect_token(tokens, Token::LeftBrace);

    let mut block_items = Vec::<BlockItem>::new();

    let mut next_token = tokens.front().unwrap();
    while *next_token != Token::RightBrace {
        let block_item = parse_block_item(tokens);

        if let BlockItem::Declaration(_) = block_item {
            if matches!(block_items.last(), Some(BlockItem::Statement(Statement::Case(_, _))))  {
                println!("Declaration right after case isn't allowed!");
                process::exit(41);
            }
        }

        block_items.push(block_item);
        next_token = tokens.front().unwrap();
    }

    expect_token(tokens, Token::RightBrace);

    return Block {
        block_items,
    }
}

fn parse_block_item(tokens: &mut Tokens) -> BlockItem {
    println!("Parsing block item");
    let next_token = tokens.front().expect("Parser | Expected token but didn't get one");
    return match next_token {
        Token::Int => BlockItem::Declaration(parse_declaration(tokens)),
        _ => BlockItem::Statement(parse_statement(tokens))
    }
}

fn parse_for_initializer(tokens: &mut Tokens) -> ForInitializer {
    let next_token = tokens.front().expect("Parser | Expected token but didn't get one");
    return match next_token {
        Token::Int => {
            let Declaration::Variable(declaration) = parse_declaration(tokens) else {
                println!("for initializer must be a variable declaration not a function declaration");
                process::exit(9);
            };

            ForInitializer::Declaration(declaration)
        },
        Token::Semicolon => {
            expect_token(tokens, Token::Semicolon);
            ForInitializer::Expression(None)
        }
        _ => {
            let expr = parse_expression(tokens, 0);
            expect_token(tokens, Token::Semicolon);
            ForInitializer::Expression(Some(expr))
        },
    }
}

fn parse_declaration(tokens: &mut Tokens) -> Declaration {
    expect_token(tokens, Token::Int);
    let identifier = parse_identifier(tokens);

    let next_token_opt = tokens.front().expect("Expected token but didn't have one");
    return match next_token_opt {
        Token::LeftParen => {
            let params = parse_function_param_list(tokens);
            let body = if let Some(Token::Semicolon) = tokens.front() {
                expect_token(tokens, Token::Semicolon);
                None
            } else {
                Some(parse_block(tokens))
            };

            Declaration::Function(FunctionDeclaration {
                name: identifier,
                params: params.iter().map(|param| param.1.clone()).collect(),
                body
            })
        },
        Token::Assign => {
            expect_token(tokens, Token::Assign);
            let init_expr = Some(parse_expression(tokens, 0));
            expect_token(tokens, Token::Semicolon);

            Declaration::Variable(VariableDeclaration {
                name: identifier,
                initializer: init_expr,
            })
        },
        Token::Semicolon => {
            expect_token(tokens, Token::Semicolon);

            Declaration::Variable(VariableDeclaration {
                name: identifier,
                initializer: None,
            })
        },
        _ => unreachable!()
    }
}

fn parse_function_param_list(tokens: &mut Tokens) -> Vec<(String, String)> {
    let mut params = Vec::<(String, String)>::new();

    expect_token(tokens, Token::LeftParen);

    if let Some(Token::Void) = tokens.front() {
        expect_token(tokens, Token::Void);
    } else {
        loop {
            expect_token(tokens, Token::Int);
            let identifier = parse_identifier(tokens);
            params.push(("int".to_string(), identifier));

            if *tokens.front().unwrap() != Token::Comma {
                break
            }

            expect_token(tokens, Token::Comma);
        }
    }

    expect_token(tokens, Token::RightParen);

    return params;
}

fn parse_identifier(tokens: &mut Tokens) -> String {
    let token_option = tokens.pop_front();
    if let Some(Token::Identifier(value)) = token_option {
        return value
    }

    println!("! Couldn't parse identifier instead -> {:?}", token_option);
    process::exit(4);
}

fn parse_argument_list(tokens: &mut Tokens) -> Vec<Expression> {
    expect_token(tokens, Token::LeftParen);

    let mut args = Vec::<Expression>::new();
    while *tokens.front().expect("Expected token") != Token::RightParen {
        let expr = parse_expression(tokens, 0);
        args.push(expr);
        if *tokens.front().expect("Expected token") != Token::RightParen {
            expect_token(tokens, Token::Comma);
            if let Some(Token::RightParen) = tokens.front() {
                println!("No trailing comma in argument list");
                process::exit(2);
            }
        }
    }

    expect_token(tokens, Token::RightParen);

    return args;
}

fn parse_factor_expression(tokens: &mut Tokens) -> Expression {
    let next_token = tokens.front().expect("Parser | Expect token but didn't have one");
    println!("{:?}", next_token);
    let expression = match next_token {
        Token::Const(_) => {
            Expression::Const(parse_int(tokens))
        },
        Token::IncrementOp | Token::DecrementOp => Expression::Unary {
            operator: parse_unary_operator(tokens, false),
            inner_expression: Box::new(parse_factor_expression(tokens))
        },
        Token::Identifier(_) => {
            let identifier = parse_identifier(tokens);
            if let Some(Token::LeftParen) = tokens.front() {
                let args = parse_argument_list(tokens);
                return Expression::FunctionCall {
                    identifier: identifier.clone(),
                    args,
                }
            }

            Expression::Var(identifier)
        },
        Token::ComplementOp | Token::MinusOp | Token::LogicalNot => Expression::Unary {
            operator: parse_unary_operator(tokens, false),
            inner_expression: Box::new(parse_factor_expression(tokens))
        },
        Token::LeftParen => {
            expect_token(tokens, Token::LeftParen);
            let expr = parse_expression(tokens, 0);
            expect_token(tokens, Token::RightParen);

            expr
        },
        _ => {
            println!("parse_factor next_token = {:?}", next_token);
            process::exit(58)
        },
    };

    if let Some(Token::IncrementOp | Token::DecrementOp) = tokens.front() {
        return Expression::Unary { operator: parse_unary_operator(tokens, true), inner_expression: Box::new(expression) }
    }

    expression
}

fn parse_expression(tokens: &mut Tokens, min_prec: i32) -> Expression {
    let mut left_expr = parse_factor_expression(tokens);

    let mut next_token = tokens.front().cloned().expect("Parser | Expect token but didn't have one.");
    while is_binary_operator(&next_token) && precedence(&next_token) >= min_prec {
        if is_assignment_operator(&next_token) {
            let assignment_operator = parse_assignment_operator(tokens);
            let right_expr = parse_expression(tokens, precedence(&next_token));
            left_expr = Expression::Assignment(assignment_operator, Box::new(left_expr), Box::new(right_expr));
        } else if next_token == Token::QuestionMark {
            let middle = parse_conditional_middle(tokens);
            let right_expr = parse_expression(tokens, precedence(&next_token));
            left_expr = Expression::Conditional { condition: Box::new(left_expr.clone()), if_true: Box::new(middle), _else: Box::new(right_expr) }
        } else {
            let operator = parse_binary_operator(tokens);
            let right_expr = parse_expression(tokens, precedence(&next_token) + 1);
            left_expr = Expression::Binary { operator, left: Box::new(left_expr), right: Box::new(right_expr) };
        }
        next_token = tokens.front().cloned().expect("Parser | Expect next token but didn't have one.");
    }

    return left_expr
}

fn parse_conditional_middle(tokens: &mut Tokens) -> Expression {
    expect_token(tokens, Token::QuestionMark);
    let expr = parse_expression(tokens, 0);
    expect_token(tokens, Token::Colon);

    return expr;
}

fn is_assignment_operator(token: &Token) -> bool {
    return matches!(
        token,
        Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
    )
}

fn precedence(token: &Token) -> i32 {
    return match token {
        Token::MultiplyOp | Token::DivideOp | Token::RemainderOp => 50,
        Token::PlusOp | Token::MinusOp => 45,
        Token::BitwiseLeftShift | Token::BitwiseRightShift => 40,
        Token::LessThan | Token::LessThanOrEqual | Token::GreaterThan | Token::GreaterThanOrEqual => 35,
        Token::Equal | Token::NotEqual => 34,
        Token::BitwiseAnd => 32,
        Token::BitwiseXor => 30,
        Token::BitwiseOr => 28,
        Token::LogicalAnd => 10,
        Token::LogicalOr => 5,
        Token::QuestionMark => 3,
        Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
        => 1,
        _ => process::exit(2),
    }
}

fn parse_unary_operator(tokens: &mut Tokens, postfix: bool) -> UnaryOperator {
    let operator_token = tokens.pop_front().expect("Parser | Expect token but didn't get one.");
    return match operator_token {
        Token::ComplementOp => UnaryOperator::Complement,
        Token::MinusOp => UnaryOperator::Negate,
        Token::LogicalNot => UnaryOperator::Not,
        Token::IncrementOp => UnaryOperator::Increment { postfix },
        Token::DecrementOp => UnaryOperator::Decrement { postfix },
        _ => process::exit(2),
    }
}

fn is_binary_operator(token: &Token) -> bool {
    return match token {
        Token::MinusOp
        | Token::PlusOp
        | Token::MultiplyOp
        | Token::DivideOp
        | Token::RemainderOp
        | Token::BitwiseAnd
        | Token::BitwiseXor
        | Token::BitwiseOr
        | Token::BitwiseLeftShift
        | Token::BitwiseRightShift
        | Token::LogicalAnd
        | Token::LogicalOr
        | Token::Equal
        | Token::NotEqual
        | Token::LessThan
        | Token::LessThanOrEqual
        | Token::GreaterThan
        | Token::GreaterThanOrEqual
        | Token::Assign
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::MultiplyAssign
        | Token::DivideAssign
        | Token::RemainderAssign
        | Token::LeftShiftAssign
        | Token::RightShiftAssign
        | Token::BitwiseAndAssign
        | Token::BitwiseOrAssign
        | Token::BitwiseXorAssign
        | Token::QuestionMark
        => true,
        _ => false,
    }
}

fn parse_assignment_operator(tokens: &mut Tokens) -> Option<AssignmentOperator> {
    let assignment_token = tokens.pop_front().unwrap(); // todo remove unwrap
    match assignment_token {
        Token::Assign => None,
        Token::PlusAssign => Some(AssignmentOperator::Plus),
        Token::MinusAssign => Some(AssignmentOperator::Minus),
        Token::MultiplyAssign => Some(AssignmentOperator::Multiply),
        Token::DivideAssign => Some(AssignmentOperator::Divide),
        Token::RemainderAssign => Some(AssignmentOperator::Remainder),
        Token::LeftShiftAssign => Some(AssignmentOperator::LeftShift),
        Token::RightShiftAssign => Some(AssignmentOperator::RightShift),
        Token::BitwiseAndAssign => Some(AssignmentOperator::BitwiseAnd),
        Token::BitwiseOrAssign => Some(AssignmentOperator::BitwiseOr),
        Token::BitwiseXorAssign => Some(AssignmentOperator::BitwiseXor),
        _ => process::exit(890),
    }
}

fn parse_binary_operator(tokens: &mut Tokens) -> BinaryOperator {
    let operator_token = tokens.pop_front().expect("Parser | Expect token but didn't get one.");
    return match operator_token {
        Token::MinusOp => BinaryOperator::Subtract,
        Token::PlusOp => BinaryOperator::Add,
        Token::MultiplyOp => BinaryOperator::Multiply,
        Token::DivideOp => BinaryOperator::Divide,
        Token::RemainderOp => BinaryOperator::Remainder,
        Token::BitwiseAnd => BinaryOperator::BitwiseAnd,
        Token::BitwiseXor => BinaryOperator::BitwiseXor,
        Token::BitwiseOr => BinaryOperator::BitwiseOr,
        Token::BitwiseLeftShift => BinaryOperator::BitwiseLeftShift,
        Token::BitwiseRightShift => BinaryOperator::BitwiseRightShift,
        Token::LogicalAnd => BinaryOperator::LogicalAnd,
        Token::LogicalOr => BinaryOperator::LogicalOr,
        Token::Equal => BinaryOperator::Equal,
        Token::NotEqual => BinaryOperator::NotEqual,
        Token::LessThan => BinaryOperator::LessThan,
        Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
        Token::GreaterThan => BinaryOperator::GreaterThan,
        Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
        _ => process::exit(5),
    }
}

fn parse_int(tokens: &mut Tokens) -> i32 {
    let token_option = tokens.pop_front();
    if let Some(Token::Const(int_value)) = token_option {
        return int_value
    }

    process::exit(8);
}

fn parse_statement(tokens: &mut Tokens) -> Statement {
    let next_token = tokens.front().cloned().expect("Parser | Expected token but didn't get one.");

    if next_token == Token::Case {
        expect_token(tokens, Token::Case);
        let expr = parse_expression(tokens, 0);
        expect_token(tokens, Token::Colon);

        return Statement::Case(expr, "case".to_string());
    }

    if next_token == Token::Default {
        expect_token(tokens, Token::Default);
        expect_token(tokens, Token::Colon);

        return Statement::Default("case".to_string());
    }

    if next_token == Token::Switch {
        expect_token(tokens, Token::Switch);
        expect_token(tokens, Token::LeftParen);

        let expr = parse_expression(tokens, 0);

        expect_token(tokens, Token::RightParen);

        let body: Block = if let Some(Token::LeftBrace) = tokens.front() {
            parse_block(tokens)
        } else {
            Block { block_items: vec![parse_block_item(tokens)] }
        };

        return Statement::Switch {
            control_expression: expr,
            body,
            label: "switch".to_string(),
            cases: None,
            default: false,
        }
    }


    if next_token == Token::Return {
        expect_token(tokens, Token::Return);
        let return_value = parse_expression(tokens, 0);
        expect_token(tokens, Token::Semicolon);
        return Statement::Return(return_value)
    }

    if next_token == Token::LeftBrace {
        let block = parse_block(tokens);
        return Statement::Compound(block);
    }

    if next_token == Token::Goto {
        expect_token(tokens, Token::Goto);
        let label = parse_identifier(tokens);
        expect_token(tokens, Token::Semicolon);

        return Statement::Goto(label)
    }

    if next_token == Token::Break {
        expect_token(tokens, Token::Break);
        expect_token(tokens, Token::Semicolon);

        return Statement::Break("break".to_string())
    }

    if next_token == Token::Continue {
        expect_token(tokens, Token::Continue);
        expect_token(tokens, Token::Semicolon);

        return Statement::Continue("continue".to_string())
    }

    if next_token == Token::While {
        expect_token(tokens, Token::While);
        expect_token(tokens, Token::LeftParen);

        let condition = parse_expression(tokens, 0);

        expect_token(tokens, Token::RightParen);

        let body = parse_statement(tokens);

        return Statement::While {
            condition,
            body: Box::new(body),
            label: "while".to_string(),
        }
    }

    if next_token == Token::Do {
        expect_token(tokens, Token::Do);

        let body = parse_statement(tokens);

        expect_token(tokens, Token::While);
        expect_token(tokens, Token::LeftParen);

        let condition = parse_expression(tokens, 0);

        expect_token(tokens, Token::RightParen);
        expect_token(tokens, Token::Semicolon);
        return Statement::DoWhile {
            body: Box::new(body),
            condition,
            label: "do_while".to_string(),
        }
    }

    if next_token == Token::For {
        println!("parsing for:");
        expect_token(tokens, Token::For);
        expect_token(tokens, Token::LeftParen);
        println!("  consumed 'for (");

        let for_init = parse_for_initializer(tokens);

        println!("  for_init = {:?}", for_init);
        println!("  next_token = {:?}", tokens.front());

        let mut condition = None;
        if !matches!(tokens.front(), Some(Token::Semicolon)) {
            condition = Some(parse_expression(tokens, 0));
        }

        println!("  condition = {:?}", condition);

        expect_token(tokens, Token::Semicolon);
        println!("  consumed )");
        println!("  next_token = {:?}", tokens.front());

        let mut post = None;
        if !matches!(tokens.front(), Some(Token::RightParen)) {
            post = Some(parse_expression(tokens, 0));
        }

        println!("  post = {:?}", condition);

        expect_token(tokens, Token::RightParen);

        let body = parse_statement(tokens);
        println!("  body = {:?}", body);

        return Statement::For {
            init: for_init,
            condition,
            post,
            body: Box::new(body),
            label: "for".to_string(),
        }
    }

    if let Token::Identifier(_) = next_token {
        let label = parse_identifier(tokens);
        let next_token = tokens.front();
        if let Some(Token::Colon) = next_token {
            expect_token(tokens, Token::Colon);
            let statement = parse_statement(tokens);
            return Statement::Label(label, Box::new(statement));
        } else {
            tokens.push_front(Token::Identifier(label));
        }
    }

    if next_token == Token::Semicolon {
        expect_token(tokens, Token::Semicolon);
        return Statement::Null;
    }

    if next_token == Token::If {
        expect_token(tokens, Token::If);
        expect_token(tokens, Token::LeftParen);
        let condition = parse_expression(tokens, 0);
        expect_token(tokens, Token::RightParen);
        let then = Box::new(parse_statement(tokens));

        println!("here should be the else but {:?}", tokens.front());
        let mut _else = None;
        if let Some(Token::Else) = tokens.front() {
            expect_token(tokens, Token::Else);
            _else = Some(Box::new(parse_statement(tokens)));
        }

        return Statement::If { condition, then, _else }
    }

    let expression = parse_expression(tokens, 0);
    expect_token(tokens, Token::Semicolon);

    return Statement::Expression(expression);
}

fn is_typename(token: &Token) -> bool {
    return match *token {
        Token::Int => true,
        _ => false,
    }
}

fn expect_token(tokens: &mut Tokens, expected: Token) -> Token {
    println!("expecting token: {:?}", expected);
    let token_option = tokens.pop_front();
    if token_option.as_ref().is_none_or(|token| *token != expected) {
        println!("Expected token {:?} but got {:?}", expected, token_option);
        println!("Remaining tokens: {:?}", tokens);
        process::exit(9);
    }

    return token_option.unwrap();
}
