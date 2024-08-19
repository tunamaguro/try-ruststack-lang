use std::{collections::BTreeMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    Finish, IResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    FnInvoke(&'src str, Vec<Expression<'src>>),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
}

type Statements<'a> = Vec<Statement<'a>>;

fn space_delimited<'src, O, E, F>(f: F) -> impl FnMut(&'src str) -> IResult<&'src str, O, E>
where
    E: ParseError<&'src str>,
    F: Parser<&'src str, O, E>,
{
    delimited(multispace0, f, multispace0)
}

fn number(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, recognize_float, multispace0)(input)?;

    Ok((
        r,
        Expression::NumLiteral(v.parse().map_err(|_| {
            nom::Err::Error(nom::error::Error {
                input,
                code: nom::error::ErrorKind::Digit,
            })
        })?),
    ))
}

fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0(alphanumeric1)))(input)
}

fn ident(input: &str) -> IResult<&str, Expression> {
    let (r, v) = delimited(multispace0, identifier, multispace0)(input)?;

    Ok((r, Expression::Ident(v)))
}

fn paren(input: &str) -> IResult<&str, Expression> {
    space_delimited(delimited(char('('), expr, char(')')))(input)
}

fn func_call(input: &str) -> IResult<&str, Expression> {
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, args) = space_delimited(delimited(
        char('('),
        many0(delimited(
            multispace0,
            expr,
            space_delimited(opt(char(','))),
        )),
        char(')'),
    ))(input)?;

    Ok((input, Expression::FnInvoke(ident, args)))
}

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((number, func_call, ident, paren))(input)
}

fn term(input: &str) -> IResult<&str, Expression> {
    let (input, left) = factor(input)?;
    fold_many0(
        pair(space_delimited(alt((char('*'), char('/')))), factor),
        move || left.clone(),
        |acc, (op, val)| match op {
            '*' => Expression::Mul(Box::new(acc), Box::new(val)),
            '/' => Expression::Div(Box::new(acc), Box::new(val)),
            _ => panic!("Multiplicative expression should have '*' or '/' operator"),
        },
    )(input)
}

fn num_expr(input: &str) -> IResult<&str, Expression> {
    let (input, left) = term(input)?;

    fold_many0(
        pair(space_delimited(alt((char('+'), char('-')))), term),
        move || left.clone(),
        |acc, (op, val)| match op {
            '+' => Expression::Add(Box::new(acc), Box::new(val)),
            '-' => Expression::Sub(Box::new(acc), Box::new(val)),
            _ => panic!("Additive expression should have '+' or '/' operator"),
        },
    )(input)
}

fn open_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('{'))(input)?;
    Ok((input, ()))
}

fn close_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('}'))(input)?;
    Ok((input, ()))
}

fn if_expr(input: &str) -> IResult<&str, Expression> {
    let (input, _) = space_delimited(tag("if"))(input)?;
    let (input, cond) = expr(input)?;
    let (input, true_case) = delimited(open_brace, expr, close_brace)(input)?;
    let (input, _) = space_delimited(tag("else"))(input)?;
    let (input, false_case) = opt(delimited(open_brace, expr, close_brace))(input)?;

    Ok((
        input,
        Expression::If(
            Box::new(cond),
            Box::new(true_case),
            false_case.map(Box::new),
        ),
    ))
}

fn expr(input: &str) -> IResult<&str, Expression> {
    alt((if_expr, num_expr))(input)
}

fn var_def(input: &str) -> IResult<&str, Statement> {
    // ここはspace_delimitedだとvara=1みたいなのが許されるのでmultispace1
    let (input, _) = delimited(multispace0, tag("var"), multispace1)(input)?;
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarDef(ident, expr)))
}

fn var_assign(input: &str) -> IResult<&str, Statement> {
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarAssign(ident, expr)))
}

fn expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = expr(input)?;
    Ok((input, Statement::Expression(expr)))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    alt((var_def, var_assign, expr_statement))(input)
}

fn statements(input: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = separated_list0(char(';'), statement)(input).finish()?;
    Ok(res)
}

type Variables<'a> = BTreeMap<&'a str, f64>;

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(Vec<Expression>, &Variables) -> f64 {
    move |args, variables| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
            variables,
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(Vec<Expression>, &Variables) -> f64 {
    move |args, vars| {
        let mut args = args.into_iter();
        let lhs = eval(args.next().expect("function missing first argument"), vars);
        let rhs = eval(args.next().expect("function missing second argument"), vars);
        f(lhs, rhs)
    }
}

fn eval(expr: Expression, vars: &Variables) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(ident) => *vars.get(ident).expect("Variables not found"),
        Expression::NumLiteral(num) => num,
        Expression::Add(lhs, rhs) => eval(*lhs, vars) + eval(*rhs, vars),
        Expression::Sub(lhs, rhs) => eval(*lhs, vars) - eval(*rhs, vars),
        Expression::Mul(lhs, rhs) => eval(*lhs, vars) * eval(*rhs, vars),
        Expression::Div(lhs, rhs) => eval(*lhs, vars) / eval(*rhs, vars),
        Expression::FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args, vars),
        Expression::FnInvoke("sin", args) => unary_fn(f64::sin)(args, vars),
        Expression::FnInvoke("cos", args) => unary_fn(f64::cos)(args, vars),
        Expression::FnInvoke("tan", args) => unary_fn(f64::tan)(args, vars),
        Expression::FnInvoke("asin", args) => unary_fn(f64::asin)(args, vars),
        Expression::FnInvoke("acos", args) => unary_fn(f64::acos)(args, vars),
        Expression::FnInvoke("atan", args) => unary_fn(f64::atan)(args, vars),
        Expression::FnInvoke("atan2", args) => binary_fn(f64::atan2)(args, vars),
        Expression::FnInvoke("pow", args) => binary_fn(f64::powf)(args, vars),
        Expression::FnInvoke("exp", args) => unary_fn(f64::exp)(args, vars),
        Expression::FnInvoke("log", args) => binary_fn(f64::log)(args, vars),
        Expression::FnInvoke("log10", args) => unary_fn(f64::log10)(args, vars),
        Expression::FnInvoke(name, _) => panic!("Unknown function {name:?}"),
        Expression::If(cond, true_case, false_case) => {
            if (eval(*cond, vars)) != 0.0 {
                eval(*true_case, vars)
            } else {
                false_case.map(|f_case| eval(*f_case, vars)).unwrap_or(0.)
            }
        }
    }
}

fn main() {
    let mut buf = String::new();
    let mut variables = BTreeMap::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let parsed_statements = match statements(&buf) {
            Ok(parsed) => parsed,
            Err(e) => {
                eprintln!("Parsed error: {e:?}");
                return;
            }
        };
        for statement in parsed_statements {
            match statement {
                Statement::Expression(expr) => println!("eval : {:?}", eval(expr, &variables)),
                Statement::VarDef(ident, expr) => {
                    let value = eval(expr, &variables);
                    variables.insert(ident, value);
                }
                Statement::VarAssign(ident, expr) => {
                    if !variables.contains_key(ident) {
                        panic!("Variables is not found")
                    }
                    let value = eval(expr, &variables);
                    variables.insert(ident, value);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{expr, Expression::*};

    #[test]
    fn test_nested_add() {
        let txt = "((10 + 20) + 30 + 40) + 50";
        assert_eq!(
            expr(txt),
            Ok((
                "",
                Add(
                    Box::new(Add(
                        Box::new(Add(
                            Box::new(Add(Box::new(NumLiteral(10.0)), Box::new(NumLiteral(20.0)))),
                            Box::new(NumLiteral(30.0))
                        )),
                        Box::new(NumLiteral(40.0))
                    )),
                    Box::new(NumLiteral(50.0))
                )
            ))
        )
    }
}
