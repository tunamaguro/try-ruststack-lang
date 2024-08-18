use std::io::Read;

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
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
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

fn expr(input: &str) -> IResult<&str, Expression> {
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

fn var_def(input: &str) -> IResult<&str, Statement> {
    // ここはspace_delimitedだとvara=1みたいなのが許されるのでmultispace1
    let (input, _) = delimited(multispace0, tag("var"), multispace1)(input)?;
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarDef(ident, expr)))
}

fn expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = expr(input)?;
    Ok((input, Statement::Expression(expr)))
}

fn statement(input: &str) -> IResult<&str, Statement> {
    alt((var_def, expr_statement))(input)
}

fn statements(input: &str) -> Result<Statements, nom::error::Error<&str>> {
    let (_, res) = separated_list0(char(';'), statement)(input).finish()?;
    Ok(res)
}

fn unary_fn(f: fn(f64) -> f64) -> impl Fn(Vec<Expression>) -> f64 {
    move |args| {
        f(eval(
            args.into_iter().next().expect("function missing argument"),
        ))
    }
}

fn binary_fn(f: fn(f64, f64) -> f64) -> impl Fn(Vec<Expression>) -> f64 {
    move |args| {
        let mut args = args.into_iter();
        let lhs = eval(args.next().expect("function missing first argument"));
        let rhs = eval(args.next().expect("function missing second argument"));
        f(lhs, rhs)
    }
}

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(ident) => panic!("Unknown name {:?}", ident),
        Expression::NumLiteral(num) => num,
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
        Expression::FnInvoke("sqrt", args) => unary_fn(f64::sqrt)(args),
        Expression::FnInvoke("sin", args) => unary_fn(f64::sin)(args),
        Expression::FnInvoke("cos", args) => unary_fn(f64::cos)(args),
        Expression::FnInvoke("tan", args) => unary_fn(f64::tan)(args),
        Expression::FnInvoke("asin", args) => unary_fn(f64::asin)(args),
        Expression::FnInvoke("acos", args) => unary_fn(f64::acos)(args),
        Expression::FnInvoke("atan", args) => unary_fn(f64::atan)(args),
        Expression::FnInvoke("atan2", args) => binary_fn(f64::atan2)(args),
        Expression::FnInvoke("pow", args) => binary_fn(f64::powf)(args),
        Expression::FnInvoke("exp", args) => unary_fn(f64::exp)(args),
        Expression::FnInvoke("log", args) => binary_fn(f64::log)(args),
        Expression::FnInvoke("log10", args) => unary_fn(f64::log10)(args),
        Expression::FnInvoke(name, _) => panic!("Unknown function {name:?}"),
    }
}

fn main() {
    let mut buf = String::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let parsed_statements = match statements(&buf) {
            Ok(parsed) => parsed,
            Err(e) => {
                eprintln!("Parsed error: {e:?}");
                return;
            }
        };
        println!("{:?}", parsed_statements)
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
