use nom::{
    branch::alt,
    character::complete::{alpha1, alphanumeric1, char, multispace0},
    combinator::recognize,
    error::ParseError,
    multi::{fold_many0, many0},
    number::complete::recognize_float,
    sequence::{delimited, pair},
    IResult, Parser,
};

#[derive(Debug, PartialEq, Clone)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
    Sub(Box<Expression<'src>>, Box<Expression<'src>>),
    Mul(Box<Expression<'src>>, Box<Expression<'src>>),
    Div(Box<Expression<'src>>, Box<Expression<'src>>),
}

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

fn factor(input: &str) -> IResult<&str, Expression> {
    alt((number, ident, paren))(input)
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

fn eval(expr: Expression) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(ident) => panic!("Unknown name {:?}", ident),
        Expression::NumLiteral(num) => num,
        Expression::Add(lhs, rhs) => eval(*lhs) + eval(*rhs),
        Expression::Sub(lhs, rhs) => eval(*lhs) - eval(*rhs),
        Expression::Mul(lhs, rhs) => eval(*lhs) * eval(*rhs),
        Expression::Div(lhs, rhs) => eval(*lhs) / eval(*rhs),
    }
}

fn main() {
    fn ex_eval(input: &str) -> Result<f64, nom::Err<nom::error::Error<&str>>> {
        expr(input).map(|(_, e)| eval(e))
    }
    let input = "123";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
  
    let input = "2 * pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
  
    let input = "(123 + 456 ) + pi";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
  
    let input = "10 - (100 + 1)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
  
    let input = "(3 + 7) / (2 + 3)";
    println!("source: {:?}, parsed: {:?}", input, ex_eval(input));
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
