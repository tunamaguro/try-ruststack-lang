#[derive(Debug, PartialEq)]
enum Expression<'src> {
    Ident(&'src str),
    NumLiteral(f64),
    Add(Box<Expression<'src>>, Box<Expression<'src>>),
}

fn advance_char(input: &str) -> &str {
    let mut chars = input.chars();
    chars.next();
    chars.as_str()
}

fn peek_char(input: &str) -> Option<char> {
    input.chars().next()
}

fn whitespace(mut input: &str) -> &str {
    while matches!(peek_char(input), Some(' ')) {
        input = advance_char(input);
    }
    input
}

fn number(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
    };

    if let Ok(num) = start[..start.len() - input.len()].parse::<f64>() {
        Some((input, Expression::NumLiteral(num)))
    } else {
        None
    }
}

fn ident(mut input: &str) -> Option<(&str, Expression)> {
    let start = input;
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input)
        }
        Some((
            input,
            Expression::Ident(&start[..start.len() - input.len()]),
        ))
    } else {
        None
    }
}

fn lparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('(')) {
        input = advance_char(input);
        Some(input)
    } else {
        None
    }
}

fn rparen(mut input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some(')')) {
        input = advance_char(input);
        Some(input)
    } else {
        None
    }
}

fn plus(input: &str) -> Option<&str> {
    if matches!(peek_char(input), Some('+')) {
        Some(advance_char(input))
    } else {
        None
    }
}

fn token(input: &str) -> Option<(&str, Expression)> {
    if let Some(ident_res) = ident(whitespace(input)) {
        return Some(ident_res);
    }

    if let Some(number_res) = number(whitespace(input)) {
        return Some(number_res);
    }

    None
}

fn add_term(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = term(input)?;

    let next_input = plus(whitespace(next_input))?;
    Some((next_input, lhs))
}

fn add(mut input: &str) -> Option<(&str, Expression)> {
    let mut left = None;
    while let Some((next_input, expr)) = add_term(input) {
        if let Some(prev_left) = left {
            left = Some(Expression::Add(Box::new(prev_left), Box::new(expr)))
        } else {
            left = Some(expr)
        }

        input = next_input;
    }
    let left = left?;

    let (next_input, rhs) = expr(input)?;

    Some((next_input, Expression::Add(Box::new(left), Box::new(rhs))))
}

fn paren(input: &str) -> Option<(&str, Expression)> {
    let next_input = lparen(whitespace(input))?;
    let (next_input, expr) = expr(next_input)?;

    let next_input = rparen(next_input)?;

    Some((next_input, expr))
}

fn term(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = paren(input) {
        return Some(res);
    }

    if let Some(res) = token(input) {
        return Some(res);
    }

    None
}

fn expr(input: &str) -> Option<(&str, Expression)> {
    if let Some(res) = add(input) {
        return Some(res);
    }
    if let Some(res) = term(input) {
        return Some(res);
    }
    None
}

fn main() {
    let s = "123 + 345";
    println!("source: {:?}", s);
    println!("parsed: {:?}", expr(s));
}

#[cfg(test)]
mod tests {
    use crate::{expr, Expression::*};

    #[test]
    fn test_simple_add() {
        let txt = "100 + 200";
        assert_eq!(
            expr(txt),
            Some((
                "",
                Add(Box::new(NumLiteral(100.0)), Box::new(NumLiteral(200.0)))
            ))
        )
    }

    #[test]
    fn test_nested_add() {
        let txt = "((10 + 20) + 30 + 40) + 50";
        assert_eq!(
            expr(txt),
            Some((
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
