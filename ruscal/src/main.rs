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

fn add(input: &str) -> Option<(&str, Expression)> {
    let (next_input, lhs) = expr(input)?;

    let next_input = plus(whitespace(next_input))?;

    let (next_input, rhs) = term(next_input)?;

    Some((next_input, Expression::Add(Box::new(lhs), Box::new(rhs))))
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
    
}
