#[derive(Debug, PartialEq, Eq)]
enum Token {
    Ident,
    Number,
}

fn whitespace(mut input: &str) -> &str {
    while matches!(input.chars().next(), Some(' ')) {
        let mut chars = input.chars();
        chars.next();
        input = chars.as_str();
    }
    input
}

fn number(mut input: &str) -> (&str, Option<Token>) {
    let mut chars = input.chars();
    if matches!(chars.next(), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        while matches!(chars.next(), Some(_x @ ('.' | '0'..='9'))) {
            input = chars.as_str();
        }
        (input, Some(Token::Number))
    } else {
        (input, None)
    }
}
fn ident(mut input: &str) -> (&str, Option<Token>) {
    if matches!(input.chars().next(), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        while matches!(
            input.chars().next(),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            let mut chars = input.chars();
            chars.next();
            input = chars.as_str();
        }
        (input, Some(Token::Ident))
    } else {
        (input, None)
    }
}

fn token(input: &str) -> (&str, Option<Token>) {
    if let (input, Some(ident_res)) = ident(whitespace(input)) {
        return (input, Some(ident_res));
    }

    if let (input, Some(number_res)) = number(whitespace(input)) {
        return (input, Some(number_res));
    }

    (input, None)
}

fn source(mut input: &str) -> Vec<Token> {
    let mut tokens = vec![];
    while !input.is_empty() {
        input = if let (next_input, Some(token)) = token(input) {
            tokens.push(token);
            next_input
        } else {
            break;
        }
    }
    tokens
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace() {
        assert_eq!(whitespace("    "), "");
    }

    #[test]
    fn test_number() {
        assert_eq!(number("+1.123 "), (" ", Some(Token::Number)));
    }

    #[test]
    fn test_ident() {
        assert_eq!(ident("AbcDE0123"), ("", Some(Token::Ident)));
    }

    #[test]
    fn test_source() {
        assert_eq!(
            source("    aaaa -123 ko090"),
            vec![Token::Ident, Token::Number, Token::Ident]
        )
    }
}
