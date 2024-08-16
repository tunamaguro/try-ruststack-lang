#[derive(Debug, PartialEq, Eq)]
enum Token {
    Ident,
    Number,
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

fn number(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(_x @ ('-' | '+' | '.' | '0'..='9'))) {
        input = advance_char(input);
        while matches!(peek_char(input), Some(_x @ ('.' | '0'..='9'))) {
            input = advance_char(input);
        }
        (input, Some(Token::Number))
    } else {
        (input, None)
    }
}
fn ident(mut input: &str) -> (&str, Option<Token>) {
    if matches!(peek_char(input), Some(_x @ ('a'..='z' | 'A'..='Z'))) {
        input = advance_char(input);
        while matches!(
            peek_char(input),
            Some(_x @ ('a'..='z' | 'A'..='Z' | '0'..='9'))
        ) {
            input = advance_char(input)
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
