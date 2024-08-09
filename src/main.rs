#[derive(Debug, PartialEq, Eq)]
enum Value<'src> {
    Num(i32),
    Op(&'src str),
    Block(Vec<Value<'src>>),
}

impl<'src> Value<'src> {
    fn as_num(&self) -> i32 {
        match self {
            Self::Num(val) => *val,
            _ => panic!("not number"),
        }
    }
}

fn add(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap().as_num();
    let left = stack.pop().unwrap().as_num();
    stack.push(Value::Num(left + right))
}

fn sub(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap().as_num();
    let left = stack.pop().unwrap().as_num();
    stack.push(Value::Num(left - right))
}

fn mul(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap().as_num();
    let left = stack.pop().unwrap().as_num();
    stack.push(Value::Num(left * right))
}

fn div(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap().as_num();
    let left = stack.pop().unwrap().as_num();
    stack.push(Value::Num(left / right))
}

fn parse_block<'src, 'a>(input: &'a [&'src str]) -> (Value<'src>, &'a [&'src str]) {
    let mut tokens = vec![];
    let mut words = input;

    while let Some((&word, mut rest)) = words.split_first() {
        if word.is_empty() {
            break;
        }
        if word == "{" {
            let value;
            (value, rest) = parse_block(rest);
            tokens.push(value);
        } else if word == "}" {
            return (Value::Block(tokens), rest);
        } else if let Ok(value) = word.parse::<i32>() {
            tokens.push(Value::Num(value))
        } else {
            tokens.push(Value::Op(word));
        }

        words = rest;
    }
    (Value::Block(tokens), words)
}

fn parse(line: & str) -> Vec<Value> {
    let mut stack = vec![];
    let input: Vec<_> = line.split(' ').collect();
    let mut words = &input[..];
    while let Some((&word, mut rest)) = words.split_first() {
        if word == "{" {
            // ブロックのパース
            let value;
            (value, rest) = parse_block(rest);
            stack.push(value);
        } else if let Ok(val) = word.parse::<i32>() {
            // 数値のパース
            stack.push(Value::Num(val));
        } else {
            // 演算子のパース
            match word {
                "+" => add(&mut stack),
                "-" => sub(&mut stack),
                "*" => mul(&mut stack),
                "/" => div(&mut stack),
                _ => unreachable!(),
            }
        }
        words = rest;
    }
    println!("Stack: {stack:?}");

    stack
}

fn main() {
    for line in std::io::stdin().lines().map_while(Result::ok) {
        parse(&line);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_group() {
        assert_eq!(
            parse("1 2 + { 3 4 }"),
            vec![
                Value::Num(3),
                Value::Block(vec![Value::Num(3), Value::Num(4)])
            ]
        )
    }
}
