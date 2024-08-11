use std::collections::BTreeMap;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Value<'src> {
    Num(i32),
    Op(&'src str),
    Block(Vec<Value<'src>>),
    Sym(&'src str),
}

impl<'src> Value<'src> {
    fn as_num(&self) -> i32 {
        match self {
            Self::Num(val) => *val,
            _ => panic!("not number"),
        }
    }

    fn to_block(self) -> Vec<Value<'src>> {
        match self {
            Self::Block(val) => val,
            _ => panic!("Value is not a block"),
        }
    }
}

struct Vm<'src> {
    stack: Vec<Value<'src>>,
    vars: BTreeMap<&'src str, Value<'src>>,
}

impl<'src> Vm<'src> {
    fn new() -> Self {
        Self {
            stack: vec![],
            vars: BTreeMap::new(),
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

fn op_if(stack: &mut Vec<Value>) {
    let false_branch = stack.pop().unwrap().to_block();
    let true_branch = stack.pop().unwrap().to_block();
    let cond_branch = stack.pop().unwrap().to_block();

    for code in cond_branch {
        eval(code, stack);
    }

    let cond_result = stack.pop().unwrap().as_num();

    if cond_result != 0 {
        for code in true_branch {
            eval(code, stack);
        }
    } else {
        for code in false_branch {
            eval(code, stack);
        }
    }
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

fn eval<'src>(code: Value<'src>, stack: &mut Vec<Value<'src>>) {
    match code {
        Value::Op(op) => match op {
            "+" => add(stack),
            "-" => sub(stack),
            "*" => mul(stack),
            "/" => div(stack),
            "if" => op_if(stack),
            _ => panic!("{op:?} could not be parsed"),
        },
        _ => stack.push(code.clone()),
    }
}

fn parse(line: &str) -> Vec<Value> {
    let mut vm = Vm::new();
    let input: Vec<_> = line.split(' ').collect();
    let mut words = &input[..];
    while let Some((&word, mut rest)) = words.split_first() {
        if word == "{" {
            // ブロックのパース
            let value;
            (value, rest) = parse_block(rest);
            vm.stack.push(value);
        } else {
            // ブロックじゃないものはNumかOpであると仮定している
            let code = if let Ok(val) = word.parse::<i32>() {
                Value::Num(val)
            } else if word.starts_with('/') {
                Value::Sym(word)
            } else {
                Value::Op(word)
            };
            eval(code, &mut vm.stack);
        }
        words = rest;
    }
    println!("Stack: {:?}", vm.stack);

    vm.stack
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

    #[test]
    fn test_if_false() {
        assert_eq!(
            parse("{ 1 -1 + } { 100 } { -100 } if"),
            vec![Value::Num(-100)]
        )
    }

    #[test]
    fn test_if_true() {
        assert_eq!(
            parse("{ 1 0 + } { 100 } { -100 } if"),
            vec![Value::Num(100)]
        )
    }
}
