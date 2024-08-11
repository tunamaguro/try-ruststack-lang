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

macro_rules! impl_op {
    ($name:ident,$op:tt) => {
        fn $name(stack: &mut Vec<Value>) {
            let right = stack.pop().unwrap().as_num();
            let left = stack.pop().unwrap().as_num();
            stack.push(Value::Num((left $op right) ))
        }
    };
}

impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);

fn lt(stack: &mut Vec<Value>) {
    let right = stack.pop().unwrap().as_num();
    let left = stack.pop().unwrap().as_num();

    stack.push(Value::Num(if left < right { 1 } else { 0 }))
}

fn op_if(vm: &mut Vm) {
    let false_branch = vm.stack.pop().unwrap().to_block();
    let true_branch = vm.stack.pop().unwrap().to_block();
    let cond_branch = vm.stack.pop().unwrap().to_block();

    for code in cond_branch {
        eval(code, vm);
    }

    let cond_result = vm.stack.pop().unwrap().as_num();

    if cond_result != 0 {
        for code in true_branch {
            eval(code, vm);
        }
    } else {
        for code in false_branch {
            eval(code, vm);
        }
    }
}

fn op_def(vm: &mut Vm) {
    let num = vm.stack.pop().unwrap();
    let sym = vm.stack.pop().unwrap();

    match sym {
        Value::Sym(ident) => {
            vm.vars.insert(ident, num);
        }
        _ => panic!("is not symbol"),
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

fn eval<'src>(code: Value<'src>, vm: &mut Vm<'src>) {
    match code {
        Value::Op(op) => match op {
            "+" => add(&mut vm.stack),
            "-" => sub(&mut vm.stack),
            "*" => mul(&mut vm.stack),
            "/" => div(&mut vm.stack),
            "<" => lt(&mut vm.stack),
            "if" => op_if(vm),
            "def" => op_def(vm),
            _ => {
                let val = vm
                    .vars
                    .get(op)
                    .unwrap_or_else(|| panic!("{op:?} is not a defined operation"));
                vm.stack.push(val.clone());
            }
        },
        _ => vm.stack.push(code.clone()),
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
            } else if let Some(var_name) = word.strip_prefix('/') {
                // '/'を取り除いたものが変数名
                Value::Sym(var_name)
            } else {
                Value::Op(word)
            };
            eval(code, &mut vm);
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

    #[test]
    fn test_def() {
        assert_eq!(
            parse("/x 10 def /y 20 def { x y < } { x } { y } if"),
            vec![Value::Num(10)]
        )
    }
}
