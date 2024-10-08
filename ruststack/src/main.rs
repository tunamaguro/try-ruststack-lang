use std::{
    collections::BTreeMap,
    io::{BufRead, BufReader},
};

#[derive(Clone)]
struct NativeOp(fn(&mut Vm));

impl PartialEq for NativeOp {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0 as *const fn(), other.0 as *const fn())
    }
}

impl Eq for NativeOp {}

impl std::fmt::Debug for NativeOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeOp>")
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Value {
    Num(i32),
    Op(String),
    Block(Vec<Value>),
    Sym(String),
    Native(NativeOp),
}

impl Value {
    fn as_num(&self) -> i32 {
        match self {
            Self::Num(val) => *val,
            _ => panic!("not number"),
        }
    }

    fn as_sym(&self) -> &str {
        match self {
            Self::Sym(s) => &s,
            _ => panic!("not symbol"),
        }
    }

    fn to_block(self) -> Vec<Value> {
        match self {
            Self::Block(val) => val,
            _ => panic!("Value is not a block"),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Value::Num(i) => i.to_string(),
            Value::Op(ref s) | Value::Sym(ref s) => s.clone(),
            Value::Block(_) => "<Block>".to_string(),
            Value::Native(ref op) => format!("{op:?}"),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug)]
struct Vm {
    stack: Vec<Value>,
    vars: Vec<BTreeMap<String, Value>>,
    blocks: Vec<Vec<Value>>,
}

type OpFunc = fn(&mut Vm);

impl Vm {
    fn new() -> Self {
        // 書籍ではArrayだが面倒なのでVecを使う
        let native_functions: Vec<(&str, OpFunc)> = vec![
            ("+", add),
            ("-", sub),
            ("*", mul),
            ("/", div),
            ("<", lt),
            ("puts", puts),
            ("if", op_if),
            ("def", op_def),
            ("dup", dup),
            ("exch", exch),
        ];
        Self {
            stack: vec![],
            vars: vec![native_functions
                .into_iter()
                .map(|(name, func)| (name.to_owned(), Value::Native(NativeOp(func))))
                .collect()],
            blocks: vec![],
        }
    }

    fn find_var(&self, name: &str) -> Option<Value> {
        self.vars
            .iter()
            .rev()
            .find_map(|val| val.get(name))
            .map(|val| val.to_owned())
    }
}

macro_rules! impl_op {
    ($name:ident,$op:tt) => {
        fn $name(vm: &mut Vm) {
            let right = vm.stack.pop().unwrap().as_num();
            let left = vm.stack.pop().unwrap().as_num();
            vm.stack.push(Value::Num((left $op right) ))
        }
    };
}

impl_op!(add, +);
impl_op!(sub, -);
impl_op!(mul, *);
impl_op!(div, /);

fn lt(vm: &mut Vm) {
    let right = vm.stack.pop().unwrap().as_num();
    let left = vm.stack.pop().unwrap().as_num();

    vm.stack.push(Value::Num(if left < right { 1 } else { 0 }))
}

fn puts(vm: &mut Vm) {
    let val = vm.stack.pop().unwrap();
    println!("{}", val.to_string());
}

fn dup(vm: &mut Vm) {
    let val = vm.stack.last().unwrap();
    vm.stack.push(val.clone())
}

fn exch(vm: &mut Vm) {
    let last = vm.stack.pop().unwrap();
    let second = vm.stack.pop().unwrap();
    vm.stack.push(last);
    vm.stack.push(second);
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
    // MEMO: なんでここでブロックの中を実行している?
    // let val = vm.stack.pop().unwrap();
    // eval(val, vm);
    let val = vm.stack.pop().unwrap();
    let sym = vm.stack.pop().unwrap().as_sym().to_string();

    vm.vars.last_mut().unwrap().insert(sym, val);
}

fn parse_block<'src, 'a>(input: &'a [&'src str]) -> (Value, &'a [&'src str]) {
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
            tokens.push(Value::Op(word.to_owned()));
        }

        words = rest;
    }
    (Value::Block(tokens), words)
}

fn eval(code: Value, vm: &mut Vm) {
    if let Some(top_block) = vm.blocks.last_mut() {
        top_block.push(code);
        return;
    }
    if let Value::Op(ref op) = code {
        let val = vm
            .find_var(op)
            .unwrap_or_else(|| panic!("{op:?} is not a defined operation"));
        match val {
            Value::Block(block) => {
                vm.vars.push(Default::default());
                for code in block {
                    eval(code, vm)
                }
                vm.vars.pop().unwrap();
            }
            Value::Native(op) => op.0(vm),
            _ => vm.stack.push(val),
        }
    } else {
        vm.stack.push(code.clone())
    }
}

fn parse_word(word: &str, vm: &mut Vm) {
    if word.is_empty() {
        return;
    }
    if word == "{" {
        vm.blocks.push(vec![]);
    } else if word == "}" {
        let top_block = vm.blocks.pop().expect("Block stack underrun!");
        eval(Value::Block(top_block), vm);
    } else {
        // ブロックじゃないものはNumかOpであると仮定している
        let code = if let Ok(val) = word.parse::<i32>() {
            Value::Num(val)
        } else if let Some(var_name) = word.strip_prefix('/') {
            // '/'を取り除いたものが変数名
            Value::Sym(var_name.to_owned())
        } else {
            Value::Op(word.to_owned())
        };
        eval(code, vm);
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
                Value::Sym(var_name.to_owned())
            } else {
                Value::Op(word.to_owned())
            };
            eval(code, &mut vm);
        }
        words = rest;
    }
    println!("Stack: {:?}", vm.stack);

    vm.stack
}

fn parse_batch<T>(source: T) -> Vec<Value>
where
    T: BufRead,
{
    let mut vm = Vm::new();
    for line in source.lines().map_while(Result::ok) {
        for word in line.split(' ') {
            parse_word(word, &mut vm);
        }
    }
    vm.stack
}

fn parse_interactive() {
    let mut vm = Vm::new();
    for line in std::io::stdin().lines().map_while(Result::ok) {
        for word in line.split(' ') {
            parse_word(word, &mut vm);
        }
        println!("stack :{:?}", vm.stack)
    }
}

fn main() {
    if let Some(f) = std::env::args()
        .nth(1)
        .and_then(|f| std::fs::File::open(f).ok())
    {
        parse_batch(BufReader::new(f));
    } else {
        parse_interactive();
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

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

    #[test]
    fn test_if_multi_line() {
        let txt = "
        /x 10 def
        /y 20 def
        {
        x y <
        }
        {
        x
        }
        {
        y
        } if
        ";

        assert_eq!(parse_batch(Cursor::new(txt)), vec![Value::Num(10)])
    }

    #[test]
    fn test_dup() {
        let txt = "
        1 2 dup
        ";

        assert_eq!(
            parse_batch(Cursor::new(txt)),
            vec![Value::Num(1), Value::Num(2), Value::Num(2)]
        )
    }

    #[test]
    fn test_exch() {
        let txt = "
        1 2 exch
        ";

        assert_eq!(
            parse_batch(Cursor::new(txt)),
            vec![Value::Num(2), Value::Num(1)]
        )
    }

    #[test]
    fn test_define_function() {
        let txt = "
        /double { 2 * } def
        /square { dup * } def
        /vec2sqlen { square exch square + } def
        1 2 vec2sqlen
        ";

        assert_eq!(parse_batch(Cursor::new(txt)), vec![Value::Num(5)])
    }

    #[test]
    fn test_factorial() {
        let txt = "
        /factorial_int {
            /acc exch def
            /n exch def
            { n 2 < }
            { acc }
            { 
                n 1 -
                acc n *
                factorial_int
            }
            if
        } def
        /factorial { 1 factorial_int } def
        10 factorial
        ";

        assert_eq!(parse_batch(Cursor::new(txt)), vec![Value::Num(3628800)])
    }

    #[test]
    fn test_fib() {
        let txt = "
        /fib {
            /n exch def
            { n 1 < }
            { 0 }
            { 
                { n 2 < }
                { 1 }
                {
                
                    n 1 -
                    fib
                    n 2 -
                    fib
                    +
                }
                if
            }
            if
        } def
        10 fib
        ";

        assert_eq!(parse_batch(Cursor::new(txt)), vec![Value::Num(55)])
    }
}
