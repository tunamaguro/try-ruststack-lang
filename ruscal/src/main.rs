use std::{collections::BTreeMap, io::Read};

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, multispace0, multispace1},
    combinator::{opt, recognize},
    error::ParseError,
    multi::{fold_many0, many0, separated_list0},
    number::complete::recognize_float,
    sequence::{delimited, pair, terminated},
    IResult, Parser,
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
    If(
        Box<Expression<'src>>,
        Box<Expression<'src>>,
        Option<Box<Expression<'src>>>,
    ),
}

#[derive(Debug, PartialEq, Clone)]
enum Statement<'src> {
    Expression(Expression<'src>),
    VarDef(&'src str, Expression<'src>),
    VarAssign(&'src str, Expression<'src>),
    For {
        loop_var: &'src str,
        start: Expression<'src>,
        end: Expression<'src>,
        stmts: Statements<'src>,
    },
    FnDef {
        name: &'src str,
        args: Vec<&'src str>,
        stmts: Statements<'src>,
    },
}

type Statements<'a> = Vec<Statement<'a>>;

#[derive(Debug, PartialEq, Clone)]
struct UserFn<'src> {
    args: Vec<&'src str>,
    stmts: Statements<'src>,
}

struct NativeFn {
    code: Box<dyn Fn(&[f64]) -> f64>,
}

impl std::fmt::Display for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeOp>")
    }
}

enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn),
}

impl<'src> FnDef<'src> {
    fn call(&self, args: &[f64], stackframe: &StackFrame) -> f64 {
        match self {
            FnDef::User(user_fn) => {
                let mut new_frame = StackFrame::push_stack(stackframe);
                new_frame.vars = args
                    .iter()
                    .zip(user_fn.args.iter())
                    .map(|(arg, arg_name)| (arg_name.to_string(), *arg))
                    .collect();
                eval_stmts(&user_fn.stmts, &mut new_frame)
            }
            FnDef::Native(native_fn) => (native_fn.code)(args),
        }
    }
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

fn num_expr(input: &str) -> IResult<&str, Expression> {
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

fn open_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('{'))(input)?;
    Ok((input, ()))
}

fn close_brace(input: &str) -> IResult<&str, ()> {
    let (input, _) = space_delimited(char('}'))(input)?;
    Ok((input, ()))
}

fn if_expr(input: &str) -> IResult<&str, Expression> {
    let (input, _) = space_delimited(tag("if"))(input)?;
    let (input, cond) = expr(input)?;
    let (input, true_case) = delimited(open_brace, expr, close_brace)(input)?;
    let (input, _) = space_delimited(tag("else"))(input)?;
    let (input, false_case) = opt(delimited(open_brace, expr, close_brace))(input)?;

    Ok((
        input,
        Expression::If(
            Box::new(cond),
            Box::new(true_case),
            false_case.map(Box::new),
        ),
    ))
}

fn expr(input: &str) -> IResult<&str, Expression> {
    alt((if_expr, num_expr))(input)
}

fn var_def(input: &str) -> IResult<&str, Statement> {
    // ここはspace_delimitedだとvara=1みたいなのが許されるのでmultispace1
    let (input, _) = delimited(multispace0, tag("var"), multispace1)(input)?;
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarDef(ident, expr)))
}

fn var_assign(input: &str) -> IResult<&str, Statement> {
    let (input, ident) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('='))(input)?;
    let (input, expr) = space_delimited(expr)(input)?;
    Ok((input, Statement::VarAssign(ident, expr)))
}

fn expr_statement(input: &str) -> IResult<&str, Statement> {
    let (input, expr) = expr(input)?;
    Ok((input, Statement::Expression(expr)))
}

fn for_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = space_delimited(tag("for"))(input)?;
    let (input, loop_var) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(tag("in"))(input)?;
    let (input, start) = space_delimited(expr)(input)?;
    let (input, _) = space_delimited(tag("to"))(input)?;
    let (input, end) = space_delimited(expr)(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace)(input)?;
    Ok((
        input,
        Statement::For {
            loop_var,
            start,
            end,
            stmts,
        },
    ))
}

fn fn_statement(input: &str) -> IResult<&str, Statement> {
    let (input, _) = delimited(multispace0, tag("fn"), multispace1)(input)?;
    let (input, fn_name) = space_delimited(identifier)(input)?;
    let (input, _) = space_delimited(char('('))(input)?;
    let (input, arg_names) = separated_list0(char(','), space_delimited(identifier))(input)?;
    let (input, _) = space_delimited(char(')'))(input)?;
    let (input, stmts) = delimited(open_brace, statements, close_brace)(input)?;
    Ok((
        input,
        Statement::FnDef {
            name: fn_name,
            args: arg_names,
            stmts,
        },
    ))
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        for_statement,
        fn_statement,
        //   for文以外は終わりに";"が付く
        terminated(alt((var_def, var_assign, expr_statement)), char(';')),
    ))(i)
}

fn statements(input: &str) -> IResult<&str, Statements> {
    let (input, stmts) = many0(statement)(input)?;
    let (input, _) = opt(char(';'))(input)?;
    Ok((input, stmts))
}

type Variables = BTreeMap<String, f64>;
type Functions<'src> = BTreeMap<String, FnDef<'src>>;

struct StackFrame<'src> {
    vars: Variables,
    funcs: Functions<'src>,
    uplevel: Option<&'src StackFrame<'src>>,
}

fn print(arg: f64) -> f64 {
    println!("print: {arg}");
    0.
}

impl<'src> StackFrame<'src> {
    fn new() -> Self {
        let default_funcs = vec![
            ("sqrt", unary_fn(f64::sqrt)),
            ("sin", unary_fn(f64::sin)),
            ("cos", unary_fn(f64::cos)),
            ("tan", unary_fn(f64::tan)),
            ("asin", unary_fn(f64::atan)),
            ("acos", unary_fn(f64::acos)),
            ("atan", unary_fn(f64::atan)),
            ("atan2", binary_fn(f64::atan2)),
            ("pow", binary_fn(f64::powf)),
            ("exp", unary_fn(f64::exp)),
            ("log", binary_fn(f64::log)),
            ("log10", unary_fn(f64::log10)),
            ("print", unary_fn(print)),
        ];

        Self {
            vars: Default::default(),
            funcs: default_funcs
                .into_iter()
                .map(|(name, func)| (name.to_string(), func))
                .collect(),
            uplevel: None,
        }
    }

    fn get_fn(&self, name: &str) -> Option<&FnDef<'src>> {
        if let Some(func) = self.funcs.get(name) {
            Some(func)
        } else if let Some(up) = self.uplevel {
            up.get_fn(name)
        } else {
            None
        }
    }

    fn push_stack(frame: &'src StackFrame<'src>) -> Self {
        Self {
            uplevel: Some(frame),
            funcs: Default::default(),
            vars: Default::default(),
        }
    }
}

fn unary_fn<'a>(f: fn(f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| f(*args.iter().next().expect("function missing argument"))),
    })
}

fn binary_fn<'a>(f: fn(f64, f64) -> f64) -> FnDef<'a> {
    FnDef::Native(NativeFn {
        code: Box::new(move |args| {
            let mut args = args.iter();
            let lhs = args.next().expect("function missing first argument");
            let rhs = args.next().expect("function missing second argument");
            f(*lhs, *rhs)
        }),
    })
}

fn eval(expr: &Expression, stackframe: &StackFrame) -> f64 {
    match expr {
        Expression::Ident("pi") => std::f64::consts::PI,
        Expression::Ident(ident) => *stackframe.vars.get(*ident).expect("Variables not found"),
        Expression::NumLiteral(num) => *num,
        Expression::Add(lhs, rhs) => eval(lhs, stackframe) + eval(rhs, stackframe),
        Expression::Sub(lhs, rhs) => eval(lhs, stackframe) - eval(rhs, stackframe),
        Expression::Mul(lhs, rhs) => eval(lhs, stackframe) * eval(rhs, stackframe),
        Expression::Div(lhs, rhs) => eval(lhs, stackframe) / eval(rhs, stackframe),
        Expression::If(cond, true_case, false_case) => {
            if (eval(cond, stackframe)) != 0.0 {
                eval(true_case, stackframe)
            } else if let Some(false_case) = false_case {
                eval(false_case, stackframe)
            } else {
                0.0
            }
        }
        Expression::FnInvoke(name, args) => {
            if let Some(func) = stackframe.get_fn(name) {
                let args: Vec<_> = args.iter().map(|arg| eval(arg, stackframe)).collect();
                func.call(&args, stackframe)
            } else {
                panic!("Unknown function {name:?}")
            }
        }
    }
}

fn eval_stmts<'src>(stmts: &[Statement<'src>], stackframe: &mut StackFrame<'src>) -> f64 {
    let mut last_result = 0.0;
    for statement in stmts {
        match statement {
            Statement::Expression(expr) => {
                last_result = eval(expr, stackframe);
            }
            Statement::VarDef(ident, expr) => {
                let value = eval(expr, stackframe);
                stackframe.vars.insert(ident.to_string(), value);
            }
            Statement::VarAssign(ident, expr) => {
                if !stackframe.vars.contains_key(*ident) {
                    panic!("Variables is not found")
                }
                let value = eval(expr, stackframe);
                stackframe.vars.insert(ident.to_string(), value);
            }
            Statement::For {
                loop_var,
                start,
                end,
                stmts,
            } => {
                let start = eval(start, stackframe) as i32;
                let end = eval(end, stackframe) as i32;
                for i in start..end {
                    stackframe.vars.insert(loop_var.to_string(), i.into());
                    eval_stmts(stmts, stackframe);
                }
            }
            Statement::FnDef { name, args, stmts } => {
                stackframe.funcs.insert(
                    name.to_string(),
                    FnDef::User(UserFn {
                        args: args.clone(),
                        stmts: stmts.clone(),
                    }),
                );
            }
        }
    }
    last_result
}

fn main() {
    let mut buf = String::new();
    let mut stackframe = StackFrame::new();
    if std::io::stdin().read_to_string(&mut buf).is_ok() {
        let (rest, parsed_statements) = match statements(&buf) {
            Ok(parsed) => parsed,
            Err(e) => {
                eprintln!("Parsed error: {e:?}");
                return;
            }
        };
        dbg!(rest, &parsed_statements);
        eval_stmts(&parsed_statements, &mut stackframe);
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
