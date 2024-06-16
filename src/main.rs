fn add(stack: &mut Vec<i32>) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(left + right)
}

fn sub(stack: &mut Vec<i32>) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(left - right)
}

fn mul(stack: &mut Vec<i32>) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(left * right)
}

fn div(stack: &mut Vec<i32>) {
    let right = stack.pop().unwrap();
    let left = stack.pop().unwrap();
    stack.push(left / right)
}

fn main() {
    for line in std::io::stdin().lines() {
        let mut stack = vec![];
        if let Ok(line) = line {
            let tokens = line.split(' ').collect::<Vec<_>>();
            for token in tokens {
                if let Ok(num) = token.parse::<i32>() {
                    stack.push(num);
                } else {
                    match token {
                        "+" => add(&mut stack),
                        "-" => sub(&mut stack),
                        "*" => mul(&mut stack),
                        "/" => div(&mut stack),
                        _ => unreachable!(),
                    }
                }
            }
        }
        println!("Stack: {stack:?}")
    }
}
