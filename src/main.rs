fn add(stack: &mut Vec<i32>) {
    let left = stack.pop().unwrap();
    let right = stack.pop().unwrap();
    stack.push(left + right)
}

fn main() {
    let mut stack = vec![];

    stack.push(32);
    stack.push(44);

    add(&mut stack);

    stack.push(100);
    add(&mut stack);

    println!("Stack: {stack:?}")
}
