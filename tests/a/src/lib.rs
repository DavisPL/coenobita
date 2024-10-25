#![allow(unused)]

fn test_let_annotation() {
    #[cnbt::tag({b}{b})]
    let x = b::value();

    #[cnbt::tag({a,b,c}{a,b,c})]
    let x = b::value();
}

fn test_basic_if_else() {
    #[cnbt::tag({a}{a,c})]
    let y = if c::boolean() { 6 } else { 7 };

    #[cnbt::tag({a,b,c}{a,b,c})]
    let z = y;
}

fn test_addition() {
    #[cnbt::tag({b,c}{a,b,c})]
    let x = b::value() + c::value();

    #[cnbt::tag({a,b,c}{a,b,c})]
    let y = 5 + x;
}

fn test_addition_in_if_else() {
    #[cnbt::tag({a,b}{a,b,c})]
    let y = if c::boolean() { 5 + b::value() } else { 7 };
}

fn test_nested_if_else() {
    #[cnbt::tag({a}{a,b,c})]
    let y = if b::boolean() {
        if c::boolean() {
            5
        } else {
            6
        }
    } else {
        if !c::boolean() {
            7
        } else {
            8
        }
    };
}

fn test_nested_item_fn() {
    #[cnbt::tag({*}{*} fn() -> {a,b}{a})]
    fn boo() -> i32 {
        6
    }

    #[cnbt::tag({*}{*})]
    let x = boo();

    #[cnbt::tag({a,b}{*})]
    let y = boo();
}

fn test_if_else_guard_expr() {
    #[cnbt::tag({a}{a,b,c})]
    let y = if c::boolean() && b::boolean() { 6 } else { 7 };

    #[cnbt::tag({a,b,c}{a,b,c})]
    let z = y;
}

fn test_assign_mut() {
    #[cnbt::tag({a}{a,b,c})]
    let mut x = 5;

    if c::boolean() || b::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}

fn test_struct() {
    struct Boo {
        #[cnbt::tag({a}{a})]
        x: i32,
        y: bool,
    }

    #[cnbt::tag({a}{a})]
    let boo = Boo { x: 5, y: true };
}

fn test_if_struct() {
    struct Boo {
        #[cnbt::tag({a}{a,c})]
        x: i32,
        y: bool,
    }

    #[cnbt::tag({a}{a,c})]
    let boo = if c::boolean() {
        Boo { x: 5, y: true }
    } else {
        Boo { x: 6, y: true }
    };
}

// fn test_loop_with_pattern() {
//     #[cnbt::tag({a}{a,c})]
//     let mut x = 5;

//     for i in 0..c::value() {
//         x += 1;
//     }
// }
