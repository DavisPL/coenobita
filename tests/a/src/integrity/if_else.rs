fn test_basic_if_else() {
    #[cnbt::tag({a}{a,c})]
    let y = if c::boolean() { 6 } else { 7 };

    #[cnbt::tag({a,b,c}{a,b,c})]
    let z = y;
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

fn test_some() {
    #[cnbt::tag({a}{a})]
    let mut x = 5;

    if c::boolean() {
        x = 6;
    } else {
        x = 7;
    }
}