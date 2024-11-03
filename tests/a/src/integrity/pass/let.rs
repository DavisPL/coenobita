fn main() {
    #[cnbt::tag({a}{a})]
    let x = 5;

    #[cnbt::tag({a}{a})]
    let y = x;
}