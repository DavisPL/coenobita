use coenobita::cap;

fn main() {
    // We can read or delete this directory, and we can read or delete ALL of its descendants
    let dir_cap = cap!("examples/files" with (Read, Delete), (), (Read, Delete));
    println!("{:?}", dir_cap);
}
