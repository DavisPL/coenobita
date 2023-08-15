use coenobita::{ cap };

fn main() {
    cap!("example.txt" with Read, Write | Delete, Create | Move, Copy)
}
