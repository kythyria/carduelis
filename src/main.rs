#![allow(dead_code)]

mod caracara_6;
fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let bracecount: usize = args[1].parse().unwrap();
    //let bracecount = 384;
    let mut evil = String::with_capacity(2 + (bracecount * 2));
    evil.push_str("\\g");
    for _ in 0..bracecount {
        evil.push('{');
    }
    /*for _ in 0..bracecount {
        evil.push('}');
    }*/

    println!("Parsing {} bytes: {}", evil.len(), evil);
    let evil_result= caracara_6::parse_str(&evil, Box::new(|_| None));
    println!("Result: {:?}", evil_result);
}