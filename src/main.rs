#![allow(dead_code)]

//mod caracara_1;
//mod caracara_2;
mod caracara_3;
mod pest_visualiser;
//mod caracara_4;
//mod caracara_5;
mod caracara_6;
fn main() {

    if false {
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
        let evil_result= caracara_3::parser::get_parse(&evil);
        println!("parsed!");
        match evil_result {
            Ok(_p) => {
                let mut _evil_tree = Vec::<u8>::new();
                //let r = pest_visualiser::draw(&mut evil_tree, p);
                //r.unwrap();
            },
            Err(e) => {
                println!("{}", e);
            }
        }
        
    
        let pr = caracara_3::parser::get_parse(
            //r#"\z[foo bar=baz   qux="f\e{bird}g" r="why"]"#
            r#"foo\z[v=z foo="bar" flip] bar baz"#
        );
        match pr {
            Ok(p) => {
                pest_visualiser::draw(&mut std::io::stdout(), p).unwrap();
            },
            Err(e) => println!("{}", e)
        }
    }
}