mod ast;
mod errors;
mod lexer;

use lexer::*;

fn main() {
  let mut lex = Lexer::new("hello how are you");
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
}
