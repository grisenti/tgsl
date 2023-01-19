mod lexer;
mod errors;

use lexer::*;
use errors::*;

fn main() {
  let mut lex = Lexer::new("hello how are you");
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
  println!("{:?}", lex.next_token());
}
