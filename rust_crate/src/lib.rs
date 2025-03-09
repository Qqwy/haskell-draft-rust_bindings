#[no_mangle]
pub extern "C" fn rs_hello_world() {
  println!("Hello, world!");
}

#[no_mangle]
pub extern "C" fn rs_add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}
