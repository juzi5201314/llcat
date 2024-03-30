
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("lcoal var `{0}` does not exist")]
    LocalNotExist(usize),
    #[error("function `{0}` does not exist")]
    FuncNotExist(usize),
    #[error("function has {0} param but there are only {1} values on the stack")]
    FuncParamsNotMatch(usize, usize),
}