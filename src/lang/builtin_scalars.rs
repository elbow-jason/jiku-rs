pub trait ScalarImpl {
    fn name() -> &'static str;
    fn serialize<'a>(&self, buf: &'a mut str) -> Result<usize, &'static str>;
    fn deserialize<'a>(s: &'a str) -> Result<Self, &'static str>;
}
