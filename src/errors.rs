pub trait PrintError {
    fn print(&self, leading_whitespace: usize) -> String;
}
