#![warn(clippy::pedantic)]

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct Location {
    pub source_code: String,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

//TODO merge TLocation into
pub trait Node {
    fn children(&self) -> impl Iterator;
}

pub trait TLocation {
    fn location(&self) -> Location;
    fn source_code(&self) -> String;
    fn start_line(&self) -> usize;
    fn start_col(&self) -> usize;
    fn end_line(&self) -> usize;
    fn end_col(&self) -> usize;
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Visibility {
    Public,
    Private,
    Restricted,
}

impl Visibility {
    #[must_use]
    pub fn from_syn_visibility(visibility: &syn::Visibility) -> Self {
        match visibility {
            syn::Visibility::Public(_) => Visibility::Public,
            syn::Visibility::Inherited => Visibility::Private,
            syn::Visibility::Restricted(_) => Visibility::Restricted,
        }
    }
}

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
    Constant,
}

#[macro_export]
macro_rules! location {
    ($item:expr) => {{
        use syn::spanned::Spanned;
        $crate::node::Location {
            source_code: $item.span().source_text().unwrap_or_default(),
            start_line: $item.span().start().line as usize,
            start_col: $item.span().start().column as usize,
            end_line: $item.span().end().line as usize,
            end_col: $item.span().end().column as usize,
        }
    }};
}

#[macro_export]
macro_rules! source_code {
    ($item:expr) => {{
        use syn::spanned::Spanned;
        $item.span().source_text().unwrap_or_default()
    }};
}

#[cfg(test)]
mod tests {
    use crate::utils::test::create_mock_location;

    #[test]
    fn test_mock_location() {
        let location = create_mock_location();

        assert_eq!(location.source_code, "fn main() {}".to_string());
        assert_eq!(location.start_line, 1);
        assert_eq!(location.start_col, 1);
        assert_eq!(location.end_line, 1);
        assert_eq!(location.end_col, 14);
    }
}
