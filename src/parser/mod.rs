mod config;
pub use config::ParserConfig;

mod error;
pub use error::ParserError;

mod values;

mod traits;

mod schema;
pub use schema::*;

mod query;
pub use query::*;

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::PathBuf;

    fn list_directory(dir: &str) -> Vec<PathBuf> {
        let paths: Vec<PathBuf> = fs::read_dir(dir)
            .unwrap()
            .map(|d| d.unwrap())
            .map(|d| d.path())
            .collect();
        assert!(paths.len() > 0);
        paths
    }

    #[test]
    fn query_parser_can_parse_all_query_fixtures() {
        let paths = list_directory("fixtures/queries");
        assert_eq!(paths.len(), 35);
        for path in paths {
            let data = fs::read_to_string(&path).unwrap();
            match parse_query(&data[..]) {
                Ok(_) => (),
                Err(e) => panic!("Failed to parse query - path: {:?} error: {:?}", path, e),
            }
        }
    }

    #[test]
    fn schema_parser_can_parse_all_schema_fixtures() {
        let paths = list_directory("fixtures/schemas");
        assert_eq!(paths.len(), 27);
        for path in paths {
            let data = fs::read_to_string(&path).unwrap();
            match parse_schema(&data[..]) {
                Ok(_) => (),
                Err(e) => panic!("Failed to parse schema - path: {:?} error: {:?}", path, e),
            }
        }
    }
}
