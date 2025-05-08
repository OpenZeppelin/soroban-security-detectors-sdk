use soroban_security_detectors_sdk::{DetectorResult, SealedCodebase};

soroban_security_detectors_sdk::detector! {
    #[type_name = FileWithoutNoStd]
    fn file_without_no_std<SealedCodebase>(
        codebase: &SealedCodebase,
    ) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for file in codebase.files() {
            if !file.has_no_std() {
                errors.push(DetectorResult {
                    file_path: file.path.clone(),
                    offset_start: 0,
                    offset_end: 0,
                    extra: None,
                });
            }
        }
        if errors.is_empty() {
            None
        } else {
            Some(errors)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use soroban_security_detectors_sdk::build_codebase;
    use std::collections::HashMap;

    #[test]
    fn test_file_without_no_std_1() {
        let detector = FileWithoutNoStd;
        let src = "";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_some());
        assert_eq!(result.as_ref().unwrap().len(), 1, "{result:?}");
        let detector_result = result.as_ref().unwrap().first().unwrap();
        assert_eq!(detector_result.file_path, "test.rs");
        assert_eq!(detector_result.offset_start, 0);
        assert_eq!(detector_result.offset_end, 0);
        assert_eq!(detector_result.extra, None);
    }

    #[test]
    fn test_file_without_no_std_2() {
        let detector = FileWithoutNoStd;
        let src = "#![no_std]";
        let mut data = HashMap::new();
        data.insert("test.rs".to_string(), src.to_string());
        let codebase = build_codebase(&data).unwrap();
        let result = detector.check(codebase.as_ref());
        assert!(result.is_none());
    }
}
