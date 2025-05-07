use soroban_security_detectors_sdk::{DetectorResult, SealedCodebase};

soroban_security_detectors_sdk::detector! {
    #[type_name = FileWithoutNoStd]
    fn array_loop_bound_check<SealedCodebase>(
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
