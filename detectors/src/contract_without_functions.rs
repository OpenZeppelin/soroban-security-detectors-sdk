use soroban_security_detectors_sdk::{DetectorResult, SealedCodebase};

soroban_security_detectors_sdk::detector! {
    #[type_name = ContractWithoutFunctions]
    fn array_loop_bound_check<SealedCodebase>(
        codebase: &SealedCodebase,
    ) -> Option<Vec<DetectorResult>> {
        let mut errors = Vec::new();
        for contract in codebase.contracts() {
            if contract.methods.borrow().is_empty() {
                errors.push(DetectorResult {
                    file_path: contract.name.clone(),
                    offset_start: contract.location.offset_start,
                    offset_end: contract.location.offset_end,
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
