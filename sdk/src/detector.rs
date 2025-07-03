//! Detector module
//! This module provides the Detector trait and macros for implementing detectors.
//!
//! # Public members
//!
//! - `detector!` macro for defining a detector. It automatically creates the structure for the provided `type_name` in the attribute and implements `Detector` trait. It can be applied to a single function with `type_name` attribute and follows `check` function signature.
//! - `detectors!` macro for defining multiple detectors at once. It can be applied to a list of functions with `type_name` attribute and follows `check` function signature.
//! - `Detector` trait for implementing a detector. It has a single method `check` that takes a `Codebase` and returns an optional vector of `DetectorResult`.
//! - `DetectorResult` struct for representing the result of a detector. It contains the file path, start and end offsets, and an optional map of extra information. Extra information is used to store a map of symbol replacements in the detector template.
//!   For example, if the detector template contains a symbol `$NAME`, the extra information can be used to replace it with the actual name.
//! - `DetectorReportTemplate` trait for implementing a detector report template. It has methods for generating the report title, body, and closing.
//! - `CombinedDetector` a union trait to force the implementor to implement both `Detector` and `DetectorReportTemplate` traits.
//! - `SorobanDetector` a boxed version of `CombinedDetector`.
//! - `DetectorOpaque` a struct that is used to wrap a raw pointer to a detector. It is used to operate with detectors using C API.
use std::{collections::HashMap, fmt::Display};

use crate::codebase::{Codebase, SealedState};

/// Detector macro
/// This macro is used to define a detector. It accepts a function (signature and body) with a `type_name` attribute.
/// The function signature must follow the `check` function signature from the `Detector` trait.
/// It automatically creates the structure for the provided `type_name` in the attribute and implements the `Detector` trait.
/// The `DetectorReportTemplate` trait should be implemented to satisfy the `CombinedDetector` contract.
#[macro_export]
macro_rules! detector {
    (
        #[type_name = $tname:ident]
        $(#[$attr:meta])*
        $vis:vis fn $name:ident < $($gen:tt),* > ( $($params:tt)* )
        $(-> $ret:ty)?
        $(where $($where:tt)*)?
        $body:block
    ) => {
        use $crate::detector::Detector;
        pub struct $tname;

        impl $crate::detector::Detector<$($gen)*> for $tname {
            fn check(
                &self,
                $($params)*
            ) -> Option<Vec<$crate::detector::DetectorResult>> {
                $body
            }
        }
    };
    () => {};
}

/// Detectors macro
/// This macro is used to define multiple detectors at once.
/// It accepts a list of functions (signature and body) with a `type_name` attribute similar to the `detector!` macro.
#[macro_export]
macro_rules! detectors {
    (
        $(
            #[type_name = $tname:ident]
            $(#[$attr:meta])*
            $vis:vis fn $name:ident $(< $($gen:tt)* >)? ( $($params:tt)* )
            $(-> $ret:ty)?
            $(where $($where:tt)*)?
            $body:block
        )*
    ) => {
        $(
            detector! {
                #[type_name = $tname]
                $(#[$attr])*
                $vis fn $name $(< $($gen)* >)? ( $($params)* )
                $(-> $ret)?
                $(where $($where)*)?
                $body
            }
        )*
    };
    () => {};
}

/// WARNING: This struct is used to wrap a raw pointer to a detector.
/// In you write detectors in a separate library, you should not use this struct to cast the pointer to `Detector`.
#[repr(C)]
pub struct DetectorOpaque {
    _private: [u8; 0],
}

pub type SealedCodebase = Codebase<SealedState>;

/// `CombinedDetector` trait
/// A union trait to force a `Detector` implementation to implement both `Detector` and `DetectorReportTemplate` traits.
pub trait CombinedDetector<T>: Detector<T> + DetectorReportTemplate {}

impl<T: Detector<U> + DetectorReportTemplate, U> CombinedDetector<U> for T {}

/// `SorobanDetector` type
/// An alias for a boxed version of `CombinedDetector`.
pub type SorobanDetector<T> = Box<dyn CombinedDetector<T>>;

/// `DetectorResult` struct
/// Represents the result of a detector.
///
/// # Fields
///
/// - `file_path`: The path to the file where the detector found an issue.
/// - `offset_start`: The start offset of the issue in the file.
/// - `offset_end`: The end offset of the issue in the file.
/// - `extra`: An optional map of extra information. This can be used to store symbol replacements for the report template substitution.
#[derive(Debug, Clone)]
pub struct DetectorResult {
    pub file_path: String,
    pub offset_start: u32,
    pub offset_end: u32,
    pub extra: Option<HashMap<String, String>>,
}

/// `Detector` trait
/// The base `Detector` functional interface.
///
/// # Functions
/// - `check`: The main function that takes a `Codebase` and returns an optional vector of `DetectorResult`.
pub trait Detector<T> {
    fn check(&self, codebase: &T) -> Option<Vec<DetectorResult>>;
}

/// `DetectorReportTemplate` trait
/// The `DetectorReportTemplate` trait is used to define the report template for a detector.
/// This trait exposes the `Detector` metadata used for generating the report.
///
/// # Functions
/// - `id`: Returns the unique identifier of the detector.
/// - `uid`: Returns the short detector identifier.
/// - `description`: Returns a description of the detector.
/// - `severity`: Returns the severity of the issue found by a detector.
/// - `tags`: Returns a list of tags associated with the detector. Soroban detectors always have the `soroban` tag.
/// - `title_single_instance`: Returns the title for a single instance of the issue.
/// - `title_multiple_instance`: Returns the title for multiple instances of the issue.
/// - `opening`: Returns the opening message for the report.
/// - `body_single_file_single_instance`: Returns the body of the report for a single file with a single issue instance.
/// - `body_single_file_multiple_instance`: Returns the body of the report for a single file with multiple issue instances.
/// - `body_multiple_file_multiple_instance`: Returns the body of the report for multiple files with multiple issue instances.
/// - `body_list_item_single_file`: Returns the body of the report for a single file with a single issue instance.
/// - `body_list_item_multiple_file`: Returns the body of the report for multiple files with a single issue instance.
/// - `closing`: Returns the closing message for the report.
/// - `template`: Returns the template string for the report in `yml` format.
///
/// # Metadata Example
/// ```yml
/// metadata:
///  id: assertion-error-message-verbose
///  uid: 3HgyHb
///  description: Detects assert statements that expose overly verbose or technical error messages directly to users, which can leak sensitive implementation details or confuse end users. Ensuring concise, user-friendly error messages helps maintain security and usability.
///  report:
///    severity: low
///    tags:
///      - audit
///      - reportable
///      - soroban
///    template:
///      title: Verbose Assertion Error Message Exposed
///      opening: Assert statements should provide clear and user-friendly error messages. Verbose or technical error messages may inadvertently reveal internal logic or sensitive information, and can be confusing for end users.
///      body-single-file-single-instance: In `$file_name`, an assert statement in the `$PARENT_NAME` $PARENT_TYPE on line $instance_line exposes a verbose or technical error message directly to users.
///      body-single-file-multiple-instance: In `$file_name`, multiple assert statements expose verbose or technical error messages directly to users.
///      body-multiple-file-multiple-instance: Across $total_files files, multiple assert statements expose verbose or technical error messages directly to users.
///      body-list-item-intro: 'The following assert statements were found with verbose or technical error messages:'
///      body-list-item-single-file: '- In `$PARENT_NAME` $PARENT_TYPE on line $instance_line of [`$file_name`]($instance_line_link)'
///      body-list-item-multiple-file: '- In `$PARENT_NAME` $PARENT_TYPE on line $instance_line of [`$file_name`]($instance_line_link)'
///      closing: To improve security and user experience, use concise and human-readable error messages in assert statements. Avoid exposing internal details or technical jargon, as this may confuse users or leak sensitive information.
///```
pub trait DetectorReportTemplate {
    fn id(&self) -> String;
    fn uid(&self) -> String;
    fn description(&self) -> String;
    fn severity(&self) -> String;
    fn tags(&self) -> Vec<String>;
    fn title_single_instance(&self) -> String;
    fn title_multiple_instance(&self) -> String;
    fn opening(&self) -> String;
    fn body_single_file_single_instance(&self) -> String;
    fn body_single_file_multiple_instance(&self) -> String;
    fn body_multiple_file_multiple_instance(&self) -> String;
    fn body_list_item_single_file(&self) -> String;
    fn body_list_item_multiple_file(&self) -> String;
    fn closing(&self) -> String;
    fn template(&self) -> String;
}

impl<T> Display for dyn CombinedDetector<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_combined_detector_display() {
        /// Dummy detector implementing both traits
        struct Dummy;
        impl Detector<Codebase<SealedState>> for Dummy {
            fn check(&self, _codebase: &Codebase<SealedState>) -> Option<Vec<DetectorResult>> {
                Some(vec![DetectorResult {
                    file_path: "f".into(),
                    offset_start: 0,
                    offset_end: 1,
                    extra: None,
                }])
            }
        }
        impl DetectorReportTemplate for Dummy {
            fn id(&self) -> String {
                "dummy".into()
            }
            fn uid(&self) -> String {
                "uid".into()
            }
            fn description(&self) -> String {
                String::new()
            }
            fn severity(&self) -> String {
                String::new()
            }
            fn tags(&self) -> Vec<String> {
                vec![]
            }
            fn title_single_instance(&self) -> String {
                String::new()
            }
            fn title_multiple_instance(&self) -> String {
                String::new()
            }
            fn opening(&self) -> String {
                String::new()
            }
            fn body_single_file_single_instance(&self) -> String {
                String::new()
            }
            fn body_single_file_multiple_instance(&self) -> String {
                String::new()
            }
            fn body_multiple_file_multiple_instance(&self) -> String {
                String::new()
            }
            fn body_list_item_single_file(&self) -> String {
                String::new()
            }
            fn body_list_item_multiple_file(&self) -> String {
                String::new()
            }
            fn closing(&self) -> String {
                String::new()
            }
            fn template(&self) -> String {
                String::new()
            }
        }
        let det: SorobanDetector<Codebase<SealedState>> = Box::new(Dummy);
        assert_eq!(det.to_string(), "dummy");
    }
}
