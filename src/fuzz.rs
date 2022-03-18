use fuzzcheck::sensors_and_pools::{CodeCoverageSensor, SimplestToActivateCounterPool};
use fuzzcheck::StringSerializer;
use syn::visit::Visit;
use syn::DeriveInput;

// This function should never panic. It is the “test function” that we want to fuzz
fn test_parse_declaration(ts: &fuzzcheck_proc_macro2::TokenStream) {
    let ts: proc_macro2::TokenStream = ts.clone().into();

    // fuzzcheck replaces the panic hook to report and categorise test failures
    // but in this case, we are okay with `venial` panicking, so we replace
    // fuzzcheck’s hook with one that does nothing
    let old_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| {}));

    let result = std::panic::catch_unwind(|| crate::parse_declaration(ts.clone()));
    // now we reinstate fuzzcheck's panic hook
    std::panic::set_hook(old_hook);

    let syn_result: Result<syn::DeriveInput, _> = syn::parse2(ts);

    if let Ok(derive_input) = syn_result {
        if result.is_err() && !accept_derive_input(&derive_input) {
            panic!("syn can parse but venial can't")
        }
    }
}

// This is the actual fuzz test
// launch it using cargo fuzzcheck
#[test]
fn fuzz_parse() {
    // This sensor observes the code coverage that results from running each test.
    // Here, we tell it to observe all code from files with a relative path (i.e. from this crate)
    // as well as code from syn-1.0.89 (but not from the gen/ module as it contains the `visit` code, which
    // is not that interesting).
    let sensor = CodeCoverageSensor::new(|f| {
        f.is_relative()
            || (f.to_str().unwrap().contains("/syn-1.0.89/src/")
                && !f.to_str().unwrap().contains("/syn-1.0.89/src/gen/"))
    });

    // This pool tries to find the smallest token stream that reaches each code region
    // It is a good, basic pool to use to maximise code coverage
    let pool = SimplestToActivateCounterPool::new("simplest_cov", sensor.count_instrumented);

    let result = fuzzcheck::fuzz_test(test_parse_declaration)
        // specify how to generate values of type TokenStream (we use the default)
        .default_mutator()
        // and specify how to save token streams to the file system (here, we convert them from/to strings,
        // which I am worried may be lossy)
        .serializer(StringSerializer::new("rs"))
        // then we give the sensor and pool we have constructed
        .sensor_and_pool(sensor, pool)
        // take additional arguments from the cargo-fuzzcheck command line tool
        .arguments_from_cargo_fuzzcheck()
        // but override the maximum complexity, there's no need to generate huge token streams
        .maximum_complexity(1024.)
        // and stop the fuzzer after the first test failure is found
        // it makes sense to do so because there is only one panic statement in the test function
        // so unless there is a panic within syn, only one test failure will ever be found
        .stop_after_first_test_failure(true)
        // that's it, now launch the fuzz test
        .launch();

    assert!(!result.found_test_failure);
}

/// Returns true if it's okay for this input not to be parsed by venial
fn accept_derive_input(i: &DeriveInput) -> bool {
    let mut visitor = AcceptDeriveInputVisitor(false);
    visitor.visit_derive_input(i);
    visitor.0
}

/// A visitor to detect values that we are ok with venial not parsing
struct AcceptDeriveInputVisitor(bool);

impl<'ast> Visit<'ast> for AcceptDeriveInputVisitor {
    fn visit_expr_closure(&mut self, _i: &'ast syn::ExprClosure) {
        // enum A { X = |a, | {} }
        self.0 = true;
    }
    fn visit_attr_style(&mut self, i: &'ast syn::AttrStyle) {
        match i {
            syn::AttrStyle::Outer => {}
            syn::AttrStyle::Inner(_bang) => {
                // enum A { #![a] }
                self.0 = true;
            }
        }
    }
    fn visit_variadic(&mut self, _i: &'ast syn::Variadic) {
        // fn x(...)
        self.0 = true;
    }
    fn visit_fn_arg(&mut self, i: &'ast syn::FnArg) {
        match i {
            syn::FnArg::Receiver(_) => {
                // fn x(self)
                self.0 = true;
            }
            syn::FnArg::Typed(x) => {
                let pat = x.pat.as_ref();
                match pat {
                    syn::Pat::Ident(_) => {}
                    _ => {
                        // any function argument that's not an identifier is accepted
                        self.0 = true;
                    }
                }
            }
        }
    }
    fn visit_type_macro(&mut self, _i: &'ast syn::TypeMacro) {
        // enum A where b !{ : } : { }
        // this is actually very difficult for the fuzzer to find
        // I got lucky the first time
        self.0 = true;
    }
}
