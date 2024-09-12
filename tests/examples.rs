/* Sunbeam Tests */

use snake::runner;

macro_rules! mk_test {
    ($test_name:ident, $file_name:expr, $expected_output:expr) => {
        #[test]
        fn $test_name() -> std::io::Result<()> {
            test_example_file($file_name, $expected_output)
        }
    };
}
macro_rules! mk_any_test {
    ($test_name:ident, $file_name:expr) => {
        #[test]
        fn $test_name() -> std::io::Result<()> {
            test_example_any($file_name)
        }
    };
}

macro_rules! mk_fail_test {
    ($test_name:ident, $file_name:expr, $expected_output:expr) => {
        #[test]
        fn $test_name() -> std::io::Result<()> {
            test_example_fail($file_name, $expected_output)
        }
    };
}

mk_test!(simple_range_1, "simple_range_1.sb", "[3, 4]");
mk_test!(string_range_1, "string_range_1.sb", "there");
mk_fail_test!(char_add1_err_1, "char_add1_err_1.sb", "arithmetic expected a number");
mk_fail_test!(char_plus_err_1, "char_plus_err_1.sb", "arithmetic expected a number");
mk_fail_test!(string_idx_oob_err_1, "string_idx_oob_err_1.sb", "index out of bounds");
mk_test!(arr_range_1, "arr_range_1.sb", "[0, 4]");
mk_fail_test!(char_comp_err_1, "char_comp_err_1.sb", "comparison expected a number");
mk_fail_test!(range_idx_wrong_direction, "range_indx_wrong_direction.sb", "range based indexing must be non-decreasing");
mk_fail_test!(string_slice_oob, "string_slice_oob.sb", "index out of bounds");
mk_test!(slice_after_concat, "slice_after_concat.sb", "onpee");
mk_test!(array_of_strings, "array_of_strings.sb", "coulda\nwood\na");
mk_test!(string_arr_slice, "string_arr_slice.sb", "me");
mk_test!(andrew_test, "Andrew_Case.sb", "Andrew");
mk_test!(milo_test, "Milo_Case.sb", "Milo R. Baumhardt");
mk_test!(char_array, "char_array.sb", "[a, r, r, a, y]");
mk_test!(char_init, "char_init.sb", "a");
mk_fail_test!(concat_arrays, "concat_arrays.sb", "arithmetic expected a number");
mk_fail_test!(concat_bools, "concat_bools.sb", "arithmetic expected a number");
mk_fail_test!(concat_ints, "concat_ints.sb", "arithmetic expected a number");
mk_test!(concatenation_test_five, "concatenation_test_five.sb", "This Case  Is A Bit More Complicated");
mk_test!(concatenation_test_four, "concatenation_test_four.sb", "Hello");
mk_test!(concatenation_test_three, "concatenation_test_three.sb", "Hello World!");
mk_test!(concatenation_test_two, "concatenation_test_two.sb", "Hello");
mk_test!(concatenation_test_one, "concatenation_test_one.sb", "yo");
mk_test!(index_string, "index_string.sb", "1\n2\n3\n4\n5\n6\n7\n8\n9");
mk_test!(loop_print, "loop_print.sb", "Looping!...\nCurrently on interation: \n0\nCurrently on interation: \n1\nCurrently on interation: \n2\nCurrently on interation: \n3\nCurrently on interation: \n4\nCurrently on interation: \n5");
mk_test!(print_char, "print_char.sb", "a\na");
mk_test!(print_string, "print_string.sb", "Hello world\nHello world");
mk_fail_test!(set_nonchar_one, "set_nonchar_one.sb", "attempted to set string index as nonchar");
mk_fail_test!(set_nonchar_two, "set_nonchar_two.sb", "attempted to set string index as nonchar");
mk_fail_test!(set_nonchar_three, "set_nonchar_three.sb", "attempted to set string index as nonchar");
mk_test!(set_string_index, "set_string_index.sb", "a\nb\nc\nd\ne\nf\ng\nh\ni");
mk_fail_test!(set_string_to_string, "set_string_to_string.sb", "attempted to set string index as nonchar");
mk_test!(strtest, "str.sb", "This program is working!");
mk_test!(strinit, "str_init.sb", "abcd");
mk_test!(char_eq_one, "char_eq_one.sb", "false\ntrue\nfalse\nfalse\nfalse");
mk_test!(str_eq, "str_eq.sb", "false\ntrue\nfalse\ntrue\nfalse\ntrue");
mk_fail_test!(gt_char, "gt_char.sb", "comparison expected a number");
mk_fail_test!(gt_string, "gt_string.sb", "comparison expected a number");
mk_fail_test!(gte_char, "gte_char.sb", "comparison expected a number");
mk_fail_test!(lt_char, "lt_char.sb", "comparison expected a number");
mk_fail_test!(lt_string, "lt_string.sb", "comparison expected a number");
mk_fail_test!(lte_char, "lte_char.sb", "comparison expected a number");
mk_fail_test!(and_char, "and_char.sb", "logic expected a boolean");
mk_fail_test!(and_string, "and_string.sb", "logic expected a boolean");
mk_fail_test!(or_char, "or_char.sb", "logic expected a boolean");
mk_fail_test!(or_string, "or_string.sb", "logic expected a boolean");
mk_test!(test2to2, "2to2.sb", "m");
mk_test!(char_params, "char_params.sb", "i true\ni\ni false\ni");
mk_test!(cmp_indices, "cmp_indices.sb", "true");
mk_test!(cmp_slices, "cmp_slices.sb", "false");
mk_test!(ischar, "ischar.sb", "true\nfalse\ntrue\nfalse\ntrue\nfalse\nfalse");
mk_test!(isstring, "isstring.sb", "true\ntrue\ntrue\nfalse\nfalse\nfalse\nfalse\nfalse");
mk_fail_test!(neg_index, "neg_index.sb", "index out of bounds");
mk_fail_test!(neg_range, "neg_range.sb", "index out of bounds");
mk_fail_test!(range_on_bool, "range_on_bool.sb", "indexed into non-string/non-array");
mk_fail_test!(range_on_char, "range_on_char.sb", "indexed into non-string/non-array");
mk_fail_test!(range_on_int, "range_on_int.sb", "indexed into non-string/non-array");
mk_test!(slice_vs_index, "slice_vs_index.sb", "false");
mk_test!(string_params, "string_params.sb", "INPUT true\nINPUT\nINPUT false\nINPUT");
mk_test!(recursive_concatenation, "recursive_concatenation.sb", "WRONG INPUTS IDIOT\nWRONG INPUTS IDIOT\n:3333333333333333333\nD:<<<<<<<<<<<");


// IMPLEMENTATION
fn test_example_file(f: &str, expected_str: &str) -> std::io::Result<()> {
    use std::path::Path;
    let p_name = format!("examples/{}", f);
    let path = Path::new(&p_name);

    let tmp_dir = tempfile::TempDir::new()?;
    let mut w = Vec::new();
    match runner::compile_and_run_file(path, tmp_dir.path(), &mut w) {
        Ok(()) => {
            let stdout = std::str::from_utf8(&w).unwrap();
            assert_eq!(stdout.trim(), expected_str)
        }
        Err(e) => {
            panic!("Expected {}, got an error: {}", expected_str, e)
        }
    }
    Ok(())
}

fn test_example_any(f: &str) -> std::io::Result<()> {
    use std::path::Path;
    let p_name = format!("examples/{}", f);
    let path = Path::new(&p_name);

    let tmp_dir = tempfile::TempDir::new()?;
    let mut w = Vec::new();
    match runner::compile_and_run_file(path, tmp_dir.path(), &mut w) {
        Ok(()) => {}
        Err(e) => {
            panic!("Got an error: {}", e)
        }
    }
    Ok(())
}

fn test_example_fail(f: &str, includes: &str) -> std::io::Result<()> {
    use std::path::Path;

    let tmp_dir = tempfile::TempDir::new()?;
    let mut w_run = Vec::new();
    match runner::compile_and_run_file(
        Path::new(&format!("examples/{}", f)),
        tmp_dir.path(),
        &mut w_run,
    ) {
        Ok(()) => {
            let stdout = std::str::from_utf8(&w_run).unwrap();
            panic!("Expected a failure but got: {}", stdout.trim())
        }
        Err(e) => {
            let msg = format!("{}", e);
            assert!(
                msg.contains(includes),
                "Expected error message to include the string \"{}\" but got the error: {}",
                includes,
                msg
            )
        }
    }
    Ok(())
}
