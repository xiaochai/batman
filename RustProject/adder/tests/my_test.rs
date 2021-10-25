mod sub;

#[test]
fn adder_test() {
    sub::setup();
    assert_eq!(adder::adder(9.0, 10.0), 19.0);
}

#[test]
fn my_note() {
    assert_eq!(adder::adder(9.0, 10.0), 19.0);
}