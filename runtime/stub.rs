#[repr(C)]
#[derive(PartialEq, Eq, Copy, Clone)]
struct SnakeVal(u64);

#[repr(C)]
struct SnakeHeap {
    size: u64,
    elts: *const SnakeVal,
}

use std::collections::HashSet;

/* You can use this function to cast a pointer to an array on the heap
 * into something more convenient to access
 *
 */
fn load_snake_heap(p: *const u64) -> SnakeHeap {
    unsafe {
        let size = *p;
        SnakeHeap {
            size,
            elts: std::mem::transmute(p.add(1)),
        }
    }
}

#[export_name = "\x01isbool"]
extern "sysv64" fn isbool(v: SnakeVal) -> SnakeVal {
    if(v == SNAKE_FLS || v == SNAKE_TRU){
        return SNAKE_TRU;
    }
    return SNAKE_FLS;
}


#[export_name = "\x01isnum"]
extern "sysv64" fn isnum(v: SnakeVal) -> SnakeVal {
    if(v.0 & 1 == 0){
        return SNAKE_TRU;
    }
    return SNAKE_FLS;
}




static BOOL_TAG: u64 = 0x00_00_00_00_00_00_00_07;
static ARR_TAG: u64 = 0x00_00_00_00_00_00_00_01;
static STR_TAG: u64 = 0x00_00_00_00_00_00_00_03;
static CHAR_TAG: u64 = 0x00_00_00_00_00_00_00_05;

static SNAKE_TRU: SnakeVal = SnakeVal(0xFF_FF_FF_FF_FF_FF_FF_FF);
static SNAKE_FLS: SnakeVal = SnakeVal(0x7F_FF_FF_FF_FF_FF_FF_FF);

#[link(name = "compiled_code", kind = "static")]
extern "sysv64" {

    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    #[link_name = "\x01start_here"]
    fn start_here() -> SnakeVal;
}

fn unsigned_to_signed(x: u64) -> i64 {
    i64::from_le_bytes(x.to_le_bytes())
}

fn sprint_snake_val(x: SnakeVal) -> String {
    //println!("YELLO");

    fn print_snake(x: SnakeVal, s: &mut HashSet<u64>) -> String {
        //println!("MELLO");

        if x == SNAKE_TRU {return String::from("true")}
        else if x == SNAKE_FLS {return String::from("false")}
        // this should get the first tag bit
        match x.0 & 0b1 {
            // num case
            0b0 => {
                //println!("NOT ARRAY");
                format!("{}", unsigned_to_signed(x.0) >> 1)
            },
            // array or closure case
            0b1 => {
                //println!("ARRAY!!");
                // array

                if x.0 & 0b111 == 0b001 {
                    let mut arr_out: String = "[".to_string();

                    //println!("IN ARRAY PRINT");
                    if s.contains(&x.0.clone()) {return String::from("<loop>")}
                    
                    s.insert(x.0);
                    let addr : *const u64 = (x.0 - 1) as *const u64 ;
                    //println!("ADDR {}", x.0 - 1);
                    let arr = load_snake_heap(addr);
                    let mut cur_val = arr.elts;


                    
                    //println!("ARR SIZE{}", arr.size);
                    for i in 0 .. (arr.size) {
                        arr_out = arr_out + &format!("{}, ", print_snake(unsafe{*cur_val}, &mut s.clone()));
                        // this should be fine unless it goes outside the heap, but hopefully not a problem
                        cur_val = unsafe{std::mem::transmute(cur_val.add(1))};
                    }

                    arr_out.pop();
                    arr_out.pop();

                    if(arr_out.len() == 0){
                        arr_out = "[".to_string();
                    }

                    arr_out + "]"
                }
                // string
                else if x.0 & 0b111 == 0b011 {
                    let mut str_out = "".to_string();
                    // TODO: UPDATE TO PRINT STRINGS
                    // 
                    s.insert(x.0);
                    let addr : *const u64 = (x.0 - 3) as *const u64 ;
                    //println!("ADDR {}", x.0 - 1);
                    let st = load_snake_heap(addr);
                    let mut cur_val = st.elts;

                    //println!("ARR SIZE{}", arr.size);
                    for i in 0 .. (st.size) {
                        str_out = str_out + &format!("{}", print_snake(unsafe{*cur_val}, &mut s.clone()));
                        // this should be fine unless it goes outside the heap, but hopefully not a problem
                        cur_val = unsafe{std::mem::transmute(cur_val.add(1))};
                    }
                    str_out
                }
                // char
                else if x.0 & 0b111 == 0b101 {
                    return String::from( char::from_u32(x.0 as u32  >> 3 ).unwrap())
                }
                // invalid
                else { return format!("Invalid snake value 0x{:x}", x.0)} 
            }
            otherwise => return format!("Invalid snake value 0x{:x}", x.0)
        } // match x.0 & 0b1
    } // print_snake
    print_snake(x, &mut HashSet::new())
}

#[export_name = "\x01err"]
extern "sysv64" fn err() {
    std::process::exit(1)
}

#[export_name = "\x01print_snake_val"]
extern "sysv64" fn print_snake_val(v: SnakeVal) -> SnakeVal {
    println!("{}", sprint_snake_val(v));
    v
}

#[export_name = "\x01isarray"]
extern "sysv64" fn isarray(v: SnakeVal) -> SnakeVal {
    if(v.0 & 0b111 == 1){
        return SNAKE_TRU;
    }
    return SNAKE_FLS;
}

#[export_name = "\x01isstring"]
extern "sysv64" fn isstring(v: SnakeVal) -> SnakeVal {
    if(v.0 & 0b111 == 3){
        return SNAKE_TRU;
    }
    return SNAKE_FLS;
}

#[export_name = "\x01ischar"]
extern "sysv64" fn ischar(v: SnakeVal) -> SnakeVal {
    if(v.0 & 0b111 == 5){
        return SNAKE_TRU;
    }
    return SNAKE_FLS;
}


#[export_name = "\x01dbg"]
extern "sysv64" fn dbg(stack: u64) {
    println!("Rsp: 0x{:x}", stack);
}

#[export_name = "\x01snake_error"]
extern "sysv64" fn snake_error(ecode: i64) -> SnakeVal {
        
        if ecode == 0 {
            eprintln!("Operation overflowed");
        }
        else if ecode == 1 {
            eprintln!("arithmetic expected a number");
        }
        else if ecode == 2 {
            eprintln!("comparison expected a number");
        }
        else if ecode == 3 {
            eprintln!("logic expected a boolean");
        }
        else if ecode == 4 {
            eprintln!("if expected a boolean");
        }
        else if ecode == 5 {
            eprintln!("range based indexing must be non-decreasing");
        } 
        else if ecode == 6 {
            eprintln!("index out of bounds");
        } 
        else if ecode == 7 {
            eprintln!("indexed into non-string/non-array");
        } 
        else if ecode == 8 {
            eprintln!("attempted to set string index as nonchar");
        } 
        else if ecode == 9 {
            eprintln!("index not a number");
        }
        else if ecode == 10 {
            eprintln!("length called on non-string/non-array");
        }
    

    std::process::exit(1)
}

fn main() {
    let output = unsafe { start_here() };
    println!("{}", sprint_snake_val(output));
}
