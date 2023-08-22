// --- Constants ---
const INPUT_FILE:&'static str = "../input/input.txt";

fn sanitize(a: &mut [u8]) {
    for i in 0 .. a.len() {
        if a[i] == 'i' as u8 || a[i] == 'o' as u8 || a[i] == 'l' as u8 {
            a[i] += 1;
            for j in i + 1 .. a.len() {
                a[j] = 'a' as u8;
            }
            break;
        }
    }
}

fn next(a: &mut [u8]) {
    for i in (0 .. a.len()).rev() {
        a[i] += 1;
        if a[i] == 'i' as u8 || a[i] == 'o' as u8 || a[i] == 'l' as u8 {
            a[i] += 1;
        }
        if a[i] > 'z' as u8 {
            a[i] = 'a' as u8;
        } else {
            break;
        }
    }
}

fn rule_1(a: &[u8]) -> bool {
    for i in 0 .. a.len() - 2 {
        if a[i + 1] == a[i] + 1 && a[i + 2] == a[i] + 2 { return true; }
    }
    false
}

fn rule_2(a: &[u8]) -> bool {
    let mut c = 0u8;
    let mut n = 0;
    for i in 0 .. a.len() - 1 {
        if a[i] == a[i + 1] && c != a[i] {
            n += 1;
            c = a[i];
        }
        if n == 2 { return true; }
    }
    false
}

// --- Main function ---
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // --- Read and parse the input file ---
    let text = std::fs::read_to_string(INPUT_FILE)?.trim().to_string();
    let mut pass = text.into_bytes();

    // --- Search next password ---
    let mut count = 1;
    sanitize(&mut pass);
    loop {
        next(&mut pass);
        if rule_1(&pass) && rule_2(&pass) {
            println!("{}. Next password: {}", count, String::from_utf8(pass.clone())?);
            count += 1;
            if count > 2 { break; }
        }
    }

    Ok(())
}

//res.into_iter().collect()
