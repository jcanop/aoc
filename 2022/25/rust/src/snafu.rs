pub fn to(value: u64) -> String {
    let mut value = value;
    let mut vec: Vec<char> = Vec::new();
    while value > 0 {
        let mut overflow: bool = false;
        match value % 5 {
            0 => vec.insert(0, '0'),
            1 => vec.insert(0, '1'),
            2 => vec.insert(0, '2'),
            3 => { vec.insert(0, '='); overflow = true },
            4 => { vec.insert(0, '-'); overflow = true },
            _ => unreachable!()
        }
        value /= 5;
        if overflow { value += 1; }
    }
    vec.into_iter().collect()
}

pub fn from(value: &str) -> u64 {
    let mut result = 0;
    for (i, c) in value.chars().enumerate() {
        let p = 5u64.pow((value.len() - i - 1) as u32);
        match c {
            '0' => (),
            '1' => result += 1 * p,
            '2' => result += 2 * p,
            '=' => result -= 2 * p,
            '-' => result -= 1 * p,
            _ => unreachable!()
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    const DECIMALS: [u64; 29] = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022,
        12345, 314159265, 1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3,
        37, 4890 ];
    const SNAFUS: [&'static str; 29] = [ "1", "2", "1=", "1-", "10", "11", "12", "2=",
        "2-", "20", "1=0","1-0", "1=11-2", "1-0---0", "1121-1110-1=0", "1=-0-2",
        "12111", "2=0=", "21", "2=01", "111", "20012", "112", "1=-1=", "1-12", "12",
        "1=", "122", "2=-1=0" ];

    #[test]
    fn test_to() {
        for i in 0..DECIMALS.len() {
            assert_eq!(to(DECIMALS[i]), SNAFUS[i]);
        }
    }
}
