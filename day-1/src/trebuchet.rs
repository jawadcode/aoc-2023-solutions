use std::str::Lines;

pub fn one(document: Lines) -> u32 {
    document
        .map(|line| {
            let mut digits = line.bytes().filter_map(|ch| {
                #[allow(clippy::manual_is_ascii_check)]
                if (b'0'..=b'9').contains(&ch) {
                    Some((ch - b'0') as u32)
                } else {
                    None
                }
            });
            let first = digits.next().unwrap();
            let last = digits.last().unwrap_or(first);
            first * 10 + last
        })
        .sum()
}

struct Digits<'a> {
    line: &'a [u8],
    index: usize,
}

impl<'a> Digits<'a> {
    fn new(line: &'a str) -> Self {
        Self {
            line: line.as_bytes(),
            index: 0,
        }
    }
}

impl Iterator for Digits<'_> {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match &self.line[self.index..] {
                [b'o', b'n', b'e', ..] => {
                    self.index += 2;
                    return Some(1);
                }
                [b't', b'w', b'o', ..] => {
                    self.index += 2;
                    return Some(2);
                }
                [b't', b'h', b'r', b'e', b'e', ..] => {
                    self.index += 4;
                    return Some(3);
                }
                [b'f', b'o', b'u', b'r', ..] => {
                    self.index += 3;
                    return Some(4);
                }
                [b'f', b'i', b'v', b'e', ..] => {
                    self.index += 3;
                    return Some(5);
                }
                [b's', b'i', b'x', ..] => {
                    self.index += 2;
                    return Some(6);
                }
                [b's', b'e', b'v', b'e', b'n', ..] => {
                    self.index += 4;
                    return Some(7);
                }
                [b'e', b'i', b'g', b'h', b't', ..] => {
                    self.index += 4;
                    return Some(8);
                }
                [b'n', b'i', b'n', b'e', ..] => {
                    self.index += 3;
                    return Some(9);
                }
                [x @ b'0'..=b'9', ..] => {
                    self.index += 1;
                    return Some((x - b'0') as u32);
                }
                [] => return None,
                _ => self.index += 1,
            }
        }
    }
}

pub fn two(document: Lines) -> u32 {
    document
        .map(|line| {
            let mut digits = Digits::new(line);
            let first = digits.next().unwrap();
            let last = digits.last().unwrap_or(first);
            first * 10 + last
        })
        .sum()
}
