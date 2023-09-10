use std::cell::RefCell;
use std::fmt::{self, Debug, Formatter};
use std::str::from_utf8;
use std::vec;

/// Represent a (monoalphabetic substitution) mapping from encoded letter to decoded letter.
#[derive(Clone, Default)]
struct Mapping {
    map: [Option<u8>; 26], // index map encoded letter to decoded letter
    members: u32,          // bitset of all decoded letters that are mapped
}

impl Mapping {
    pub fn get(&self, c: u8) -> Option<u8> {
        self.map[(c - b'A') as usize]
    }

    pub fn set(&self, c: u8, l: u8) -> Result<Mapping, ()> {
        let idx_c = (c - b'A') as usize;

        if self.map[idx_c].is_none() || self.map[idx_c] == Some(l) {
            let mut result = self.clone();

            result.map[idx_c] = Some(l);
            let idx_l = (l - b'a') as usize;
            result.members |= 1 << idx_l;

            Ok(result)
        } else {
            Err(())
        }
    }

    pub fn exists(&self, l: u8) -> bool {
        let idx_l = (l - b'a') as usize;
        self.members & (1 << idx_l) != 0
    }

    pub fn apply(&self, ciper: &[u8]) -> Vec<u8> {
        ciper
            .iter()
            .map(|c| {
                if c.is_ascii_uppercase() {
                    self.get(*c).unwrap_or(*c)
                } else {
                    *c
                }
            })
            .collect()
    }
}

impl Debug for Mapping {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for i in 0..26 {
            write!(f, "{}", (i + b'A') as char)?;
        }
        writeln!(f)?;

        for l in self.map.iter() {
            if let Some(l) = l {
                write!(f, "{}", *l as char)?;
            } else {
                write!(f, "?")?;
            }
        }
        writeln!(f)?;

        Ok(())
    }
}

/// A list of candidates of decoded text, stored as sorted list of words bytes.
#[derive(Clone, Copy)]
struct CandidateList<'a>(&'a [&'a [u8]]);
impl<'a> CandidateList<'a> {
    pub fn new(words: &'a [&'a [u8]]) -> Self {
        // check sorted
        for i in 1..words.len() {
            debug_assert!(words[i - 1] < words[i]);
        }
        // check same size
        for i in words {
            debug_assert_eq!(words[0].len(), i.len());
        }

        Self(words)
    }
}

/// Represents a ciper (encoded word) along with a list of candidates of corresponding decoded word.
#[derive(Clone, Copy)]
struct CrackWord<'a> {
    ciper: &'a [u8],
    candidates: CandidateList<'a>,
}

fn main() {
    let ciper = b"PRCSOFQX FP QDR AFOPQ CZSPR LA JFPALOQSKR, QDFP FP ZK LIU BROJZK MOLTROE";

    // Generate the crack pharse
    let clean_ciper = ciper
        .iter()
        .map(|&c| c.to_ascii_uppercase())
        .filter(|&c| c.is_ascii_uppercase() || c == b' ')
        .collect::<Vec<_>>();
    let mut crack_pharse = generate_crack_pharse(&clean_ciper);
    // special assumption: PRCSOFQX -> SECURITY
    if let Some(i) = crack_pharse.iter().rposition(|cw| cw.ciper == b"PRCSOFQX") {
        crack_pharse.remove(i);
        crack_pharse.insert(
            0,
            CrackWord {
                ciper: b"PRCSOFQX",
                candidates: CandidateList::new(&[b"security"]),
            },
        );
    } else {
        panic!("ciper PRCSOFQX not found");
    }

    // Run the cracker
    let mappings = crack_mapping(Mapping::default(), &crack_pharse);
    for mapping in mappings {
        let decoded = mapping.apply(ciper);
        let decoded = from_utf8(&decoded).unwrap();

        println!("====================");
        for c in 'A'..='Z' {
            print!("{c}");
        }
        println!();
        for c in b'A'..=b'Z' {
            let l = mapping.get(c).unwrap_or(b'?') as char;
            print!("{l}");
        }
        println!("\n\n{decoded}");
        println!("====================\n");
    }
}

fn generate_crack_pharse(ciper: &[u8]) -> Vec<CrackWord<'_>> {
    if !ciper.iter().all(|&c| c == b' ' || c.is_ascii_uppercase()) {
        panic!(
            "ciper must only be uppercase alphabet or space: (ciper=`{}`)",
            from_utf8(ciper).unwrap()
        );
    }

    let max_length = ciper
        .split(|b| b == &b' ')
        .map(|s| s.len())
        .max()
        .unwrap_or(0);

    // Group ciper by length
    let mut ciper_by_length = vec![Vec::new(); max_length + 1];
    for word in ciper.split(|b| b == &b' ') {
        ciper_by_length[word.len()].push(word);
    }
    // Get cadidate words by length
    let candidate_by_length = candidate_by_length(max_length);

    // Create the CrackPharse (list of CrackWord, a ciper along with their candidates word)
    // this information fully define the problem of finding the mapping.
    ciper_by_length
        .into_iter()
        .zip(candidate_by_length.iter())
        .flat_map(|(cipers, candidates)| {
            cipers.into_iter().map(|ciper| CrackWord {
                ciper,
                candidates: *candidates,
            })
        })
        .collect::<Vec<_>>()
}

fn candidate_by_length<'a>(max_length: usize) -> Vec<CandidateList<'a>> {
    let two_words: &[&[u8]] = &[
        b"am", b"an", b"as", b"at", b"be", b"by", b"do", b"go", b"he", b"if", b"in", b"is", b"it",
        b"me", b"my", b"no", b"of", b"on", b"or", b"so", b"to", b"up", b"us", b"we",
    ];
    let three_words: &[&[u8]] = &[
        b"all", b"and", b"any", b"are", b"boy", b"but", b"can", b"day", b"did", b"for", b"get",
        b"had", b"has", b"her", b"him", b"his", b"how", b"its", b"let", b"man", b"new", b"not",
        b"now", b"old", b"one", b"our", b"out", b"put", b"say", b"see", b"she", b"the", b"too",
        b"two", b"use", b"was", b"way", b"who", b"you",
    ];
    let else_words = include_bytes!("../words.txt").split(|b| b == &b'\n');

    // Build list of candidate words by length
    let mut words_by_length: Vec<Vec<&[u8]>> = vec![Vec::new(); max_length + 1];
    for word in else_words {
        if word.is_empty() {
            continue;
        }
        let word = &word[..word.len() - 1];
        if word.len() <= 3 || word.len() > max_length {
            continue;
        }
        words_by_length[word.len()].push(word);
    }
    let words_by_length = words_by_length.leak(); // BAD, but whatever

    // Build candidate words by length
    // using the above words_by_length, but special case for 2&3 length words
    let mut candidate_by_length = words_by_length
        .iter_mut()
        .map(|v| {
            v.sort_unstable();
            CandidateList::new(v.as_slice())
        })
        .collect::<Vec<_>>();
    if let Some(cans) = candidate_by_length.get_mut(2) {
        *cans = CandidateList::new(two_words);
    }
    if let Some(cans) = candidate_by_length.get_mut(3) {
        *cans = CandidateList::new(three_words);
    }

    candidate_by_length
}

fn crack_mapping(base: Mapping, crack_pharse: &[CrackWord]) -> Vec<Mapping> {
    let mappings = RefCell::new(Vec::new());
    pharse_mapping(base, crack_pharse, |mapping| {
        mappings.borrow_mut().push(mapping)
    });
    mappings.take()
}

fn pharse_mapping(base: Mapping, crack_pharse: &[CrackWord], produce: impl Fn(Mapping) + Copy) {
    match crack_pharse {
        [crack_word] => word_mapping(base, *crack_word, produce),
        [crack_word, rest_pharse @ ..] => word_mapping(base, *crack_word, |new_base| {
            pharse_mapping(new_base, rest_pharse, produce)
        }),
        [] => (),
    }
}

fn word_mapping(base: Mapping, crack_word: CrackWord, produce: impl Fn(Mapping) + Copy) {
    fn inner(i: usize, base: Mapping, crack_word: CrackWord, produce: impl Fn(Mapping) + Copy) {
        let CrackWord { ciper, candidates } = crack_word;
        if i >= ciper.len() {
            produce(base);
            return;
        }

        let c = ciper[i];
        if let Some(l) = base.get(c) {
            // Filter only candidates that the first letter = decoded(first ciper letter)
            // utilizing sorted property of candidates

            let a = candidates.0.partition_point(|can| can[i] < l);
            let b = candidates.0.partition_point(|can| can[i] <= l);
            let candidates = &candidates.0[a..b];

            if candidates.is_empty() {
                return;
            }

            let new_crackword = CrackWord {
                ciper,
                candidates: CandidateList::new(candidates),
            };
            inner(i + 1, base, new_crackword, produce)
        } else {
            // try all possible mapping (a-z)
            for l in b'a'..=b'z' {
                if base.exists(l) {
                    continue;
                }

                let a = candidates.0.partition_point(|can| can[i] < l);
                let b = candidates.0.partition_point(|can| can[i] <= l);
                let candidates = &candidates.0[a..b];

                if candidates.is_empty() {
                    continue;
                }

                if let Ok(new_base) = base.set(c, l) {
                    let new_crackword = CrackWord {
                        ciper,
                        candidates: CandidateList::new(candidates),
                    };
                    inner(i + 1, new_base, new_crackword, produce);
                }
            }
        }
    }

    inner(0, base, crack_word, produce)
}
