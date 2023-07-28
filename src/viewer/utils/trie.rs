#![allow(dead_code)]

use std::str;

use trie_rs::TrieBuilder;

pub struct Trie {
    items: Vec<String>,
    trie: trie_rs::Trie<u8>,
}

impl Clone for Trie {
    fn clone(&self) -> Self {
        Self::from_iter(self.items.iter().cloned())
    }
}

impl FromIterator<String> for Trie {
    fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
        let mut builder = TrieBuilder::new();

        let mut items = Vec::new();
        for id in iter {
            builder.push(&id);
            items.push(id);
        }
        let trie = builder.build();

        Trie { items, trie }
    }
}

impl Trie {
    pub fn all(&self) -> &[String] {
        &self.items
    }

    pub fn autocomplete(&self, key: &str) -> Option<String> {
        if key.is_empty() {
            longest_common_prefix(&self.items)
        } else {
            longest_common_prefix(&self.predict(key))
        }
    }

    pub fn predict(&self, key: &str) -> Vec<String> {
        let trie_search_result = self.trie.predictive_search(key);
        (trie_search_result.into_iter()).map(|s| String::from_utf8(s).unwrap()).collect()
    }
}

// https://leetcode.com/problems/longest-common-prefix/solutions/1134124/faster-than-100-in-memory-and-runtime-by-rust/
fn longest_common_prefix(strs: &[String]) -> Option<String> {
    if strs.is_empty() {
        return None;
    }

    let mut str_iters = strs.iter().map(|s| s.chars()).collect::<Vec<_>>();

    for (i, c) in strs[0].char_indices() {
        for str_iter in &mut str_iters {
            if str_iter.next().filter(|&x| x == c).is_none() {
                return Some(strs[0][..i].to_string());
            }
        }
    }

    Some(strs[0].clone())
}
