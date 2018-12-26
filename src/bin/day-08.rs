use std::str::FromStr;

#[allow(dead_code)]
static TEST_INPUT: &'static str = include_str!("day-08.test");
#[allow(dead_code)]
static INPUT: &'static str = include_str!("day-08.input");

trait Sink {
    type Node: std::fmt::Debug;
    fn new_node(&mut self) -> Self::Node;
    fn add_child(&mut self, parent: &mut Self::Node, child: Self::Node);
    fn add_metadata(&mut self, node: &mut Self::Node, metadata: usize);
}

fn parse<S: Sink, I: Iterator<Item = usize>>(sink: &mut S, source: &mut I) -> S::Node {
    let child_count = source.next().expect("expected child count");
    let meta_count = source.next().expect("expected metadata count");

    let mut node = sink.new_node();

    for _ in 0..child_count {
        let child = parse(sink, source);
        sink.add_child(&mut node, child);
    }

    for _ in 0..meta_count {
        let meta = source.next().expect("expected metadata");
        sink.add_metadata(&mut node, meta);
    }

    node
}

struct SumMetadata {
    next_name: u32,
    meta_total: usize,
}

impl SumMetadata {
    fn new() -> SumMetadata {
        SumMetadata {
            next_name: 'A' as u32,
            meta_total: 0,
        }
    }
}

impl Sink for SumMetadata {
    type Node = char;

    fn new_node(&mut self) -> Self::Node {
        let name = self.next_name;
        self.next_name += 1;
        std::char::from_u32(name).expect("ran out of unicode characters")
    }

    fn add_child(&mut self, parent: &mut Self::Node, child: Self::Node) {
        println!("parent {:?} has child {:?}", parent, child);
    }

    fn add_metadata(&mut self, node: &mut Self::Node, metadata: usize) {
        println!("parent {:?} has metadata {}", node, metadata);
        self.meta_total += metadata;
    }
}

#[derive(Debug, Default)]
struct Node {
    children: Vec<Node>,
    meta_sum: usize,
    value: usize,
}

struct FancyNodes;
impl Sink for FancyNodes {
    type Node = Node;

    fn new_node(&mut self) -> Self::Node {
        Node::default()
    }

    fn add_child(&mut self, parent: &mut Self::Node, child: Self::Node) {
        parent.children.push(child);
    }

    fn add_metadata(&mut self, node: &mut Self::Node, metadata: usize) {
        node.value += if node.children.is_empty() {
            metadata
        } else if metadata < 1 {
            0
        } else if metadata <= node.children.len() {
            node.children[metadata - 1].value
        } else {
            0
        }
    }
}

fn main() {
    let numbers: Vec<_> = INPUT
        .split_terminator(&[' ', '\n'][..])
        .map(|w| usize::from_str(w).expect("bad number in input"))
        .collect();

    let mut summer = SumMetadata::new();
    let _root = parse(&mut summer, &mut numbers.iter().cloned());
    println!("total: {}", summer.meta_total);

    let mut fancy = FancyNodes;
    let root = parse(&mut fancy, &mut numbers.iter().cloned());
    println!("root value: {}", root.value);
}
