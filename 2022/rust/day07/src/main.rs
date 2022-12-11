use std::str::FromStr;
use std::collections::*;
use std::io::prelude::*;
use std::io;
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::min;

struct Node {
    size: usize,
    children: HashMap<String, NodeRef>,
    parent: Option<NodeRef>
}

impl Node {
    fn new(size: usize) -> Rc<RefCell<Node>> {
        let n = Node {
            size: size,
            children: HashMap::new(),
            parent: None
        };
        Rc::new(RefCell::new(n))
    }

    fn total(self: &Self) -> usize {
        self.size + self.children.values().map(|n| n.borrow().total()).sum::<usize>()
    }

    fn total_with_max(self: &Self, max: usize) -> usize {
        let total = self.total();
        let self_total = if total <= max {
            total
        } else {
            0
        };
        self_total + self.children.values()
            .map(|n| n.borrow())
            .filter(|n| n.size == 0)
            .map(|n| n.total_with_max(max))
            .sum::<usize>()
    }

    fn smallest_total(self: &Self, target: usize) -> usize {
        let total = self.total();
        if total < target {
            usize::MAX
        } else {
            let smallest_child = self.children.values()
                .map(|n| n.borrow())
                .filter(|n| n.size == 0)
                .map(|n| n.smallest_total(target))
                .min().unwrap_or(usize::MAX);
            min(total, smallest_child)
        }
    }
}

type NodeRef = Rc<RefCell<Node>>;

fn add(node: &NodeRef, name: String, other: &NodeRef) -> NodeRef {
    let children = &mut node.borrow_mut().children;
    if let Some(existing) = children.get(&name) {
        Rc::clone(existing)
    } else {
        other.borrow_mut().parent = Some(Rc::clone(node));
        children.insert(name, Rc::clone(other));
        Rc::clone(other)
    }
}

fn process(cmd: String, cwd: NodeRef, root: NodeRef) -> NodeRef {
    if cmd.starts_with("$ cd /") {
        root
    } else if cmd.starts_with("$ ls") {
        cwd
    } else if cmd.starts_with("$ cd ..") {
        cwd.borrow().parent.as_ref().map(Rc::clone).unwrap()
    } else if cmd.starts_with("$ cd ") {
        if let Some((_, new_dir)) = cmd.rsplit_once(' ') {
            add(&cwd, String::from(new_dir), &Node::new(0))
        } else {
            cwd
        }
    } else {
        match cmd.split_once(' ') {
            Some(("dir", dir_name)) => {
                add(&cwd, String::from(dir_name), &Node::new(0));
            },
            Some((file_size, file_name)) => {
                let file_size = usize::from_str(file_size).unwrap();
                add(&cwd, String::from(file_name), &Node::new(file_size));
            },
            None => {}
        }
        cwd
    }
}

fn main() {
    let root = Node::new(0);
    let mut cwd = Rc::clone(&root);
    for line in io::stdin().lock().lines() {
        cwd = process(line.unwrap(), cwd, Rc::clone(&root));
    }
    let root = root.borrow();

    println!("{}", root.total_with_max(100000));

    let unused = 70000000 - root.total();
    let needed = 30000000 - unused;

    println!("{}", root.smallest_total(needed));
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_tree() {
        let root = Node::new(0);
        add(&root, String::from("a"), &Node::new(3));
        add(&root, String::from("b"), &Node::new(4));
        assert_eq!(root.borrow().total(), 7);
    }

    #[test]
    fn test_tree_dupes() {
        let root = Node::new(0);
        add(&root, String::from("a"), &Node::new(3));
        add(&root, String::from("a"), &Node::new(4));
        assert_eq!(root.borrow().total(), 3);
    }

    #[test]
    fn test_tree_nested() {
        let root = Node::new(0);
        let n1 = Node::new(1);
        let n2 = Node::new(2);
        add(&root, String::from("a"), &n1);
        add(&n1, String::from("a"), &n2);
        assert_eq!(root.borrow().total(), 3);
        assert_eq!(n1.borrow().total(), 3);
        assert_eq!(n2.borrow().total(), 2);
    }

    #[test]
    fn test_process() {
        let root = Node::new(0);
        let cwd = Rc::clone(&root);
        let cwd = process(String::from("$ cd a"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ ls"), cwd, Rc::clone(&root));
        let cwd = process(String::from("100 a"), cwd, Rc::clone(&root));
        assert_eq!(root.borrow().total(), 100);
        assert_eq!(cwd.borrow().total(), 100);
    }

    #[test]
    fn test_process_2() {
        let root = Node::new(0);
        let cwd = Rc::clone(&root);
        let cwd = process(String::from("$ cd a"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ ls"), cwd, Rc::clone(&root));
        let cwd = process(String::from("100 a"), cwd, Rc::clone(&root));
        let cwd = process(String::from("dir b"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ cd b"), cwd, Rc::clone(&root));
        let cwd = process(String::from("200 c"), cwd, Rc::clone(&root));
        assert_eq!(root.borrow().total(), 300);
        assert_eq!(cwd.borrow().total(), 200);
        assert_eq!(root.borrow().total_with_max(200), 200);
    }

    #[test]
    fn test_process_with_up() {
        let root = Node::new(0);
        let cwd = Rc::clone(&root);
        let cwd = process(String::from("$ cd a"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ ls"), cwd, Rc::clone(&root));
        let cwd = process(String::from("100 a"), cwd, Rc::clone(&root));
        let cwd = process(String::from("dir b"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ cd b"), cwd, Rc::clone(&root));
        let cwd = process(String::from("200 c"), cwd, Rc::clone(&root));
        let cwd = process(String::from("$ cd .."), cwd, Rc::clone(&root));
        assert_eq!(root.borrow().total(), 300);
        assert_eq!(cwd.borrow().total(), 300);
        assert_eq!(root.borrow().total_with_max(200), 200);
    }
}