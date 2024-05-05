use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
    ops::{Bound, RangeBounds},
    rc::Rc,
    sync::Mutex,
};

/// A rope is a way of representing a string that improves the efficiency of modifications.
#[derive(Clone, Debug)]
pub struct Rope {
    /// The node which is an ancestor of the entire string.
    root: Rc<Node>,
    /// The length of the string represented by this rope.
    len: usize,
    /// The maximum number of steps to get from `root` to any leaf.
    depth: usize,
}

impl Rope {
    /// Whether the rope contains any characters.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// The string length of this rope.
    pub fn len(&self) -> usize {
        self.len
    }

    /// If the given rope is not balanced, rebuilds the tree structure so that it is.
    #[must_use]
    pub fn rebalanced(&self) -> Self {
        /// The nth fibonacci number.
        fn fib(n: usize) -> usize {
            /// Cache all computed values of `fib(_)`.
            static MEMOIZED: Mutex<Vec<usize>> = Mutex::new(vec![]);

            let mut guard = MEMOIZED.lock().unwrap();
            if guard.len() < 2 {
                guard.extend([0, 1]);
            }
            while guard.len() <= n {
                let next = guard.iter().rev().take(2).copied().sum();
                guard.push(next);
            }
            guard[n]
        }

        /// Converts a list of leaves into a single rope.
        fn treeify(leaves: &[Rc<str>]) -> Rope {
            /// The actual recursion of `treeify`.
            fn treeify(subtrees: &[Rope]) -> Rope {
                match subtrees {
                    [] => unreachable!(
                        "treeify will only be called on leaves collected from a pre-existing rope"
                    ),
                    [r] => r.clone(),
                    [r1, r2] => r1.concat_unbalanced(r2),
                    [..] => {
                        let midpoint = subtrees.len() / 2;
                        let left = treeify(&subtrees[..midpoint]);
                        let right = treeify(&subtrees[midpoint..]);
                        left.concat_unbalanced(&right)
                    }
                }
            }

            let ropes = leaves.iter().cloned().map(Rope::from).collect::<Vec<_>>();
            treeify(&ropes)
        }

        let is_balanced = fib(self.depth + 2) <= self.root.weight();
        if !is_balanced {
            let leaves = self.root.iter().collect::<Vec<_>>();
            treeify(&leaves)
        } else {
            self.clone()
        }
    }

    /// Like `concat()`, but doesn't guarantee that the returned rope is balanced.
    #[must_use]
    fn concat_unbalanced(&self, rhs: &Self) -> Self {
        if self.is_empty() {
            rhs.clone()
        } else if rhs.is_empty() {
            self.clone()
        } else {
            Self {
                root: Node::boxed_branch(self.len, Rc::clone(&self.root), Rc::clone(&rhs.root)),
                len: self.len + rhs.len,
                depth: self.depth.max(rhs.depth) + 1,
            }
        }
    }

    /// Concatenates two ropes. If `self` represents the string `s1` and `rhs` represents the
    /// string `s2`, then their concatenation will represent the string `s1 + s2`.
    #[must_use]
    pub fn concat(&self, rhs: &Self) -> Self {
        self.concat_unbalanced(rhs).rebalanced()
    }

    /// Inserts the string `s` between the byte at `idx` and the byte at `idx + 1`. Returns `None`
    /// if `idx > self.len()` or the bytes at `idx` and `idx + 1` are both part of the same
    /// character.
    #[must_use]
    pub fn insert<S>(&self, s: S, idx: usize) -> Option<Self>
    where
        S: Into<String>,
    {
        let s = Self::from(s.into());
        if idx == self.len {
            Some(self.concat(&s))
        } else {
            let (left, right) = self.split(idx)?;
            Some(left.concat_unbalanced(&s).concat(&right))
        }
    }

    /// Splits the rope into a rope for the first `idx` bytes of the string representation and a
    /// rope containing the rest of the bytes. If `idx > self.len()` or the split would result in a
    /// multi-byte character being split between the two ropes, `None` is returned instead.
    #[must_use]
    pub fn split(&self, idx: usize) -> Option<(Self, Self)> {
        /// Splits the root if it's a branch.
        fn split_branch(this: &Rope) -> Option<(Rope, Rope)> {
            match Rc::as_ref(&this.root) {
                Node::Branch { left, right, .. } => Some((
                    Rope {
                        root: Rc::clone(left),
                        len: this.root.weight(),
                        depth: this.depth - 1,
                    },
                    Rope {
                        root: Rc::clone(right),
                        len: this.len - this.root.weight(),
                        depth: this.depth - 1,
                    },
                )),
                Node::Leaf(_) => None,
            }
        }

        if idx > self.len {
            None
        } else if idx == 0 {
            Some((Rope::from("".to_owned()), self.clone()))
        } else if idx == self.len {
            Some((self.clone(), Rope::from("".to_owned())))
        } else if idx > self.root.weight() {
            // The left child of `root` can be transplanted into the left result without
            // modification.
            let (left_left, right) = match split_branch(self) {
                Some(subtrees) => subtrees,
                None => unreachable!(
                    "We already know that root.weight < idx < len, so root can't be a leaf"
                ),
            };
            let (left_right, right) = right.split(idx - self.root.weight())?;
            Some((left_left.concat(&left_right), right))
        } else {
            // Either `root` is a leaf and must be split into two or the right child of `root` can
            // be transplanted into the right result without modification.
            let (left, right_right) = match split_branch(self) {
                Some(subtrees) => subtrees,
                None => {
                    let Node::Leaf(s) = Rc::as_ref(&self.root) else {
                        unreachable!("split_branch returned None")
                    };
                    // TODO(Rust 1.80.0): Use `s.split_at_checked(idx)` instead.
                    return if !s.is_char_boundary(idx) {
                        None
                    } else {
                        let (left, right) = s.split_at(idx);
                        Some((Rope::from(left.to_owned()), Rope::from(right.to_owned())))
                    };
                }
            };
            let (left, right_left) = left.split(idx)?;
            Some((left, right_left.concat(&right_right)))
        }
    }

    /// Produces a sub-rope of `self` which represents `s[idx]` where `s` is the string represented
    /// by `self`.
    #[must_use]
    pub fn sub_rope(&self, idx: impl RangeBounds<usize>) -> Option<Self> {
        let init = match idx.end_bound() {
            Bound::Unbounded => self.clone(),
            Bound::Excluded(&end) => self.split(end)?.0,
            Bound::Included(&end) => self.split(end + 1)?.0,
        };
        match idx.start_bound() {
            Bound::Unbounded => Some(init),
            Bound::Excluded(&start) => Some(init.split(start + 1)?.1),
            Bound::Included(&start) => Some(init.split(start)?.1),
        }
    }

    /// Like [`str::is_char_boundary()`].
    ///
    /// [`str::is_char_boundary()`]: https://doc.rust-lang.org/std/primitive.str.html#method.is_char_boundary
    pub fn is_char_boundary(&self, idx: usize) -> bool {
        if idx > self.len {
            false
        } else if idx == 0 || idx == self.len {
            true
        } else {
            match Rc::as_ref(&self.root) {
                Node::Leaf(s) => s.is_char_boundary(idx),
                Node::Branch {
                    left_weight,
                    left,
                    right,
                } => match idx.cmp(left_weight) {
                    Ordering::Equal => true,
                    Ordering::Less => Rope {
                        root: Rc::clone(left),
                        len: *left_weight,
                        depth: self.depth - 1,
                    }
                    .is_char_boundary(idx),
                    Ordering::Greater => Rope {
                        root: Rc::clone(right),
                        len: self.len - left_weight,
                        depth: self.depth - 1,
                    }
                    .is_char_boundary(idx - left_weight),
                },
            }
        }
    }

    /// Creates a rope that represents `self.to_string().replace_range(idx, "")`.
    #[must_use]
    pub fn without_range(&self, idx: impl RangeBounds<usize>) -> Option<Self> {
        let (init, tail) = match idx.end_bound() {
            Bound::Unbounded => (self.clone(), Rope::from("".to_owned())),
            Bound::Included(&end) => self.split(end + 1)?,
            Bound::Excluded(&end) => self.split(end)?,
        };
        let init = match idx.start_bound() {
            Bound::Unbounded => return Some(tail),
            Bound::Included(&start) => init.split(start)?.0,
            Bound::Excluded(&start) => init.split(start + 1)?.0,
        };
        Some(init.concat(&tail))
    }

    /// Creates a rope that represents `self.to_string().replace_range(idx, to_insert.to_string())`.
    pub fn splice(&self, to_remove: impl RangeBounds<usize>, to_insert: Self) -> Option<Self> {
        let (init, tail) = match to_remove.end_bound() {
            Bound::Unbounded => (self.clone(), Self::from("".to_string())),
            Bound::Excluded(&end) => self.split(end)?,
            Bound::Included(&end) => self.split(end + 1)?,
        };
        let init = match to_remove.start_bound() {
            Bound::Unbounded => return Some(to_insert.concat(&tail)),
            Bound::Excluded(&start) => init.split(start + 1)?.0,
            Bound::Included(&start) => init.split(start)?.0,
        };
        Some(init.concat(&to_insert).concat(&tail))
    }
}

impl Display for Rope {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.root.iter().try_for_each(|s| write!(f, "{s}"))
    }
}

impl From<Rc<str>> for Rope {
    fn from(value: Rc<str>) -> Self {
        let len = value.len();
        Self {
            root: Node::boxed_leaf(value),
            len,
            depth: 0,
        }
    }
}

impl From<String> for Rope {
    fn from(value: String) -> Self {
        Self::from(Rc::from(value.into_boxed_str()))
    }
}

/// A node in the tree representation of a rope.
#[derive(Debug)]
enum Node {
    /// A non-leaf node.
    Branch {
        /// The total length of the rope rooted at this node's left child.
        left_weight: usize,
        /// The left child of this node.
        left: Rc<Self>,
        /// The right child of this node.
        right: Rc<Self>,
    },
    /// A leaf node.
    Leaf(Rc<str>),
}

impl Node {
    /// Creates a branch that is already in an `Rc`.
    fn boxed_branch(left_weight: usize, left: Rc<Self>, right: Rc<Self>) -> Rc<Self> {
        Rc::new(Self::Branch {
            left_weight,
            left,
            right,
        })
    }

    /// Creates a leaf that is already in a `Rc`.
    fn boxed_leaf(s: Rc<str>) -> Rc<Self> {
        Rc::new(Self::Leaf(s))
    }

    /// The weight of the node. The weight of a branch node is the total length of the rope rooted
    /// at its left child. The weight of a leaf node is the length of the fragment located at that
    /// leaf.
    fn weight(&self) -> usize {
        match *self {
            Self::Branch { left_weight, .. } => left_weight,
            Self::Leaf(ref s) => s.len(),
        }
    }

    /// A shortcut for `(&self).into_iter()`.
    fn iter(&self) -> impl Iterator<Item = Rc<str>> + '_ {
        self.into_iter()
    }
}

impl IntoIterator for Node {
    type IntoIter = NodeIter;

    type Item = Rc<str>;

    fn into_iter(self) -> Self::IntoIter {
        NodeIter::new(Rc::new(self))
    }
}

/// An iterator over the leaves descended from an owned `Node`.
struct NodeIter {
    /// The nodes that have not yet been visited by the iterator. The last node is a `Leaf`. All
    /// other nodes are the right child of one of that `Leaf`'s ancestors.
    nodes: Vec<Rc<Node>>,
}

impl NodeIter {
    /// Creates a new iterator over the leaves descended from `root`.
    fn new(root: Rc<Node>) -> Self {
        let mut unvisited = vec![];
        let mut node = root;
        while let Node::Branch { left, right, .. } = Rc::as_ref(&node) {
            unvisited.push(Rc::clone(right));
            node = Rc::clone(left);
        }
        unvisited.push(node);
        Self { nodes: unvisited }
    }
}

impl Iterator for NodeIter {
    type Item = Rc<str>;

    fn next(&mut self) -> Option<Self::Item> {
        match Rc::as_ref(&self.nodes.pop()?) {
            Node::Leaf(s) => Some(Rc::clone(s)),
            Node::Branch { left, right, .. } => {
                self.nodes.push(Rc::clone(right));
                let mut node = Rc::clone(left);
                while let Node::Branch { left, right, .. } = Rc::as_ref(&node) {
                    self.nodes.push(Rc::clone(right));
                    node = Rc::clone(left);
                }
                match Rc::as_ref(&node) {
                    Node::Leaf(s) => Some(Rc::clone(s)),
                    Node::Branch { .. } => unreachable!("Can't break loop if node is a branch"),
                }
            }
        }
    }
}

impl<'a> IntoIterator for &'a Node {
    type IntoIter = BorrowedNodeIter<'a>;

    type Item = Rc<str>;

    fn into_iter(self) -> Self::IntoIter {
        BorrowedNodeIter::new(self)
    }
}

/// An iterator over the leaves descended from a borrowed node.
struct BorrowedNodeIter<'a> {
    /// The nodes that have not yet been visited.
    nodes: Vec<&'a Node>,
}

impl<'a> BorrowedNodeIter<'a> {
    /// Creates an iterator over the leaves descended from `root`.
    fn new(root: &'a Node) -> Self {
        let mut unvisited = vec![];
        let mut node = root;
        while let Node::Branch { left, right, .. } = node {
            unvisited.push(&**right);
            node = &**left;
        }
        unvisited.push(node);
        Self { nodes: unvisited }
    }
}

impl<'a> Iterator for BorrowedNodeIter<'a> {
    type Item = Rc<str>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.nodes.pop()? {
            Node::Leaf(s) => Some(Rc::clone(s)),
            Node::Branch { left, right, .. } => {
                self.nodes.push(&**right);
                let mut node = &**left;
                while let Node::Branch { left, right, .. } = node {
                    self.nodes.push(&**right);
                    node = left;
                }
                match node {
                    Node::Leaf(s) => Some(Rc::clone(s)),
                    Node::Branch { .. } => {
                        unreachable!("Can't break the loop until node is a leaf")
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    /// Makes an `Rc<str>` out of a string literal. To make a `Rc<str>` out of an owned `String`,
    /// use `Rc::from(String::into_boxed_str())`.
    macro_rules! boxed_str {
        ($s:literal) => {
            Rc::from(String::from($s).into_boxed_str())
        };
    }

    fn mk_simon_rope() -> Rope {
        Rope {
            root: Node::boxed_branch(
                9,
                Node::boxed_branch(
                    6,
                    Node::boxed_leaf(boxed_str!("Hello_")),
                    Node::boxed_leaf(boxed_str!("my_")),
                ),
                Node::boxed_branch(
                    6,
                    Node::boxed_branch(
                        2,
                        Node::boxed_leaf(boxed_str!("na")),
                        Node::boxed_leaf(boxed_str!("me_i")),
                    ),
                    Node::boxed_branch(
                        1,
                        Node::boxed_leaf(boxed_str!("s")),
                        Node::boxed_leaf(boxed_str!("_Simon")),
                    ),
                ),
            ),
            len: 22,
            depth: 3,
        }
    }

    #[test]
    fn displays_rope() {
        let expected = "Hello_my_name_is_Simon";
        let rope = mk_simon_rope();
        let actual = rope.to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn leading_substring() {
        let expected = "Hello";
        let rope = mk_simon_rope();
        let actual = rope.sub_rope(..expected.len()).unwrap().to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn inner_substring() {
        let expected = "is";
        let rope = mk_simon_rope();
        let start = "Hello_my_name_".len();
        let actual = rope
            .sub_rope(start..(start + expected.len()))
            .unwrap()
            .to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn final_substring() {
        let expected = "Simon";
        let rope = mk_simon_rope();
        let start = "Hello_my_name_is_".len();
        let actual = rope.sub_rope(start..).unwrap().to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn split() {
        let (expected_left, expected_right) = ("Hello_my_na", "me_is_Simon");
        let rope = mk_simon_rope();
        let (actual_left, actual_right) = rope.split(11).unwrap();
        assert_eq!(expected_left, actual_left.to_string());
        assert_eq!(expected_right, actual_right.to_string());
    }

    #[test]
    fn concat() {
        let expected = "Hello_my_name_is_Simon!";
        let rope = mk_simon_rope();
        let actual = rope.concat(&Rope::from("!".to_string())).to_string();
        assert_eq!(expected, actual);
    }

    #[test]
    fn insert() {
        let expected = "Hello_my_name_is_not_Simon";
        let rope = mk_simon_rope();
        let actual = rope
            .insert("not_", "Hello_my_name_is_".len())
            .unwrap()
            .to_string();
        assert_eq!(expected, actual);
    }
}
