use std::cmp::Ordering;

/// A splay tree is a binary tree which rotates the accessed node to the root on each access.
#[derive(Clone, Debug)]
pub struct SplayTree<T> {
    /// The most-recently-accessed node.
    root: SubTree<T>,
}

/// A node in a splay tree.
#[derive(Clone, Debug, Eq, PartialEq)]
struct Node<T> {
    /// The value of the node.
    value: T,
    /// The left subtree of the node.
    left: SubTree<T>,
    /// The right subtree of the node.
    right: SubTree<T>,
}

impl<T> Node<T> {
    /// A const version of [`From::from()`].
    const fn new(value: T) -> Self {
        Self {
            value,
            left: None,
            right: None,
        }
    }

    /// Perform the "zig left" rotation.
    fn zig_left(&mut self) {
        match self.left.take() {
            None => {}
            Some(mut left) => {
                self.left = left.right.take();
                std::mem::swap(&mut *left, self);
                self.right = Some(left);
            }
        }
    }

    /// Perform the "zig right" rotation.
    fn zig_right(&mut self) {
        match self.right.take() {
            None => {}
            Some(mut right) => {
                self.right = right.left.take();
                std::mem::swap(&mut *right, self);
                self.left = Some(right);
            }
        }
    }

    /// Perform the "zig-zig left" rotation.
    fn zig_zig_left(&mut self) {
        self.zig_left();
        self.zig_left();
    }

    /// Perform the "zig-zig right" rotation.
    fn zig_zig_right(&mut self) {
        self.zig_right();
        self.zig_right();
    }

    /// Perform the "zig-zag left" rotation.
    fn zig_zag_left(&mut self) {
        if let Some(left) = self.left.as_mut() {
            left.zig_right();
            self.zig_left();
        }
    }

    /// Perform the "zig-zag right" rotation.
    fn zig_zag_right(&mut self) {
        if let Some(right) = self.right.as_mut() {
            right.zig_left();
            self.zig_right();
        }
    }
}

impl<T> From<T> for Node<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// The left or right child of a Node.
type SubTree<T> = Option<Box<Node<T>>>;

impl<T> SplayTree<T> {
    /// A const version of [`Default::default()`].
    pub const fn new() -> Self {
        Self { root: None }
    }
}

impl<T> SplayTree<T>
where
    T: Ord,
{
    /// Splits this tree into two trees: the tree containing all elements less than or equal to
    /// `split_point`, which will be the new tree at `self`; and the tree containing all elements
    /// greater than `split_point`, which will be returned.
    #[must_use]
    pub fn split<C>(&mut self, split_point: &C) -> Self
    where
        C: PartialOrd<T>,
    {
        let (left, right) = self.split_nodes(split_point);
        self.root = left;
        Self { root: right }
    }

    /// Like [`split()`] except that the subtrees are not re-wrapped in `SplayTree`. As a
    /// consequence, `self` will be empty.
    #[must_use]
    fn split_nodes<C>(&mut self, split_point: &C) -> (SubTree<T>, SubTree<T>)
    where
        C: PartialOrd<T>,
    {
        self.splay(split_point);
        match &mut self.root {
            None => (None, None),
            Some(box Node {
                value, left: None, ..
            }) if split_point < value => (None, self.root.take()),
            Some(box Node { right, .. }) => {
                let right = right.take();
                (self.root.take(), right)
            }
        }
    }

    /// Bring the greatest element less than or equal to `splay_point` to the root of the tree.
    fn splay<C>(&mut self, splay_point: &C)
    where
        C: PartialOrd<T>,
    {
        let Some(root) = self.root.as_mut() else {
            return;
        };
        loop {
            match splay_point.partial_cmp(&root.value) {
                None | Some(Ordering::Equal) => return,
                _ if root.left.is_none() && root.right.is_none() => return,
                Some(Ordering::Less) => {
                    let Some(left) = root.left.as_ref() else {
                        return;
                    };
                    match splay_point.partial_cmp(&left.value) {
                        None | Some(Ordering::Equal) => {
                            root.zig_left();
                            return;
                        }
                        Some(Ordering::Less) => root.zig_zig_left(),
                        Some(Ordering::Greater) if left.right.is_none() => {
                            root.zig_left();
                            return;
                        }
                        Some(Ordering::Greater) => root.zig_zag_left(),
                    }
                }
                Some(Ordering::Greater) => {
                    let Some(right) = root.right.as_ref() else {
                        return;
                    };
                    match splay_point.partial_cmp(&right.value) {
                        None | Some(Ordering::Equal) => {
                            root.zig_right();
                            return;
                        }
                        Some(Ordering::Less) if right.left.is_none() => {
                            // Asymmetric with Less/Greater because otherwise we would just
                            // oscillate between this configuration and the configuration from
                            // `root.zig_right()` forever.
                            return;
                        }
                        Some(Ordering::Less) => root.zig_zag_right(),
                        Some(Ordering::Greater) => root.zig_zig_right(),
                    }
                }
            }
        }
    }

    /// Inserts `value` into the tree at the root.
    pub fn insert(&mut self, value: T) {
        let (left, right) = self.split_nodes(&value);
        let root = Some(Box::new(Node { value, left, right }));
        self.root = root;
    }

    /// Joins `self` and `other` into a single tree. Assumes that the greatest element of `self` is
    /// less than or equal to the least element of `other`.
    fn join(&mut self, other: Self) {
        let Some(root) = self.root.as_mut() else {
            self.root = other.root;
            return;
        };
        while root.right.is_some() {
            root.zig_zig_right();
        }
        root.right = other.root;
    }

    /// Removes and returns the element nearest the root that is equal to `value` if such an
    /// element is present.
    pub fn remove<C>(&mut self, value: &C) -> Option<T>
    where
        C: PartialOrd<T>,
    {
        self.splay(value);
        let root = self.root.as_mut()?;
        if value == &root.value {
            let left = root.left.take();
            let right = root.right.take();
            let Some(old_root) = std::mem::replace(&mut self.root, left) else {
                unreachable!("We already took the children of `self.root`")
            };
            self.join(Self { root: right });
            Some(old_root.value)
        } else {
            None
        }
    }

    /// Checks whether some element is equal to `value`.
    pub fn contains<C>(&mut self, value: &C) -> bool
    where
        C: PartialOrd<T>,
    {
        self.splay(value);
        self.root
            .as_ref()
            .filter(|root| value == &root.value)
            .is_some()
    }
}

impl<T> Default for SplayTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! subtree {
        ($name:literal) => {
            Some(Box::new(Node::new($name)))
        };
    }

    #[test]
    fn test_zig_left() {
        let mut node1 = Node {
            value: "p",
            left: Some(Box::new(Node {
                value: "x",
                left: subtree!("A"),
                right: subtree!("B"),
            })),
            right: subtree!("C"),
        };
        let node2 = Node {
            value: "x",
            left: subtree!("A"),
            right: Some(Box::new(Node {
                value: "p",
                left: subtree!("B"),
                right: subtree!("C"),
            })),
        };
        node1.zig_left();
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_zig_right() {
        let mut node1 = Node {
            value: "p",
            left: subtree!("A"),
            right: Some(Box::new(Node {
                value: "x",
                left: subtree!("B"),
                right: subtree!("C"),
            })),
        };
        let node2 = Node {
            value: "x",
            left: Some(Box::new(Node {
                value: "p",
                left: subtree!("A"),
                right: subtree!("B"),
            })),
            right: subtree!("C"),
        };
        node1.zig_right();
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_zig_zig_left() {
        let mut node1 = Node {
            value: "g",
            left: Some(Box::new(Node {
                value: "p",
                left: Some(Box::new(Node {
                    value: "x",
                    left: subtree!("A"),
                    right: subtree!("B"),
                })),
                right: subtree!("C"),
            })),
            right: subtree!("D"),
        };
        let node2 = Node {
            value: "x",
            left: subtree!("A"),
            right: Some(Box::new(Node {
                value: "p",
                left: subtree!("B"),
                right: Some(Box::new(Node {
                    value: "g",
                    left: subtree!("C"),
                    right: subtree!("D"),
                })),
            })),
        };
        node1.zig_zig_left();
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_zig_zig_right() {
        let mut node1 = Node {
            value: "g",
            left: subtree!("A"),
            right: Some(Box::new(Node {
                value: "p",
                left: subtree!("B"),
                right: Some(Box::new(Node {
                    value: "x",
                    left: subtree!("C"),
                    right: subtree!("D"),
                })),
            })),
        };
        let node2 = Node {
            value: "x",
            left: Some(Box::new(Node {
                value: "p",
                left: Some(Box::new(Node {
                    value: "g",
                    left: subtree!("A"),
                    right: subtree!("B"),
                })),
                right: subtree!("C"),
            })),
            right: subtree!("D"),
        };
        node1.zig_zig_right();
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_zig_zag_left() {
        let mut node1 = Node {
            value: "g",
            left: Some(Box::new(Node {
                value: "p",
                left: subtree!("A"),
                right: Some(Box::new(Node {
                    value: "x",
                    left: subtree!("B"),
                    right: subtree!("C"),
                })),
            })),
            right: subtree!("D"),
        };
        let node2 = Node {
            value: "x",
            left: Some(Box::new(Node {
                value: "p",
                left: subtree!("A"),
                right: subtree!("B"),
            })),
            right: Some(Box::new(Node {
                value: "g",
                left: subtree!("C"),
                right: subtree!("D"),
            })),
        };
        node1.zig_zag_left();
        assert_eq!(node1, node2);
    }

    #[test]
    fn test_zig_zag_right() {
        let mut node1 = Node {
            value: "g",
            left: subtree!("A"),
            right: Some(Box::new(Node {
                value: "p",
                left: Some(Box::new(Node {
                    value: "x",
                    left: subtree!("B"),
                    right: subtree!("C"),
                })),
                right: subtree!("D"),
            })),
        };
        let node2 = Node {
            value: "x",
            left: Some(Box::new(Node {
                value: "g",
                left: subtree!("A"),
                right: subtree!("B"),
            })),
            right: Some(Box::new(Node {
                value: "p",
                left: subtree!("C"),
                right: subtree!("D"),
            })),
        };
        node1.zig_zag_right();
        assert_eq!(node1, node2);
    }
}
