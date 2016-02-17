
use std::collections::HashSet;

struct QuickUnion {
    id: Vec<usize>
}

impl QuickUnion {
    pub fn new(n: usize) -> QuickUnion {
        let mut ids : Vec<usize> = Vec::with_capacity(n);

        for i in 0..n {
            ids.push(i as usize);
        }

        QuickUnion { id: ids }
    }
    
    fn root(&self, i: usize) -> usize {
        let mut res = i;

        while self.id[res] != res {
            res = self.id[res];
        }
        res
    }

    pub fn connected(&self, i: usize, j: usize) -> bool {
        self.root(i) == self.root(j)
    }

    pub fn union(&mut self, i: usize, j: usize) {
        let root1 = self.root(i);
        let root2 = self.root(j);

        self.id[root1] = root2;
    }

    pub fn count_components(&self) -> usize {
        let mut set = HashSet::new();
        for i in self.id.iter() {
            set.insert(self.root(*i));
        }
        set.len()
    }
}

fn main() {
    let mut qu = QuickUnion::new(10);

    println!("3 connected to 4? {}", qu.connected(3, 4));

    qu.union(3, 4);
    println!("Unionized 3 and 4");

    println!("3 connected to 4? {}", qu.connected(3, 4));
    println!("# of components: {}", qu.count_components());

    qu.union(0, 1);
    qu.union(5, 6);
    qu.union(0, 6);

    println!("# of components: {}", qu.count_components());
}

