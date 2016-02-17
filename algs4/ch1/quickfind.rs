
use std::collections::HashSet;

struct QuickFind {
    id: Vec<i32>
}

impl QuickFind {
    pub fn new(n: usize) -> QuickFind {
        let mut ids : Vec<i32> = Vec::with_capacity(n);

        for i in 0..n {
            ids.push(i as i32);
        }

        QuickFind { id: ids }
    }

    pub fn connected(&self, i: usize, j: usize) -> bool {
        self.id[i] == self.id[j]
    }

    pub fn union(&mut self, i: usize, j: usize) {
        if !self.connected(i, j) {
            let id1 = self.id[i];
            let id2 = self.id[j];
            
            for k in 0 .. self.id.len() {
                if self.id[k] == id1 {
                    self.id[k] = id2;
                }
            }
        }
    }

    pub fn count_components(&self) -> usize {
        let mut set = HashSet::new();
        for i in self.id.iter() {
            set.insert(i);
        }
        set.len()
    }
}

fn main() {
    let mut qf = QuickFind::new(10);

    println!("3 connected to 4? {}", qf.connected(3, 4));

    qf.union(3, 4);
    println!("Unionized 3 and 4");

    println!("3 connected to 4? {}", qf.connected(3, 4));
    println!("# of components: {}", qf.count_components());

    qf.union(0, 1);
    qf.union(5, 6);
    qf.union(0, 6);

    println!("# of components: {}", qf.count_components());
}
