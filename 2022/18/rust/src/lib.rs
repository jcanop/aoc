use std::collections::HashSet;

pub type Point = (usize, usize, usize);
pub type Data = Vec<Vec<Vec<u8>>>;

fn from(p: &Point, dx: isize, dy: isize, dz: isize) -> Point {
    let x = (p.0 as isize + dx) as usize;
    let y = (p.1 as isize + dy) as usize;
    let z = (p.2 as isize + dz) as usize;
    (x, y, z)
}

// --- Calcualtes the surface ---
pub fn calculate(list: &Vec<Point>, max: &[usize; 3]) -> usize {
    let mut total = 0;
    for p in list {
        let (x, y, z) = *p;
        total += 6;

        if x > 0 { let n = from(&p, -1,  0,  0); if list.contains(&n) { total -= 1; } }
        if y > 0 { let n = from(&p,  0, -1,  0); if list.contains(&n) { total -= 1; } }
        if z > 0 { let n = from(&p,  0,  0, -1); if list.contains(&n) { total -= 1; } }
        if x < max[0] { let n = from(&p,  1,  0,  0); if list.contains(&n) { total -= 1; } }
        if y < max[1] { let n = from(&p,  0,  1,  0); if list.contains(&n) { total -= 1; } }
        if z < max[2] { let n = from(&p,  0,  0,  1); if list.contains(&n) { total -= 1; } }
    }
    total
}

// --- Searches for a path to the edge of the model ---
pub fn dfs(root: &Point, list: &Vec<Point>, max: &[usize; 3], closed: &mut HashSet<Point>) -> bool {
    let mut queue: Vec<Point> = Vec::new();
    let mut visited: HashSet<Point> = HashSet::new();
    queue.push(*root);
    visited.insert(*root);

    while queue.len() > 0 {
        let p = queue.remove(0);
        let (x, y, z) = p;
        if  x == 0 || x == max[0] ||
            y == 0 || y == max[1] ||
            z == 0 || z == max[2] { return true; }

        let ns = [
            from(&p, -1,  0,  0), from(&p, 1, 0, 0),
            from(&p,  0, -1,  0), from(&p, 0, 1, 0),
            from(&p,  0,  0, -1), from(&p, 0, 0, 1)
        ];
        for n in ns {
            if !visited.contains(&n) && !list.contains(&n) { queue.push(n); visited.insert(n); }
        }
    }

    for p in visited.into_iter() { closed.insert(p); }

    false
}
