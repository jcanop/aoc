use regex::Regex;
use std::collections::HashMap;
use std::collections::HashSet;

// --- Constants ---
const REGEX: &'static str = r"Valve (?:([A-Z]{2})) has flow rate=(?:(\d+)); tunnels? leads? to valves? (?:([A-Z]{2}(,\s*[A-Z]{2})*))";

// --- Floyd Warshall Algorithm ---
fn floyd_warshall(
        valves: &HashSet<String>,
        tunnels: &HashMap<String, Vec<String>>
    ) -> HashMap<String, HashMap<String, u64>> {

    let mut distances = HashMap::new();
    for from in valves {
        let mut map: HashMap<String, u64>  = HashMap::new();
        for to in valves {
            map.insert(to.to_owned(), (valves.len() + 1) as u64);
        }
        distances.insert(from.to_owned(), map);
    }

    for (from, vec) in tunnels {
        for to in vec {
            distances.get_mut(from).unwrap().insert(to.to_owned(), 1);
        }
    }

    for from in valves {
        distances.get_mut(from).unwrap().insert(from.to_owned(), 0);
    }

    for via in valves {
        for from in valves {
            for to in valves {
                let d = distances.get(from).unwrap().get(via).unwrap() +
                    distances.get(via).unwrap().get(to).unwrap();
                if distances.get(from).unwrap().get(to).unwrap() > &d {
                    distances.get_mut(from).unwrap().insert(to.to_owned(), d);
                }
            }
        }
    }

    distances
}

pub struct Layout {
    flows: HashMap<String, u64>,
    useful_valves: HashSet<String>,
    all: HashMap<String, HashMap<String, u64>>
}

impl Layout {
    // --- Read and parse the input file ---
    pub fn load(filename: &str) -> Result<Self, Box<dyn std::error::Error>> {
        let mut valves: HashSet<String> = HashSet::new();
        let mut flows = HashMap::new();
        let mut tunnels: HashMap<String, Vec<String>> = HashMap::new();
        let mut useful_valves = HashSet::new();

        let text = std::fs::read_to_string(filename)?;
        let regex = Regex::new(REGEX)?;
        for cap in regex.captures_iter(&text) {
            let id = &cap[1];
            let flow: u64 = cap[2].parse()?;
            let routes: Vec<String> = cap[3].split(", ").map(|s| s.to_owned()).collect();
            valves.insert(id.to_owned());
            flows.insert(id.to_owned(), flow);
            tunnels.insert(id.to_owned(), routes);
            if flow > 0 { useful_valves.insert(id.to_owned()); }
        }
        let all = floyd_warshall(&valves, &tunnels);

        let layout = Self { flows,  useful_valves, all };
        Ok(layout)
    }

    pub fn get_flow(&self, set: &HashSet<&str>) -> u64 {
        set.iter().map(|x| self.flows.get(*x).unwrap()).sum()
    }

    // --- Depth First Search: Puzzle 1 ---
    pub fn dfs1(&self) -> u64 {
        let mut open: HashSet<&str> = HashSet::new();
        self.do_dfs1("AA", 0, 0, &mut open)
    }

    fn do_dfs1<'a>(&'a self, current: &str, time: u64, total: u64, open: &mut HashSet<&'a str>) -> u64 {
        let mut max = total + self.get_flow(open) * (30 - time);
        for next in &self.useful_valves {
            if open.contains(next.as_str()) { continue; }
            let delta = self.all.get(current).unwrap().get(next).unwrap() + 1;
            if time + delta >= 30 { continue; }
            let new_total = total + delta * self.get_flow(open);
            open.insert(next);
            let value = self.do_dfs1(next, time + delta, new_total, open);
            if max < value { max = value; }
            open.remove(next.as_str());
        }
        max
    }

    // --- Depth First Search: Puzzle 2 ---
    pub fn dfs2(&self) -> u64 {
        let mut open: HashSet<&str> = HashSet::new();
        let useful = self.useful_valves.iter().map(|x| x.as_str()).collect();
        self.do_dfs2("AA", false, 0, 0, &mut open, &useful)
    }

    fn do_dfs2<'a>(&'a self, current: &str, elephant: bool, time: u64, total: u64, open: &mut HashSet<&'a str>, useful: &HashSet<&'a str>) -> u64 {
        let mut max = total + self.get_flow(open) * (26 - time);
        if !elephant {
            let mut new_candidates: HashSet<&'a str> = useful.iter().map(|x| *x).collect();
            for v in open.iter() { new_candidates.remove(v); }
            let mut new_open = HashSet::new();
            let max_elephant = self.do_dfs2("AA", true, 0, 0, &mut new_open, &new_candidates);
            max = total + self.get_flow(open) * (26 - time) + max_elephant;
        }
        for next in useful {
            if open.contains(next) { continue; }
            let delta = self.all.get(current).unwrap().get(*next).unwrap() + 1;
            if time + delta >= 26 { continue; }
            let new_total = total + delta * self.get_flow(open);
            open.insert(next.to_owned());
            let value = self.do_dfs2(next, elephant, time + delta, new_total, open, useful);
            if max < value { max = value; }
            open.remove(next);
        }
        max
    }
}
