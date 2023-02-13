import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

public class Layout {
	// --- Constants ---
	private static final String REGEX = "Valve ([A-Z]{2}) has flow rate=(\\d+); tunnels? leads? to valves? ([A-Z]{2}(,\\s*[A-Z]{2})*)";

	// --- Data ---
	private final Set<String> valves;
	private final Map<String, Long> flows;
	private final Map<String, List<String>> tunnels;
	private final Set<String> usefulValves;
	private Map<String, Map<String, Integer>> all;

	private Layout() {
		valves = new HashSet<>();
		flows = new HashMap<>();
		tunnels = new HashMap<>();
		usefulValves = new HashSet<>();
		all = new HashMap<>();
	}

	// --- Read and parse the input file ---
	public static Layout load(String filename) throws IOException {
		Layout layout = new Layout();

		String text = Files.readString(Paths.get(filename));
		Pattern pattern = Pattern.compile(REGEX);
		Matcher matcher = pattern.matcher(text);
		while (matcher.find()) {
			String id = matcher.group(1);
			long flow = Long.parseLong(matcher.group(2));
			String[] routes = matcher.group(3).split("\\s*,\\s*");
			layout.valves.add(id);
			layout.flows.put(id, flow);
			layout.tunnels.put(id, Arrays.asList(routes));
			if (flow > 0) layout.usefulValves.add(id);
		}
		layout.all = layout.fw();
		return layout;
	}

	// --- Floyd Warshall Algorithm ---
	private Map<String, Map<String, Integer>> fw() {
		Map<String, Map<String, Integer>> distances = new HashMap<>();
		for (String from: valves) {
			Map<String, Integer> map = new HashMap<>();
			distances.put(from, map);
			for (String to: valves) {
				map.put(to, valves.size() + 1);
			}
		}

		for (String from: tunnels.keySet()) {
			for (String to: tunnels.get(from)) {
				distances.get(from).put(to, 1);
			}
		}
		for (String from: valves) {
			distances.get(from).put(from, 0);
		}

		for (String via: valves) {
			for (String from: valves) {
				for (String to: valves) {
					Integer d = distances.get(from).get(via) + distances.get(via).get(to);
					if (distances.get(from).get(to) > d) {
						distances.get(from).put(to,  d);
					}
				}
			}
		}
		return distances;
	}

	private long getFlow(Set<String> set) {
		return set.stream().mapToLong(x -> flows.get(x).longValue()).sum();
	}

	// --- Depth First Search: Puzzle 1 ---
	public long dfs1() {
		Set<String> open = new HashSet<>();
		return dfs1("AA", 0, 0, open);
	}

	private long dfs1(String current, long time, long total, Set<String> open) {
		long max = total + getFlow(open) * (30 - time);
		for (String next: usefulValves) {
			if (open.contains(next)) continue;
			long delta = all.get(current).get(next) + 1;
			if (time + delta >= 30) continue;
			long newTotal = total + delta * getFlow(open);
			open.add(next);
			long value = dfs1(next, time + delta, newTotal, open);
			if (max < value) max = value;
			open.remove(next);
		}
		return max;
	}

	// --- Depth First Search: Puzzle 2 ---
	public long dfs2() {
		Set<String> open = new HashSet<>();
		return dfs2("AA", false, 0, 0, open, usefulValves, 0);
	}

	private long dfs2(String current, boolean elephant, long time, long total, Set<String> open, Set<String> useful, long totalFlow) {
		long max = total + totalFlow * (26 - time);
		if (!elephant) {
			Set<String> newCandidates = new HashSet<>(useful);
			for (String v: open) newCandidates.remove(v);

			Set<String> newOpen = new HashSet<>();
			long maxElephant = dfs2("AA", true, 0, 0, newOpen, newCandidates, 0);
			max = total + totalFlow * (26 - time) + maxElephant;
		}

		for (String next: useful) {
			if (open.contains(next)) continue;
			long delta = all.get(current).get(next) + 1;
			if (time + delta >= 26) continue;
			long newTotal = total + delta * totalFlow;
			open.add(next);
			long nextFlow = flows.get(next).longValue();
			totalFlow += nextFlow;
			long value = dfs2(next, elephant, time + delta, newTotal, open, useful, totalFlow);
			if (max < value) max = value;
			open.remove(next);
			totalFlow -= nextFlow;
		}
		return max;
	}
}
