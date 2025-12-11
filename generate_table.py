import os

base_github_url = "https://github.com/kkmonlee/aoc/blob/main/"
repo_dir = "."

LANGUAGES = {
    ".adb": "Ada", 
    ".ads": "Ada", 
    ".apl": "APL", 
    ".s": "x86-64 Assembly",
    ".cc": "C++", 
    ".clj": "Clojure", 
    ".cpp": "C++", 
    ".d": "D", 
    ".erl": "Erlang",
    ".f90": "Fortran", 
    ".go": "Go", 
    ".hs": "Haskell", 
    ".jl": "Julia",
    ".kt": "Kotlin", 
    ".lisp": "Common Lisp", 
    ".lua": "Lua", 
    ".ml": "OCaml",
    ".wl": "Mathematica", 
    ".nim": "Nim", 
    ".rb": "Ruby", 
    ".rkt": "Racket",
    ".rs": "Rust", 
    ".scala": "Scala", 
    ".zig": "Zig",
}

PROBLEMS = {
    # 2025
    ("2025", 1): ("Trivial", "modular arithmetic"),
    ("2025", 2): ("Trivial", "number theory, repunits"),
    ("2025", 3): ("Trivial", "greedy digit selection"),
    ("2025", 4): ("Trivial", "convolution, cellular automata, fixed-point"),
    ("2025", 5): ("Trivial", "interval merging"),
    ("2025", 6): ("Trivial", "parsing, 2D array traversal"),
    ("2025", 7): ("Trivial", "state propagation, path counting"),
    ("2025", 8): ("Easy", "union-find, Kruskal's algorithm"),
    ("2025", 9): ("Easy", "scanline range compression, pruned enumeration"),
    ("2025", 10): ("Easy", "Galois field (mod-2 arithmetic with XOR), integer linear programming"),
    ("2025", 11): ("Trivial", "DFS, memoization, DAG path counting"),
    # 2024
    ("2024", 1): ("Trivial", "sorting, frequency map"),
    ("2024", 2): ("Trivial", "monotonicity check"),
    ("2024", 3): ("Trivial", "parsing, state machine"),
    ("2024", 4): ("Trivial", "grid search, direction vectors"),
    ("2024", 5): ("Trivial", "topological sort, dependency graph"),
    ("2024", 6): ("Easy", "cycle detection, grid traversal"),
    ("2024", 7): ("Trivial", "brute force, operator enumeration"),
    ("2024", 8): ("Trivial", "coordinate geometry, gcd normalisation"),
    ("2024", 9): ("Easy", "two-pointer, segment fitting"),
    ("2024", 10): ("Trivial", "DFS, grid traversal, path counting"),
    ("2024", 11): ("Trivial", "memoisation, counting map, digit manipulation"),
    ("2024", 12): ("Easy", "flood fill, boundary analysis"),
    ("2024", 13): ("Trivial", "linear algebra, Cramer's rule"),
    ("2024", 14): ("Trivial", "modular arithmetic"),
    ("2024", 15): ("Easy", "BFS, transitive closure"),
    ("2024", 16): ("Easy", "Dijkstra, state-space search, path reconstruction"),
    ("2024", 17): ("Medium", "reverse engineering, constraint propagation, bit manipulation"),
    ("2024", 18): ("Easy", "BFS, binary search, bidirectional search"),
    ("2024", 19): ("Trivial", "memoisation, string prefix matching, DP"),
    ("2024", 20): ("Trivial", "BFS, precomputed distances"),
    ("2024", 21): ("Easy", "recursive DP, memoisation"),
    ("2024", 22): ("Trivial", "sequence enumeration, hash aggregation"),
    ("2024", 23): ("Easy", "Bron-Kerbosch, max clique"),
    ("2024", 24): ("Medium", "graph analysis, ripple-carry adder"),
    ("2024", 25): ("Trivial", "parsing, pattern counting"),
}

def language_from_extension(ext):
    return LANGUAGES.get(ext, "Unknown")

def extract_day_from_filename(filename: str) -> int:
    stem = filename.split(".", 1)[0] 
    digits = "".join(ch for ch in stem if ch.isdigit())
    if not digits:
        raise ValueError(f"Could not extract day number from filename '{filename}'")
    return int(digits)

def generate_html_table():
    tables = {}
    valid_extensions = tuple(LANGUAGES.keys())
    
    for year in sorted(os.listdir(repo_dir), reverse=True):
        year_path = os.path.join(repo_dir, year)
        if os.path.isdir(year_path) and year.isdigit():
            year_table = []

            year_files = [
                file for file in os.listdir(year_path)
                if file.endswith(valid_extensions)
            ]

            year_files.sort(key=extract_day_from_filename, reverse=True)

            for file in year_files:
                try:
                    day = extract_day_from_filename(file)
                except ValueError:
                    continue

                ext = os.path.splitext(file)[1]
                language = language_from_extension(ext)
                link = f"<a href='{base_github_url}{year}/{file}'>{language}</a>"

                difficulty, techniques = PROBLEMS.get((year, day), ("", ""))

                year_table.append(
                    f"<tr><td>{day:02d}</td><td>{link}</td>"
                    f"<td>{difficulty}</td><td>{techniques}</td></tr>"
                )

            tables[year] = year_table
    return tables


def save_html_to_markdown(tables, output_file):
    with open(output_file, "w") as f:
        for year, rows in sorted(tables.items(), reverse=True):
            f.write(f"## {year}\n\n")
            f.write("<table style='width:100%; border-collapse: collapse; text-align: left;'>\n")
            f.write("    <thead>\n        <tr>\n")
            f.write("            <th style='width:8%;'>Day</th>\n")
            f.write("            <th style='width:20%;'>Link</th>\n")
            f.write("            <th style='width:12%;'>Difficulty</th>\n")
            f.write("            <th style='width:60%;'>Techniques</th>\n")
            f.write("        </tr>\n    </thead>\n    <tbody>\n")
            f.write("      " + "\n      ".join(rows) + "\n")
            f.write("    </tbody>\n</table>\n\n---\n\n")

if __name__ == "__main__":
    tables = generate_html_table()
    save_html_to_markdown(tables, "table.md")
    print("tables created in table.md")