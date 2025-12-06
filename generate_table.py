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
    ".rb": "Ruby",
    ".rs": "Rust",
    ".scala": "Scala",
    ".zig": "Zig",
}


def language_from_extension(file_extension):
    return LANGUAGES.get(file_extension, "Unknown")


def generate_html_table():
    tables = {}
    valid_extensions = tuple(LANGUAGES.keys())

    for year in sorted(os.listdir(repo_dir), reverse=True):
        year_path = os.path.join(repo_dir, year)
        if os.path.isdir(year_path) and year.isdigit():
            year_table = []
            for file in sorted(os.listdir(year_path), reverse=True):
                if file.endswith(valid_extensions):
                    day = int(file.split(".")[0])
                    ext = os.path.splitext(file)[1]
                    language = language_from_extension(ext)
                    link = f"<a href='{base_github_url}{year}/{file}'>{language}</a>"
                    year_table.append(
                        f"<tr><td style='width:15%;'>{day:02d}</td><td style='width:85%;'>{link}</td></tr>"
                    )
            tables[year] = year_table
    return tables


def save_html_to_markdown(tables, output_file):
    with open(output_file, "w") as file:
        for year, rows in sorted(tables.items(), reverse=True):
            file.write(f"## {year}\n\n")

            file.write(
                "<table style='width:100%; border-collapse: collapse; text-align: left;'>\n"
            )
            file.write("    <thead>\n")
            file.write("        <tr>\n")
            file.write(
                "            <th style='width:15%; text-align: left;'>Day</th>\n"
            )
            file.write(
                "            <th style='width:85%; text-align: left;'>Link</th>\n"
            )
            file.write("        </tr>\n")
            file.write("    </thead>\n")
            file.write("    <tbody>\n")
            file.write("      " + "\n      ".join(rows) + "\n")
            file.write("    </tbody>\n")
            file.write("</table>\n\n")
            file.write("---\n\n")


if __name__ == "__main__":
    tables = generate_html_table()
    save_html_to_markdown(tables, "table.md")
    print("tables created in table.md")
