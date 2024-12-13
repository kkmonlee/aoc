import os

base_github_url = "https://github.com/kkmonlee/aoc/blob/main/"

repo_dir = "."


def language_from_extension(file_extension):
    return {
        ".rs": "Rust",
        ".cpp": "C++",
        ".clj": "Clojure",
        ".hs": "Haskell",
        ".scala": "Scala",
        ".lisp": "Common Lisp",
        ".ml": "OCaml",
        ".jl": "Julia",
    }.get(file_extension, "Unknown")


def generate_markdown_table():
    markdown_table = []
    for year in sorted(os.listdir(repo_dir), reverse=True):
        year_path = os.path.join(repo_dir, year)
        if os.path.isdir(year_path) and year.isdigit():
            for file in sorted(os.listdir(year_path)):
                if file.endswith(
                    (".rs", ".cpp", ".clj", ".hs", ".scala", ".lisp", ".ml", ".jl")
                ):
                    day = int(file.split(".")[0])
                    ext = os.path.splitext(file)[1]
                    language = language_from_extension(ext)
                    link = f"[{language}]({base_github_url}{year}/{file})"
                    markdown_table.append([int(year), day, link])

    markdown_table.sort(key=lambda x: (x[0], x[1]), reverse=True)
    return markdown_table


def save_markdown_to_readme(table):
    markdown = "| Year | Day | Link |\n"
    markdown += "|------|-----|------|\n"
    for row in table:
        markdown += f"| {row[0]} | {row[1]:02d} | {row[2]} |\n"

    with open("table.md", "w") as readme_file:
        readme_file.write(markdown)


if __name__ == "__main__":
    table = generate_markdown_table()
    save_markdown_to_readme(table)
    print("table created!")
