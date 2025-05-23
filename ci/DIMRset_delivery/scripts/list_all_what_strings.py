import argparse
import os
import string
import sys
from datetime import datetime

"""
This script lists all what strings, starting with @(#)Deltares or containing HeadURL,
in all subdirectories of a given root directory.
The root directory is specified by the argument --srcdir.
"""


def extract_printable_strings(filename, min_length=4):
    try:
        with open(filename, "r", errors="ignore") as f:
            printable_set = set(string.printable)
            result = ""
            while True:
                chunk = f.read(4096)
                if not chunk:
                    break
                for c in chunk:
                    if c in printable_set:
                        result += c
                        continue
                    if len(result) >= min_length:
                        yield result
                    result = ""
            if len(result) >= min_length:
                yield result
    except Exception as e:
        # Could not read file, skip it
        yield from ()


def list_what_strings(file_path: str, log_file) -> None:
    try:
        string_list = list(extract_printable_strings(file_path))
    except Exception as e:
        log_file.write(f"\t[Unreadable file] {file_path}: {e}\n")
        return

    what_strings = []
    for s in string_list:
        if "@(#)Deltares" in s:
            what_strings.append(s[s.find("@(#)Deltares") :])
        if "HeadURL" in s:
            what_strings.append(s[s.find("HeadURL") :])
    if what_strings:
        log_file.write(f"\t{file_path}\n")
        for s in what_strings:
            if s.startswith("@(#)"):
                log_file.write(f"\t\t{s[4:]}\n")
            elif s.startswith("HeadURL"):
                log_file.write(f"\t\t{s[9:]}\n")


def walk_and_list_what_strings(root_folder, log_file):
    for current_dir, subdirs, files in os.walk(root_folder):
        if current_dir != root_folder:
            print(f"\t{current_dir}")
            for file_name in files:
                file_path = os.path.join(current_dir, file_name)
                list_what_strings(file_path, log_file)


def get_command_line_args():
    parser = argparse.ArgumentParser(
        description="Batch process to list all what-strings"
    )
    parser.add_argument(
        "-s",
        "--srcdir",
        help="Root directory from which the what-strings are listed",
        dest="src_dir",
    )
    parser.add_argument("-o", "--output", help="Output filename.", dest="out_put")
    args = parser.parse_args()
    return args


if __name__ == "__main__":
    start_time = datetime.now()

    args = get_command_line_args()

    src_dir = "."
    out_put = "dimr_version.txt"
    if args.src_dir:
        src_dir = args.src_dir
    start_dir = os.getcwd()
    src_dir = os.path.normpath(os.path.join(start_dir, src_dir))
    if not os.path.exists(src_dir):
        print("Given directory does not exist: %s" % src_dir)
        sys.exit(1)

    if args.out_put:
        out_put = args.out_put
    if os.path.exists(out_put):
        os.remove(out_put)

    print("Start: %s\n" % start_time)
    print("%s\n" % sys.version)
    print("Listing is written to: %s" % out_put)
    print("Root Directory: %s" % src_dir)

    with open(out_put, "a") as log_file:
        log_file.write("Start: %s\n" % start_time)
        log_file.write("Root Directory: %s\n" % src_dir)
        walk_and_list_what_strings(src_dir, log_file)
        log_file.write("Processing done\n")
        log_file.write("\nStart: %s\n" % start_time)
        log_file.write("End  : %s\n" % datetime.now())
        log_file.write("Klaar\n")

    print("Processing done")
    print("\nStart: %s" % start_time)
    print("End  : %s" % datetime.now())
    print("Klaar")
