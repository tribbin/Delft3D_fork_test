"""XML parsing functionality for extracting testcase data."""

import os
from pathlib import Path
from typing import List

from dvc.repo import Repo


def find_dvc_root_in_parent_directories(start_path: Path) -> str:
    """Find the DVC repository root by looking for .dvc directory.

    Parameters
    ----------
    start_path : Path
        Starting path to search from.

    Returns
    -------
    str
        Path to the DVC repository root.
    """
    current = os.path.abspath(start_path)
    while current != "/":
        if os.path.isdir(os.path.join(current, ".dvc")):
            return current
        parent = os.path.dirname(current)
        if parent == current:  # Reached filesystem root
            break
        current = parent
    raise ValueError("Could not find DVC repository root (.dvc directory)")


def add_directory_to_dvc(path: Path, repo: Repo) -> List[Path]:
    """Add downloaded case and reference data to DVC tracking and return created .dvc files."""
    if not path.exists():
        raise FileNotFoundError(f"Path does not exist: {path}")

    try:
        stages = repo.add(str(path))  # returns list of stage-like objects
        dvc_files: List[Path] = []

        for s in stages:
            if hasattr(s, "dvcfile") and hasattr(s.dvcfile, "path"):
                dvc_files.append(Path(s.dvcfile.path))

        return dvc_files

    except Exception as e:
        print(f"Error adding to DVC: {e}")
        return []


def push_dvc_files_to_remote(repo: Repo, dvc_files: list[Path]) -> None:
    """Push all .dvc files to remote S3 storage."""
    print(f"Pushing {len(dvc_files)} *.dvc files to S3 storage...")

    successful_pushes = 0
    failed_pushes = 0
    for i, dvc_file in enumerate(dvc_files, 1):
        try:
            print(f"Pushing file {i}/{len(dvc_files)}: {dvc_file}")
            path = str(dvc_file)
            pushed_files = repo.push(targets=[path], remote="storage")

            if pushed_files:
                successful_pushes += 1
                print(f"  SUCCESS: {pushed_files} files pushed")
            else:
                successful_pushes += 1
                print("  SUCCESS: Already up to date")

        except Exception as push_error:
            failed_pushes += 1
            print(f"  FAILED: {str(push_error)}")

    print(f"\nPush summary: {successful_pushes} successful, {failed_pushes} failed")

    if successful_pushes > 0:
        print("Files have been successfully uploaded to S3!")
    if failed_pushes > 0:
        print(f"Warning: {failed_pushes} files failed to push. Check errors above.")
