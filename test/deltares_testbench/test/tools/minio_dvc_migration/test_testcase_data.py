"""Unit tests for XML parser functions."""

from pathlib import Path

import pytest
from pyfakefs.fake_filesystem import FakeFilesystem

from tools.minio_dvc_migration.testcase_data import is_case_with_doc_folder


@pytest.mark.parametrize(
    ("case_path"),
    [
        Path("data/cases/E07_sobek/F61_rws_acceptance/C13_maas_14_js4/input"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
    ],
)
def test_is_case_folder_valid_pattern_with_doc(fs: FakeFilesystem, case_path: Path) -> None:
    """Test is_case_folder returns True for valid pattern with doc folder."""
    # Arrange
    fs.create_dir(case_path)

    doc_path = case_path / "doc"
    fs.create_dir(doc_path)

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is True


def test_is_case_folder_valid_pattern_without_doc(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for valid pattern without doc folder."""
    # Arrange
    case_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")
    fs.create_dir(case_path)

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is False


def test_is_case_folder_doc_is_file_not_directory(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False when 'doc' exists but is a file, not directory."""
    # Arrange
    case_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")
    fs.create_dir(case_path)

    doc_path = case_path / "doc"
    fs.create_file(doc_path, contents="This is a doc file")

    # Act
    result = is_case_with_doc_folder(case_path)

    # Assert
    assert result is False


def test_is_case_folder_invalid_pattern(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for paths that don't match the pattern."""
    invalid_paths = [
        Path("data/cases/invalid/path"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/output"),
        Path("data/cases/e07_sobek/invalid"),
        Path("data/cases/e07_sobek/f61_rws_acceptance/13c_maas_14_js4/input"),
        Path("other/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
        Path("data/references/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input"),
    ]

    # Arrange
    for path in invalid_paths:
        fs.create_dir(path)
        fs.create_dir(path / "doc")

    # Act & Assert
    for path in invalid_paths:
        result = is_case_with_doc_folder(path)
        assert result is False, f"Expected False for {path}"


def test_is_case_folder_nonexistent_directory(fs: FakeFilesystem) -> None:
    """Test is_case_folder returns False for non-existent directories."""
    # Arrange
    nonexistent_path = Path("data/cases/e07_sobek/f61_rws_acceptance/c13_maas_14_js4/input")

    # Act
    result = is_case_with_doc_folder(nonexistent_path)

    # Assert
    assert result is False
