"""Unit tests for XML file with testcase data migration."""

from datetime import datetime, timezone
from pathlib import Path

from src.config.test_case_path import TestCasePath
from test.helpers.xml_config_helper import make_test_case_config_xml
from tools.minio_dvc_migration.xml_file_with_testcase_data import XmlFileWithTestCaseData


def test_migration_of_minio_to_dvc_testcases_xml(tmp_path: Path) -> None:
    """Test that XML file migration updates paths correctly."""
    # Arrange
    version = datetime.now(timezone.utc).isoformat()
    xml_content_stream = make_test_case_config_xml(
        test_case_path=TestCasePath("test/case/path", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    temp_file_path = tmp_path / "test_config.xml"
    with open(temp_file_path, "wb") as temp_file:
        temp_file.write(xml_content_stream.read())

    xml_data = XmlFileWithTestCaseData(temp_file_path, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert
    with open(temp_file_path, "r", encoding="utf-8") as f:
        modified_content = f.read()

    assert "./data/cases" in modified_content
    assert "{server_base_url}/cases" not in modified_content
    assert "{server_base_url}/references" not in modified_content
    assert 'version="DVC"' in modified_content
    assert f'version="{version}"' not in modified_content


def test_migration_of_minio_to_dvc_testcases_xml_with_included_xml(tmp_path: Path) -> None:
    """Test that XML file migration with xi:include updates both main and included files correctly."""
    # Arrange
    version = datetime.now(timezone.utc).isoformat()

    # Create the included XML file
    included_xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("included/test/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
    )

    included_file_path = tmp_path / "included_config.xml"
    with open(included_file_path, "wb") as included_file:
        included_file.write(included_xml_content.read())

    # Create the main XML file that includes the other file
    xi_include = '<xi:include href="included_config.xml"/>'
    main_xml_content = make_test_case_config_xml(
        test_case_path=TestCasePath("main/test/case", version=version),
        case_root="{server_base_url}/cases",
        reference_root="{server_base_url}/references",
        include=xi_include,
    )

    main_file_path = tmp_path / "main_config.xml"
    with open(main_file_path, "wb") as main_file:
        main_file.write(main_xml_content.read())

    xml_data = XmlFileWithTestCaseData(main_file_path, [])

    # Act
    xml_data.migrate_xml_to_dvc()

    # Assert - Read the main file and check changes
    with open(main_file_path, "r", encoding="utf-8") as f:
        main_modified_content = f.read()

    assert "./data/cases" in main_modified_content
    assert "{server_base_url}/cases" not in main_modified_content
    assert "{server_base_url}/references" not in main_modified_content
    assert 'version="DVC"' in main_modified_content
    assert f'version="{version}"' not in main_modified_content

    # Assert - Read the included file and check changes
    with open(included_file_path, "r", encoding="utf-8") as f:
        included_modified_content = f.read()

    assert "./data/cases" in included_modified_content
    assert "{server_base_url}/cases" not in included_modified_content
    assert "{server_base_url}/references" not in included_modified_content
    assert 'version="DVC"' in included_modified_content
    assert f'version="{version}"' not in included_modified_content
