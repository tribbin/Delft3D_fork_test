from pathlib import Path
from typing import List

from dvc.repo import Repo
from lxml import etree

from src.utils.minio_rewinder import Rewinder
from tools.minio_dvc_migration.testcase_data import TestCaseData, is_case_with_doc_folder, move_doc_folder_to_parent


class XmlFileWithTestCaseData:
    """Test case data for XML parsing."""

    xml_file: Path
    testcases: list[TestCaseData]

    def __init__(self, xml_file: Path, testcases: list[TestCaseData]) -> None:
        self.xml_file = xml_file
        self.testcases = testcases

    def migrate_xml_to_dvc(self, xml_path: Path | None = None) -> None:
        """Update XML file to use DVC and local data paths."""
        if xml_path is None:
            xml_path = self.xml_file
        print(f"Update xml: {xml_path}")

        if not xml_path.exists():
            raise FileNotFoundError(f"XML file does not exist: {xml_path}")

        try:
            # Read the file to detect original encoding
            with open(xml_path, "rb") as f:
                content = f.read()

            # Detect encoding from XML declaration
            encoding = "utf-8"  # default
            if content.startswith(b"<?xml"):
                xml_decl = content.split(b"?>")[0] + b"?>"
                if b"encoding=" in xml_decl:
                    # Extract encoding from declaration
                    encoding_part = xml_decl.split(b"encoding=")[1]
                    quote_char = encoding_part[0:1]  # ' or "
                    encoding = encoding_part[1:].split(quote_char)[0].decode("ascii")

            parser = etree.XMLParser(remove_blank_text=True)
            tree = etree.parse(xml_path, parser)
            root = tree.getroot()

            namespace = {"tb": "http://schemas.deltares.nl/deltaresTestbench_v3"}

            self.__update_local_paths(root, namespace)
            self.__update_location_roots(root, namespace)
            self.__update_testcase_version(root, namespace)
            self.__process_xi_includes(root, xml_path)

            # Write the modified XML back
            tree.write(xml_path, encoding=encoding, xml_declaration=True, pretty_print=True)
            print(f"Successfully updated XML file: {xml_path}")

        except Exception as e:
            print(f"Error updating XML file {xml_path}: {e}")

    def __update_local_paths(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update local paths to use ./data/cases."""
        # Update testCasesDir
        test_cases_dir = root.find(".//tb:testCasesDir", namespace)
        if test_cases_dir is not None:
            test_cases_dir.text = "./data/cases"

        # Update referenceDir
        reference_dir = root.find(".//tb:referenceDir", namespace)
        if reference_dir is not None:
            reference_dir.text = "./data/cases"

    def __update_location_roots(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update location roots to use ./data/cases."""
        locations = root.findall(".//tb:location", namespace)
        for location in locations:
            root_elem = location.find("tb:root", namespace)
            if root_elem is not None:
                root_text = root_elem.text
                # Replace exact matches only: {server_base_url}/references and {server_base_url}/cases
                if root_text in ["{server_base_url}/references", "{server_base_url}/cases"]:
                    root_elem.text = "./data/cases"

    def __update_testcase_version(self, root: etree._Element, namespace: dict[str, str]) -> None:
        """Update the version attribute for all testcases to 'DVC'."""
        testcases = root.findall(".//tb:testCase", namespace)
        for testcase in testcases:
            path_elem = testcase.find("tb:path", namespace)
            if path_elem is not None and path_elem.get("version"):
                path_elem.set("version", "DVC")

    def __process_xi_includes(self, root: etree._Element, current_file: Path) -> None:
        """Process xi:include elements and update the included files recursively."""
        xi_ns = {"xi": "http://www.w3.org/2001/XInclude"}

        includes = root.findall(".//xi:include", xi_ns)

        for include in includes:
            href = include.get("href")
            if href:
                # Resolve the path relative to the current file
                include_path = Path(current_file).parent / href
                if include_path.exists():
                    print(f"Processing included file: {include_path}")
                    self.migrate_xml_to_dvc(include_path)
                else:
                    print(f"Warning: Included file does not exist: {include_path}")

    def download_from_minio_in_new_folder_structure(self, rewinder: Rewinder) -> None:
        """Download all testcases from MinIO using the new folder structure."""
        print(f"Download {len(self.testcases)} testcases from {self.xml_file}")
        for i, testcase in enumerate(self.testcases, start=1):
            testcase.download(rewinder=rewinder)
            print(f"Downloaded {i}/{len(self.testcases)}: {testcase.name}")

    def move_testcases_doc_folder_to_parent(self) -> None:
        """Move doc folder to parent folder for all testcases."""
        for testcase in self.testcases:
            local_path = testcase.case.to_local()
            if is_case_with_doc_folder(local_path):
                move_doc_folder_to_parent(local_path)

    def add_to_dvc(self, repo: Repo) -> List[Path]:
        """Add all testcases folders to DVC tracking."""
        dvc_files = []
        for i, testcase in enumerate(self.testcases, start=1):
            print(f"Adding: {testcase.name} - {i}/{len(self.testcases)} cases - {self.xml_file}")
            dvc_files.extend(testcase.add_to_dvc(repo=repo))

        return dvc_files
