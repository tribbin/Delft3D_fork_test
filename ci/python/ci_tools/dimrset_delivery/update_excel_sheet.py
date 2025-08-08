#!/usr/bin/env python3
"""Update the Excel sheet with this week's release information."""

from datetime import datetime, timezone
from typing import Dict, List

from openpyxl import load_workbook
from openpyxl.worksheet.worksheet import Worksheet

from ci_tools.dimrset_delivery.common_utils import get_testbank_result_parser
from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.helpers.result_testbank_parser import ResultTestBankParser
from ci_tools.dimrset_delivery.lib.ssh_client import Direction
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.general_settings import (
    DRY_RUN_PREFIX,
    LINUX_ADDRESS,
    NAME_COLUMN,
    SHEET_NAME,
    VERSIONS_EXCEL_FILENAME,
)


class ExcelHelper(object):
    """Object responsible for updating the Excel sheet."""

    def __init__(
        self,
        teamcity: TeamCity,
        filepath: str,
        dimr_version: str,
        kernel_versions: Dict[str, str],
        parser: ResultTestBankParser,
    ) -> None:
        """
        Create a new instance of ExcelHelper.

        Args:
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            filepath (str): Path to the Excel file.
            dimr_version (str): The DIMR version to update the Excel for.
            kernel_versions (Dict[str, str]): A dictionary mapping kernel names to their version.
            parser (ResultTestBankParser): A parser for the latest test bench results.
        """
        self.__teamcity = teamcity
        self.__filepath = filepath
        self.__dimr_version = dimr_version
        self.__kernel_versions = kernel_versions
        self.__parser = parser

    def append_row(self) -> None:
        """Append a new row to the Excel sheet with this week's DIMR information."""
        row = self.__prepare_row_to_insert()
        print(row)

        try:
            workbook = load_workbook(filename=self.__filepath)
            worksheet = workbook[SHEET_NAME]

            if self.__worksheet_already_contains_row(worksheet):
                print("The Excel sheet already contains a row for this DIMRset value and revision number.")
                return

            worksheet.append(row)
            workbook.save(filename=self.__filepath)
        except Exception as e:
            print("Could not update the excel: \n")
            print(e)
        finally:
            workbook.close()

    def __prepare_row_to_insert(self) -> List[str]:
        """Prepare a row to be inserted in the Excel sheet."""
        row = []

        row.append("")  # Column A (empty column)
        row.append(str(datetime.now(tz=timezone.utc).date()))  # Column B (Date)
        row.append(f"DIMRset {self.__dimr_version}")  # Column C (DIMR version)
        row.append("")  # Column D (Revision)
        row.append("FLOW1D2D now in GitHub")  # Column E (Flow1D)
        row.append("OSS")  # Column F (FlowFM)
        row.append(self.__kernel_versions["build.vcs.number"])  # Column G (OSS)
        row.append("DRR now in GitHub")  # Column H (RR)
        row.append("FBC now in GitHub")  # Column I (FBC)
        row.append(self.__parser.get_percentage_total_passing())  # Column J (Percentage passing)
        row.append(self.__parser.get_total_tests())  # Column K (Total Number of          cases)
        row.append(self.__parser.get_total_passing())  # Column L (      Number of green    cases)
        row.append(self.__parser.get_total_failing())  # Column M (      Number of red      cases)
        row.append(self.__parser.get_total_exceptions())  # Column N (      Number of crashing cases)
        row.append("")  # Column O (Docker hub)
        row.append("Flow1D and RR: only Windows")  # Column P (Remarks)

        return row

    def __worksheet_already_contains_row(self, worksheet: Worksheet) -> bool:
        """
        Check if the Excel sheet already contains a row for the given DIMRset.

        Return True if the Excel sheet already contains such a row.
        """
        name_already_exists = False

        name_column = worksheet[NAME_COLUMN]

        for cell in name_column:
            if cell.value == f"DIMRset {self.__dimr_version}":
                name_already_exists = True

        return name_already_exists


def update_excel_sheet(context: DimrAutomationContext) -> None:
    """Update the Excel sheet with this week's release information.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Updating Excel sheet...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would update Excel sheet with DIMR version:", dimr_version)
        print(f"{DRY_RUN_PREFIX} Would download Excel from network drive")
        print(f"{DRY_RUN_PREFIX} Would append new row with release information")
        print(f"{DRY_RUN_PREFIX} Would upload updated Excel back to network drive")
        return

    parser = get_testbank_result_parser()
    path_to_excel_file = f"/p/d-hydro/dimrset/{VERSIONS_EXCEL_FILENAME}"

    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")
    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")

    context.ssh_client.secure_copy(LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.FROM)
    helper = ExcelHelper(
        teamcity=context.teamcity,
        filepath=VERSIONS_EXCEL_FILENAME,
        dimr_version=dimr_version,
        kernel_versions=kernel_versions,
        parser=parser,
    )
    helper.append_row()
    context.ssh_client.secure_copy(LINUX_ADDRESS, VERSIONS_EXCEL_FILENAME, path_to_excel_file, Direction.TO)

    print("Excel sheet update completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False)

    print("Starting Excel sheet update...")
    update_excel_sheet(context)
    print("Finished")
