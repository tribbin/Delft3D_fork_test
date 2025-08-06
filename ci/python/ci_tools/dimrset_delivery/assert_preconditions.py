#!/usr/bin/env python3
"""Assert preconditions are met before the DIMR release process is run."""

import os

from .dimr_context import DimrAutomationContext, create_context_from_args, parse_common_arguments
from .helpers.git_client import GitClient
from .helpers.ssh_client import SshClient
from .lib.atlassian import Atlassian
from .lib.teamcity import TeamCity
from .settings.general_settings import DRY_RUN_PREFIX, LINUX_ADDRESS, NETWORK_BASE_PATH


class PreconditionsHelper(object):
    """Class to check preconditions before running the main DIMR automation script."""

    def __init__(self, atlassian: Atlassian, teamcity: TeamCity, ssh_client: SshClient, git_client: GitClient) -> None:
        """
        Create a new instance of PreconditionsHelper.

        Args:
            atlassian (Atlassian): A wrapper for the Atlassian Confluence REST API.
            teamcity (TeamCity): A wrapper for the TeamCity REST API.
            ssh_client: A wrapper for a SSH client.
            git_client: A wrapper for a Git client.
        """
        self.__teamcity = teamcity
        self.__atlassian = atlassian
        self.__ssh_client = ssh_client
        self.__git_client = git_client

    def assert_preconditions(self, dry_run: bool) -> None:
        """Assert if all preconditions are met.

        Parameters
        ----------
        dry_run : bool
            Whether to run in dry-run mode without making actual connections.
        """
        print("Asserting if all preconditions are met...")

        if not self.__teamcity.test_api_connection(dry_run):
            raise AssertionError("Failed to connect to the TeamCity REST API.")

        if not self.__atlassian.test_api_connection(dry_run):
            raise AssertionError("Failed to connect to the Atlassian Confluence REST API.")

        print("Checking read/write access to the network drive...")
        try:
            if (os.access(NETWORK_BASE_PATH, os.W_OK) and os.access(NETWORK_BASE_PATH, os.R_OK)) or dry_run:
                if dry_run:
                    print(f"{DRY_RUN_PREFIX} Checking read/write access to {NETWORK_BASE_PATH}")
                print(f"Successfully checked for read and write access to {NETWORK_BASE_PATH}.")
            else:
                raise AssertionError(f"Could not read or write to {NETWORK_BASE_PATH}: insufficient permissions.")
        except Exception as e:
            raise AssertionError(f"Could not read or write to {NETWORK_BASE_PATH}:\n{e}.") from e

        print("Checking if ssh connection to Linux can be made...")
        try:
            self.__ssh_client.test_connection(LINUX_ADDRESS, dry_run)
        except Exception as e:
            raise AssertionError(f"Could not establish ssh connection to {LINUX_ADDRESS}:\n{e}") from e

        print("Checking if git connection can be made...")
        try:
            self.__git_client.test_connection(dry_run)
        except Exception as e:
            raise AssertionError(f"Could not establish git connection:\n{e}") from e

        print("Successfully asserted all preconditions.")


def assert_preconditions(context: DimrAutomationContext) -> None:
    """Assert some preconditions are met before the script is fully run.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Asserting preconditions...")

    if context.atlassian is None:
        raise ValueError("Atlassian client is required but not initialized")
    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")
    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")
    if context.git_client is None:
        raise ValueError("Git client is required but not initialized")

    preconditions = PreconditionsHelper(
        atlassian=context.atlassian,
        teamcity=context.teamcity,
        ssh_client=context.ssh_client,
        git_client=context.git_client,
    )
    preconditions.assert_preconditions(context.dry_run)

    print("Preconditions check completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args)

    print("Starting preconditions check...")
    assert_preconditions(context)
    print("Finished")
