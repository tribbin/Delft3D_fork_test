import argparse
import os
from dataclasses import dataclass
from getpass import getpass
from typing import Dict, Optional

from ci_tools.dimrset_delivery.settings.teamcity_settings import Settings
from ci_tools.example_utils.logger import Logger, LogLevel


@dataclass
class ServiceRequirements:
    """
    Specifies which services are required for DIMR automation.

    This dataclass is used to indicate which external services are needed for a given automation run.
    """

    atlassian: bool = True
    teamcity: bool = True
    ssh: bool = True
    git: bool = True


@dataclass
class DimrCredentials:
    """
    Stores credentials for DIMR automation services.

    This dataclass holds usernames and passwords for all supported external services.
    """

    atlassian_username: Optional[str] = None
    atlassian_password: Optional[str] = None
    teamcity_username: Optional[str] = None
    teamcity_password: Optional[str] = None
    ssh_username: Optional[str] = None
    ssh_password: Optional[str] = None
    git_username: Optional[str] = None
    git_pat: Optional[str] = None


class DimrAutomationContext:
    """
    Shared context for DIMR automation steps.

    Provides access to credentials, requirements, settings, and cached data for automation scripts.
    """

    def __init__(
        self,
        build_id: str,
        dry_run: bool = False,
        credentials: Optional[DimrCredentials] = None,
        requirements: Optional[ServiceRequirements] = None,
        teamcity_logger: bool = False,
    ) -> None:
        """
        Initialize DIMR automation context.

        Parameters
        ----------
        build_id : str
            The TeamCity build ID.
        dry_run : bool, optional
            Whether to run in dry-run mode. Default is False.
        credentials : Optional[DimrCredentials], optional
            Credentials for various services. Default is None.
        requirements : Optional[ServiceRequirements], optional
            Requirements for various services. Default is None.
        """
        self.build_id = build_id
        self.dry_run = dry_run

        # Initialize requirements if not provided
        if requirements is None:
            requirements = ServiceRequirements()
        self.requirements = requirements

        # Initialize credentials if not provided
        if credentials is None:
            credentials = DimrCredentials()

        # Prompt for any missing required credentials
        self._prompt_for_credentials(credentials, requirements)
        self.credentials = credentials

        settings_path = os.path.join(os.path.dirname(__file__), "settings", "teamcity_settings.json")
        self.settings = Settings(settings_path)

        # Cache for commonly needed data
        self.kernel_versions: Dict[str, str] = {}
        self.dimr_version: str = ""
        self.branch_name: str = ""
        self.logger = Logger(teamcity_logger)

    def log(self, *args: object, sep: str = " ", severity: LogLevel = LogLevel.NORMAL) -> None:
        """
        Print status message with dry-run prefix if applicable.

        Parameters
        ----------
        args : object
            Objects to print.
        sep : str, optional
            Separator between objects. Default is a space.
        """
        message = f"{sep.join(str(arg) for arg in args)}"
        if self.dry_run:
            message = f"{self.settings.dry_run_prefix}{sep}{message}"

        self.logger.log(message, severity)

    def _prompt_for_credentials(self, credentials: DimrCredentials, requirements: ServiceRequirements) -> None:
        """
        Prompt for any missing required credentials and validate presence after prompting.

        Parameters
        ----------
        credentials : DimrCredentials
            Credentials object to fill in.
        requirements : ServiceRequirements
            Service requirements to check.

        Raises
        ------
        ValueError
            If any required credentials are missing after prompting.
        """
        if requirements.atlassian and (not credentials.atlassian_username or not credentials.atlassian_password):
            print("Atlassian/Confluence credentials:")
            credentials.atlassian_username = credentials.atlassian_username or input("Enter your Atlassian username:")
            credentials.atlassian_password = credentials.atlassian_password or getpass(
                prompt="Enter your Atlassian password:", stream=None
            )
        if requirements.teamcity and (not credentials.teamcity_username or not credentials.teamcity_password):
            print("TeamCity credentials:")
            credentials.teamcity_username = credentials.teamcity_username or input("Enter your TeamCity username:")
            credentials.teamcity_password = credentials.teamcity_password or getpass(
                prompt="Enter your TeamCity password:", stream=None
            )
        if requirements.ssh and (not credentials.ssh_username or not credentials.ssh_password):
            print("SSH (H7) credentials:")
            credentials.ssh_username = credentials.ssh_username or input("Enter your SSH username:")
            credentials.ssh_password = credentials.ssh_password or getpass(
                prompt="Enter your SSH password:", stream=None
            )
        if requirements.git and (not credentials.git_username or not credentials.git_pat):
            print("Git credentials:")
            credentials.git_username = credentials.git_username or input("Enter your Git username:")
            credentials.git_pat = credentials.git_pat or getpass(prompt="Enter your Git PAT:", stream=None)

        # Validation: raise ValueError if any required credentials are still missing or empty
        if requirements.atlassian:
            if not credentials.atlassian_username or not credentials.atlassian_password:
                raise ValueError("Atlassian credentials are required but not provided")
        if requirements.teamcity:
            if not credentials.teamcity_username or not credentials.teamcity_password:
                raise ValueError("TeamCity credentials are required but not provided")
        if requirements.ssh:
            if not credentials.ssh_username or not credentials.ssh_password:
                raise ValueError("SSH credentials are required but not provided")
        if requirements.git:
            if not credentials.git_username or not credentials.git_pat:
                raise ValueError("Git credentials are required but not provided")


def parse_common_arguments() -> argparse.Namespace:
    """
    Parse common command line arguments for DIMR automation scripts.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.
    """
    parser = argparse.ArgumentParser(description="DIMR Automation Script")

    parser.add_argument("--build_id", type=str, required=True, help="Build ID chain for the DIMR release")
    parser.add_argument(
        "--dry-run", action="store_true", default=False, help="Run in dry-run mode without making any changes"
    )

    parser.add_argument("--atlassian-username", type=str, default=None, help="Atlassian/Confluence username")
    parser.add_argument("--atlassian-password", type=str, default=None, help="Atlassian/Confluence password")

    parser.add_argument("--teamcity-username", type=str, default=None, help="TeamCity username")
    parser.add_argument("--teamcity-password", type=str, default=None, help="TeamCity password")

    parser.add_argument("--ssh-username", type=str, default=None, help="SSH username for H7 server")
    parser.add_argument("--ssh-password", type=str, default=None, help="SSH password for H7 server")

    parser.add_argument("--git-username", type=str, default=None, help="Git username")
    parser.add_argument("--git-PAT", type=str, default=None, help="Git Personal Access Token")

    return parser.parse_args()


def create_context_from_args(
    args: argparse.Namespace,
    require_atlassian: bool = True,
    require_git: bool = True,
    require_teamcity: bool = True,
    require_ssh: bool = True,
) -> DimrAutomationContext:
    """
    Create automation context from parsed arguments.

    Parameters
    ----------
    args : argparse.Namespace
        Parsed command line arguments.
    require_atlassian : bool, optional
        Whether Atlassian credentials are required. Default is True.
    require_git : bool, optional
        Whether Git credentials are required. Default is True.
    require_teamcity : bool, optional
        Whether TeamCity credentials are required. Default is True.
    require_ssh : bool, optional
        Whether SSH credentials are required. Default is True.

    Returns
    -------
    DimrAutomationContext
        The constructed automation context.
    """
    credentials = DimrCredentials(
        atlassian_username=args.atlassian_username,
        atlassian_password=args.atlassian_password,
        teamcity_username=args.teamcity_username,
        teamcity_password=args.teamcity_password,
        ssh_username=args.ssh_username,
        ssh_password=args.ssh_password,
        git_username=args.git_username,
        git_pat=getattr(args, "git_PAT", None),
    )

    requirements = ServiceRequirements(
        atlassian=require_atlassian, teamcity=require_teamcity, ssh=require_ssh, git=require_git
    )
    context = DimrAutomationContext(
        build_id=args.build_id,
        dry_run=args.dry_run,
        credentials=credentials,
        requirements=requirements,
        teamcity_logger=True,
    )

    return context
