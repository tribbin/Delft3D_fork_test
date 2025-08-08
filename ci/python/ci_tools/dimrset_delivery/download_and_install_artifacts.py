#!/usr/bin/env python3
"""Download the artifacts and install them on Linux machine."""

import os
import tarfile
import zipfile
from typing import List

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.lib.ssh_client import SshClient
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX, LINUX_ADDRESS
from ci_tools.dimrset_delivery.settings.teamcity_settings import (
    NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT,
    NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT,
    TeamcityIds,
)


def download_and_install_artifacts(context: DimrAutomationContext) -> None:
    """Download the artifacts and install them on Linux machine.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Downloading and installing artifacts...")

    # Get required information
    branch_name = context.get_branch_name()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would download artifacts for build from TeamCity:", context.build_id)
        print(f"{DRY_RUN_PREFIX} Would publish artifacts to network drive")
        print(f"{DRY_RUN_PREFIX} Would publish weekly DIMR via H7")
        return

    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")
    if context.ssh_client is None:
        raise ValueError("SSH client is required but not initialized")

    helper = ArtifactInstallHelper(
        teamcity=context.teamcity,
        ssh_client=context.ssh_client,
        dimr_version=dimr_version,
        branch_name=branch_name,
    )
    helper.publish_artifacts_to_network_drive(context.build_id)
    helper.publish_weekly_dimr_via_h7()

    print("Artifacts download and installation completed successfully!")


class ArtifactInstallHelper(object):
    """Class responsible for downloading, unpacking and installing the DIMR artifacts."""

    def __init__(
        self,
        teamcity: TeamCity,
        ssh_client: SshClient,
        dimr_version: str,
        branch_name: str,
    ) -> None:
        """
        Create a new instance of ArtifactInstallHelper.

        Arguments:
            teamcity (TeamCity): A reference to a TeamCity REST API wrapper.
            ssh_client (Ssh_Client): A wrapper for a SSH client.
            full_dimr_version (str): The full DIMR version to download and install.
        """
        self.__teamcity = teamcity
        self.__ssh_client = ssh_client
        self.__dimr_version = dimr_version
        self.__branch_name = branch_name

    def publish_artifacts_to_network_drive(self, build_id_chain: str) -> None:
        """Download the DIMR artifacts to the network drive."""
        windows_collect_id = self.__teamcity.get_dependent_build_id(
            build_id_chain, TeamcityIds.DELFT3D_WINDOWS_COLLECT_BUILD_TYPE_ID.value
        )
        linux_collect_id = self.__teamcity.get_dependent_build_id(
            build_id_chain, TeamcityIds.DELFT3D_LINUX_COLLECT_BUILD_TYPE_ID.value
        )

        self.__publish_artifact_to_file_share(
            str(windows_collect_id) if windows_collect_id is not None else "",
            NAME_OF_DIMR_RELEASE_SIGNED_WINDOWS_ARTIFACT,
        )
        self.__publish_artifact_to_file_share(
            str(linux_collect_id) if linux_collect_id is not None else "", NAME_OF_DIMR_RELEASE_SIGNED_LINUX_ARTIFACT
        )

    def publish_weekly_dimr_via_h7(self) -> None:
        """Installs DIMR on the Linux machine via SSH."""
        print(f"Installing DIMR on {LINUX_ADDRESS} via SSH...")

        # setup the command
        command = f"cd /p/d-hydro/dimrset/weekly/{self.__dimr_version}/lnx64/bin;"
        command += "./libtool_install.sh;"
        command += "cd /p/d-hydro/dimrset/weekly;"
        command += f"chgrp -R dl_acl_dsc {self.__dimr_version}/;"
        command += f"chmod -R a+x,a-s {self.__dimr_version}/;"

        # only update the latest symlink if we are on the main branch we don't do this for the release branche
        if self.__branch_name == "main":
            command += "unlink latest;"
            command += f"ln -s {self.__dimr_version} latest;"
            command += "cd /p/d-hydro/dimrset;"
            command += "unlink latest;"
            command += f"ln -s weekly/{self.__dimr_version} latest;"

        # execute command
        self.__ssh_client.execute(address=LINUX_ADDRESS, command=command)
        print(f"Successfully installed DIMR on {LINUX_ADDRESS}.")

    def __publish_artifact_to_file_share(self, build_id: str, artifact_name_key: str) -> None:
        """
        Download and unpack artifacts from a TeamCity build that match the specified artifact name key.

        Args:
            build_id (str): The ID of the TeamCity build to retrieve artifacts from.
            artifact_name_key (str): Substring to filter artifact names for downloading.

        Returns
        -------
            None
        """
        artifact_names = self.__teamcity.get_build_artifact_names(build_id=build_id)
        if artifact_names is None:
            raise ValueError(f"Could not retrieve artifact names for build {build_id}")
        artifacts_to_download = [a["name"] for a in artifact_names["file"] if artifact_name_key in a["name"]]
        self.__download_and_unpack_dimr_artifacts_via_h7(
            artifacts_to_download=artifacts_to_download,
            build_id=build_id,
        )

    def __download_and_unpack_dimr_artifacts_via_h7(self, artifacts_to_download: List[str], build_id: str) -> None:
        """
        Download the provided artifact names from the provided TeamCity build id.

        Args:
            artifacts_to_download List[str]: A list of artifact names to download.
            build_id (str): The build id for the build to download the artifacts from.
        """
        file_path = f"{self.__dimr_version}"
        if not os.path.exists(f"{file_path}"):
            os.makedirs(f"{file_path}")

        for artifact_to_download in artifacts_to_download:
            print(f"Downloading {artifact_to_download}...")
            artifact = self.__teamcity.get_build_artifact(build_id=build_id, path_to_artifact=artifact_to_download)

            if artifact is None:
                raise ValueError(f"Could not download artifact {artifact_to_download}")

            with open(artifact_to_download, "wb") as f:
                f.write(artifact)
            print(f"Unpacking {artifact_to_download}...")

            self.__extract_archive(artifact_to_download, file_path)
            print(f"Deleting {artifact_to_download}...")
            os.remove(artifact_to_download)

            remote_path = "/p/d-hydro/dimrset/weekly"
            self.__ssh_client.secure_copy(
                address=LINUX_ADDRESS,
                local_path=file_path,
                remote_path=remote_path,
            )
            print(f"Successfully deployed {artifact_to_download} to {remote_path}.")

    def __extract_archive(self, archive_path: str, target_path: str) -> None:
        """
        Extract a tar or zip archive to a target path.

        Parameters
        ----------
        archive_path: str
            The path to the archive (tar or zip).
        target_path: str
            The path to extract the archive to.

        Notes
        -----
        - The function removes the Unix permission bits when on Windows, as the permission bits are not
        supported on Windows.
        """
        if archive_path.endswith(".tar.gz") or archive_path.endswith(".tgz"):
            with tarfile.open(archive_path, "r:gz") as tar:
                for member in tar.getmembers():
                    # Strip the Unix permission bits when on Windows
                    if os.name == "nt":
                        member.mode = 0o644  # Default file permissions for Windows
                    tar.extract(member, path=target_path)
        elif archive_path.endswith(".zip"):
            with zipfile.ZipFile(archive_path, "r") as zip_ref:
                zip_ref.extractall(target_path)
        else:
            raise ValueError("Unsupported archive format. Only .tar.gz, .tgz, and .zip are supported.")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_git=False)

    print("Starting artifact download and installation...")
    download_and_install_artifacts(context)
    print("Finished")
