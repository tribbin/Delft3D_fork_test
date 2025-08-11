#!/usr/bin/env python3
"""Pin and tag the appropriate builds in TeamCity."""

from ci_tools.dimrset_delivery.dimr_context import (
    DimrAutomationContext,
    create_context_from_args,
    parse_common_arguments,
)
from ci_tools.dimrset_delivery.lib.teamcity import TeamCity
from ci_tools.dimrset_delivery.settings.general_settings import DRY_RUN_PREFIX
from ci_tools.dimrset_delivery.settings.teamcity_settings import TeamcityIds


def pin_and_tag_builds_teamcity(teamcity: TeamCity, dimr_version: str, build_id_chain: str) -> None:
    """Tag all builds and pin the appropriate builds in TeamCity."""
    tag = f"DIMRset_{dimr_version}"
    teamcity.add_tag_to_build_with_dependencies(build_id_chain, tag=tag)
    # Only pin specific builds
    teamcity_ids_list = [member.value for member in TeamcityIds]
    build_ids_to_pin = teamcity.get_dependent_build_ids_with_filter(build_id_chain, teamcity_ids_list)
    build_ids_to_pin.append(build_id_chain)
    for build_id in build_ids_to_pin:
        teamcity.pin_build(build_id=build_id)


def pin_and_tag_builds(context: DimrAutomationContext) -> None:
    """Pin and tag the appropriate builds.

    Parameters
    ----------
    context : DimrAutomationContext
        The automation context containing necessary clients and configuration.
    """
    context.print_status("Pinning and tagging builds...")

    # Get required information
    kernel_versions = context.get_kernel_versions()
    dimr_version = context.get_dimr_version()

    if context.dry_run:
        print(f"{DRY_RUN_PREFIX} Would pin and tag builds in TeamCity for build chain:", context.build_id)
        print(f"{DRY_RUN_PREFIX} Would add tag:", f"DIMRset_{dimr_version}")
        print(
            f"{DRY_RUN_PREFIX} Would tag commit with:",
            f"commit={kernel_versions['build.vcs.number']}, tag=DIMRset_{dimr_version}",
        )
        return

    if context.teamcity is None:
        raise ValueError("TeamCity client is required but not initialized")
    if context.git_client is None:
        raise ValueError("Git client is required but not initialized")

    pin_and_tag_builds_teamcity(
        teamcity=context.teamcity,
        dimr_version=dimr_version,
        build_id_chain=context.build_id,
    )

    # Also tag the git commit
    context.git_client.tag_commit(kernel_versions["build.vcs.number"], f"DIMRset_{dimr_version}")

    print("Build pinning and tagging completed successfully!")


if __name__ == "__main__":
    args = parse_common_arguments()
    context = create_context_from_args(args, require_atlassian=False, require_ssh=False)

    print("Starting build pinning and tagging...")
    pin_and_tag_builds(context)
    print("Finished")
